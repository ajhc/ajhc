module Ho.Library(
    readDescFile,
    findLibrary,
    collectLibraries,
    libModMap,
    libHash,
    libMgHash,
    libProvides,
    libName,
    libBaseName,
    libHoLib,
    listLibraries
    ) where

import Char
import Control.Monad
import Data.List
import Data.Maybe
import Data.Version(showVersion)
import System.Directory
import System.IO
import Text.Printf
import qualified Data.Map as Map
import qualified Data.Set as Set

import Data.Monoid
import GenUtil
import Ho.Binary
import Ho.Type
import Options
import PackedString(PackedString,packString,unpackPS)
import qualified CharIO
import qualified FlagDump as FD
import qualified Support.MD5 as MD5

libModMap (Library _ libr _ _) = hoModuleMap libr
libHash (Library hoh _ _ _) = hohHash hoh
libMgHash mg lib = MD5.md5String $ show (libHash lib,mg)
libProvides mg (Library _ lib _ _) = [ m | (m,mg') <- Map.toList (hoModuleMap lib), mg == mg']
libName (Library HoHeader { hohName = ~(Right (name,vers)) } _ _ _) = unpackPS name ++ "-" ++ showVersion vers
libBaseName (Library HoHeader { hohName = ~(Right (name,vers)) } _ _ _) = name
libModules (Library _ lib _ _) = ([ m | (m,_) <- Map.toList (hoModuleMap lib)],Map.toList (hoReexports lib))
libHoLib (Library _ lib _ _) = lib

libVersionCompare ~(Library HoHeader { hohName = Right (_,v1) } _ _ _ ) ~(Library HoHeader { hohName =  Right (_,v2) } _ _ _) = compare v1 v2

type LibraryName = String

---------------------------------------
-- parse description file (.cabal file)
---------------------------------------

readDescFile :: FilePath -> IO [(String,String)]
readDescFile fp = do
    wdump FD.Progress $ putErrLn $ "Reading: " ++ show fp
    fc <- CharIO.readFile fp
    case parseLibraryDescription fc of
        Left err -> fail $ "Error reading library description file: " ++ show fp ++ " " ++ err
        Right ps -> return ps

parseLibraryDescription :: Monad m => String -> m [(String,String)]
parseLibraryDescription fs =  g [] (lines (f [] fs)) where
    --f rs ('\n':s:xs) | isSpace s = f rs (dropWhile isSpace xs)
    f rs ('-':'-':xs) = f rs (dropWhile (/= '\n') xs)
    f rs ('{':'-':xs) = eatCom rs xs
    f rs (x:xs) = f (x:rs) xs
    f rs [] = reverse rs
    eatCom rs ('\n':xs) = eatCom ('\n':rs) xs
    eatCom rs ('-':'}':xs) = f rs xs
    eatCom rs (_:xs) = eatCom rs xs
    eatCom rs [] = f rs []
    g rs (s:ss) | all isSpace s = g rs ss
    g rs (s:s':ss) | all isSpace s' = g rs (s:ss)
    g rs (s:(h:cl):ss) | isSpace h = g rs ((s ++ h:cl):ss)
    g rs (r:ss) | (':':bd') <- bd = g ((map toLower $ condenseWhitespace nm,condenseWhitespace bd'):rs) ss
         | otherwise = fail $ "could not find ':' marker: " ++ show (rs,(r:ss)) where
            (nm,bd) = break (== ':') r
    g rs [] = return rs

condenseWhitespace xs =  reverse $ dropWhile isSpace (reverse (dropWhile isSpace (cw xs))) where
    cw (x:y:zs) | isSpace x && isSpace y = cw (' ':zs)
    cw (x:xs) = x:cw xs
    cw [] = []


--------------------------------
-- finding and listing libraries
--------------------------------

type LibraryMap = Map.Map LibraryName FilePath

findLibrary ::  LibraryName -> IO (LibraryName,FilePath)
findLibrary pn = do
    lm <- getLibraryMap (optHlPath options)
    case Map.lookup pn lm of
        Just x  -> return (pn,x)
        Nothing -> case range (pn++"-") (pn++"-"++repeat maxBound) lm of
                 [] -> fail ("LibraryMap: Library "++pn++" not found!")
                 xs -> return $ last xs


listLibraries :: IO ()
listLibraries = do
    putStrLn "Search path:"
    mapM_ putStrLn (optHlPath options)
    putStrLn "Libraries found:"
    (_,byhashes) <- fetchAllLibraries
    let nameComp a b = compare (libName a) (libName b)
    forM_ (sortBy nameComp $ Map.elems byhashes) $ \ lib -> putStrLn (libName lib)



---- range queries for Data.Map

range :: Ord k => k -> k -> Map.Map k v -> [(k,v)]
range low high = Map.toList . fst . Map.split high . snd . Map.split low

----

getLibraryMap :: [FilePath] -> IO LibraryMap
getLibraryMap fps = fmap Map.unions $ mapM getPM fps where
    getPM fp = flip catch (\_ -> return Map.empty) $ do
        raw <- getDirectoryContents fp
        return $ Map.fromList $ flip concatMap raw $ \e ->
            case reverse e of
              ('l':'h':'.':r) -> [(reverse r,fp++"/"++e)]
              _               -> []

maxBy c x1 x2 = case x1 `c` x2 of
    LT -> x2
    _ -> x1

-- Collect all libraries and return those which are explicitly and implicitly imported.
--
-- The basic process is:
--    - Find all libraries and create two indexes, a map of named libraries to
--      the newest version of them, and a map of library hashes to the libraries
--      themselves.
--
--    - For all the libraries listed on the command line, find the newest
--      version of each of them, flag these as the explicitly imported libraries.
--
--    - recursively find the dependencies by the hash's listed in the library deps. if the names
--      match a library already loaded, ensure the hash matches up. flag these libraries as 'implicit' unless
--      already flaged 'explicit'
--
--    - perform sanity checks on final lists of implicit and explicit libraries.
--
-- Library Checks needed:
--    - We have found versions of all libraries listed on the command line
--    - We have all dependencies of all libraries and the hash matches the proper library name
--    - no libraries directly export the same modules, (but re-exporting the same module is fine)
--    - conflicting versions of any particular library are not required due to dependencies
--

fetchAllLibraries :: IO (Map.Map PackedString Library,Map.Map HoHash Library)
fetchAllLibraries = ans where
    ans = do
        (bynames',byhashes') <- unzip `fmap` concatMapM f (optHlPath options)
        let bynames = Map.unionsWith vcomb bynames'
            byhashes = Map.unions byhashes'
            vcomb = maxBy libVersionCompare
        return (bynames,byhashes)

    f fp = do
        fs <- flip catch (\_ -> return [] ) $ getDirectoryContents fp
        flip mapM fs $ \e -> case reverse e of
            ('l':'h':'.':r)  -> do
                flip catch (\_ -> return mempty) $ do
                    lib <- readHlFile  (fp ++ "/" ++ e)
                    return (Map.singleton (libBaseName lib) lib, Map.singleton (libHash lib) lib)
            _               -> return mempty

collectLibraries :: IO ([Library],[Library])
collectLibraries = ans where
    ans = do
        (bynames,byhashes) <- fetchAllLibraries
        let f pn | Just x <- Map.lookup pn bynames = return x
                 | otherwise = putErrDie $ printf "Library was not found '%s'\n" (unpackPS pn)
        es <- mapM f ( map packString $ optHls options)
        checkForModuleConficts es
        let f lmap _ [] = return lmap
            f lmap lset ((ei,l):ls)
                | libHash l `Set.member` lset = f lmap lset ls
                | otherwise = case Map.lookup (libBaseName l) lmap of
                    Nothing -> f (Map.insert (libBaseName l) (ei,l) lmap) (Set.insert (libHash l) lset) (ls ++ newdeps)
                    Just (ei',l') | libHash l == libHash l' -> f  (Map.insert (libBaseName l) (ei || ei',l) lmap) lset ls
                    Just (_,l')  -> putErrDie $ printf  "Conflicting versions of library '%s' are required. [%s]\n" (libName l) (show (libHash l,libHash l'))
              where newdeps = [ (False,fromMaybe (error $ printf "Dependency '%s' with hash '%s' needed by '%s' was not found" (unpackPS p) (show h) (libName l)) (Map.lookup h byhashes)) | let Library HoHeader { hohLibDeps = ldeps } _ _ _ = l , (p,h) <- ldeps ]
        finalmap <- f Map.empty Set.empty [ (True,l) | l <- es ]
        checkForModuleConficts [ l | (_,l) <- Map.elems finalmap ]
        when verbose $ forM_ (Map.toList finalmap) $ \ (n,(e,l)) -> do
            printf "-- Base: %s Exported: %s Hash: %s Name: %s\n" (unpackPS n) (show e) (show $ libHash l) (libName l)

        return ([ l | (True,l) <- Map.elems finalmap ],[ l | (False,l) <- Map.elems finalmap ])

    checkForModuleConficts ms = do
        let mbad = Map.toList $ Map.filter (\c -> case c of [_] -> False; _ -> True)  $ Map.fromListWith (++) [ (m,[l]) | l <- ms, m <- fst $ libModules l]
        forM_ mbad $ \ (m,l) -> putErrLn $ printf "Module '%s' is exported by multiple libraries: %s" (show m) (show $ map libName l)
        unless (null mbad) $ putErrDie "There were conflicting modules!"


