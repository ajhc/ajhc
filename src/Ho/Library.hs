module Ho.Library(
    LibDesc(..),
    readDescFile,
    collectLibraries,
    libModMap,
    libHash,
    libMgHash,
    libProvides,
    libName,
    libBaseName,
    libHoLib,
    preprocess,
    listLibraries
    ) where

import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Version
import Data.Yaml.Syck
import System.Directory
import Text.Printf
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified System.FilePath as FP

import Ho.Binary
import Ho.ReadSource
import Ho.Type
import Name.Name(Module)
import Options
import PackedString(PackedString,packString,unpackPS)
import Util.Gen
import Util.YAML
import qualified FlagDump as FD
import qualified FlagOpts as FO
import qualified Support.MD5 as MD5

libModMap = hoModuleMap . libHoLib
libHash  = hohHash . libHoHeader
libMgHash mg lib = MD5.md5String $ show (libHash lib,mg)
libProvides mg lib = [ m | (m,mg') <- Map.toList (libModMap lib), mg == mg']
libName lib = let HoHeader { hohName = ~(Right (name,vers)) } = libHoHeader lib in unpackPS name ++ "-" ++ showVersion vers
libVersion lib = let HoHeader { hohName = ~(Right (_name,vers)) } = libHoHeader lib in vers
libBaseName lib = let HoHeader { hohName = ~(Right (name,_vers)) } = libHoHeader lib in name
libModules l = let lib = libHoLib l in ([ m | (m,_) <- Map.toList (hoModuleMap lib)],Map.toList (hoReexports lib))

libVersionCompare l1 l2 = compare (libVersion l1) (libVersion l2)

--------------------------------
-- finding and listing libraries
--------------------------------

instance ToNode Module where
    toNode m = toNode $ show m
instance ToNode HoHash where
    toNode m = toNode $ show m
instance ToNode PackedString where
    toNode m = toNode $ unpackPS m

listLibraries :: IO ()
listLibraries = do
    (_,byhashes) <- fetchAllLibraries
    let libs = Map.toList byhashes
    if not verbose then putStr $ showYAML (sort $ map (libName . snd) libs) else do
    let f (h,l) = (show h,[
            ("Name",toNode (libName l)),
            ("BaseName",toNode (libBaseName l)),
            ("Version",toNode (showVersion $ libVersion l)),
            ("FilePath",toNode (libFileName l)),
            ("LibDeps",toNode [ h | (_,h) <- hohLibDeps (libHoHeader l)]),
            ("Exported-Modules",toNode $ mod ++ fsts rmod)
            ]) where
          (mod,rmod) = libModules l
    putStr $ showYAML (map f libs)

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

fetchAllLibraries :: IO (Map.Map PackedString [Library],Map.Map HoHash Library)
fetchAllLibraries = ans where
    ans = do
        (bynames',byhashes') <- unzip `fmap` concatMapM f (optHlPath options)
        let bynames = Map.map (reverse . sortBy libVersionCompare) $ Map.unionsWith (++) bynames'
            byhashes = Map.unions byhashes'
        return (bynames,byhashes)
    f fp = do
        fs <- flip catch (\_ -> return [] ) $ getDirectoryContents fp
        forM fs $ \e -> case reverse e of
            ('l':'h':'.':r)  -> flip catch (\_ -> return mempty) $ do
                lib <- readHlFile  (fp ++ "/" ++ e)
                return (Map.singleton (libBaseName lib) [lib], Map.singleton (libHash lib) lib)
            _               -> return mempty

splitOn' :: (a -> Bool) -> [a] -> [[a]]
splitOn' f xs = split xs
  where split xs = case break f xs of
          (chunk,[])     -> [chunk]
          (chunk,_:rest) -> chunk : split rest

splitVersion :: String -> (String,Data.Version.Version)
splitVersion s = ans where
    ans = case reverse (splitOn' ('-' ==) s) of
        (vrs:bs@(_:_)) | Just vrs <- runReadP parseVersion vrs -> (intercalate "-" (reverse bs),vrs)
        _ -> (s,Data.Version.Version [] [])

collectLibraries :: [String] -> IO ([Library],[Library])
collectLibraries libs = ans where
    ans = do
        (bynames,byhashes) <- fetchAllLibraries
        let f (pn,vrs) = lname pn vrs `mplus` lhash pn vrs where
                lname pn vrs = do
                    xs <- Map.lookup (packString pn) bynames
                    (x:_) <- return $ filter isGood xs
                    return x
                isGood lib = versionBranch vrs `isPrefixOf` versionBranch (libVersion lib)
                lhash pn vrs = do
                    [] <- return $ versionBranch vrs
                    Map.lookup pn byhashes'
            byhashes' = Map.fromList [ (show x,y) | (x,y) <- Map.toList byhashes]
        let es' = [ (x,f $ splitVersion x) | x <- libs ]
            es = [ l | (_,Just l) <- es' ]
            bad = [ n | (n,Nothing) <- es' ]
        unless (null bad) $ do
            putErrLn "Libraries not found:"
            forM_ bad $ \b -> putErrLn ("    " ++ b)
            exitFailure

        checkForModuleConficts es
        let f lmap _ [] = return lmap
            f lmap lset ((ei,l):ls)
                | libHash l `Set.member` lset = f lmap lset ls
                | otherwise = case Map.lookup (libBaseName l) lmap of
                    Nothing -> f (Map.insert (libBaseName l) (ei,l) lmap) (Set.insert (libHash l) lset) (ls ++ newdeps)
                    Just (ei',l') | libHash l == libHash l' -> f  (Map.insert (libBaseName l) (ei || ei',l) lmap) lset ls
                    Just (_,l')  -> putErrDie $ printf  "Conflicting versions of library '%s' are required. [%s]\n" (libName l) (show (libHash l,libHash l'))
              where newdeps = [ (False,fromMaybe (error $ printf "Dependency '%s' with hash '%s' needed by '%s' was not found" (unpackPS p) (show h) (libName l)) (Map.lookup h byhashes)) | let HoHeader { hohLibDeps = ldeps } = libHoHeader l , (p,h) <- ldeps ]
        finalmap <- f Map.empty Set.empty [ (True,l) | l <- es ]
        checkForModuleConficts [ l | (_,l) <- Map.elems finalmap ]
        when verbose $ forM_ (Map.toList finalmap) $ \ (n,(e,l)) ->
            printf "-- Base: %s Exported: %s Hash: %s Name: %s\n" (unpackPS n) (show e) (show $ libHash l) (libName l)

        return ([ l | (True,l) <- Map.elems finalmap ],[ l | (False,l) <- Map.elems finalmap ])

    checkForModuleConficts ms = do
        let mbad = Map.toList $ Map.filter (\c -> case c of [_] -> False; _ -> True)  $ Map.fromListWith (++) [ (m,[l]) | l <- ms, m <- fst $ libModules l]
        forM_ mbad $ \ (m,l) -> putErrLn $ printf "Module '%s' is exported by multiple libraries: %s" (show m) (show $ map libName l)
        unless (null mbad) $ putErrDie "There were conflicting modules!"

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
         | otherwise = fail $ "could not find ':' marker: " ++ show (rs,r:ss) where
            (nm,bd) = break (== ':') r
    g rs [] = return rs

condenseWhitespace xs =  reverse $ dropWhile isSpace (reverse (dropWhile isSpace (cw xs))) where
    cw (x:y:zs) | isSpace x && isSpace y = cw (' ':zs)
    cw (x:xs) = x:cw xs
    cw [] = []

procCabal :: [(String,String)] -> LibDesc
procCabal xs = f xs mempty mempty where
    f [] dlm dsm = LibDesc (combineAliases dlm) dsm
    f ((map toLower -> x,y):rs) dlm dsm | x `Set.member` list_fields = f rs (Map.insert x (spit y) dlm) dsm
                                        | otherwise = f rs dlm (Map.insert x y dsm)
    spit = words . map (\c -> if c == ',' then ' ' else c)


procYaml :: YamlNode -> LibDesc
procYaml MkNode { n_elem = EMap ms } = f ms mempty mempty where
    f [] dlm dsm = LibDesc (combineAliases dlm) dsm
    f ((n_elem -> EStr (map toLower . unpackBuf -> x),y):rs) dlm dsm = if x `Set.member` list_fields then dlist y else dsing y where
        dlist (n_elem -> EStr y)  = f rs (Map.insert x [unpackBuf y] dlm) dsm
        dlist (n_elem -> ESeq ss) = f rs (Map.insert x [ unpackBuf y | (n_elem -> EStr y) <- ss ] dlm) dsm
        dlist _ = f rs dlm dsm
        dsing (n_elem -> EStr y) = f rs dlm (Map.insert x (unpackBuf y) dsm)
        dsing _ = f rs dlm dsm
    f (_:xs) dlm dsm = f xs dlm dsm
procYaml _ = LibDesc mempty mempty

list_fields = Set.fromList $ [
    "exposed-modules",
    "include-dirs",
    "extensions",
    "options",
    "build-depends"
    ] ++ map fst alias_fields
      ++ map snd alias_fields

alias_fields = [
   ("other-modules","hidden-modules"),
   ("hs-source-dir","hs-source-dirs")
   ]

combineAliases mp = f alias_fields mp where
    f [] mp = mp
    f ((x,y):rs) mp = case Map.lookup x mp of
        Nothing -> f rs mp
        Just ys -> f rs $ Map.delete x $ Map.insertWith (++) y ys mp


data LibDesc = LibDesc (Map.Map String [String]) (Map.Map String String)

readDescFile :: FilePath -> IO LibDesc
readDescFile fp = do
    wdump FD.Progress $ putErrLn $ "Reading: " ++ show fp
    let doYaml opt = do
            lbs <- LBS.readFile fp
            dt <- preprocess opt fp lbs
            desc <- catch (parseYamlBytes $ BS.concat (LBS.toChunks dt))
                (\e -> putErrDie $ "Error parsing desc file '" ++ fp ++ "'\n" ++ show e)
            when verbose2 $ do
                yaml <- emitYaml desc
                putStrLn yaml
            return $ procYaml desc
        doCabal = do
            fc <- readFile fp
            case parseLibraryDescription fc of
                Left err -> fail $ "Error reading library description file: " ++ show fp ++ " " ++ err
                Right ps -> return $ procCabal ps
    case FP.splitExtension fp of
        (_,".cabal") -> doCabal
        (_,".yaml") -> doYaml options
        (FP.takeExtension -> ".yaml",".m4") -> doYaml options { optFOptsSet = FO.M4 `Set.insert` optFOptsSet options }
        _ -> putErrDie $ "Do not recoginize description file type: " ++ fp
