module Ho.Library(
    loadLibraries,
    createLibrary
    ) where

import Control.Monad(when,foldM)
import Data.List(sort)
import Data.Monoid
import qualified Data.Map as Map
import System.IO
import Char

import GenUtil
import Ho.Build
import Ho.LibraryMap
import Ho.Type
import HsSyn
import Options
import PackedString
import qualified CharIO
import qualified FlagDump as FD
import Util.MD5(md5file)
import Version(versionString)



data Library = Library {
    libraryDesc :: [(PackedString,PackedString)],
    libraryHo   :: Ho,
    libraryFP   :: FilePath,
    libraryMD5  :: CheckSum
    }
type LMap = Map.Map LibraryName Library

-- Load a library in a recursive fashion

libraryDeps :: Library -> [(LibraryName, CheckSum)]
libraryDeps = Map.toList . hoLibraries . libraryHo

loadP :: Maybe CheckSum -> LMap -> LibraryName -> IO LMap
loadP mbcs got name = do
    case Map.lookup name got of
      Nothing -> do
        rfp <- libraryMapFind name
        pkg <- readLibraryFile name rfp mbcs
        let got' = Map.insert name pkg got
        foldM (\gm (pn,cs) -> loadP (Just cs) gm pn) got' $ libraryDeps pkg
      Just pkg | mbcs == Nothing               -> return got
               | mbcs == Just (libraryMD5 pkg) -> return got
               | otherwise                     -> fail ("Checksum mismatch for library "++name)

-- load libraries



loadLibraries :: IO Ho
loadLibraries = do
    wdump FD.Progress $ putErrLn $ "Loading libraries: " ++ show (optHls options)
    ps <- foldM (loadP Nothing) Map.empty (optHls options)
    return $ fixupHo $ mconcat (initialHo : map libraryHo (Map.elems ps))

-- Write a library and mutilate it to fit the description



createLibrary ::
    FilePath
    -> ([Module] -> IO Ho)
    -> IO ()
createLibrary fp wtd = do
    putVerboseLn $ "Creating library from description file: " ++ show fp
    desc <- readDescFile fp
    when verbose2 $ mapM_ print desc
    let field x = lookup x desc
    let jfield x = maybe (error "createLibrary: description lacks required field "++show x) id $ field x
    let mfield x = maybe [] (words . map (\c -> if c == ',' then ' ' else c)) $ field x
    let name  = jfield "name"
        vers  = jfield "version"
        hmods = mfield "hidden-modules"
        emods = mfield "exposed-modules"
    let allmods  = sort $ map Module (emods ++ hmods)
    ho <- wtd (map Module emods)
    let outName = case optOutName options of
            "hs.out" -> name ++ "-" ++ vers ++ ".hl"
            fn -> fn
    let pdesc = [(packString n, packString v) | (n,v) <- ("jhc-hl-filename",outName):("jhc-description-file",fp):("jhc-compiled-by",versionString):desc, n /= "exposed-modules" ]
    writeLibraryFile outName $ Library pdesc ho "" 0

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



readDescFile :: FilePath -> IO [(String,String)]
readDescFile fp = do
    wdump FD.Progress $ putErrLn $ "Reading: " ++ show fp
    fc <- CharIO.readFile fp
    case parseLibraryDescription fc of
        Left err -> fail $ "Error reading library description file: " ++ show fp ++ " " ++ err
        Right ps -> return ps

-- IO with Libraries


readLibraryFile :: LibraryName -> FilePath -> Maybe CheckSum -> IO Library
readLibraryFile lname fp mbcs = do
    wdump FD.Progress $ putErrLn $ "Loading library: " ++ show lname ++ " @ " ++ show fp
    pkgCS <- md5file fp
    when (maybe False (pkgCS /=) mbcs) $
        putErrDie ("Loading library "++show fp++" failed: Checksum does not match")
    mho <- checkForHoFile fp
    case mho of
      Nothing       -> putErrDie ("Loading library "++fp++" failed due to missing dependencies")
      Just (hoh,ho) -> return $
          Library { libraryDesc= hohMetaInfo hoh,
                    libraryFP  = fp,
                    libraryMD5 = pkgCS,
                    libraryHo  = ho { hoModules = Map.map (const $ Right (lname,pkgCS)) $ hoModules ho }
                  }

writeLibraryFile :: FilePath -> Library -> IO ()
writeLibraryFile fp pkg = recordHoFile (libraryHo pkg) [fp] hoh >> return ()
    where hoh = HoHeader 1 [] [] (libraryDesc pkg)
