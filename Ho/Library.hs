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
import Util.Gen
import Util.MD5(md5file)



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
        pkg <- readLibraryFile rfp mbcs
        let got' = Map.insert name pkg got
        foldM (\gm (pn,cs) -> loadP (Just cs) gm pn) got' $ libraryDeps pkg
      Just pkg | mbcs == Nothing               -> return got
               | mbcs == Just (libraryMD5 pkg) -> return got
               | otherwise                     -> fail ("Checksum mismatch for library "++name)

-- load libraries



loadLibraries :: IO Ho
loadLibraries = do
    ps <- foldM (loadP Nothing) Map.empty (optHls options)
    return $ mconcat (initialHo : map libraryHo (Map.elems ps))

-- Write a library and mutilate it to fit the description



createLibrary :: FilePath
              -> IO ()
createLibrary fp = do
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
    let noc _ hsm = fail ("createLibrary: won't compile anything, requested: "++show (map hsModuleName hsm))
    let allmods  = sort $ map Module (emods ++ hmods)
    let fun ho m = do mho <- checkForHoModule m
                      case mho of
                        Nothing      -> fail (show fp ++ ": could not find module " ++ show m)
                        Just (_,ho') -> return $ mappend ho ho'
    ho <- foldM fun mempty allmods
    let homods = sort $ Map.keys (hoExports ho)
    when (homods /= allmods) $
        putErrDie ("Final ho consists of wrong modules:\nexpected: \t"
                   ++show allmods++"\nencountered: \t"++show homods)
    let ho' = ho { hoExports = Map.difference (hoExports ho)
                               (Map.fromList [(Module x,()) | x <- hmods]) }
    let pdesc = [(packString n, packString v) | (n,v) <- desc ]
    let outName = case optOutName options of
            "hs.out" -> name ++ "-" ++ vers ++ ".hl"
            fn -> fn
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
    wdump FD.Progress $ putStrLn $ "Reading: " ++ show fp
    fc <- CharIO.readFile fp
    case parseLibraryDescription fc of
        Left err -> fail $ "Error reading library description file: " ++ show fp ++ " " ++ err
        Right ps -> return ps

-- IO with Libraries


readLibraryFile :: FilePath -> Maybe CheckSum -> IO Library
readLibraryFile fp mbcs = do
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
                    libraryHo  = ho
                  }

writeLibraryFile :: FilePath -> Library -> IO ()
writeLibraryFile fp pkg = recordHoFile (libraryHo pkg) [fp] hoh >> return ()
    where hoh = HoHeader 1 [] [] (libraryDesc pkg)
