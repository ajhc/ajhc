module Ho.Library(
    readDescFile,
    findLibrary,
    libraryList
    ) where

import Char
import Control.Monad
import System.IO
import System.Directory
import qualified Data.Map as Map

import GenUtil
import Ho.Type
import Options
import qualified CharIO
import qualified FlagDump as FD

type LibraryName = String




-- Write a library and mutilate it to fit the description




-------------------------
-- parse description file
-------------------------

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



libraryList :: IO [(LibraryName,FilePath)]
libraryList = Map.toList `fmap` getLibraryMap (optHlPath options)

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

