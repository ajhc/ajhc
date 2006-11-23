module Ho.LibraryMap(
    libraryMapFind,
    loadedLibraries,
    libraryList
    ) where

import Ho.Type
import Options(options,optHlPath,optHls)
import Control.Monad.Identity

import qualified Data.Map as Map
import System.Directory
import System.IO.Unsafe

type LibraryMap = Map.Map LibraryName FilePath

----

{-# NOINLINE globalLibraryMap #-}
globalLibraryMap :: LibraryMap
globalLibraryMap = unsafePerformIO $ getLibraryMap $ optHlPath options


loadedLibraries = runIdentity $ do
    rs <- mapM libraryMapFind (optHls options)
    return $ map fst rs

----

libraryMapFind :: Monad m => LibraryName -> m (LibraryName,FilePath)
libraryMapFind pn =
  case Map.lookup pn globalLibraryMap of
    Just x  -> return (pn,x)
    Nothing -> case range (pn++"-") (pn++"-"++repeat maxBound) globalLibraryMap of
                 [] -> fail ("LibraryMap: Library "++pn++" not found!")
                 xs -> return $ last xs

libraryList :: [(LibraryName,FilePath)]
libraryList = Map.toList globalLibraryMap

---- range queries for Data.Map

range :: Ord k => k -> k -> Map.Map k v -> [(k,v)]
range low high = Map.toList . fst . Map.split high . snd . Map.split low

----

getLibraryMap :: [FilePath] -> IO LibraryMap
getLibraryMap fps = fmap Map.unions $ mapM getPM fps

getPM fp = flip catch (\_ -> return Map.empty) $ do
    raw <- getDirectoryContents fp
    return $ Map.fromList $ flip concatMap raw $ \e ->
        case reverse e of
          ('l':'h':'.':r) -> [(reverse r,fp++"/"++e)]
          _               -> []

