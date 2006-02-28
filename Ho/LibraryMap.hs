module Ho.LibraryMap
    (libraryMapFind,libraryList
    ) where

import Ho.Type
import Options(options,optHlPath)

import Data.Map as Map
import System.Directory
import System.IO.Unsafe

type LibraryMap = Map LibraryName FilePath

----

{-# NOINLINE globalLibraryMap #-}
globalLibraryMap :: LibraryMap
globalLibraryMap = unsafePerformIO $ getLibraryMap $ optHlPath options

----

libraryMapFind :: Monad m => LibraryName -> m FilePath
libraryMapFind pn = case Map.lookup pn globalLibraryMap of
                      Just x  -> return x
                      Nothing -> fail ("LibraryMap: Library "++pn++" not found!")

libraryList :: [(LibraryName,FilePath)]
libraryList = Map.toList globalLibraryMap

----

getLibraryMap :: [FilePath] -> IO LibraryMap
getLibraryMap fps = fmap unions $ mapM getPM fps

getPM fp = flip catch (\_ -> return Map.empty) $ do
    raw <- getDirectoryContents fp
    return $ Map.fromList $ flip concatMap raw $ \e ->
        case reverse e of
          ('l':'h':'.':r) -> [(reverse r,fp++"/"++e)]
          _               -> []

