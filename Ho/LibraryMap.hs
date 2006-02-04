module Ho.LibraryMap 
    (libraryMapFind,libraryList,LibraryName,CheckSum
    ) where

import Options(options,optHlPath)

import Data.Char(isAlphaNum)
import Data.List(intersperse,sort)
import Data.Map as Map
import Data.Version
import System.Directory
import System.IO.Unsafe
import Text.ParserCombinators.ReadP

type CheckSum = Integer
type LibraryName= String
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

getPM fp = do
    raw <- getDirectoryContents fp
    return $ Map.fromList $ flip concatMap raw $ \e ->
        case reverse e of
          ('l':'h':'.':r) -> [(reverse r,fp++"/"++e)]
          _               -> []

