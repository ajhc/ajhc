{-# OPTIONS_JHC -fffi #-}
module System.Environment
    (
      getArgs,       -- :: IO [String]
      getProgName,   -- :: IO String
      getEnv,        -- :: String -> IO String
      getEnvironment -- :: IO [(String,String)]
  ) where


import System
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Array
import Foreign.C.String



-- |'getEnvironment' retrieves the entire environment as a
-- list of @(key,value)@ pairs.
--
-- If an environment entry does not contain an @\'=\'@ character,
-- the @key@ is the whole entry and the @value@ is the empty string.

getEnvironment :: IO [(String, String)]
getEnvironment = do
   pBlock <- peek c_environ
   if pBlock == nullPtr then return []
    else do
      stuff <- peekArray0 nullPtr pBlock >>= mapM peekCString
      return (map divvy stuff)
  where
   divvy str =
      case break (=='=') str of
        (xs,[])        -> (xs,[]) -- don't barf (like Posix.getEnvironment)
        (name,_:value) -> (name,value)

foreign import ccall "&environ" c_environ :: Ptr (Ptr CString)

