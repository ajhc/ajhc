{-# OPTIONS_JHC -fffi #-}
module System.Environment
    (
      getArgs,       -- :: IO [String]
      getProgName,   -- :: IO String
      getEnv,        -- :: String -> IO String
      getEnvironment -- :: IO [(String,String)]
  ) where

import Foreign.C.String
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.C.Types
import qualified Jhc.Options

getArgs     :: IO [String]
getProgName :: IO String
getEnv      :: String -> IO String

getProgName = case Jhc.Options.target of
    Jhc.Options.GhcHs -> ghc_getProgName
    _ -> peek jhc_progname >>= peekCString

getArgs = case Jhc.Options.target of
    Jhc.Options.GhcHs -> ghc_getArgs
    _ -> do
        argc <- peek jhc_argc
        argv <- peek jhc_argv
        let f n = peekElemOff argv n >>= peekCString
        mapM f [0 .. fromIntegral argc - 1]


getEnv s = withCString s c_getenv >>= \p ->
    if p == nullPtr then fail ("getEnv: " ++ show s)  else peekCString p


foreign import unsafe ccall "stdlib.h getenv" c_getenv :: Ptr CChar -> IO (Ptr CChar)

foreign import ccall "&jhc_progname" jhc_progname :: Ptr CString
foreign import ccall "&jhc_argc" jhc_argc :: Ptr CInt
foreign import ccall "&jhc_argv" jhc_argv :: Ptr (Ptr CString)

ghc_getArgs :: IO [String]
ghc_getArgs =
    alloca $ \ p_argc ->
    alloca $ \ p_argv -> do
        getProgArgv p_argc p_argv
        p    <- peek p_argc
        argv <- peek p_argv
        let f n = peekElemOff argv n >>= peekCString
        mapM f [1 .. fromIntegral p - 1]


foreign import unsafe ccall "getProgArgv"
  getProgArgv :: Ptr CInt -> Ptr (Ptr CString) -> IO ()

ghc_getProgName :: IO String
ghc_getProgName =
  alloca $ \ p_argc ->
  alloca $ \ p_argv -> do
     getProgArgv p_argc p_argv
     argv <- peek p_argv
     peekElemOff argv 0 >>= peekCString



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
