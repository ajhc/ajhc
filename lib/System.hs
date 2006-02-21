module System (
    ExitCode(ExitSuccess,ExitFailure),
    getArgs, getProgName, getEnv, system, exitWith, exitFailure
  ) where

import Foreign.C.String
import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Types

data ExitCode = ExitSuccess | ExitFailure Int
            deriving (Eq, Ord, Read, Show)

getArgs     :: IO [String]
getProgName :: IO String
getEnv      :: String -> IO String
system      :: String -> IO ExitCode
exitWith    :: ExitCode -> IO a
exitFailure :: IO a


exitWith ExitSuccess = do
    c_exit 0
    return undefined
exitWith (ExitFailure n) = do
    c_exit n
    return undefined
exitFailure = exitWith $ ExitFailure 255



getProgName = peek jhc_progname >>= peekCString
getArgs = do
    argc <- peek jhc_argc
    argv <- peek jhc_argv
    let f n = peekElemOff argv n >>= peekCString
    mapM f [0 .. fromIntegral argc - 1]


getEnv s = withCString s c_getenv >>= \p ->
    if p == nullPtr then fail ("getEnv: " ++ show s)  else peekCString p


system s = withCString s c_system >>= \r -> case r of
    0 -> return ExitSuccess
    _ -> return $ ExitFailure (fromIntegral r)

foreign import ccall "exit" c_exit :: Int -> IO ()
foreign import ccall "system" c_system :: CString -> IO CInt
foreign import ccall "stdlib.h getenv" c_getenv :: Ptr CChar -> IO (Ptr CChar)

foreign import ccall "&jhc_progname" jhc_progname :: Ptr CString
foreign import ccall "&jhc_argc" jhc_argc :: Ptr CInt
foreign import ccall "&jhc_argv" jhc_argv :: Ptr (Ptr CString)
