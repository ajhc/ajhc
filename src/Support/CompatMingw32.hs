{-# LANGUAGE CPP #-}
module Support.CompatMingw32 (
  createLinkCompat,
  raiseSigIntCompat,
  noEscapePath,
  systemCompat
  ) where

import System.Process
import System.Exit
#ifdef mingw32_HOST_OS
import System.Directory
import System.Win32.Console
#else
import System.Posix.Files
import System.Posix.Signals
#endif

createLinkCompat :: FilePath -> FilePath -> IO ()
#ifdef mingw32_HOST_OS
createLinkCompat = copyFile
#else
createLinkCompat = createLink
#endif

raiseSigIntCompat :: IO ()
#ifdef mingw32_HOST_OS
raiseSigIntCompat = generateConsoleCtrlEvent cTRL_C_EVENT 0
#else
raiseSigIntCompat = raiseSignal sigINT
#endif

noEscapePath :: String -> String
#ifdef mingw32_HOST_OS
-- Windows filepath backslash confuses jhc's lexer with escape code.
noEscapePath = replace '\\' '/'
  where replace :: Eq a => a -> a -> [a] -> [a]
        replace x y = map (\z -> if z == x then y else z)
#else
noEscapePath = id
#endif

systemCompat :: String -> IO ExitCode
#ifdef mingw32_HOST_OS
systemCompat s = system $ "sh -c \"" ++ s ++ "\""
#else
systemCompat = system
#endif
