{-# OPTIONS_JHC -fffi #-}
module System.Exit (
    ExitCode(ExitSuccess,ExitFailure),
    exitWith, exitFailure, exitSuccess
    ) where

import Jhc.IO(exitFailure)

data ExitCode = ExitSuccess | ExitFailure !Int
            deriving (Eq, Ord, Read, Show)

-- exitFailure :: IO a
exitSuccess :: IO a
exitSuccess = exitWith ExitSuccess

exitWith    :: ExitCode -> IO a
exitWith ExitSuccess = do
    c_exit 0
    return undefined
exitWith (ExitFailure n) = do
    c_exit n
    return undefined

foreign import unsafe ccall "exit" c_exit :: Int -> IO ()
