module Util.FilterInput (filterInput,readSystem,existCommand) where

import Control.Monad (when)
import Data.List
import System.IO
import System.Process
import System.Exit
import Text.Printf
import Util.Gen
import qualified Data.ByteString.Lazy.Char8 as LBS


filterInput :: String -> [String] -> Handle -> IO String
filterInput prog args ifh = do
    input <- hGetContents ifh
    (rExit, rStdout, _) <- readProcessWithExitCode prog args input
    length rStdout `seq` when (rExit /= ExitSuccess) $
      putErrDie (prog ++ " exited abnormally")
    return rStdout

readSystem :: String -> [String] -> IO LBS.ByteString
readSystem prog args = do
    (rExit, rStdout, _) <- readProcessWithExitCode prog args ""
    when (rExit /= ExitSuccess) $
      putErrDie (printf "'%s' exited abnormally (%s)" (intercalate " " (prog:args)) (show rExit))
    return $ LBS.pack rStdout

existCommand :: String -> IO Bool
existCommand cmd = do
  (rExit, _, _) <- readProcessWithExitCode "which" [cmd] ""
  return $ if rExit == ExitSuccess then True else False
