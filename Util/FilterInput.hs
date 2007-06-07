module Util.FilterInput (filterInput,readSystem) where

import CharIO
import Control.Monad (when)
import System
import System.IO(Handle)
import System.Posix


filterInput :: String -> [String] -> Handle -> IO String
filterInput prog args ifh = do
    (rfd,wfd) <- createPipe
    ifd <- handleToFd ifh
    pid <- forkProcess (do dupAndClose ifd stdInput
                           dupAndClose wfd stdOutput
                           executeFile prog True args Nothing
                           putErrDie "exec failed")
    closeFd wfd
    str <- hGetContents =<< fdToHandle rfd
    ret <- length str `seq` getProcessStatus True False pid
    when (ret /= Just (Exited ExitSuccess)) $ putErrDie (prog ++ " exited abnormally")
    return str

dupAndClose :: Fd -> Fd -> IO ()
dupAndClose from to = dupTo from to >> closeFd from

readSystem :: String -> [String] -> IO String
readSystem prog args = do
    (rfd,wfd) <- createPipe
    pid <- forkProcess (do dupAndClose wfd stdOutput
                           executeFile prog True args Nothing
                           putErrDie "exec failed")
    closeFd wfd
    str <- hGetContents =<< fdToHandle rfd
    ret <- length str `seq` getProcessStatus True False pid
    when (ret /= Just (Exited ExitSuccess)) $ putErrDie (prog ++ " exited abnormally")
    return str


