module Util.FilterInput (filterInput,readSystem) where

import Control.Monad (when)
import Data.List
import System
import System.IO
import System.Posix
import Text.Printf
import Util.Gen
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS


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

readSystem :: String -> [String] -> IO LBS.ByteString
readSystem prog args = do
    (rfd,wfd) <- createPipe
    pid <- forkProcess (do dupAndClose wfd stdOutput
                           executeFile prog True args Nothing
                           putErrDie "exec failed")
    closeFd wfd
--    printf "readSystem %s %s\n" prog (show args)
    str <- BS.hGetContents =<< fdToHandle rfd
    ret <- getProcessStatus True False pid
    when (ret /= Just (Exited ExitSuccess)) $ putErrDie (printf "'%s' exited abnormally (%s)" (intercalate " " (prog:args)) (show ret))
    return $ LBS.fromChunks [str]


