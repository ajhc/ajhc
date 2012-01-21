{-# LANGUAGE ForeignFunctionInterface #-}
-- Various routines for dealing with temporary directories and files.
module Support.TempDir(
    getTempDir,
    createTempFile,
    fileInTempDir,
    wrapMain
   ) where

import Control.Exception as E
import Control.Monad
import Data.IORef
import Data.List
import Data.Maybe
import Foreign.C
import System.Directory
import System.Exit
import System.FilePath as FP
import System.IO
import System.IO.Unsafe
import Text.Printf

data TempDir = TempDir {
    tempDirClean :: Bool,  -- ^ whether to delete the directory afterwords.
    tempDirDump :: Bool,
    tempDirPath :: Maybe String,
    tempDirCleanup :: [FilePath]
    }

putLog :: String -> IO ()
putLog = putStrLn
--log _ = return ()

getTempDir :: IO FilePath
getTempDir = do
    td <- readIORef tdRef
    case tempDirPath td of
        Just fp -> return fp
        Nothing -> do
            fp <- mkdtemp "/tmp/jhc_XXXXXX"
            putLog $ printf "Created temporary dir '%s'" fp
            writeIORef tdRef td { tempDirPath = Just fp }
            return fp

createTempFile :: FilePath -> IO (FilePath, Handle)
createTempFile fp = do
    dir <- getTempDir
    (fp,h) <- openBinaryTempFile dir (if null fp then "temp.tmp" else fp)
    putLog $ printf "Created temporary file '%s'" fp
    addCleanup fp
    return (fp,h)

fileInTempDir :: FilePath -> IO FilePath
fileInTempDir fp = do
    dir <- getTempDir
    addCleanup fp
    return (dir </> fp)

cleanUp :: IO ()
cleanUp = do
    td <- readIORef tdRef
    if not (tempDirClean td) || isNothing (tempDirPath td) then return () else do
    dir <- getTempDir
    forM_ (tempDirCleanup td) $ \fp -> do
        putLog $ printf "Removing '%s'" (dir </> fp)
        ignoreError (removeFile $ dir </> fp)
    putLog $ printf "Removing '%s'" dir
    ignoreError (removeDirectory dir)

addCleanup :: FilePath -> IO ()
addCleanup fp = do
    td <- readIORef tdRef
    writeIORef tdRef td { tempDirCleanup = fp:tempDirCleanup td }

wrapMain :: IO () -> IO ()
wrapMain main = E.catch (main >> cleanUp) $ \e -> case fromException e of
    Just code -> cleanUp >>  exitWith code
    Nothing -> do
        td <- readIORef tdRef
        case tempDirPath td of
            Just td -> hPutStrLn stderr $ printf "Exiting abnormally. Work directory is '%s'" td
            _ -> return ()
        throwIO e

-------------------
-- support routines
-------------------

ignoreError :: IO () -> IO ()
ignoreError action = Prelude.catch action (\_ -> return ())

{-# NOINLINE tdRef #-}
tdRef :: IORef TempDir
tdRef = unsafePerformIO $ newIORef TempDir {
    tempDirClean = True,
    tempDirDump = False,
    tempDirPath = Nothing,
    tempDirCleanup = []
    }

foreign import ccall unsafe "stdlib.h mkdtemp"
  c_mkdtemp :: CString -> IO CString

mkdtemp :: FilePath -> IO FilePath
mkdtemp template =
      withCString (if "XXXXXX" `isSuffixOf` template then template else (template ++ "XXXXXX")) $ \ ptr -> do
        cname <- throwErrnoIfNull "mkdtemp" (c_mkdtemp ptr)
        peekCString cname
