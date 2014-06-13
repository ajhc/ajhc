{-# LANGUAGE ForeignFunctionInterface,ViewPatterns,RecordWildCards #-}
-- Various routines for dealing with temporary directories and files.
module Support.TempDir(
    getTempDir,
    createTempFile,
    fileInTempDir,
    cleanTempDir,
    setTempDir,
    addAtExit,
    withStackStatus,
    wrapMain
   ) where

import Control.Exception as E
import Control.Monad
import Data.IORef
import Data.List
import Data.Maybe
import Foreign.C
import GenUtil (iocatch)
import System.Directory
import System.Exit
import System.FilePath as FP
import System.IO
import System.IO.Unsafe
import System.Posix.Signals
import Text.Printf
import qualified Data.Set as Set

data TempDir = TempDir {
    tempDirAtExit  :: !(IO ()),
    tempDirClean   :: !Bool,  -- ^ whether to delete the directory afterwords.
    tempDirCleanup :: Set.Set FilePath,
    tempDirDump    :: !Bool,
    tempDirPath    :: Maybe String
    }

putLog :: String -> IO ()
--putLog = putStrLn
putLog _ = return ()

cleanTempDir :: Bool -> IO ()
cleanTempDir b = modifyIORef tdRef $ \x -> x { tempDirClean = b }

setTempDir :: FilePath -> IO ()
setTempDir (FP.normalise -> fp) = do
    TempDir {..} <- readIORef tdRef
    when (isJust $ tempDirPath) $ do
        fail $ printf "Cannot set temp directory to '%s'; \
            \it is already set to '%s'." fp (fromJust tempDirPath)
    putLog $ printf "Setting work directory to '%s'" fp
    createDirectoryIfMissing False fp
    writeIORef tdRef TempDir { tempDirPath = Just fp,  .. }
    cleanTempDir False

getTempDir :: IO FilePath
getTempDir = do
    td <- readIORef tdRef
    case tempDirPath td of
        Just fp -> return fp
        Nothing -> do
            fp <- mkdtemp "/tmp/jhc_XXXXXX"
            putLog $ printf "Created work directory '%s'" fp
            writeIORef tdRef td { tempDirPath = Just fp }
            return fp

addAtExit :: IO () -> IO ()
addAtExit action = do
    td <- readIORef tdRef
    writeIORef tdRef td { tempDirAtExit = action >> tempDirAtExit td }

createTempFile :: FilePath -> IO (FilePath, Handle)
createTempFile (FP.normalise -> fp) = do
    unless (filePathSafe fp) $
        fail $ "createTempFile: unsafe path " ++ fp
    dir <- getTempDir
    (fp,h) <- openBinaryTempFile dir (if null fp then "temp.tmp" else fp)
    putLog $ printf "Created temporary file '%s'" fp
    addCleanup fp
    return (fp,h)

-- make sure nothing is sneaky about the file path
filePathSafe fp = FP.isRelative fp &&
        ".." `notElem` FP.splitPath fp && not (hasDrive fp)

fileInTempDir :: FilePath -> (FilePath -> IO ()) -> IO FilePath
fileInTempDir (FP.normalise -> fp) action = do
    unless (filePathSafe fp) $
        fail $ "fileinTempDir: unsafe path " ++ fp
    let (FP.normalise -> dpart,_) = FP.splitFileName fp
    tdir <- getTempDir
    let f ("./":ps) cp = f ps cp
        f (".":ps) cp = f ps cp
        f (p:ps) cp = do
            putLog $ printf "Creating directory '%s' '%s' '%s' '%s' '%s'" tdir cp p dpart fp
            createDirectoryIfMissing False (tdir </> cp </> p)
            let cp' = FP.normalise (cp </> p)
            addCleanup cp'
            f ps cp'
        f [] _ = return ()
    f (FP.splitPath dpart) ""
    --unless (null $ FP.normalise dpart) $
    --    fold (FP.splitPath dpart) $ addCleanup
    --    createDirectoryIfMissing True (tdir </> dpart)
    let nfp = FP.normalise (tdir </> fp)
    b <- addCleanup fp
    when b $ action nfp
    return nfp

-- |
cleanUp :: IO ()
cleanUp = do
    td <- readIORef tdRef
    tempDirAtExit td
    if not (tempDirClean td) ||
        isNothing (tempDirPath td) then return () else do
    dir <- getTempDir
    forM_ (reverse . Set.toList $ tempDirCleanup td) $ \fp -> do
        putLog $ printf "Removing '%s'" (dir </> fp)
        ignoreError (removeDirectory $ dir </> fp)
        ignoreError (removeFile $ dir </> fp)
    putLog $ printf "Removing '%s'" dir
    ignoreError (removeDirectory dir)

addCleanup :: FilePath -> IO Bool
addCleanup fp = do
    td <- readIORef tdRef
    if fp `Set.member` tempDirCleanup td then return False else do
    writeIORef tdRef td { tempDirCleanup = fp `Set.insert` tempDirCleanup td }
    return True

wrapMain :: IO () -> IO ()
wrapMain main = E.catch (main >> cleanUp) f where
    f (fromException -> Just code) = cleanUp >> exitWith code
    f (fromException -> Just UserInterrupt) = cleanUp >> raiseSignal sigINT
    f e = do
        ss <- readIORef stackRef
        td <- readIORef tdRef
        -- case tempDirPath td of
        --     Just td -> hPutStrLn stderr $
        --         printf "Exiting abnormally. Work directory is '%s'" td
        --     _ -> return ()
        unless (null ss) $
            forM_ ("Stack:":ss) (hPutStrLn stderr)
        cleanUp
        throwIO e

-------------------
-- support routines
-------------------

ignoreError :: IO () -> IO ()
ignoreError action = iocatch action (\_ -> return ())

{-# NOINLINE tdRef #-}
tdRef :: IORef TempDir
tdRef = unsafePerformIO $ newIORef TempDir {
    tempDirClean   = True,
    tempDirDump    = False,
    tempDirPath    = Nothing,
    tempDirAtExit  = return (),
    tempDirCleanup = Set.empty
    }

foreign import ccall unsafe "stdlib.h mkdtemp"
  c_mkdtemp :: CString -> IO CString

mkdtemp :: FilePath -> IO FilePath
mkdtemp template =
      withCString (if "XXXXXX" `isSuffixOf` template then template
        else (template ++ "XXXXXX")) $ \ ptr -> do
            cname <- throwErrnoIfNull "mkdtemp" (c_mkdtemp ptr)
            peekCString cname

{-# NOINLINE stackRef #-}
stackRef :: IORef [String]
stackRef = unsafePerformIO $ newIORef []

withStackStatus :: String -> IO a -> IO a
withStackStatus s action = do
    cs <- readIORef stackRef
    writeIORef stackRef (s:cs)
    r <- action
    writeIORef stackRef cs
    return r
