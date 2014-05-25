{-# OPTIONS_JHC -fffi #-}
module System.Directory (
    Permissions( Permissions, readable, writable, executable, searchable ),
    createDirectory, removeDirectory, removeFile,
    renameDirectory, renameFile, getDirectoryContents,
    getCurrentDirectory, setCurrentDirectory,
    doesFileExist, doesDirectoryExist,
    getPermissions, setPermissions,
    getModificationTime ) where

import Foreign
import Foreign.C

import System.Time

data Permissions = Permissions {
    readable,   writable,
    executable, searchable :: !Bool
   } deriving (Eq,Ord,Read,Show)

cPathMax :: CSize
cPathMax = 1024

getCurrentDirectory  :: IO FilePath
getCurrentDirectory = allocaBytes (fromIntegral cPathMax) $ \cp -> do
    cp <- throwErrnoIfNull "getCurrentDirectory" (getcwd cp cPathMax)
    peekCString cp

setCurrentDirectory :: FilePath -> IO ()
setCurrentDirectory fp = throwErrnoIfMinus1_ fp $ withCString fp chdir

foreign import ccall unsafe chdir :: CString -> IO Int
foreign import ccall unsafe getcwd :: Ptr CChar -> CSize -> IO (Ptr CChar)
foreign import ccall unsafe mkdir :: CString -> Int -> IO Int
foreign import ccall unsafe rmdir :: CString -> IO Int
foreign import ccall unsafe unlink :: CString -> IO Int
foreign import ccall unsafe rename :: CString -> CString -> IO Int

createDirectory  :: FilePath -> IO ()
createDirectory fp = throwErrnoIfMinus1_ fp $ withCString fp $ \cs -> mkdir cs (-1)

removeDirectory  :: FilePath -> IO ()
removeDirectory fp = throwErrnoIfMinus1_ fp $ withCString fp rmdir

removeFile  :: FilePath -> IO ()
removeFile fp = throwErrnoIfMinus1_ fp $ withCString fp unlink

renameDirectory  :: FilePath -> FilePath -> IO ()
renameDirectory fp1 fp2 = throwErrnoIfMinus1_ "rename" $ do
    withCString fp1 $ \fp1 -> do
    withCString fp2 $ \fp2 -> do
    rename fp1 fp2

renameFile  :: FilePath -> FilePath -> IO ()
renameFile x y = renameDirectory x y

getDirectoryContents  :: FilePath -> IO [FilePath]
getDirectoryContents = error "getDirectoryContents"

doesFileExist :: FilePath -> IO Bool
doesFileExist = error "doesFileExist"

doesDirectoryExist :: FilePath -> IO Bool
doesDirectoryExist = error "doesDirectoryExist"

getPermissions :: FilePath -> IO Permissions
getPermissions = error "getPermissions"

setPermissions :: FilePath -> Permissions -> IO ()
setPermissions = error "setPermissions"

getModificationTime :: FilePath -> IO ClockTime
getModificationTime = error "getModificationTime"
