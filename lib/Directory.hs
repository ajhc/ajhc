module Directory (
    Permissions( Permissions, readable, writable, executable, searchable ),
    createDirectory, removeDirectory, removeFile,
    renameDirectory, renameFile, getDirectoryContents,
    getCurrentDirectory, setCurrentDirectory,
    doesFileExist, doesDirectoryExist,
    getPermissions, setPermissions,
    getModificationTime ) where

import Time ( ClockTime )

data Permissions = Permissions {
    readable,   writable,
    executable, searchable :: Bool
   } deriving (Eq,Ord,Read,Show)




createDirectory  :: FilePath -> IO ()
removeDirectory  :: FilePath -> IO ()
removeFile  :: FilePath -> IO ()
renameDirectory  :: FilePath -> FilePath -> IO ()
renameFile  :: FilePath -> FilePath -> IO ()

getDirectoryContents  :: FilePath -> IO [FilePath]
getCurrentDirectory  :: IO FilePath
setCurrentDirectory  :: FilePath -> IO ()

doesFileExist :: FilePath -> IO Bool
doesDirectoryExist :: FilePath -> IO Bool

getPermissions :: FilePath -> IO Permissions
setPermissions :: FilePath -> Permissions -> IO ()

getModificationTime :: FilePath -> IO ClockTime


