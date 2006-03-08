module Jhc.Handle(
    Handle(..),
    IOMode(..),
    stdin,
    stdout,
    stderr,
    withHandle,
    hClose,
    hIsOpen,
    openFile
    ) where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Types
import Jhc.IO
import Foreign.C.String
import Foreign.Marshal.Utils
import Foreign.C.Error

data IOMode = ReadMode | WriteMode | AppendMode | ReadWriteMode
    deriving(Eq, Ord, Bounded, Enum, Read, Show)

data Handle = Handle {
    handleName :: String,
    handleFile :: Ptr (Ptr Handle),
    handleIOMode :: IOMode
    }

instance Show Handle where
    showsPrec _ h s = handleName h ++ s

stdin, stdout, stderr :: Handle

make_builtin mode name std = Handle { handleName = "(" ++ name ++ ")", handleFile = std, handleIOMode = mode }

stdin = make_builtin ReadMode "stdin" c_stdin
stdout = make_builtin WriteMode "stdout" c_stdout
stderr = make_builtin WriteMode "stderr" c_stderr

{-
stdin  = Handle (unsafePerformIO (peek c_stdin))
stdout = Handle (unsafePerformIO (peek c_stdout))
stderr = Handle (unsafePerformIO (peek c_stderr))
-}

foreign import ccall "stdio.h &stdin" c_stdin :: Ptr (Ptr Handle)
foreign import ccall "stdio.h &stdout" c_stdout :: Ptr (Ptr Handle)
foreign import ccall "stdio.h &stderr" c_stderr :: Ptr (Ptr Handle)

withHandle h action = do
    ptr <- peek (handleFile h)
    case ptr == nullPtr of
        True -> fail $ handleName h ++ ": handle  is closed"
        False -> action ptr

hClose h = do
    ptr <- peek (handleFile h)
    case ptr == nullPtr of
        True -> return ()
        False -> c_fclose ptr >> poke (handleFile h) nullPtr

hIsOpen h = do
    ptr <- peek (handleFile h)
    return (ptr /= nullPtr)

throwErrnoFN     :: String	-- ^ textual description of the error location
               -> String
	       -> IO a
throwErrnoFN loc fn  = do
    errno <- getErrno
    ioError (errnoToIOError loc errno Nothing (Just fn))

openFile :: FilePath -> IOMode -> IO Handle
openFile fp m = do
    ptr <- withCString fp $ \cfp -> c_fopen cfp (toStr m)
    if ptr == nullPtr then throwErrnoFN "openFile" fp  else do
        pptr <- new ptr
        return Handle { handleName = fp, handleIOMode = m, handleFile = pptr }

toStr ReadMode = read_str
toStr WriteMode = write_str
toStr AppendMode = append_str
toStr ReadWriteMode = readwrite_str

foreign import primitive "const.\"r\"" read_str :: Ptr CChar
foreign import primitive "const.\"w\"" write_str  :: Ptr CChar
foreign import primitive "const.\"a\"" append_str  :: Ptr CChar
foreign import primitive "const.\"r+\"" readwrite_str  :: Ptr CChar

foreign import ccall "stdio.h fclose" c_fclose :: Ptr Handle -> IO CInt
foreign import ccall "stdio.h fopen" c_fopen :: Ptr CChar -> Ptr CChar ->  IO (Ptr Handle)


