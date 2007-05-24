{-# OPTIONS_JHC -fffi #-}
module System.IO.Binary(readBinaryFile) where

import Data.Word
import Jhc.IO
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.C.Error

-- | Lazily read a file as a sequence of bytes.

readBinaryFile :: FilePath -> IO [Word8]
readBinaryFile fn = do
    file <- withCString fn $ \fnc -> c_fopen fnc read_str
    if  (file == nullPtr) then getErrno >>= \errno -> (ioError $ errnoToIOError "readBinaryFile" errno Nothing (Just fn)) else do
        let gc = do
                ch <- c_getc file
                case ch of
                    -1 -> c_fclose file >> return []
                    _ -> do
                        xs <- unsafeInterleaveIO gc
                        return (cintToWord8 ch:xs)
        unsafeInterleaveIO gc

foreign import primitive "Lobits" cintToWord8 :: CInt -> Word8
foreign import primitive "const.\"rb\"" read_str :: Ptr CChar

foreign import ccall "stdio.h getc" c_getc :: Ptr () -> IO CInt
foreign import ccall "stdio.h fopen" c_fopen :: CString -> CString -> IO (Ptr ())
foreign import ccall "stdio.h fclose" c_fclose :: Ptr () -> IO CInt
