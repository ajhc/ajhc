{-# OPTIONS_JHC -fffi #-}
module System.IO.Binary(readBinaryFile,putWord8,getWord8) where

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

foreign import ccall "stdio.h getc_unlocked" c_getc :: Ptr () -> IO CInt
foreign import ccall "stdio.h fopen" c_fopen :: CString -> CString -> IO (Ptr ())
foreign import ccall "stdio.h fclose" c_fclose :: Ptr () -> IO CInt

-- Int translates to CInt in the calling conventions so this is safe.
foreign import ccall "stdio.h putchar_unlocked" c_putchar :: Int -> IO Int
foreign import ccall "stdio.h getchar_unlocked" c_getchar :: IO Int


putWord8 :: Word8 -> IO ()
putWord8 w = c_putchar (fromIntegral w) >> return ()

getWord8 :: IO Word8
getWord8 = do
    c <- c_getchar
    case c of
        -1 -> fail "EOF"
        _ -> return $ fromIntegral c

