{-# OPTIONS_JHC -fno-prelude -fffi #-}
module System.C.Stdio where

import Data.Int
import Foreign.C.Types
import Foreign.Ptr
import Jhc.Basics

type FILE = Ptr CFile

foreign import ccall "stdio.h fopen" c_fopen               :: Ptr CChar -> Ptr CChar -> IO FILE
foreign import ccall "stdio.h popen" c_popen               :: Ptr CChar -> Ptr CChar -> IO FILE
foreign import ccall "stdio.h fclose" c_fclose             :: FILE -> IO CInt
foreign import ccall "stdio.h pclose" c_pclose             :: FILE -> IO CInt
foreign import ccall "stdio.h jhc_utf8_putchar" c_putwchar :: Int -> IO ()
foreign import ccall "wchar.h jhc_utf8_getc" c_fgetwc      :: FILE -> IO Int
foreign import ccall "wchar.h jhc_utf8_getchar" c_getwchar :: IO Int
foreign import ccall "wchar.h jhc_utf8_putc" c_fputwc      :: Int -> FILE -> IO Int
foreign import ccall "stdio.h fwrite_unlocked" c_fwrite    :: Ptr a -> CSize -> CSize -> FILE -> IO CSize
foreign import ccall "stdio.h fread_unlocked" c_fread      :: Ptr a -> CSize -> CSize -> FILE -> IO CSize
foreign import ccall "stdio.h fflush" c_fflush             :: FILE -> IO ()
foreign import ccall "stdio.h feof" c_feof                 :: FILE -> IO Int
foreign import ccall "stdio.h ftell" c_ftell               :: FILE -> IO IntMax
foreign import ccall "stdio.h fseek" c_fseek               :: FILE -> IntMax -> CInt -> IO Int
foreign import ccall "stdio.h fileno" c_fileno             :: FILE -> IO Int
foreign import primitive "const.SEEK_SET" c_SEEK_SET :: CInt
foreign import primitive "const.SEEK_CUR" c_SEEK_CUR :: CInt
foreign import primitive "const.SEEK_END" c_SEEK_END :: CInt
foreign import primitive "const._IOFBF" c__IOFBF :: CInt
foreign import primitive "const._IOLBF" c__IOLBF :: CInt
foreign import primitive "const._IONBF" c__IONBF :: CInt
