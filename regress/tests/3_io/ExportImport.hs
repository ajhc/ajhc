
{-# OPTIONS_JHC -fno-prelude -fffi #-}

import Jhc.Addr
import Jhc.Basics
import Jhc.Monad
import Jhc.Prim

putChar :: Char -> IO ()
putChar c = c_putwchar (charToInt c) >> return ()

foreign import primitive "U2U" charToInt :: Char -> Int
foreign import ccall "stdio.h jhc_utf8_putchar" c_putwchar :: Int -> IO Int

foreign export ccall "myputc" putChar :: Char -> IO ()
foreign import ccall "&myputc" p_putc :: FunPtr (Char -> IO ())
foreign import ccall "dynamic" callPutc :: FunPtr (Char -> IO ()) -> Char -> IO ()

main = do
    mapM_ (callPutc p_putc) "Hello, World!\n"
