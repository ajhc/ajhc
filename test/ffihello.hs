{-# OPTIONS_JHC -N #-}

import Jhc.Addr
import Jhc.Basics
import Jhc.Monad

putChar :: Char -> IO ()
putChar c = c_putwchar (charToInt c)

foreign import primitive "U2U" charToInt :: Char -> Int
foreign import ccall "stdio.h jhc_utf8_putchar" c_putwchar :: Int -> IO ()

foreign export ccall "myputc" putChar :: Char -> IO ()
foreign import ccall "&myputc" p_putc :: FunPtr (Char -> IO ())
foreign import ccall "dynamic" callPutc :: FunPtr (Char -> IO ()) -> Char -> IO ()

main = mapM_ (callPutc p_putc) "Hello, World!"
