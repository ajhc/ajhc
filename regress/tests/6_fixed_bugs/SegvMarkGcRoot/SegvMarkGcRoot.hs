module Main where
import Data.Word
foreign import primitive "const.sizeof(unsigned long)" sizeOfULong :: Word32
foreign import primitive "const.sizeof(char)" sizeOfChar :: Word32
foreign export ccall "xxxxxxxxx_f" f :: IO Word32
foreign export ccall "xxxxxxxxx_g" g :: IO Word32
foreign import ccall "xxxxxxxxx_f" fx :: IO Word32
foreign import ccall "xxxxxxxxx_g" gx :: IO Word32

{-
 - Add some objects to root_stack.
 -}
f :: IO Word32
f = return $ n1 + n2 + n3 + n4
n1 = sizeOfULong `div` sizeOfChar
n2 = 8 * (sizeOfULong `div` sizeOfChar)
n3 = 16  * (sizeOfULong `div` sizeOfChar)
n4 = 32  * (sizeOfULong `div` sizeOfChar)

{-
 - Use block contains some pointers.
 -}
g :: IO Word32
fact 0 = 1
fact n = n * fact (n-1)
g = return $ fact 0

main :: IO ()
main = do fx
          -- call jhc_alloc_fini and jhc_alloc_init
          gx
          putStrLn "ok"
