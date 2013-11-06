module Main where

import Foreign.Ptr
import Foreign.C.String
import Control.Monad

foreign import primitive "const.sizeof(int)" msgSize :: Int

-- return constant value to disable optimaization
foreign import ccall "get_int" getInt :: IO Int
foreign import ccall "get_str" getStr :: IO CString

-- allocate pointer
foreign import ccall "make_entry" make :: IO (Ptr Word8)
-- dump the value
foreign import ccall "dump" dump :: Ptr a -> IO ()

loop _ = do entry <- make -- allocate a pointer
            forever $ thread entry

thread msg = do n <- getInt
                if n < msgSize then
                  return ()
                else
                  do dump msg  -- dump a allocated pointer
                     str <- getStr
                     token <- peekCString str
                     thread msg -- and repeat this

main = loop ()
