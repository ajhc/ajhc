{-# LANGUAGE ForeignFunctionInterface #-}
import Control.Monad
import Control.Concurrent
import Foreign.C

foreign import ccall "unistd.h sleep" c_sleep :: CUInt -> IO ()

main :: IO ()
main = do
  forkOS $ (replicateM_ 10 $ putChar '.')
  forkOS $ (replicateM_ 10 $ putStr "..")
  replicateM_ 10 $ putStr "..."
  c_sleep 1
  putChar '\n'
