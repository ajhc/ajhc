{-# LANGUAGE ForeignFunctionInterface #-}
module Control.Concurrent (forkIO, forkOS, ThreadId) where
import Foreign.Ptr
import Foreign.StablePtr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Control.Monad (when)
import Jhc.Prim.Rts

data {-# CTYPE "rts/conc.h jhc_threadid_t" #-} CthreadIdT
data ThreadId = ThreadId CthreadIdT

foreign import ccall "rts/conc.h forkOS_createThread" forkOScreateThread ::
   FunPtr (Bang_ (StablePtr (IO ())) -> IO (Ptr ())) -> Bang_ a -> Ptr Int -> IO CthreadIdT

forkOScreateThreadWrapper :: Bang_ (StablePtr (IO ())) -> IO (Ptr ())
forkOScreateThreadWrapper b = do
  let s = fromBang_ b
  d <- deRefStablePtr s
  d
  freeStablePtr s
  return nullPtr

foreign export ccall "forkOScreateThreadWrapper" forkOScreateThreadWrapper ::
  Bang_ (StablePtr (IO ())) -> IO (Ptr ())
foreign import ccall "&forkOScreateThreadWrapper" p_forkOScreateThreadWrapper ::
  FunPtr (Bang_ (StablePtr (IO ())) -> IO (Ptr ()))

forkOS :: IO () -> IO ThreadId
forkOS f = alloca $ \ip -> do
  s <- newStablePtr f
  pth <- forkOScreateThread p_forkOScreateThreadWrapper (toBang_ s) ip
  i <- peek ip
  when (i /= 0) $ fail "Cannot create OS thread."
  return $ ThreadId pth

-- xxx Should impl user thread.
forkIO :: IO () -> IO ThreadId
forkIO = forkOS
