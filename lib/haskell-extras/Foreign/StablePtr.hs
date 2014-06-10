{-# OPTIONS_JHC -fno-prelude -fffi -funboxed-tuples #-}
module Foreign.StablePtr(
    StablePtr(),
    castStablePtrToPtr,
    castPtrToStablePtr,
    newStablePtr,
    deRefStablePtr,
    freeStablePtr
    ) where

import Jhc.Prim.Rts
import Jhc.IO
import Jhc.Type.Ptr
import Jhc.Basics

data StablePtr a

castPtrToStablePtr :: Ptr () -> StablePtr a
castPtrToStablePtr (Ptr (Addr_ p)) = fromBang_ (bangFromRaw p)

castStablePtrToPtr :: StablePtr a -> Ptr ()
castStablePtrToPtr p = Ptr (Addr_ (bangToRaw (toBang_ p)))

freeStablePtr :: StablePtr a -> IO ()
freeStablePtr p = c_freeStablePtr (toBang_ p)

-- | newStablePtr will seq its argument to get rid of nasty GC issues and be
-- compatible with FFI calling conventions, if this is an issue, you can put an
-- extra box around it.
newStablePtr :: a -> IO (StablePtr a)
newStablePtr x = fromUIO $ \w -> case c_newStablePtr (toBang_ x) w of
        (# w', s #) -> (# w', fromBang_ s #)

deRefStablePtr :: StablePtr a -> IO a
deRefStablePtr x = fromUIO $ \w -> case c_derefStablePtr (toBang_ x) w of
        (# w', s #) -> (# w', fromBang_ s #)

foreign import ccall unsafe "rts/stableptr.c c_freeStablePtr"  c_freeStablePtr   :: Bang_ (StablePtr a) -> IO ()
foreign import ccall unsafe "rts/stableptr.c c_newStablePtr"   c_newStablePtr    :: Bang_ a -> UIO (Bang_ (StablePtr a))
foreign import ccall unsafe "rts/stableptr.c c_derefStablePtr" c_derefStablePtr :: Bang_ (StablePtr a) -> UIO (Bang_ a)
