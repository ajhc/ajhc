module Foreign.StablePtr(
    StablePtr(),
    castStablePtrToPtr,
    castPtrToStablePtr,
    newStablePtr,
    deRefStablePtr,
    freeStablePtr
    ) where

import Foreign.Ptr

newtype StablePtr a = StablePtr (Ptr ())
data PlaceHolder

castPtrToStablePtr :: Ptr () -> StablePtr a
castPtrToStablePtr p = StablePtr p

castStablePtrToPtr :: StablePtr a -> Ptr ()
castStablePtrToPtr (StablePtr p) = p


freeStablePtr :: StablePtr a -> IO ()
freeStablePtr _ = return ()

newStablePtr :: a -> IO (StablePtr a)
newStablePtr x = do
    ptr <- ref_ptr (unsafeCoerce x)
    return (StablePtr ptr)

deRefStablePtr :: StablePtr a -> IO a
deRefStablePtr (StablePtr x) = do
    v <- deref_ptr x
    return (unsafeCoerce v)

deref_ptr = undefined
ref_ptr = undefined
unsafeCoerce = undefined

{-

foreign import primitive deref_ptr :: Ptr () -> IO PlaceHolder
foreign import primitive ref_ptr   :: PlaceHolder -> IO (Ptr ())

foreign import primitive unsafeCoerce :: a -> b
-}
