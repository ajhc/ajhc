module Foreign.ForeignPtr(
    ForeignPtr,  FinalizerPtr,  FinalizerEnvPtr,  newForeignPtr,
    newForeignPtr_,  addForeignPtrFinalizer,  newForeignPtrEnv,
    addForeignPtrFinalizerEnv,  withForeignPtr,  finalizeForeignPtr,
    unsafeForeignPtrToPtr,  touchForeignPtr,  castForeignPtr,
    mallocForeignPtr,  mallocForeignPtrBytes,  mallocForeignPtrArray,
    mallocForeignPtrArray0
    ) where

import Control.Monad
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable

newtype ForeignPtr a = FP (Ptr a)
    deriving(Eq,Ord)

type FinalizerPtr  a = FunPtr (Ptr a -> IO ())

newForeignPtr_ :: Ptr a -> IO (ForeignPtr a)
newForeignPtr_ = return . FP

newForeignPtr :: FinalizerPtr a -> Ptr a -> IO (ForeignPtr a)
newForeignPtr finalizer ptr = do
    fp <- newForeignPtr_ ptr
    addForeignPtrFinalizer finalizer fp
    return fp

addForeignPtrFinalizer :: FinalizerPtr a -> ForeignPtr a -> IO ()
addForeignPtrFinalizer _ _ = return ()

mallocForeignPtrBytes :: Int -> IO (ForeignPtr a)
mallocForeignPtrBytes x = do
    ptr <- mallocBytes x
    newForeignPtr finalizerFree ptr

mallocForeignPtr :: Storable a => IO (ForeignPtr a)
mallocForeignPtr = doMalloc undefined where
    doMalloc :: Storable b => b -> IO (ForeignPtr b)
    doMalloc x = mallocForeignPtrBytes (sizeOf x)

mallocForeignPtrArray  :: Storable a => Int -> IO (ForeignPtr a)
mallocForeignPtrArray  = doMalloc undefined where
    doMalloc            :: Storable a' => a' -> Int -> IO (ForeignPtr a')
    doMalloc dummy size  = mallocForeignPtrBytes (size * sizeOf dummy)

mallocForeignPtrArray0  :: Storable a => Int -> IO (ForeignPtr a)
mallocForeignPtrArray0 sz = mallocForeignPtrArray (sz + 1)

withForeignPtr :: ForeignPtr a -> (Ptr a -> IO b) -> IO b
withForeignPtr fp act = do
    r <- act (unsafeForeignPtrToPtr fp)
    touchForeignPtr fp
    return r

unsafeForeignPtrToPtr :: ForeignPtr a -> Ptr a
unsafeForeignPtrToPtr (FP x) = x

touchForeignPtr :: ForeignPtr a -> IO ()
touchForeignPtr _ = return ()

--foreign import primitive "touch_" :: ForeignPtr a ->

castForeignPtr :: ForeignPtr a -> ForeignPtr b
castForeignPtr (FP x) = FP $ castPtr x

-- |A finalizer is represented as a pointer to a foreign function that, at
-- finalisation time, gets as an argument a plain pointer variant of the
-- foreign pointer that the finalizer is associated with.
type FinalizerEnvPtr env a = FunPtr (Ptr env -> Ptr a -> IO ())

-- | This variant of newForeignPtr adds a finalizer that expects an environment in addition to the finalized pointer. The environment that will be passed to the finalizer is fixed by the second argument to newForeignPtrEnv.
newForeignPtrEnv :: FinalizerEnvPtr env a -> Ptr env -> Ptr a -> IO (ForeignPtr a)
newForeignPtrEnv f e p = do
    fp <- newForeignPtr_ p
    addForeignPtrFinalizerEnv f e fp
    return fp

-- | Like addForeignPtrFinalizerEnv but allows the finalizer to be passed an additional environment parameter to be passed to the finalizer. The environment passed to the finalizer is fixed by the second argument to addForeignPtrFinalizerEnv
addForeignPtrFinalizerEnv :: FinalizerEnvPtr env a -> Ptr env -> ForeignPtr a -> IO ()
addForeignPtrFinalizerEnv _ _ _ = return ()

-- | Causes the finalizers associated with a foreign pointer to be run immediately.
finalizeForeignPtr :: ForeignPtr a -> IO ()
finalizeForeignPtr _ = return ()
