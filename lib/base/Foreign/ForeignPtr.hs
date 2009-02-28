-- | Just a dummy skeleton, fixme.
module Foreign.ForeignPtr
    (ForeignPtr, newForeignPtr_, newForeignPtr,
     addForeignPtrFinalizer, mallocForeignPtr, mallocForeignPtrBytes,
     withForeignPtr, unsafeForeignPtrToPtr, touchForeignPtr, castForeignPtr
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


-- newForeignPtrEnv :: FinalizerEnvPtr env a -> Ptr env -> Ptr a -> IO (ForeignPtr a)
addForeignPtrFinalizer :: FinalizerPtr a -> ForeignPtr a -> IO ()
addForeignPtrFinalizer _ _ = return ()

-- addForeignPtrFinalizerEnv :: FinalizerEnvPtr env a -> Ptr env -> ForeignPtr a -> IO ()

mallocForeignPtr :: Storable a => IO (ForeignPtr a)
mallocForeignPtr = liftM FP malloc

mallocForeignPtrBytes :: Int -> IO (ForeignPtr a)
mallocForeignPtrBytes = liftM FP . mallocBytes

mallocForeignPtrArray  :: Storable a => Int -> IO (ForeignPtr a)
mallocForeignPtrArray = liftM FP . mallocArray

-- mallocForeignPtrArray0 :: Storable a => Int -> IO (ForeignPtr a)
-- mallocForeignPtrArray0 = liftM FP . mallocArray0

withForeignPtr :: ForeignPtr a -> (Ptr a -> IO b) -> IO b
withForeignPtr (FP p) act = act p

unsafeForeignPtrToPtr :: ForeignPtr a -> Ptr a
unsafeForeignPtrToPtr (FP x) = x

touchForeignPtr :: ForeignPtr a -> IO ()
touchForeignPtr _ = return ()

castForeignPtr :: ForeignPtr a -> ForeignPtr b
castForeignPtr (FP x) = FP $ castPtr x
