module Jhc.ForeignPtr(
    ForeignPtr(),
    newPlainForeignPtr_,
    newForeignPtr_,
    mallocPlainForeignPtrAlignBytes,
    mallocForeignPtrAlignBytes,
    unsafeForeignPtrToPtr,
    castForeignPtr,
    touchForeignPtr
    ) where

import Jhc.Addr
import Jhc.IO
import Jhc.Prim.Rts
import Jhc.Basics

type FinalizerPtr  a = FunPtr (Ptr a -> IO ())

-- not Addr_ because we need to make sure it is allocated in a real heap
-- location. The actual ForeignPtr heap location may contain more than the
-- single BitsPtr_ argument.
data ForeignPtr a = FP BitsPtr_

-- | This function creates a plain ForeignPtr from a Ptr, a plain foreignptr
-- may not have finalizers associated with it, hence this function may be pure.
newPlainForeignPtr_ :: Ptr a -> ForeignPtr a
newPlainForeignPtr_  (Ptr (Addr_ addr)) = FP addr

newForeignPtr_ :: Ptr a -> IO (ForeignPtr a)
newForeignPtr_ ptr = fromUIO $ \w ->
    case gc_new_foreignptr ptr w of
        (# w', bp #) -> (# w', fromBang_ bp #)

-- | This function is similar to 'mallocForeignPtrAlignBytes', except that the
-- internally an optimised ForeignPtr representation with no finalizer is used.
-- Attempts to add a finalizer will cause the program to abort.
mallocPlainForeignPtrAlignBytes
    :: Int -- ^ alignment in bytes, must be power of 2. May be zero.
    -> Int -- ^ size to allocate in bytes.
    -> IO (ForeignPtr a)
mallocPlainForeignPtrAlignBytes align size = fromUIO $ \w ->
    case gc_malloc_foreignptr (int2word align) (int2word size) False w of
        (# w', bp #) -> (# w', fromBang_ bp #)

-- | Allocate memory of the given size and alignment that will automatically be
-- reclaimed. Any Finalizers that are attached to this will run before the
-- memory is freed.
mallocForeignPtrAlignBytes
    :: Int -- ^ alignment in bytes, must be power of 2. May be zero.
    -> Int -- ^ size to allocate in bytes.
    -> IO (ForeignPtr a)
mallocForeignPtrAlignBytes align size = fromUIO $ \w ->
    case gc_malloc_foreignptr (int2word align) (int2word size) True w of
        (# w', bp #) -> (# w', fromBang_ bp #)

foreign import safe ccall gc_malloc_foreignptr
    :: Word     -- alignment in words
    -> Word     -- size in words
    -> Bool     -- false for plain foreignptrs, true for ones with finalizers.
    -> UIO (Bang_ (ForeignPtr a))

foreign import safe ccall gc_new_foreignptr ::
    Ptr a -> UIO (Bang_ (ForeignPtr a))

foreign import unsafe ccall gc_add_foreignptr_finalizer
    :: Bang_ (ForeignPtr a)
    -> FinalizerPtr a
    -> IO ()

unsafeForeignPtrToPtr :: ForeignPtr a -> Ptr a
unsafeForeignPtrToPtr (FP x) = Ptr (Addr_ x)

touchForeignPtr :: ForeignPtr a -> IO ()
touchForeignPtr x = fromUIO_ (touch_ x)

castForeignPtr :: ForeignPtr a -> ForeignPtr b
castForeignPtr x = unsafeCoerce x

foreign import primitive touch_ :: ForeignPtr a -> UIO_
foreign import primitive "B2B" int2word :: Int -> Word
foreign import primitive unsafeCoerce :: a -> b
