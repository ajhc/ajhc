{-# OPTIONS_JHC -fffi #-}
module Foreign.Marshal.Alloc (
  -- * Memory allocation
  -- ** Local allocation
  alloca,       -- :: Storable a =>        (Ptr a -> IO b) -> IO b
  allocaBytes,  -- ::               Int -> (Ptr a -> IO b) -> IO b

  -- ** Dynamic allocation
  malloc,       -- :: Storable a =>        IO (Ptr a)
  mallocBytes,  -- ::               Int -> IO (Ptr a)

  realloc,      -- :: Storable b => Ptr a        -> IO (Ptr b)
  reallocBytes, -- ::		    Ptr a -> Int -> IO (Ptr a)

  free,         -- :: Ptr a -> IO ()
  finalizerFree -- :: FinalizerPtr a
) where

import Foreign.Ptr
import Foreign.Storable
import Prelude.IO
import Foreign.C.Types
import Foreign.Marshal.Utils
import Prelude.IOError
import Jhc.Prim
import Jhc.IO
import Jhc.Int(unboxInt)
import Jhc.Addr
import qualified Jhc.Options as JO



-- TODO handle exceptions
allocaBytes' :: Int -> (Ptr a -> IO b) -> IO b
allocaBytes' b f = do
    p <- mallocBytes b
    r <- f p
    free p
    return r

allocaBytes :: Int -> (Ptr a -> IO b) -> IO b
allocaBytes num fn = etaIO $ case JO.target of
    JO.GhcHs -> case unboxInt num of n -> alloca__ n (\addr -> fn (boxAddr addr))
    _ -> allocaBytes' num fn

foreign import primitive alloca__ :: Int__ -> (Addr__ -> IO b) -> IO b
foreign import primitive "box" boxAddr :: Addr__ -> Ptr a

-- exported functions
-- ------------------

-- |Allocate a block of memory that is sufficient to hold values of type
-- @a@.  The size of the area allocated is determined by the 'sizeOf'
-- method from the instance of 'Storable' for the appropriate type.
--
-- The memory may be deallocated using 'free' or 'finalizerFree' when
-- no longer required.
--
malloc :: Storable a => IO (Ptr a)
malloc  = doMalloc undefined
doMalloc       :: Storable b => b -> IO (Ptr b)
doMalloc dummy  = mallocBytes (sizeOf dummy)


-- |@'alloca' f@ executes the computation @f@, passing as argument
-- a pointer to a temporarily allocated block of memory sufficient to
-- hold values of type @a@.
--
-- The memory is freed when @f@ terminates (either normally or via an
-- exception), so the pointer passed to @f@ must /not/ be used after this.
--
alloca :: Storable a => (Ptr a -> IO b) -> IO b
alloca fn  = etaIO $ doAlloca undefined fn where
    doAlloca       :: Storable a' => a' -> (Ptr a' -> IO b') -> IO b'
    doAlloca dummy fn = allocaBytes (sizeOf dummy) fn

failWhenNULL :: String -> IO (Ptr a) -> IO (Ptr a)
failWhenNULL name f = do
   addr <- f
   if addr == nullPtr
      then ioError (userError (name++": out of memory"))
      else return addr

-- |Allocate a block of memory of the given number of bytes.
-- The block of memory is sufficiently aligned for any of the basic
-- foreign types that fits into a memory block of the allocated size.
--
-- The memory may be deallocated using 'free' or 'finalizerFree' when
-- no longer required.
--
mallocBytes      :: Int -> IO (Ptr a)
mallocBytes size  = failWhenNULL "malloc" (_malloc (fromIntegral size))


-- |Resize a memory area that was allocated with 'malloc' or 'mallocBytes'
-- to the size needed to store values of type @b@.  The returned pointer
-- may refer to an entirely different memory area, but will be suitably
-- aligned to hold values of type @b@.  The contents of the referenced
-- memory area will be the same as of the original pointer up to the
-- minimum of the original size and the size of values of type @b@.
--
-- If the argument to 'realloc' is 'nullPtr', 'realloc' behaves like
-- 'malloc'.
--
realloc :: Storable b => Ptr a -> IO (Ptr b)
realloc  = doRealloc undefined
doRealloc           :: Storable b' => b' -> Ptr a' -> IO (Ptr b')
doRealloc dummy ptr  = let
                         size = fromIntegral (sizeOf dummy)
                       in
                       failWhenNULL "realloc" (_realloc ptr size)

-- |Resize a memory area that was allocated with 'malloc' or 'mallocBytes'
-- to the given size.  The returned pointer may refer to an entirely
-- different memory area, but will be sufficiently aligned for any of the
-- basic foreign types that fits into a memory block of the given size.
-- The contents of the referenced memory area will be the same as of
-- the original pointer up to the minimum of the original size and the
-- given size.
--
-- If the pointer argument to 'reallocBytes' is 'nullPtr', 'reallocBytes'
-- behaves like 'malloc'.  If the requested size is 0, 'reallocBytes'
-- behaves like 'free'.
--
reallocBytes          :: Ptr a -> Int -> IO (Ptr a)
reallocBytes ptr i | ptr `seq` i `seq` False = undefined
reallocBytes ptr 0     = do free ptr; return nullPtr
reallocBytes ptr size  =
  failWhenNULL "realloc" (_realloc ptr (fromIntegral size))

foreign import ccall "stdlib.h malloc" _malloc :: CSize -> IO (Ptr a)
foreign import ccall "stdlib.h free" free :: Ptr a -> IO ()
foreign import ccall "stdlib.h realloc" _realloc :: Ptr a -> CSize -> IO (Ptr b)
