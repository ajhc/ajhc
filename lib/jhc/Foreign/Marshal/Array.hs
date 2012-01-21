{-# OPTIONS_JHC -fno-prelude  #-}
module Foreign.Marshal.Array (
  -- * Marshalling arrays

  -- ** Allocation
  --
  mallocArray,    -- :: Storable a => Int -> IO (Ptr a)
  mallocArray0,   -- :: Storable a => Int -> IO (Ptr a)

  allocaArray,    -- :: Storable a => Int -> (Ptr a -> IO b) -> IO b
  allocaArray0,   -- :: Storable a => Int -> (Ptr a -> IO b) -> IO b

  reallocArray,   -- :: Storable a => Ptr a -> Int -> IO (Ptr a)
  reallocArray0,  -- :: Storable a => Ptr a -> Int -> IO (Ptr a)

  -- ** Marshalling
  --
  peekArray,      -- :: Storable a =>         Int -> Ptr a -> IO [a]
  peekArray0,     -- :: (Storable a, Eq a) => a   -> Ptr a -> IO [a]

  pokeArray,      -- :: Storable a =>      Ptr a -> [a] -> IO ()
  pokeArray0,     -- :: Storable a => a -> Ptr a -> [a] -> IO ()

  -- ** Combined allocation and marshalling
  --
  newArray,       -- :: Storable a =>      [a] -> IO (Ptr a)
  newArray0,      -- :: Storable a => a -> [a] -> IO (Ptr a)

  withArray,      -- :: Storable a =>      [a] -> (Ptr a -> IO b) -> IO b
  withArray0,     -- :: Storable a => a -> [a] -> (Ptr a -> IO b) -> IO b

  withArrayLen,   -- :: Storable a =>      [a] -> (Int -> Ptr a -> IO b) -> IO b
  withArrayLen0,  -- :: Storable a => a -> [a] -> (Int -> Ptr a -> IO b) -> IO b

  -- ** Copying

  -- | (argument order: destination, source)
  copyArray,      -- :: Storable a => Ptr a -> Ptr a -> Int -> IO ()
  moveArray,      -- :: Storable a => Ptr a -> Ptr a -> Int -> IO ()

  -- ** Finding the length
  --
  lengthArray0,   -- :: (Storable a, Eq a) => a -> Ptr a -> IO Int

  -- ** Indexing
  --
  advancePtr,     -- :: Storable a => Ptr a -> Int -> Ptr a
) where


import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import Jhc.Basics
import Jhc.IO
import Jhc.List
import Jhc.Monad
import Jhc.Num
import Jhc.Order

-- allocation
-- ----------

-- |Allocate storage for the given number of elements of a storable type
-- (like 'Foreign.Marshal.Alloc.malloc', but for multiple elements).
--
mallocArray :: Storable a => Int -> IO (Ptr a)
mallocArray  = doMalloc undefined
doMalloc            :: Storable a' => a' -> Int -> IO (Ptr a')
doMalloc dummy size  = mallocBytes (size * sizeOf dummy)

-- |Like 'mallocArray', but add an extra position to hold a special
-- termination element.
--
mallocArray0      :: Storable a => Int -> IO (Ptr a)
mallocArray0 size  = mallocArray (size + 1)

-- |Temporarily allocate space for the given number of elements
-- (like 'Foreign.Marshal.Alloc.alloca', but for multiple elements).
--
allocaArray :: Storable a => Int -> (Ptr a -> IO b) -> IO b
allocaArray  size fn = etaIO $ doAlloca undefined fn where
    doAlloca            :: Storable a' => a' ->  (Ptr a' -> IO b') -> IO b'
    doAlloca dummy fn = allocaBytes (size * sizeOf dummy) fn

-- |Like 'allocaArray', but add an extra position to hold a special
-- termination element.
--
allocaArray0      :: Storable a => Int -> (Ptr a -> IO b) -> IO b
allocaArray0 size  = allocaArray (size + 1)

-- |Adjust the size of an array
--
reallocArray :: Storable a => Ptr a -> Int -> IO (Ptr a)
reallocArray  = doRealloc undefined
  where
    doRealloc                :: Storable a' => a' -> Ptr a' -> Int -> IO (Ptr a')
    doRealloc dummy ptr size  = reallocBytes ptr (size * sizeOf dummy)

-- |Adjust the size of an array including an extra position for the end marker.
--
reallocArray0          :: Storable a => Ptr a -> Int -> IO (Ptr a)
reallocArray0 ptr size  = reallocArray ptr (size + 1)


-- |Convert an array of given length into a Haskell list.  This version
-- traverses the array backwards using an accumulating parameter,
-- which uses constant stack space.  The previous version using mapM
-- needed linear stack space.
--
peekArray          :: Storable a => Int -> Ptr a -> IO [a]
peekArray size ptr | ptr `seq` (size <= 0) = return []
                   | otherwise = f (size-1) []
  where
    f 0 acc = do e <- peekElemOff ptr 0; return (e:acc)
    f n acc = do e <- peekElemOff ptr n; f (n-1) (e:acc)

-- |Convert an array terminated by the given end marker into a Haskell list
--
peekArray0            :: (Storable a, Eq a) => a -> Ptr a -> IO [a]
peekArray0 marker ptr  = do
  size <- lengthArray0 marker ptr
  peekArray size ptr

-- finding the length
-- ------------------


-- |Write the list elements consecutive into memory
--
--pokeArray :: Storable a => Ptr a -> [a] -> IO ()
--pokeArray ptr vals =  zipWithM_ (pokeElemOff ptr) [0..] vals where
--    zipWithM_         :: (Monad m) => (a -> b -> m c) -> [a] -> [b] -> m ()
--    zipWithM_ f xs ys =  sequence_ (zipWith f xs ys)

pokeArray :: Storable a => Ptr a -> [a] -> IO ()
pokeArray ptr vals = pokeArray' ptr vals >> return ()

pokeArray' :: Storable a => Ptr a -> [a] -> IO Int
pokeArray' ptr vals =  etaIO $ f 0 vals where
    f n [] | n `seq` True = return n
    f n (x:xs) = pokeElemOff ptr n x >> f (n + 1) xs


-- |Write the list elements consecutive into memory and terminate them with the
-- given marker element
--
pokeArray0 :: Storable a => a -> Ptr a -> [a] -> IO ()
pokeArray0 marker ptr vals  = do
  lv <- pokeArray' ptr vals
  pokeElemOff ptr lv marker

-- combined allocation and marshalling
-- -----------------------------------

-- |Write a list of storable elements into a newly allocated, consecutive
-- sequence of storable values
-- (like 'Foreign.Marshal.Utils.new', but for multiple elements).
--
newArray      :: Storable a => [a] -> IO (Ptr a)
newArray vals  = do
  ptr <- mallocArray (length vals)
  pokeArray ptr vals
  return ptr

-- |Write a list of storable elements into a newly allocated, consecutive
-- sequence of storable values, where the end is fixed by the given end marker
--
newArray0             :: Storable a => a -> [a] -> IO (Ptr a)
newArray0 marker vals  = do
  ptr <- mallocArray0 (length vals)
  pokeArray0 marker ptr vals
  return ptr

-- |Temporarily store a list of storable values in memory
-- (like 'Foreign.Marshal.Utils.with', but for multiple elements).
--
withArray :: Storable a => [a] -> (Ptr a -> IO b) -> IO b
withArray vals = withArrayLen vals . const

-- |Like 'withArray', but the action gets the number of values
-- as an additional parameter
--
withArrayLen :: Storable a => [a] -> (Int -> Ptr a -> IO b) -> IO b
withArrayLen vals f  = etaIO $
  len `seq` allocaArray len $ \ptr -> do
      pokeArray ptr vals
      res <- f len ptr
      return res
  where
    len = length vals

-- |Like 'withArray', but a terminator indicates where the array ends
--
withArray0 :: Storable a => a -> [a] -> (Ptr a -> IO b) -> IO b
withArray0 marker vals = withArrayLen0 marker vals . const

-- |Like 'withArrayLen', but a terminator indicates where the array ends
--
withArrayLen0 :: Storable a => a -> [a] -> (Int -> Ptr a -> IO b) -> IO b
withArrayLen0 marker vals f  = etaIO $
  len `seq` allocaArray0 len $ \ptr -> do
      pokeArray0 marker ptr vals
      res <- f len ptr
      return res
  where
    len = length vals


-- copying (argument order: destination, source)
-- -------

-- |Copy the given number of elements from the second array (source) into the
-- first array (destination); the copied areas may /not/ overlap
--
copyArray :: Storable a => Ptr a -> Ptr a -> Int -> IO ()
copyArray  = doCopy undefined
  where
    doCopy                     :: Storable a' => a' -> Ptr a' -> Ptr a' -> Int -> IO ()
    doCopy dummy dest src size  = copyBytes dest src (size * sizeOf dummy)

-- |Copy the given number of elements from the second array (source) into the
-- first array (destination); the copied areas /may/ overlap
--
moveArray :: Storable a => Ptr a -> Ptr a -> Int -> IO ()
moveArray  = doMove undefined
  where
    doMove                     :: Storable a' => a' -> Ptr a' -> Ptr a' -> Int -> IO ()
    doMove dummy dest src size  = moveBytes dest src (size * sizeOf dummy)


-- finding the length
-- ------------------

-- |Return the number of elements in an array, excluding the terminator
--
lengthArray0            :: (Storable a, Eq a) => a -> Ptr a -> IO Int
lengthArray0 marker ptr | ptr `seq` True  = etaIO $ loop 0
  where
    loop i | i `seq` True = do
        val <- peekElemOff ptr i
        if val == marker then return i else loop (i+1)


-- indexing
-- --------

-- |Advance a pointer into an array by the given number of elements
--
advancePtr :: Storable a => Ptr a -> Int -> Ptr a
advancePtr  = doAdvance undefined
  where
    doAdvance             :: Storable a' => a' -> Ptr a' -> Int -> Ptr a'
    doAdvance dummy ptr i  = ptr `plusPtr` (i * sizeOf dummy)
