{-# OPTIONS_JHC -fno-prelude -funboxed-tuples -fffi #-}
module Data.Array.IO where

import Jhc.Prim.Array
import Data.Ix
import Jhc.IO
import Jhc.Int

data IOArray a b = IOA !a !a (MutArray_ b)

newIOArray :: Ix a => (a,a) -> b -> IO (IOArray a b)
newIOArray rng fill = fromUIO (newIOArray_ rng fill)

newIOArray_ :: Ix a => (a,a) -> b -> World__ -> (# World__, IOArray a b #)
newIOArray_ rng@(l,h) fill w1 =
    case unboxInt (rangeSize rng) of
      size__ -> case newArray__ size__ fill w1 of
                  (# w2, arr #) -> (# w2, IOA l h arr #)

boundsIOArray :: Ix a => IOArray a b -> IO (a,a)
boundsIOArray (IOA l h _) = returnIO (l,h)

readIOArray :: Ix a => IOArray a b -> a -> IO b
readIOArray (IOA l h arr) i =
    case unboxInt (index (l,h) i) of
      i' -> fromUIO (readArray__ arr i')

writeIOArray :: Ix a => IOArray a b -> a -> b -> IO ()
writeIOArray (IOA l h arr) i x =
    case unboxInt (index (l,h) i) of
      i' -> fromUIO (\w1 -> case writeArray__ arr i' x w1 of
                         w2 -> (# w2, () #))

unsafeReadIOArray :: Ix a => IOArray a b -> Int -> IO b
unsafeReadIOArray (IOA l h arr) i = case unboxInt i of i' -> fromUIO (readArray__ arr i')

unsafeWriteIOArray :: Ix a => IOArray a b -> Int -> b -> IO ()
unsafeWriteIOArray (IOA l h arr) i x =
    case unboxInt i of i' -> fromUIO (\w1 -> case writeArray__ arr i' x w1 of
                                          w2 -> (# w2, () #))

{-
freezeIOArray :: Ix a => IOArray a b -> IO (Array a b)
thawIOArray :: Ix a => Array a b -> IO (IOArray a b)

unsafeFreezeIOArray :: Ix a => IOArray a b -> IO (Array a b)
unsafeFreezeIOArray (IOA l h arr) =
    IO (\w1 -> unsafeFreezeArray__ )
-}
