{-# OPTIONS_JHC -N -funboxed-tuples -fffi #-}
module Jhc.IOArray where

import Jhc.Basics
import Data.Ix
import Jhc.Array
import Jhc.IO
import Jhc.Int
import Data.Array

data IOArray a b = IOA !a !a (MutArray__ b)

newIOArray :: Ix a => (a,a) -> b -> IO (IOArray a b)
newIOArray rng fill = IO (newIOArray_ rng fill)

newIOArray_ :: Ix a => (a,a) -> b -> World__ -> (# World__, IOArray a b #)
newIOArray_ rng@(l,h) fill w1 =
    case unboxInt (rangeSize rng) of
      size__ -> case newMutArray__ size__ fill w1 of
                  (# w2, arr #) -> (# w2, IOA l h arr #)
                                     
boundsIOArray :: Ix a => IOArray a b -> IO (a,a)
boundsIOArray (IOA l h _) = returnIO (l,h)

readIOArray :: Ix a => IOArray a b -> a -> IO b
readIOArray (IOA l h arr) i =
    case unboxInt (index (l,h) i) of
      i' -> IO (readArray__ arr i')
    
writeIOArray :: Ix a => IOArray a b -> a -> b -> IO ()
writeIOArray (IOA l h arr) i x =
    case unboxInt (index (l,h) i) of
      i' -> IO (\w1 -> case writeArray__ arr i' x w1 of
                         w2 -> (# w2, () #))

unsafeReadIOArray :: Ix a => IOArray a b -> Int -> IO b
unsafeReadIOArray (IOA l h arr) i = case unboxInt i of i' -> IO (readArray__ arr i')

unsafeWriteIOArray :: Ix a => IOArray a b -> Int -> b -> IO ()
unsafeWriteIOArray (IOA l h arr) i x =
    case unboxInt i of i' -> IO (\w1 -> case writeArray__ arr i' x w1 of
                                          w2 -> (# w2, () #))

{-
freezeIOArray :: Ix a => IOArray a b -> IO (Array a b)
thawIOArray :: Ix a => Array a b -> IO (IOArray a b)

unsafeFreezeIOArray :: Ix a => IOArray a b -> IO (Array a b)
unsafeFreezeIOArray (IOA l h arr) = 
    IO (\w1 -> unsafeFreezeArray__ )
-}
