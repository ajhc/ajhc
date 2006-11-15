{-# OPTIONS_JHC -N #-}
module Jhc.Array(
    Array__(),
    AT(),
    seqAT__,
    doneAT__,
    newAT__,
    writeAT__,
    unsafeAt__,
    newArray
    )
    where

import Jhc.Basics
import Jhc.IO(dependingOn)
import Jhc.Int

-- The internal array type
data Array__ :: * -> #

-- the built-in array quasi-monad
newtype AT a = AT (Array__ a -> Array__ a)

seqAT__ :: AT a -> AT a -> AT a
seqAT__ (AT a1) (AT a2) = AT $ \a -> case a1 a of
    a' -> a2 a'

doneAT__ :: AT a
doneAT__ = AT (\arr -> arr)

newAT__ :: Int -> AT a -> Array__ a
newAT__ n (AT a1) = case unboxInt (n `dependingOn` a1) of
    nn -> case prim_newAT__ nn of a' -> a1 a'

writeAT__ :: Int -> a -> AT a
writeAT__ i x = case unboxInt i of i' -> prim_writeAT__ i' x

-- none of these routines have run-time checks
foreign import primitive prim_newAT__ :: Int__ -> Array__ a
foreign import primitive prim_writeAT__ :: Int__ -> a -> AT a
--foreign import primitive prim_copyAT__ :: Int__ -> Int__ -> Array__ a -> AT a

-- lookup a value in an array
foreign import primitive unsafeAt__ :: Array__ a -> Int__ -> a



--newArray :: [a] -> Array__ a
--newArray xs = newAT__ (length xs) $ foldr assign doneAT__ (zip [0..] xs) where
--    assign (i,v) rs = writeAT__ i v `seqAT__` rs

newArray :: Int -> [(Int,a)] -> Array__ a
newArray n xs = newAT__ n (foldr assign doneAT__ xs) where
        assign (i,v) rs = writeAT__ i v `seqAT__` rs


{-
newArray :: Int -> [a] -> Array__ a
newArray n xs = case unboxInt n of
    un -> newAT__ un $ f zero__ xs where
        f _ [] = doneAT__
        f n (x:xs) = case increment__ n of nn -> writeAT__ n x `seqAT__` f nn xs
-}
