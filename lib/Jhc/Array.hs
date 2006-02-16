module Jhc.Array where

import Jhc.IO(World__,newWorld__)

-- The internal array type
data Array__ a

-- the built-in array quasi-monad
newtype AT a = AT (Array__ -> Array__)

seqAT__ :: AT a -> AT a -> AT a
seqAT__ (AT a1) (AT a2) = AT $ \a -> a2 (a1 a)

doneAT__ :: AT a
doneAT__ = AT id

newAT__ :: Int -> AT a -> Array__ a
newAT__ n (AT a1) = a1 (prim_newAT__ (newUnique__ a1) n)

writeAT__ :: Int -> a -> AT a
writeAT__ i x = AT $ \a -> prim_writeAT__ i x a

-- none of these routines have run-time checks
foreign import primitive "prim_newAT__" :: World__ -> Int -> Array__
foreign import primitive "prim_writeAT__" :: Int -> a -> Array__ -> Array__

-- lookup a value in an array
foreign import primitive "unsafeAt__" :: Array__ a -> Int -> a


newArray :: [a] -> Array__ a
newArray xs = newAT__ (length as) $ foldr assign doneAT (zip [0..] xs) where
    assign (i,v) rs = writeAT__ i v `seqAT__` rs


