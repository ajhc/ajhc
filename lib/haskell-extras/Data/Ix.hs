{-# OPTIONS_JHC -fno-prelude -fm4 #-}
module Data.Ix ( Ix(..) ) where

import Jhc.Int
import Jhc.Enum
import Jhc.Order
import Jhc.Basics
import Jhc.Num
import Jhc.Tuples
import Jhc.IO
import Data.Word
import Data.Int

class  Ord a => Ix a  where
    range       :: (a,a) -> [a]
    index       :: (a,a) -> a -> Int
    inRange     :: (a,a) -> a -> Bool
    rangeSize   :: (a,a) -> Int

    index b i | inRange b i = unsafeIndex b i
              | otherwise   = error "array index is out of bounds"
    rangeSize b@(_l,h) | inRange b h = unsafeIndex b h `plus` one
                       | otherwise   = zero
        -- This case is only here to
        -- check for an empty range
        -- NB: replacing (inRange b h) by (l <= h) fails for
        --     tuples.  E.g.  (1,2) <= (2,1) but the range is empty

    unsafeIndex     :: (a,a) -> a -> Int
    unsafeRangeSize :: (a,a) -> Int

    unsafeIndex b i = index b i
    unsafeRangeSize b@(_l,h) = unsafeIndex b h `plus` one

instance  Ix Char  where
    range (m,n)		= [m..n]
    index b@(c,c') ci
        | inRange b ci  =  fromEnum ci `minus` fromEnum c
        | otherwise     =  error "Ix.index: Index out of range."
    inRange (c,c') i    =  c <= i && i <= c'

m4_define(IXINST,{{
instance Ix $1 where
    range (m,n) = [m..n]
    index b@(c,c') ci
        | inRange b ci  =  fromEnum ci `minus` fromEnum c
        | otherwise     =  error "Ix.index: Index out of range :: ($1)"
    inRange (c,c') i    =  c <= i && i <= c'
}})

IXINST(Word8)
IXINST(Word16)
IXINST(Word32)
IXINST(Int8)
IXINST(Int16)
IXINST(Int32)

instance  Ix Int  where
    range (m,n)		= [m..n]
    index b@(m,n) i
        | inRange b i   =  i `minus` m
        | otherwise     =  error "Ix.index: Index out of range."
    inRange (m,n) i     =  m <= i && i <= n

instance  (Ix a, Ix b)  => Ix (a,b) where
        range   ((l,l'),(u,u')) = [(i,i') | i <- range (l,u), i' <- range (l',u')]
        index   ((l,l'),(u,u')) (i,i') =  index (l,u) i * rangeSize (l',u') + index (l',u') i'
        inRange ((l,l'),(u,u')) (i,i') = inRange (l,u) i && inRange (l',u') i'

--instance  Ix Integer  where
--    range (m,n)		= [m..n]
--    index b@(m,n) i
--        | inRange b i   =  fromInteger (i - m)
--        | otherwise     =  error "Ix.index: Index out of range."
--    inRange (m,n) i     =  m <= i && i <= n

instance  Ix Bool  where
    range (m,n)		= [m..n]
    index b@(c,c') ci
        | inRange b ci  =  fromEnum ci `minus` fromEnum c
        | otherwise     =  error "Ix.index: 'Bool' Index out of range."
    inRange (c,c') i    =  c <= i && i <= c'

instance  Ix Ordering  where
    range (m,n)		= [m..n]
    index b@(c,c') ci
        | inRange b ci  =  fromEnum ci `minus` fromEnum c
        | otherwise     =  error "Ix.index: 'Ordering' Index out of range."
    inRange (c,c') i    =  c <= i && i <= c'

-- instance (Ix a,Ix b) => Ix (a, b) -- as derived, for all tuples
-- instance Ix Bool                  -- as derived
-- instance Ix Ordering              -- as derived
-- instance Ix ()                    -- as derived
