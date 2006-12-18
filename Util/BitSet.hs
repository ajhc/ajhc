{-# OPTIONS_GHC -fbang-patterns #-}
module Util.BitSet(
    BitSet(),
    EnumBitSet(..),
    toWord,
    fromWord
    ) where


import Data.Bits
import Data.Word
import Data.Monoid
import Util.SetLike
import Util.HasSize

newtype BitSet = BitSet Word
    deriving(Eq,Ord)


instance Monoid BitSet where
    mempty = BitSet 0
    mappend (BitSet a) (BitSet b) = BitSet (a .|. b)



instance IsEmpty BitSet where
    isEmpty (BitSet n) = n == 0

instance HasSize BitSet where
    size (BitSet n) = f 0 n where
        f !c 0 = c
        f !c !v = f (c + 1) (v .&. (v - 1))

instance SetLike BitSet where
    BitSet a `difference` BitSet b = BitSet (a .&. complement b)
    BitSet a `intersection` BitSet b = BitSet (a .&. b)
    BitSet a `disjoint` BitSet b  = ((a .&. b) /= 0)
    BitSet a `isSubsetOf` BitSet b = (a .|. b) == b


instance BuildSet Int BitSet where
    insert i (BitSet v) = BitSet (v .|. bit i)
    singleton i = BitSet (bit i)
    fromList ts = BitSet (foldl setBit 0 ts)

instance ModifySet Int BitSet where
    delete i (BitSet v) = BitSet (clearBit v i)
    member i (BitSet v) = testBit v i
    toList (BitSet v) = f 1 where
        f c | c > 32 = []
            | otherwise = if v .&. bit c /= 0 then c:f (c + 1) else f (c + 1)



instance Show BitSet where
    showsPrec n bs = showsPrec n (toList bs)


newtype EnumBitSet a = EnumBitSet BitSet
    deriving(Monoid,SetLike,HasSize,Eq,Ord,IsEmpty)

instance Enum a => BuildSet a (EnumBitSet a) where
    fromList xs = EnumBitSet $ fromList (map fromEnum xs)
    insert x (EnumBitSet s) = EnumBitSet $ insert (fromEnum x) s
    singleton x = EnumBitSet $ singleton (fromEnum x)

instance Enum a => ModifySet a (EnumBitSet a) where
    toList (EnumBitSet s) = map toEnum $ toList s
    member x (EnumBitSet s) = member (fromEnum x) s
    delete x (EnumBitSet s) = EnumBitSet $ delete (fromEnum x) s

instance (Enum a,Show a) => Show (EnumBitSet a) where
    showsPrec n bs = showsPrec n (toList bs)


toWord :: BitSet -> Word
toWord (BitSet w) = w

fromWord :: Word -> BitSet
fromWord w = BitSet w

