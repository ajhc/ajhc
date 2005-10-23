module Util.ArbitraryInstances() where

import Test.QuickCheck
import Monad
import Char(chr)

instance Arbitrary a => Arbitrary (Maybe a) where
    arbitrary = do
        i <- choose ((0::Int),7)
        if i == 0 then return Nothing else do
            x <- arbitrary
            return (Just x)
    --coarbitrary Nothing = variant 0 . coarbitrary
instance (Arbitrary a, Arbitrary b) => Arbitrary (Either a b) where
    arbitrary = do
        i <- choose ((0::Int),1)
        case i of
            0 -> liftM Left arbitrary
            1 -> liftM Right arbitrary

instance Arbitrary Char where
    arbitrary = g where
        g = do
            c <- choose (0x20, 0xFF)
            if c > 0x7E && c < 0xA0 then g else return (chr c)
