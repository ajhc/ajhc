module Cmm.Number(Number(..),toIntegral) where

import Ratio
import Data.Binary

newtype Number = Number Rational
    deriving(Num,Eq,Ord,Binary,Real,Fractional,RealFrac,Enum)

instance Integral Number where
    toInteger (Number x) = case denominator x of
        1 -> numerator x
        _ -> error $ "toInteger: Number not integer " ++ show x
    quotRem x y = case toInteger x `quotRem` toInteger y  of
        (x,y) -> (fromInteger x,fromInteger y)

instance Show Number where
    showsPrec n (Number r) = case denominator r of
        1 -> showsPrec n (numerator r)
        _ -> showsPrec n (realToFrac r :: Double)

toIntegral :: (Integral i,Monad m) => Number -> m i
toIntegral (Number r) = case denominator r of
    1 -> return $ fromInteger (numerator r)
    _ -> fail $ "toInteger: Number not integer " ++ show r

