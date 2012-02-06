module Jhc.Num(module Jhc.Num, module Jhc.Class.Num,Ratio(..)) where

import Jhc.Class.Num
import Jhc.Class.Real
import Jhc.Class.Ord
import Jhc.Type.Float
import Jhc.Basics

numerator, denominator  :: Ratio a -> a
numerator (x :% _)      =  x
denominator (_ :% y)    =  y

fromIntegral   :: (Integral a, Num b) => a -> b
fromIntegral x =  fromInteger (toInteger x)

realToFrac     :: (Real a, Fractional b) => a -> b
realToFrac x   =  fromRational (toRational x)

{-# RULES
"realToFrac/toRational"     realToFrac = toRational
"realToFrac/fromRational"   realToFrac = fromRational
"realToFrac/toDouble"       realToFrac = toDouble
"realToFrac/fromDouble"     realToFrac = fromDouble
#-}

{-# RULES
"fromIntegral/Int"          fromIntegral = (id :: Int -> Int)
"fromIntegral/Integer"      fromIntegral = (id :: Integer -> Integer)
"fromIntegral/toInt"        fromIntegral = toInt
"fromIntegral/fromInt"      fromIntegral = fromInt
"fromIntegral/toInteger"    fromIntegral = toInteger
"fromIntegral/fromInteger"  fromIntegral = fromInteger
#-}

{-# INLINE subtract #-}
subtract         :: (Num a) => a -> a -> a
subtract         =  flip (-)

{-# INLINE even #-}
{-# INLINE odd #-}

even, odd        :: (Integral a) => a -> Bool
even n           =  n `rem` 2 == 0
odd  n           =  n `rem` 2 /= 0
