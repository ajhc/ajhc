module Jhc.Class.Real where

import Jhc.Basics
import Jhc.Class.Num
import Jhc.Class.Ord
import Jhc.Enum
import Jhc.Float
import Jhc.Type.Float

infixl 7  /, `quot`, `rem`, `div`, `mod`

type  Rational = Ratio Integer

class  (Num a, Ord a) => Real a  where
    toRational       ::  a -> Rational
    toDouble         ::  a -> Double
    toDouble x = rationalToDouble (toRational x)

class  (Real a, Enum a) => Integral a  where
    quot, rem        :: a -> a -> a
    div, mod         :: a -> a -> a
    quotRem, divMod  :: a -> a -> (a,a)
    toInteger        :: a -> Integer
    toInt            :: a -> Int

        -- Minimal complete definition:
        --      quotRem, toInteger
    n `quot` d       =  q  where (q,r) = quotRem n d
    n `rem` d        =  r  where (q,r) = quotRem n d
    n `div` d        =  q  where (q,r) = divMod n d
    n `mod` d        =  r  where (q,r) = divMod n d
    divMod n d       =  n `seq` d `seq` if signum r == - signum d then (q-1, r+d) else qr
                        where qr@(q,r) = quotRem n d
    quotRem n d       =  (n `quot` d, n `rem` d)
    toInteger x = toInteger (toInt x)
    toInt x = toInt (toInteger x)

class  (Num a) => Fractional a  where
    (/)              :: a -> a -> a
    recip            :: a -> a
    fromRational     :: Rational -> a
    fromDouble       :: Double   -> a

        -- Minimal complete definition:
        --      fromRational and (recip or (/))
    recip x          =  1 / x
    x / y            =  x * recip y

    --fromDouble x = fromRational (doubleToRational x)
