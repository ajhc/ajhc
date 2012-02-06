{-# OPTIONS_JHC -fno-prelude -fffi -fm4 #-}
module Jhc.Float(
    Float(..),
    Double(..),
    floatToDouble,
    doubleToFloat,
    Floating(..),
    RealFrac(..),
    RealFloat(..),
    rationalToDouble
    ) where

import Jhc.Basics
import Jhc.Enum
import Jhc.Inst.Num
import Jhc.Order
import Jhc.Type.Float
import Jhc.Class.Num
import Jhc.Class.Real

infixr 8  **

foreign import primitive "F2F" floatToDouble :: Float -> Double
foreign import primitive "F2F" doubleToFloat :: Double -> Float

 -- floating point stuff

class  (Fractional a) => Floating a  where
    pi                  :: a
    exp, log, sqrt      :: a -> a
    (**), logBase       :: a -> a -> a
    sin, cos, tan       :: a -> a
    asin, acos, atan    :: a -> a
    sinh, cosh, tanh    :: a -> a
    asinh, acosh, atanh :: a -> a

        -- Minimal complete definition:
        --      pi, exp, log, sin, cos, sinh, cosh
        --      asin, acos, atan
        --      asinh, acosh, atanh
    x ** y           =  exp (log x * y)
    logBase x y      =  log y / log x
    sqrt x           =  x ** (1 / 2) -- 0.5        -- TODO Doubles
    tan  x           =  sin  x / cos  x
    tanh x           =  sinh x / cosh x

-- TODO Doubles
class  (Real a, Fractional a) => RealFrac a  where
    properFraction   :: (Integral b) => a -> (b,a)
    truncate, round  :: (Integral b) => a -> b
    ceiling, floor   :: (Integral b) => a -> b

        -- Minimal complete definition:
        --      properFraction
    truncate x       =  m  where (m,_) = properFraction x

    round x          =  let (n,r) = properFraction x
                            m     = if r < 0 then n - 1 else n + 1
                          in case signum (abs r - (1 / 2)) of
                                -1 -> n
                                0  -> if n `rem` 2 == 0 then n else m
                                1  -> m

    ceiling x        =  if r > 0 then n + 1 else n
                        where (n,r) = properFraction x

    floor x          =  if r < 0 then n - 1 else n
                        where (n,r) = properFraction x

        -- Minimal complete definition:
        --      properFraction

    properFractionf   :: a -> (a,a)
    truncatef, roundf :: a -> a
    ceilingf, floorf  :: a -> a

    truncatef x       =  m  where (m,_) = properFractionf x
    roundf x          =  fromInteger (round x)
    ceilingf x        =  if r > 0 then n + 1 else n
                        where (n,r) = properFractionf x
    floorf x          =  if r < 0 then n - 1 else n
                        where (n,r) = properFractionf x

-- TODO Doubles
class  (RealFrac a, Floating a) => RealFloat a  where
    floatRadix       :: a -> Integer
    floatDigits      :: a -> Int
    floatRange       :: a -> (Int,Int)
    decodeFloat      :: a -> (Integer,Int)
    encodeFloat      :: Integer -> Int -> a
    exponent         :: a -> Int
    significand      :: a -> a
    scaleFloat       :: Int -> a -> a
    isNaN, isInfinite, isDenormalized, isNegativeZero, isIEEE
                     :: a -> Bool
    atan2            :: a -> a -> a

        -- Minimal complete definition:
        --      All except exponent, significand,
        --                 scaleFloat, atan2
    exponent x       =  if m == 0 then 0 else n + floatDigits x
                        where (m,n) = decodeFloat x

    significand x    =  encodeFloat m (- floatDigits x)
                        where (m,_) = decodeFloat x

    scaleFloat k x   =  encodeFloat m (n+k)
                        where (m,n) = decodeFloat x

    atan2 y x
      | x>0           =  atan (y/x)
      | x==0 && y>0   =  pi/2
      | x<0  && y>0   =  pi + atan (y/x)
      |(x<=0 && y<0)  ||
       (x<0 && isNegativeZero y) ||
       (isNegativeZero x && isNegativeZero y)
                      = -atan2 (-y) x
      | y==0 && (x<0 || isNegativeZero x)
                      =  pi    -- must be after the previous test on zero y
      | x==0 && y==0  =  y     -- must be after the other double zero tests
      | otherwise     =  x + y -- x or y is a NaN, return a NaN (via +)

    decodeFloatf     :: a -> (a,Int)
    decodeFloatf x    = case decodeFloat x of
        (v,exp) -> (fromInteger v,exp)

    encodeFloatf     :: a -> Int -> a
    encodeFloatf a i = scaleFloat i a

rationalToDouble :: Rational -> Double
rationalToDouble (x:%y) = fromInteger x `divideDouble` fromInteger y

foreign import primitive "FDiv" divideDouble ::  Double -> Double -> Double

m4_define(NUMINSTANCE,
instance Num $1 where
    $1 x * $1 y = $1 (times$1 x y)
    $1 x + $1 y = $1 (plus$1 x y)
    $1 x - $1 y = $1 (minus$1 x y)
    abs ($1 x) = $1 (abs$1 x)
    negate ($1 x) = $1 (neg$1 x)
    fromInt x = fromInt$1 x
    fromInteger x = fromInteger$1 x
    signum x = case compare x 0 of
        EQ -> 0
        GT -> 1
        LT -> -1

foreign import primitive "FMul" times$1 :: $2 -> $2 -> $2
foreign import primitive "FAdd" plus$1  :: $2 -> $2 -> $2
foreign import primitive "FSub" minus$1 :: $2 -> $2 -> $2

foreign import primitive "FAbs" abs$1 :: $2 -> $2
foreign import primitive "FNeg" neg$1 :: $2 -> $2

foreign import primitive "I2F"  fromInt$1 :: Int -> $1
foreign import primitive "I2F"  fromInteger$1 :: Integer -> $1

foreign import primitive "F2I"  toInt$1 :: $1 -> Int

instance Enum $1 where
    succ = increment$1
    pred = decrement$1
    toEnum x = fromInt$1 x
    fromEnum x = toInt$1 x

    enumFrom x  | x `seq` True     =  x:enumFrom (increment$1 x)
    enumFromTo x y = f x where
        f x | x > y = []
            | otherwise = x:f (increment$1 x)
    enumFromThen x y | x `seq` y `seq` True = f x where
        z = y `fminus$1` x
        f x = x:f (x `fplus$1` z)
    enumFromThenTo x y z | y >= x = f x where
        inc = y `fminus$1` x
        f x | x <= z = x:f (x `fplus$1` inc)
            | otherwise = []
    enumFromThenTo x y z  = f x where
        inc = y `fminus$1` x
        f x | x >= z = x:f (x `fplus$1` inc)
            | otherwise = []

foreign import primitive "fincrement" increment$1 :: $1 -> $1
foreign import primitive "fdecrement" decrement$1 :: $1 -> $1
foreign import primitive "FAdd" fplus$1  :: $1 -> $1 -> $1
foreign import primitive "FSub" fminus$1 :: $1 -> $1 -> $1

instance Eq $1 where
    $1 x == $1 y = (x `eq$2` y)
    $1 x /= $1 y = (x `neq$2` y)

instance Ord $1 where
    $1 x < $1 y = (flt$2 x y)
    $1 x > $1 y = (fgt$2 x y)
    $1 x <= $1 y = (flte$2 x y)
    $1 x >= $1 y = (fgte$2 x y)

foreign import primitive "FEq" eq$2   :: $2 -> $2 -> Bool
foreign import primitive "FNEq" neq$2 :: $2 -> $2 -> Bool
foreign import primitive "FLt" flt$2  :: $2 -> $2 -> Bool
foreign import primitive "FLte" flte$2 :: $2 -> $2 -> Bool
foreign import primitive "FGt" fgt$2 :: $2 -> $2 -> Bool
foreign import primitive "FGte" fgte$2 :: $2 -> $2 -> Bool
)

NUMINSTANCE(Float,Float32_)
NUMINSTANCE(Double,Float64_)
