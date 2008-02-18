{-# OPTIONS_JHC -N -fffi -fm4 #-}

module Jhc.Float(
    Float(..),
    Double(..),
    floatToDouble,
    doubleToFloat,
    Floating(..),
    RealFrac(..),
    RealFloat(..),
    rationalToDouble
    )
    where

import Jhc.Basics
import Jhc.Int
import Jhc.IO(error)
import Jhc.Num
import Jhc.Order
import Jhc.Types

infixr 8  **

data Float = Float Float32_
data Double = Double Float64_


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


instance Eq Float where
    Float x == Float y = boxBool (x `eqFloat` y)
    Float x /= Float y = boxBool (x `neqFloat` y)

instance Ord Float where
    Float x < Float y = boxBool (float32FLt x y)
    Float x > Float y = boxBool (float32FGt x y)
    Float x <= Float y = boxBool (float32FLte x y)
    Float x >= Float y = boxBool (float32FGte x y)



foreign import primitive "FEq" eqFloat :: Float32_ -> Float32_ -> Bool__
foreign import primitive "FNEq" neqFloat :: Float32_ -> Float32_ -> Bool__
foreign import primitive "FLt" float32FLt :: Float32_ -> Float32_ -> Bool__
foreign import primitive "FLte" float32FLte :: Float32_ -> Float32_ -> Bool__
foreign import primitive "FGt" float32FGt :: Float32_ -> Float32_ -> Bool__
foreign import primitive "FGte" float32FGte :: Float32_ -> Float32_ -> Bool__

instance Eq Double where
    Double x == Double y = boxBool (x `eqDouble` y)
    Double x /= Double y = boxBool (x `neqDouble` y)

instance Ord Double where
    Double x < Double y = boxBool (float64FLt x y)
    Double x > Double y = boxBool (float64FGt x y)
    Double x <= Double y = boxBool (float64FLte x y)
    Double x >= Double y = boxBool (float64FGte x y)

foreign import primitive "FLt" float64FLt :: Float64_ -> Float64_ -> Bool__
foreign import primitive "FLte" float64FLte :: Float64_ -> Float64_ -> Bool__
foreign import primitive "FGt" float64FGt :: Float64_ -> Float64_ -> Bool__
foreign import primitive "FGte" float64FGte :: Float64_ -> Float64_ -> Bool__

foreign import primitive "FEq" eqDouble :: Float64_ -> Float64_ -> Bool__
foreign import primitive "FNEq" neqDouble :: Float64_ -> Float64_ -> Bool__


foreign import primitive "box" boxBool :: Bool__ -> Bool


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

)




NUMINSTANCE(Float,Float32_)
NUMINSTANCE(Double,Float64_)

