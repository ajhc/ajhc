{-# OPTIONS_JHC -N -fffi #-}
module Jhc.Float where

import Jhc.IO(error)
import Jhc.Order
import Jhc.Int
import Jhc.Num
import Jhc.Basics

infixr 8  **

data Float
data Double


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

foreign import primitive "divide" divideDouble ::  Double -> Double -> Double
