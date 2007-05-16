-- Standard functions on rational numbers

module  Data.Ratio (
    Ratio, Rational, (%), numerator, denominator, approxRational ) where

import Prelude.Text
import Jhc.Num
import Jhc.Float

infixl 7  %

ratPrec = 7 :: Int



(%)                     :: (Integral a) => a -> a -> Ratio a
approxRational          :: (RealFrac a) => a -> a -> Rational



x % y                   =  reduce (x * signum y) (abs y)




-- "reduce" is a subsidiary function used only in this module.
-- It normalises a ratio by dividing both numerator
-- and denominator by their greatest common divisor.
--
-- E.g., 12 `reduce` 8    ==  3 :%   2
--       12 `reduce` (-8) ==  3 :% (-2)

reduce _ 0              =  error "Ratio.% : zero denominator"
reduce x y              =  (x `quot` d) :% (y `quot` d)
                           where d = gcd x y

instance  (Integral a)  => Eq (Ratio a)  where
    (x:%y) == (x':%y')  =  x == x' && y == y'

instance  (Integral a)  => Ord (Ratio a)  where
    (x:%y) <= (x':%y')  =  x * y' <= x' * y
    (x:%y) <  (x':%y')  =  x * y' <  x' * y

--negateRatio (x:%y)       =  (-x) :% y
--(x:%y) `plusRatio` (x':%y')   =  reduce ((x*y') + (x'*y)) (y*y')
--absRatio (x:%y)          =  abs x :% y

instance  (Integral a)  => Num (Ratio a)  where
    --(+) = plusRatio
    (x:%y) + (x':%y')   =  reduce (x*y' + x'*y) (y*y')
    (x:%y) * (x':%y')   =  reduce (x * x') (y * y')
    --negate x {-(x:%y)-}   =  negateRatio x -- (-x) :% y
    negate (x:%y)       =  (-x) :% y
    --abs (x:%y)        =  abs x :% y
    abs (x:%y)          =  abs x :% y
    signum (x:%y)       =  signum x :% 1
    fromInteger x       =  fromInteger x :% 1
    fromInt     x       =  fromInt x :% 1

instance  (Integral a)  => Real (Ratio a)  where
    toRational (x:%y)   =  toInteger x :% toInteger y

    toDouble  x         = rationalToDouble (toRational x)

instance  (Integral a)  => Fractional (Ratio a)  where
    (x:%y) / (x':%y')   =  (x*y') % (y*x')
    recip (x:%y)        =  y % x
    fromRational (x:%y) =  fromInteger x :% fromInteger y
    fromDouble   x      = fromRational (doubleToRational x)

instance  (Integral a)  => RealFrac (Ratio a)  where
    properFraction (x:%y) = (fromIntegral q, r:%y)
                            where (q,r) = quotRem x y

{-
instance  (Integral a)  => Enum (Ratio a)  where
    succ x           =  x+1
    pred x           =  x-1
    toEnum           =  fromIntegral
    fromEnum         =  fromInteger . truncate	-- May overflow
--    enumFrom         =  numericEnumFrom		-- These numericEnumXXX functions
--    enumFromThen     =  numericEnumFromThen	-- are as defined in Prelude.hs
--    enumFromTo       =  numericEnumFromTo	-- but not exported from it!
--    enumFromThenTo   =  numericEnumFromThenTo
-}

instance  (Read a, Integral a)  => Read (Ratio a)  where
    readsPrec p  =  readParen (p > ratPrec)
                              (\r -> [(reduce (x * signum y) (abs y),u) | (x,s)   <- readsPrec (ratPrec+1) r,
                                                ("%",t) <- lex s,
                                                (y,u)   <- readsPrec (ratPrec+1) t ])

instance  (Integral a)  => Show (Ratio a)  where
    showsPrec p (x:%y)  =  showParen (p > ratPrec)
                               (showsPrec (ratPrec+1) x .
			        showString " % " .
				showsPrec (ratPrec+1) y)

approxRational x eps    =  simplest (x-eps) (x+eps)
        where simplest x y | y < x      =  simplest y x
                           | x == y     =  xr
                           | x > 0      =  simplest' n d n' d'
                           | y < 0      =  - simplest' (-n') d' (-n) d
                           | otherwise  =  0 :% 1
                                        where xr@(n:%d) = toRational x
                                              (n':%d')  = toRational y

              simplest' n d n' d'       -- assumes 0 < n%d < n'%d'
                        | r == 0     =  q :% 1
                        | q /= q'    =  (q+1) :% 1
                        | otherwise  =  (q*n''+d'') :% n''
                                     where (q,r)      =  quotRem n d
                                           (q',r')    =  quotRem n' d'
                                           (n'':%d'') =  simplest' d' r' d r
