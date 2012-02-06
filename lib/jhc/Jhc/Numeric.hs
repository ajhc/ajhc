module Jhc.Numeric where

import Jhc.IO
import Jhc.Num
import Jhc.Type.Basic
-- CI import Jhc.Type.Float
import Jhc.Float
import Jhc.Order

infixr 8  ^, ^^

{-# SPECIALIZE gcd :: Int -> Int -> Int #-}
{-# SPECIALIZE gcd :: Integer -> Integer -> Integer #-}
gcd              :: (Integral a) => a -> a -> a
gcd 0 0          =  error "Prelude.gcd: gcd 0 0 is undefined"
gcd x y          =  gcd' (abs x) (abs y)
                    where gcd' x 0  =  x
                          gcd' x y  =  gcd' y (x `rem` y)

{-# SPECIALIZE lcm :: Int -> Int -> Int #-}
{-# SPECIALIZE lcm :: Integer -> Integer -> Integer #-}
lcm              :: (Integral a) => a -> a -> a
lcm _ 0          =  0
lcm 0 _          =  0
lcm x y          =  abs ((x `quot` (gcd x y)) * y)

{-# SPECIALIZE (^) :: Int -> Int -> Int #-}
{-# SPECIALIZE (^) :: Integer -> Int -> Integer #-}
{-# SPECIALIZE (^) :: Double -> Int -> Double #-}

(^)              :: (Num a, Integral b) => a -> b -> a
x ^ 0            =  1
x ^ n | n > 0    =  f x (n-1) x
                    where f _ 0 y = y
                          f x n y = g x n  where
                                    g x n | even n  = g (x*x) (n `quot` 2)
                                          | True = f x (n-1) (x*y)
_ ^ _            = error "Prelude.^: negative exponent"

(^^)             :: (Fractional a, Integral b) => a -> b -> a
x ^^ n           =  if n >= 0 then x^n else recip (x^(-n))
