{-# OPTIONS_JHC -fno-prelude #-}
module Jhc.Class.Num where

import Jhc.Class.Ord
import Jhc.Show
import Jhc.Type.Basic

infixl 7  *
infixl 6  +, -

class  (Eq a, Show a) => Num a  where
    (+), (-), (*)    :: a -> a -> a
    negate           :: a -> a
    abs, signum      :: a -> a
    fromInteger      :: Integer -> a
    fromInt          :: Int -> a

        -- Minimal complete definition:
        --      All, except negate or (-)
    x - y            =  x + negate y
    negate x         =  0 - x
    fromInt i = fromInteger (int2integer i)
    fromInteger x = fromInt (integer2int x)

foreign import "I2I" integer2int :: Integer -> Int
foreign import "I2I" int2integer :: Int -> Integer
