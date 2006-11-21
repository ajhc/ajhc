{-# OPTIONS_JHC -N #-}
module Jhc.Enum(Enum(..),Bounded(..)) where
-- Enumeration and Bounded classes

import Data.Int
import Jhc.Basics
import Jhc.Order
import Jhc.Int

class  Enum a  where
    succ, pred       :: a -> a
    toEnum           :: Int -> a
    fromEnum         :: a -> Int
    enumFrom         :: a -> [a]             -- [n..]
    enumFromThen     :: a -> a -> [a]        -- [n,n'..]
    enumFromTo       :: a -> a -> [a]        -- [n..m]
    enumFromThenTo   :: a -> a -> a -> [a]   -- [n,n'..m]

        -- Minimal complete definition:
        --      toEnum, fromEnum
--
-- NOTE: these default methods only make sense for types
--   that map injectively into Int using fromEnum
--  and toEnum.
    succ             =  toEnum . increment . fromEnum
    pred             =  toEnum . decrement . fromEnum
    enumFrom x       =  map toEnum [fromEnum x ..]
    enumFromTo x y   =  map toEnum [fromEnum x .. fromEnum y]
    enumFromThen x y =  map toEnum [fromEnum x, fromEnum y ..]
    enumFromThenTo x y z =
                        map toEnum [fromEnum x, fromEnum y .. fromEnum z]


class Bounded a  where
    minBound         :: a
    maxBound         :: a

instance Enum Int where
    succ = increment
    pred = decrement
    toEnum x = x
    fromEnum x = x

    enumFrom x  | x `seq` True     =  x:enumFrom (increment x)
    enumFromTo x y = f x where
        f x | x > y = []
            | otherwise = x:f (increment x)
    enumFromThen x y | x `seq` y `seq` True = f x where
        z = y `minus` x
        f x = x:f (x `plus` z)
    enumFromThenTo x y z | y >= x = f x where
        inc = y `minus` x
        f x | x <= z = x:f (x `plus` inc)
            | otherwise = []
    enumFromThenTo x y z  = f x where
        inc = y `minus` x
        f x | x >= z = x:f (x `plus` inc)
            | otherwise = []

