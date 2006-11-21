{-# OPTIONS_JHC -N -fffi #-}

module Jhc.Order where

import Jhc.Enum
import Jhc.Basics

data Bool = False | True
    deriving (Eq, Ord, Bounded, Enum)

data  Ordering    =  LT | EQ | GT
    deriving (Eq, Ord, Bounded, Enum)

infix  4  ==, /=, <, <=, >=, >

class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool
    x == y = case x /= y of
        True -> False
        False -> True
    x /= y = case x == y of
        True -> False
        False -> True

class  (Eq a) => Ord a  where
    compare              :: a -> a -> Ordering
    (<), (<=), (>=), (>) :: a -> a -> Bool
    max, min             :: a -> a -> a

    compare x y | x == y    = EQ
                | x <= y    = LT
                | otherwise = GT

    x <= y  = compare x y /= GT
    x <  y  = compare x y == LT
    x >= y  = compare x y /= LT
    x >  y  = compare x y == GT

    -- Note that (min x y, max x y) = (x,y) or (y,x)
    max x y | x <= y    =  y
            | otherwise =  x
    min x y | x <= y    =  x
            | otherwise =  y


instance Eq () where
    () == () = True
    () /= () = False

instance Ord () where
    () <= () = True
    () <  () = False
    () >= () = True
    () >  () = False
    max () () = ()
    min () () = ()
    compare () () = EQ

instance Bounded () where
    minBound = ()
    maxBound = ()

instance Eq a => Eq [a] where
    [] == [] = True
    (x:xs) == (y:ys) | x == y = xs == ys
    _ == _ = False

instance Ord a => Ord [a] where
    compare (x:xs) (y:ys) = case compare x y of
        EQ -> compare xs ys
        z -> z
    compare [] [] = EQ
    compare [] _ = LT
    compare _ [] = GT

infixr 3  &&
infixr 2  ||

{-# INLINE (&&), (||), not, otherwise #-}
(&&), (||)       :: Bool -> Bool -> Bool
True  && x       =  x
False && _       =  False
True  || _       =  True
False || x       =  x


not              :: Bool -> Bool
not x = if x then False else True


otherwise        :: Bool
otherwise        =  True

