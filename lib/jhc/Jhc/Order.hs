{-# OPTIONS_JHC -N -fffi #-}

module Jhc.Order(
    Bool(..),
    Ordering(..),
    Eq(..),
    Ord(..),
    (&&),
    (||),
    not,
    otherwise
    ) where

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

    [] < [] = False
    [] < _ = True
    (x:xs) < (y:ys) = if x == y then xs < ys else x < y

    x > y = y < x

    x >= y = not (x < y)
    x <= y = not (y < x)


instance Eq Char where
    Char x == Char y = boxBool (equalsChar x y)
    Char x /= Char y = boxBool (nequalsChar x y)

instance Ord Char where
    Char x < Char y = boxBool (bits32ULt x y)
    Char x > Char y = boxBool (bits32UGt x y)
    Char x <= Char y = boxBool (bits32ULte x y)
    Char x >= Char y = boxBool (bits32UGte x y)

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

foreign import primitive "Eq" equalsChar :: Char__ -> Char__ -> Bool__
foreign import primitive "NEq" nequalsChar :: Char__ -> Char__ -> Bool__
foreign import primitive "ULt" bits32ULt :: Char__ -> Char__ -> Bool__
foreign import primitive "ULte" bits32ULte :: Char__ -> Char__ -> Bool__
foreign import primitive "UGt" bits32UGt :: Char__ -> Char__ -> Bool__
foreign import primitive "UGte" bits32UGte :: Char__ -> Char__ -> Bool__
foreign import primitive "box" boxBool :: Bool__ -> Bool

