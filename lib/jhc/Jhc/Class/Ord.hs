module Jhc.Class.Ord where

import Jhc.Type.Basic
import Jhc.Prim.Prim

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
                | True = GT

    x <= y  = case compare x y of GT -> False; _ -> True
    x <  y  = case compare x y of LT -> True; _ -> False
    y >= x  = case compare x y of GT -> False; _ -> True
    y >  x  = case compare x y of LT -> True; _ -> False

    -- Note that (min x y, max x y) = (x,y) or (y,x)
    max x y | x <= y    =  y
            | True =  x
    min x y | x <= y    =  x
            | True =  y
