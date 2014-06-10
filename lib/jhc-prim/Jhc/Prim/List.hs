module Jhc.Prim.List where

import Jhc.Prim.Prim

-- Basic list routines used by desugaring and fusion
infixr 5  ++

filter :: (a -> Bool) -> [a] -> [a]
filter p xs = f xs where
    f [] = []
    f (x:xs) = if p x then x:f xs else f xs

concat :: [[a]] -> [a]
concat [] = []
concat (x:xs) = case x of
    [] -> concat xs
    (y:ys) -> y:concat (ys:xs)

concatMap :: (a -> [b]) -> [a] -> [b]
concatMap f xs = g xs where
    g [] = []
    g (x:xs) = f x ++ g xs

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr k z xs = f xs where
    f [] = z
    f (x:xs) = k x (foldr k z xs)

map :: (a -> b) -> [a] -> [b]
map f xs = go xs where
    go [] = []
    go (x:xs) = f x : go xs

(++) :: [a] -> [a] -> [a]
[]     ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)
