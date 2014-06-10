{-# OPTIONS_JHC -fno-prelude -fno-sugar -fforall #-}
module Jhc.Prim.List where

import Jhc.Prim.Prim
import Jhc.Prim.Basics

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

build :: (forall b . (a -> b -> b) -> b -> b) -> [a]
build g = g (:) []

augment :: forall a. (forall b. (a->b->b) -> b -> b) -> [a] -> [a]
augment g xs = g (:) xs

{-# RULES "foldr/nil" forall k z.   foldr k z []  = z  #-}
{-# RULES "foldr/single"  forall k z x . foldr k z [x] = k x z #-}
{-# RULES "foldr/id"      foldr (:) [] = \x -> x  #-}
{-# RULES "foldr/build" forall k z (g :: forall b . (a -> b -> b) -> b -> b) . foldr k z (build g) = g k z #-}
{-# RULES "foldr/augment" forall k z xs (g::forall b. (a->b->b) -> b -> b) .  foldr k z (augment g xs) = g k (foldr k z xs) #-}

{-# RULES "augment/build" forall (g::forall b. (a->b->b) -> b -> b)
		       (h::forall b. (a->b->b) -> b -> b) .
		       augment g (build h) = build (\c n -> g c (h c n)) #-}
{-# RULES "augment/nil"   forall (g::forall b. (a->b->b) -> b -> b) .  augment g [] = build g #-}

{-# CATALYST "concatMap/foldr" forall f . concatMap f = foldr ((++) . f) [] #-}
{-# CATALYST "concat/foldr" forall . concat = foldr (++) [] #-}
