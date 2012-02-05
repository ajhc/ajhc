{-# OPTIONS_JHC -fno-prelude -fffi #-}
module Jhc.Maybe(Maybe(..), module Jhc.Maybe) where

import Jhc.Basics
import Jhc.List
import Jhc.Monad
import Jhc.Num
import Jhc.Order
import Jhc.Show
import Jhc.Type.Basic

instance Monad Maybe where
    return x = Just x
    Nothing >>= _ = Nothing
    Just x >>= y = y x
    fail _ = Nothing

instance Functor Maybe where
    fmap _ Nothing = Nothing
    fmap f (Just x) = Just (f x)

instance Eq a => Eq (Maybe a) where
    Nothing == Nothing = True
    Just x == Just y = x == y
    _ == _ = False

instance (Show a) => Show (Maybe a) where
    showsPrec d (Nothing) = showString "Nothing"
    showsPrec d (Just aa) = showParen (d >= 10)
	      (showString "Just" . showChar ' ' . showsPrec 10 aa)

instance Ord a => Ord (Maybe a) where
    Just x `compare` Just y = x `compare` y
    Nothing `compare` Nothing = EQ
    Nothing `compare` Just _  = LT
    Just _ `compare` Nothing  = GT

maybe :: b -> (a -> b) -> Maybe a -> b
maybe n f m = case m of
    Just x -> f x
    Nothing -> n

-- either instances
instance (Eq a,Eq b) => Eq (Either a b) where
    (Left aa) == (Left aa') = aa == aa'
    (Right aa) == (Right aa') = aa == aa'
    _ == _ = False

instance (Ord a,Ord b) => Ord (Either a b) where
    compare (Left aa) (Left aa') = compare aa aa'
    compare (Left aa) (Right aa') = LT
    compare (Right aa) (Left aa') = GT
    compare (Right aa) (Right aa') = compare aa aa'

instance (Show a,Show b) => Show (Either a b) where
    showsPrec d (Left aa) = showParen (d >= 10)
	      (showString "Left" . showChar ' ' . showsPrec 10 aa)
    showsPrec d (Right aa) = showParen (d >= 10)
	      (showString "Right" . showChar ' ' . showsPrec 10 aa)
