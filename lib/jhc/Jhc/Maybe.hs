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

instance Show a => Show (Maybe a) where
    showsPrec d (Just m) = showParen (d > app_prec) $
             showString "Just " . showsPrec (app_prec+1) m
          where app_prec = 10
    showsPrec _ Nothing = showString "Nothing"

instance Ord a => Ord (Maybe a) where
    Just x `compare` Just y = x `compare` y
    Nothing `compare` Nothing = EQ
    Nothing `compare` Just _  = LT
    Just _ `compare` Nothing  = GT

maybe :: b -> (a -> b) -> Maybe a -> b
maybe n f m = case m of
    Just x -> f x
    Nothing -> n
