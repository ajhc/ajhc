{-# OPTIONS_JHC -fno-prelude -fffi #-}
module Jhc.Maybe where

import Jhc.Basics
import Jhc.List
import Jhc.Monad
import Jhc.Num
import Jhc.Order
import Jhc.Show

instance Monad Maybe where
    return x = Just x
    Nothing >>= _ = Nothing
    Just x >>= y = y x
    fail _ = Nothing



instance Functor Maybe where
    fmap _ Nothing = Nothing
    fmap f (Just x) = Just (f x)



-- Maybe
-- need to add Read instance

data Maybe a  =  Nothing | Just a
    deriving (Eq, Ord, Show)

maybe :: b -> (a -> b) -> Maybe a -> b
maybe n f m = case m of
    Just x -> f x
    Nothing -> n
