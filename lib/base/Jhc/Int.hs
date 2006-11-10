{-# OPTIONS_JHC -N #-}

-- just a few basic operations on integers to jumpstart things
module Jhc.Int(Int(),increment,decrement,plus,minus,times,divide,modulus,zero,one) where

import Data.Int(Int())

foreign import primitive increment :: Int -> Int
foreign import primitive decrement :: Int -> Int
foreign import primitive plus      :: Int -> Int -> Int
foreign import primitive minus     :: Int -> Int -> Int
foreign import primitive times     :: Int -> Int -> Int
foreign import primitive divide    :: Int -> Int -> Int
foreign import primitive modulus   :: Int -> Int -> Int
foreign import primitive zero      :: Int
foreign import primitive one       :: Int
