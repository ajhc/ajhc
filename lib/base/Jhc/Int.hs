{-# OPTIONS_JHC -N #-}

-- just a few basic operations on integers to jumpstart things
module Jhc.Int(Int(),Int__(),increment,decrement,plus,minus,times,divide,modulus,zero,one,boxInt,unboxInt) where

import Data.Int(Int())
import Jhc.Prim(Int__())

foreign import primitive increment :: Int -> Int
foreign import primitive decrement :: Int -> Int
foreign import primitive plus      :: Int -> Int -> Int
foreign import primitive minus     :: Int -> Int -> Int
foreign import primitive times     :: Int -> Int -> Int
foreign import primitive divide    :: Int -> Int -> Int
foreign import primitive modulus   :: Int -> Int -> Int
foreign import primitive zero      :: Int
foreign import primitive one       :: Int

foreign import primitive "box" boxInt :: Int__ -> Int
foreign import primitive "unbox" unboxInt :: Int -> Int__
