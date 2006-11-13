{-# OPTIONS_GHC -fglasgow-exts -fno-implicit-prelude #-}
module Main(main) where

import GHC.IOBase
import GHC.Prim
import GHC.Base
import GHC.Ptr

type World__ = State# RealWorld

type Nothing = ()

theNothing :: Nothing
theNothing = ()

main :: IO ()
main = IO $ \rw -> case theRealMain rw of rw' -> (# rw', () #)

unPtr :: Ptr a -> Addr#
unPtr ptr = case ptr of
    Ptr addr -> addr

unFunPtr :: FunPtr a -> Addr#
unFunPtr ptr = case ptr of
    FunPtr addr -> addr

fromBool :: Bool -> Int#
fromBool b = case b of
    False -> 0#
    True -> 1#

gteChar# a b = gtChar# a b || eqChar# a b
lteChar# a b = ltChar# a b || eqChar# a b

convertString :: [Char] -> ListTCon Char
convertString [] = jhc_EmptyList
convertString (x:xs) = jhc_Cons x (convertString xs)

theRealMain :: World__ -> World__
