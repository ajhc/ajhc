{-# OPTIONS_GHC -fglasgow-exts #-}
module Main(main) where

import GHC.IOBase
import GHC.Prim
import GHC.Base
import GHC.Ptr

type World__ = State# RealWorld

main :: IO ()
main = IO $ \rw -> case theRealMain rw of rw' -> (# rw', () #)

unPtr :: Ptr a -> Addr#
unPtr ptr = case ptr of
    Ptr addr -> addr

unFunPtr :: FunPtr a -> Addr#
unFunPtr ptr = case ptr of
    FunPtr addr -> addr

theRealMain :: World__ -> World__
