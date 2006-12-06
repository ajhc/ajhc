{-# OPTIONS_GHC -fglasgow-exts -fno-implicit-prelude #-}
module Main(main) where

import GHC.Int
import GHC.Word
import GHC.IOBase
import GHC.Prim
import GHC.Base
import GHC.Ptr

type World__ = State# RealWorld
type Array__ a = Array# a
type MutArray__ a = MutableArray# RealWorld a
type Ref__ a = MutVar# RealWorld a

type Nothing = ()

theNothing :: Nothing
theNothing = ()

type JIO a = World__ -> (# World__, a #)

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


alloca__ :: Int# -> (Addr# -> JIO a) -> JIO a
alloca__ size action s =
     case newPinnedByteArray# size s      of { (# s, mbarr# #) ->
     case unsafeFreezeByteArray# mbarr# s of { (# s, barr#  #) ->
     case action (byteArrayContents# barr#) s of { (# s, r #) ->
     case touch# barr# s of { s -> (# s, r #) }
     }}}

word2Char__ x = chr# (word2Int# x)
char2Word__ x = int2Word# (ord# x)

convertString :: [Char] -> ListTCon Char
convertString [] = jhc_EmptyList
convertString (x:xs) = jhc_Cons x (convertString xs)

{-# NOINLINE newWorld__ #-}
newWorld__ :: a -> World__
newWorld__ a = case lazy a of
    _ -> realWorld#

theRealMain :: World__ -> World__

