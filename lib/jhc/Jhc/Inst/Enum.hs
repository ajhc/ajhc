{-# OPTIONS_JHC -N -fffi -funboxed-values -fm4 #-}

module Jhc.Inst.Enum() where

import Data.Word
import Data.Int
import Jhc.Enum
import Jhc.Num
import Jhc.Order
import Jhc.IO(error)
import Jhc.Basics

m4_define(ENUMINST,{{
instance Enum $1 where
    toEnum = fromInt
    fromEnum = toInt
    succ = increment$1
    pred = decrement$1
    enumFrom c        = [c .. maxBound]
    enumFromThen c c' = last `seq` [c, c' .. last]
                      where last | c' < c    = minBound
                                 | otherwise = maxBound
    enumFromTo x y = f x where
        f x | x > y = []
            | otherwise = x:f (x + 1)
    enumFromThenTo x y z | y >= x = inc `seq` z `seq` f x where
        inc = y - x
        f x | x <= z = x:f (x + inc)
            | otherwise = []
    enumFromThenTo x y z  = dec `seq` z `seq` f x where
        dec = x - y
        f x | x >= z = x:f (x - dec)
            | otherwise = []

foreign import primitive "increment" increment$1 :: $1 -> $1
foreign import primitive "decrement" decrement$1 :: $1 -> $1

}})

ENUMINST(Word)
ENUMINST(Word8)
ENUMINST(Word16)
ENUMINST(Word32)
ENUMINST(Word64)
ENUMINST(WordPtr)
ENUMINST(WordMax)

ENUMINST(Int8)
ENUMINST(Int16)
ENUMINST(Int32)
ENUMINST(Int64)
ENUMINST(IntPtr)
ENUMINST(IntMax)
ENUMINST(Integer)


instance Enum () where
    succ _      = error "Prelude.Enum.().succ: bad argument"
    pred _      = error "Prelude.Enum.().pred: bad argument"

    toEnum x | x == 0 = ()
             | otherwise    = error "Prelude.Enum.().toEnum: bad argument"

    fromEnum () = 0
    enumFrom () 	= [()]
    enumFromThen () () 	= let many = ():many in many
    enumFromTo () () 	= [()]
    enumFromThenTo () () () = let many = ():many in many


