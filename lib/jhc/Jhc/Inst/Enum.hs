{-# OPTIONS_JHC -fno-prelude -fffi -funboxed-values -fm4 #-}

module Jhc.Inst.Enum() where

import Data.Word
import Data.Int
import Jhc.Enum
import Jhc.Num
import Jhc.Order
import Jhc.IO(error)
import Jhc.Basics
import Jhc.Inst.Order

m4_include(Jhc/Enum.m4)

ENUMINST(Word)
ENUMINST(Word8)
ENUMINST(Word16)
ENUMINST(Word32)
ENUMINST(Word64)
ENUMINST(WordPtr)
ENUMINST(WordMax)

UBOUNDED(Word)
UBOUNDED(Word8)
UBOUNDED(Word16)
UBOUNDED(Word32)
UBOUNDED(Word64)
UBOUNDED(WordPtr)
UBOUNDED(WordMax)

ENUMINST(Int8)
ENUMINST(Int16)
ENUMINST(Int32)
ENUMINST(Int64)
ENUMINST(IntPtr)
ENUMINST(IntMax)
ENUMINST(Integer)

BOUNDED(Int8)
BOUNDED(Int16)
BOUNDED(Int32)
BOUNDED(Int64)
BOUNDED(IntPtr)
BOUNDED(IntMax)

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
