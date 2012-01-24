{-# OPTIONS_JHC -fm4 -fno-prelude -fffi -funboxed-tuples -funboxed-values #-}
module Jhc.Inst.Storable() where

import Data.Int
import Data.Word
import Jhc.Type.Word
import Jhc.Type.C
import Foreign.Storable
import Jhc.Addr
import Jhc.Basics
import Jhc.Float
import Jhc.IO
import Jhc.Int
import Jhc.Prim
import Jhc.Prim.Bits

m4_include(Foreign/Storable.m4)

INST_STORABLE(Float,Float32_,fbits32)
INST_STORABLE(Double,Float64_,fbits64)

INST_STORABLE(Int,Bits32_,bits32)
INST_STORABLE(Word,Bits32_,bits32)

INST_STORABLE(Int8,Bits8_,bits8)
INST_STORABLE(Word8,Bits8_,bits8)
INST_STORABLE(Int16,Bits16_,bits16)
INST_STORABLE(Word16,Bits16_,bits16)
INST_STORABLE(Int32,Bits32_,bits32)
INST_STORABLE(Word32,Bits32_,bits32)
INST_STORABLE(Int64,Bits64_,bits64)
INST_STORABLE(Word64,Bits64_,bits64)

INST_STORABLE(IntMax,BitsMax_,bits<max>)
INST_STORABLE(WordMax,BitsMax_,bits<max>)
INST_STORABLE(IntPtr,BitsPtr_,bits<ptr>)
INST_STORABLE(WordPtr,BitsPtr_,bits<ptr>)

INST_STORABLE(CChar,Int8,bits8)
INST_STORABLE(CSChar,Int8,bits8)
INST_STORABLE(CUChar,Word8,bits8)
INST_STORABLE(CInt,Int,bits<int>)
INST_STORABLE(CUInt,Word,bits<int>)
INST_STORABLE(CWchar,Word32,bits<wchar_t>)
INST_STORABLE(CSize,WordPtr,bits<size_t>)
