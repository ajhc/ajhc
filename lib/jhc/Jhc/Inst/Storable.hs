{-# OPTIONS_JHC -fm4 -fno-prelude -fffi -funboxed-tuples -funboxed-values #-}
module Jhc.Inst.Storable() where

import Jhc.Type.C
import Foreign.Storable
import Jhc.Addr
import Jhc.Basics
import Jhc.Float
import Jhc.IO
import Jhc.Type.Word
-- CI import Jhc.Int
-- CI import Jhc.Prim
-- CI import Jhc.Prim.Bits

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

INST_STORABLE_Y(CChar,Bits8_,bits8)
INST_STORABLE_Y(CSChar,Bits8_,bits8)
INST_STORABLE_Y(CUChar,Bits8_,bits8)
INST_STORABLE_Y(CInt,Int_,bits<int>)
INST_STORABLE_Y(CUInt,Word_,bits<int>)
INST_STORABLE_Y(CWchar,Bits32_,bits<wchar_t>)
INST_STORABLE_Y(CSize,BitsPtr_,bits<size_t>)
