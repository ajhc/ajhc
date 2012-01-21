{-# OPTIONS_JHC -fm4 -fno-prelude -fffi -funboxed-tuples -funboxed-values #-}
module Jhc.Inst.Storable() where



import Data.Int
import Data.Word
import Foreign.C.Types
import Foreign.Storable
import Jhc.Addr
import Jhc.Basics
import Jhc.Float
import Jhc.Int
import Jhc.IO
import Jhc.Prim
import Jhc.Types

m4_include(Foreign/Storable.m4)

INST_STORABLE(Float,Float,Float32_,fbits32)
INST_STORABLE(Double,Double,Float64_,fbits64)


INST_STORABLE_XXX(Int,Bits32_,bits32)
INST_STORABLE_XXX(Word,Bits32_,bits32)

INST_STORABLE_XXX(Int8,Bits8_,bits8)
INST_STORABLE_XXX(Word8,Bits8_,bits8)

INST_STORABLE_XXX(Int16,Bits16_,bits16)
INST_STORABLE_XXX(Word16,Bits16_,bits16)

INST_STORABLE_XXX(Int32,Bits32_,bits32)
INST_STORABLE_XXX(Word32,Bits32_,bits32)
INST_STORABLE_XXX(Int64,Bits64_,bits64)
INST_STORABLE_XXX(Word64,Bits64_,bits64)

INST_STORABLE_XXX(IntMax,BitsMax_,bits<max>)
INST_STORABLE_XXX(WordMax,BitsMax_,bits<max>)
INST_STORABLE_XXX(IntPtr,BitsPtr_,bits<ptr>)
INST_STORABLE_XXX(WordPtr,BitsPtr_,bits<ptr>)

--type BitsWchar_ = Bits32_
--type BitsSize_ = BitsPtr_
--type BitsInt_ = Bits32_

INST_STORABLE_XXX(CChar,Bits8_,bits8)
INST_STORABLE_XXX(CSChar,Bits8_,bits8)
INST_STORABLE_XXX(CUChar,Bits8_,bits8)
INST_STORABLE_XXX(CInt,BitsInt_,bits<int>)
INST_STORABLE_XXX(CUInt,BitsInt_,bits<int>)
INST_STORABLE_XXX(CWchar,BitsWchar_,bits<wchar_t>)
--XINST_STORABLE_XXX(CWchar,BitsWchar_,bits32)
INST_STORABLE_XXX(CSize,BitsSize_,bits<size_t>)
