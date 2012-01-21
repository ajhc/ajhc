{-# OPTIONS_JHC -fm4 -fno-prelude -fffi #-}
module Jhc.Inst.Order() where


m4_include(Jhc/Order.m4)

import Data.Word
import Data.Int
import Jhc.Order
import Jhc.Prim
import Foreign.C.Types

INST_EQORDER(Int8,,Int8,)
INST_EQORDER(Int16,,Int16,)
INST_EQORDER(Int32,,Int32,)
INST_EQORDER(Int64,,Int64,)
INST_EQORDER(IntPtr,,IntPtr,)
INST_EQORDER(IntMax,,IntMax,)

INST_EQORDER(Word,,Word,U)
INST_EQORDER(Word8,,Word8,U)
INST_EQORDER(Word16,,Word16,U)
INST_EQORDER(Word32,,Word32,U)
INST_EQORDER(Word64,,Word64,U)
INST_EQORDER(WordPtr,,WordPtr,U)
INST_EQORDER(WordMax,,WordMax,U)


INST_EQORDER(CChar,,CChar,)
INST_EQORDER(CUChar,,CUChar,)
INST_EQORDER(CSChar,,CSChar,)
INST_EQORDER(CWchar,,CWchar,)
INST_EQORDER(CInt,,CInt,)
INST_EQORDER(CUInt,,CUInt,)
INST_EQORDER(CSize,,CSize,)
