{-# OPTIONS_JHC -fm4 -N -fffi -funboxed-tuples -funboxed-values #-}
module Jhc.Inst.Storable() where

m4_include(Foreign/Storable.m4)

import Jhc.Types
import Jhc.Float
import Foreign.Storable
import Jhc.Prim
import Jhc.Basics
import Jhc.Addr
import Jhc.Int
import Jhc.IO


INST_STORABLE(Float,Float32_,fbits32)
INST_STORABLE(Double,Float64_,fbits64)


