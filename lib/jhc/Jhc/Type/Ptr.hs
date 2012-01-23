module Jhc.Type.Ptr where

import Jhc.Prim.Bits

data Ptr a = Ptr BitsPtr_
data FunPtr a = FunPtr BitsPtr_
