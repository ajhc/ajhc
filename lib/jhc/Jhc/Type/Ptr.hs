module Jhc.Type.Ptr where

import Jhc.Prim.Bits

data Ptr a = Ptr Addr_
data FunPtr a = FunPtr FunAddr_
