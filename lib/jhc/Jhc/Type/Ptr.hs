module Jhc.Type.Ptr where

import Jhc.Prim.Bits

data {-# CTYPE "HsPtr" #-} Ptr a = Ptr Addr_
data {-# CTYPE "HsFunPtr" #-} FunPtr a = FunPtr FunAddr_
