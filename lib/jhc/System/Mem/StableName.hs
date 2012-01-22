{-# OPTIONS_JHC -fno-prelude -fffi -fm4   #-}
module System.Mem.StableName(StableName(),makeStableName,hashStableName) where

import Jhc.Basics
import Jhc.IO
import Jhc.Order
import Jhc.Prim.Bits

m4_include(Jhc/Order.m4)

data StableName a = StableName BitsPtr_

makeStableName :: a -> IO (StableName a)
makeStableName x = returnIO $ StableName (toHeapAddr x)

hashStableName :: StableName a -> Int
hashStableName (StableName a) = bitsPtrToInt (jhc_hashptr a)

foreign import primitive toHeapAddr :: a -> BitsPtr_
foreign import primitive "U2I" bitsPtrToInt :: BitsPtr_ -> Int
foreign import jhc_hashptr :: BitsPtr_ -> BitsPtr_

INST_EQORDER((StableName a),StableName,BitsPtr_,U)
