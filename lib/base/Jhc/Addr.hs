{-# OPTIONS_JHC -N -fffi -funboxed-values -fm4 #-}

m4_include(Jhc/Order.m4)
m4_include(Foreign/Storable.m4)

module Jhc.Addr(
    Addr(),
    FunAddr(),
    Ptr(..),
    FunPtr(..),
    ptrFromAddr__,
    nullAddr,
    nullFunAddr,
    plusAddr,
    addrToWordPtr,
    wordPtrToAddr,
    wordPtrToFunAddr,
    funAddrToWordPtr
    ) where

import Jhc.Int
import Data.Word
import Jhc.Prim
import Jhc.Types
import Jhc.Order
import Jhc.Basics
import Jhc.IO
import Foreign.Storable

data Addr = Addr BitsPtr_
data FunAddr = FunAddr BitsPtr_

newtype Ptr a = Ptr Addr
newtype FunPtr a = FunPtr FunAddr

nullAddr = Addr 0#
nullFunAddr = FunAddr 0#

INST_EQORDER(Addr,BitsPtr_)
INST_EQORDER(FunAddr,BitsPtr_)

INST_STORABLE(Addr,BitsPtr_,bits<ptr>)
INST_STORABLE(FunAddr,BitsPtr_,bits<ptr>)

{-# INLINE plusAddr #-}
plusAddr :: Addr -> Int -> Addr
plusAddr (Addr addr) off = case unboxInt off of
    off_ -> Addr (addr `plusWordPtr` intToPtr__ off_)

foreign import primitive "U2U" addrToWordPtr :: Addr -> WordPtr
foreign import primitive "U2U" wordPtrToAddr :: WordPtr -> Addr
foreign import primitive "U2U" wordPtrToFunAddr :: WordPtr -> FunAddr
foreign import primitive "U2U" funAddrToWordPtr :: FunAddr -> WordPtr

foreign import primitive "Sx" intToPtr__ :: Int__ -> BitsPtr_

foreign import primitive "Add" plusWordPtr :: BitsPtr_ -> BitsPtr_ -> BitsPtr_

ptrFromAddr__ :: Addr__ -> Ptr a
ptrFromAddr__ addr = Ptr (Addr addr)

