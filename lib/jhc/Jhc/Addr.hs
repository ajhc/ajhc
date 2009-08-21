{-# OPTIONS_JHC -N -fffi -funboxed-values -fm4 #-}

m4_include(Jhc/Order.m4)

module Jhc.Addr(
    Ptr(..),
    FunPtr(..),
    nullPtr,
    nullFunPtr,
    castPtr,
    plusPtr,
    minusPtr
    ) where

import Jhc.Basics
import Jhc.Int
import Jhc.Order
import Jhc.Prim
import Jhc.Types


data Ptr a = Ptr BitsPtr_
data FunPtr a = FunPtr BitsPtr_

nullPtr :: Ptr a
nullFunPtr :: FunPtr a
nullPtr = Ptr 0#
nullFunPtr = FunPtr 0#

INST_EQORDER((Ptr a),Ptr,BitsPtr_,U)
INST_EQORDER((FunPtr a),FunPtr,BitsPtr_,U)


{-# INLINE plusPtr #-}
plusPtr :: Ptr a -> Int -> Ptr a
plusPtr (Ptr addr) off = case unboxInt off of
    off_ -> Ptr (addr `plusWordPtr` intToPtr__ off_)

{-# INLINE minusPtr #-}
minusPtr :: Ptr a -> Ptr a -> Int
minusPtr (Ptr a1) (Ptr a2) = boxInt (ptrToInt__ (a1 `minusWP` a2))


foreign import primitive "Sx" intToPtr__ :: Int__ -> BitsPtr_
foreign import primitive "I2I" ptrToInt__ :: BitsPtr_ -> Int__

foreign import primitive "Add" plusWordPtr :: BitsPtr_ -> BitsPtr_ -> BitsPtr_
foreign import primitive "Sub" minusWP :: BitsPtr_ -> BitsPtr_ -> BitsPtr_


castPtr :: Ptr a -> Ptr b
castPtr (Ptr addr) = Ptr addr

castFunPtr :: FunPtr a -> FunPtr b
castFunPtr (FunPtr addr) = FunPtr addr

