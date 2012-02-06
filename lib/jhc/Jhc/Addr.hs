{-# OPTIONS_JHC -fno-prelude -fffi -funboxed-values -fm4 #-}
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
import Jhc.Type.Ptr

m4_include(Jhc/Order.m4)

nullPtr :: Ptr a
nullFunPtr :: FunPtr a
nullPtr = Ptr 0#
nullFunPtr = FunPtr 0#

INST_EQORDER((Ptr a),Ptr,Addr_,U)
INST_EQORDER((FunPtr a),FunPtr,FunAddr_,U)

{-# INLINE plusPtr #-}
plusPtr :: Ptr a -> Int -> Ptr a
plusPtr (Ptr addr) off = case unboxInt off of
    off_ -> Ptr (addr `plusWordPtr` intToPtr__ off_)

{-# INLINE minusPtr #-}
minusPtr :: Ptr a -> Ptr a -> Int
minusPtr (Ptr a1) (Ptr a2) = boxInt (a1 `minusWP` a2)

foreign import primitive "Sx" intToPtr__ :: Int__ -> Addr_
foreign import primitive "I2I" ptrToInt__ :: Addr_ -> Int__

foreign import primitive "Add" plusWordPtr :: Addr_ -> Addr_ -> Addr_
foreign import primitive "Sub" minusWP :: Addr_ -> Addr_ -> Int__

castPtr :: Ptr a -> Ptr b
castPtr (Ptr addr) = Ptr addr

castFunPtr :: FunPtr a -> FunPtr b
castFunPtr (FunPtr addr) = FunPtr addr
