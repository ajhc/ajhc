{-# OPTIONS_JHC -N -fffi #-}

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

import Data.Word
import Data.Int
import Jhc.Prim

data Addr
data FunAddr

newtype Ptr a = Ptr Addr
newtype FunPtr a = FunPtr FunAddr

nullAddr = wordPtrToAddr zeroWordPtr
nullFunAddr = wordPtrToFunAddr zeroWordPtr


{-# INLINE plusAddr #-}
plusAddr :: Addr -> Int -> Addr
plusAddr addr off = wordPtrToAddr (addrToWordPtr addr `plusWordPtr` intToWordPtr off)

foreign import primitive "U2U" addrToWordPtr :: Addr -> WordPtr
foreign import primitive "U2U" wordPtrToAddr :: WordPtr -> Addr
foreign import primitive "U2U" wordPtrToFunAddr :: WordPtr -> FunAddr
foreign import primitive "U2U" funAddrToWordPtr :: FunAddr -> WordPtr

foreign import primitive "Sx" intToWordPtr :: Int -> WordPtr

foreign import primitive "zero" zeroWordPtr :: WordPtr
foreign import primitive "Add" plusWordPtr :: WordPtr -> WordPtr -> WordPtr

foreign import primitive "box" boxAddr :: Addr__ -> Addr

ptrFromAddr__ :: Addr__ -> Ptr a
ptrFromAddr__ addr = Ptr (boxAddr addr)

