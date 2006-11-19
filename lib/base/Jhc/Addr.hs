{-# OPTIONS_JHC -N -fffi #-}

module Jhc.Addr(
    Addr(),
    FunAddr(),
    Ptr(..),
    FunPtr(..),
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

data Addr
data FunAddr

newtype Ptr a = Ptr Addr
newtype FunPtr a = FunPtr FunAddr

nullAddr = wordPtrToAddr zeroWordPtr
nullFunAddr = wordPtrToFunAddr zeroWordPtr


{-# INLINE plusAddr #-}
plusAddr :: Addr -> Int -> Addr
plusAddr addr off = wordPtrToAddr (addrToWordPtr addr `plusWordPtr` intToWordPtr off)

--foreign import primitive unsafeCoerce :: a -> b
--foreign import primitive integralCast :: a -> b
foreign import primitive "integralCast" addrToWordPtr :: Addr -> WordPtr
foreign import primitive "integralCast" wordPtrToAddr :: WordPtr -> Addr
foreign import primitive "integralCast" wordPtrToFunAddr :: WordPtr -> FunAddr
foreign import primitive "integralCast" funAddrToWordPtr :: FunAddr -> WordPtr

foreign import primitive "integralCast" intToWordPtr :: Int -> WordPtr

foreign import primitive "zero" zeroWordPtr :: WordPtr
foreign import primitive "plus" plusWordPtr :: WordPtr -> WordPtr -> WordPtr


