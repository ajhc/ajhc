module Jhc.Addr where

import Data.Word

data Addr
data FunAddr

nullAddr = wordPtrToAddr 0
nullFunAddr = wordPtrToFunAddr 0

{-
addrToWordPtr :: Addr -> WordPtr
addrToWordPtr = integralCast
wordPtrToAddr :: WordPtr -> Addr
wordPtrToAddr = integralCast


wordPtrToFunAddr :: WordPtr -> FunAddr
wordPtrToFunAddr = integralCast
funAddrToWordPtr :: FunAddr -> WordPtr
funAddrToWordPtr = integralCast
-}
{-# INLINE plusAddr #-}
plusAddr addr off = wordPtrToAddr $ addrToWordPtr addr + fromInt off

--foreign import primitive unsafeCoerce :: a -> b
--foreign import primitive integralCast :: a -> b
foreign import primitive "integralCast" addrToWordPtr :: Addr -> WordPtr
foreign import primitive "integralCast" wordPtrToAddr :: WordPtr -> Addr
foreign import primitive "integralCast" wordPtrToFunAddr :: WordPtr -> FunAddr
foreign import primitive "integralCast" funAddrToWordPtr :: FunAddr -> WordPtr
