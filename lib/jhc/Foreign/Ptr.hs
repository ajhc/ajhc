{-# OPTIONS_JHC -fno-prelude -fffi -funboxed-tuples #-}

module Foreign.Ptr(
    Ptr(),
    nullPtr,
    castPtr,
    plusPtr,
    alignPtr,
    minusPtr,
    FunPtr(),
    nullFunPtr,
    castFunPtr,
    castFunPtrToPtr,
    castPtrToFunPtr,
    freeHaskellFunPtr
    ) where

import Data.Word
import Foreign.Storable
import Jhc.Addr
import Jhc.Basics
import Jhc.IO
import Jhc.Inst.Show
import Jhc.Monad
import Jhc.Num
import Jhc.Order
import Jhc.Prim.Bits
import Jhc.Show

instance Show (Ptr a) where
    showsPrec n x = showsPrec n (toInteger (ptrToWordPtr  x))

alignPtr :: Ptr a -> Int -> Ptr a
alignPtr = error "alignPtr"
--alignPtr addr@(Ptr a) (I# i)
--  = case remAddr# a i of {
--      0# -> addr;
--      n -> Ptr (plusAddr# a (i -# n)) }

castFunPtr :: FunPtr a -> FunPtr b
castFunPtr (FunPtr x) = FunPtr x

castFunPtrToPtr :: FunPtr a -> Ptr b
castFunPtrToPtr (FunPtr (FunAddr_ x)) = Ptr (Addr_ x)

castPtrToFunPtr :: Ptr a -> FunPtr b
castPtrToFunPtr (Ptr (Addr_ x)) = FunPtr (FunAddr_ x)

freeHaskellFunPtr :: FunPtr a -> IO ()
freeHaskellFunPtr _ = error "freeHaskellFunPtr"

--foreign import primitive "U2U" ptrToWordPtr :: Ptr a -> WordPtr
--foreign import primitive "U2U" wordPtrToPtr :: WordPtr -> Ptr a

ptrToWordPtr :: Ptr a -> WordPtr
ptrToWordPtr (Ptr w) = boxWordPtr w

wordPtrToPtr :: WordPtr -> Ptr a
wordPtrToPtr w = Ptr (unboxWordPtr w)

foreign import primitive "box" boxWordPtr :: Addr_ -> WordPtr
foreign import primitive "unbox" unboxWordPtr :: WordPtr -> Addr_
