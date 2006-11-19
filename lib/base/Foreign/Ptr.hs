{-# OPTIONS_JHC -fffi #-}
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


import Jhc.Addr
import Foreign.Storable



instance Storable (Ptr a) where
    sizeOf (Ptr a) = sizeOf a
    alignment (Ptr a) = alignment a
    peek p = peek (castPtr p) >>= return . Ptr
    poke p (Ptr x) = poke (castPtr p) x

instance Eq (Ptr a) where
    Ptr a == Ptr b = a == b
    Ptr a /= Ptr b = a /= b

instance Ord (Ptr a) where
    compare (Ptr a) (Ptr b) = compare a b
    Ptr a <= Ptr b = a <= b
    Ptr a < Ptr b = a < b
    Ptr a > Ptr b = a > b
    Ptr a >= Ptr b = a >= b

instance Show (Ptr a) where
    showsPrec n (Ptr x) = showsPrec n (toInteger (addrToWordPtr  x))

nullPtr :: Ptr a
nullPtr = Ptr nullAddr

plusPtr :: Ptr a -> Int -> Ptr b
plusPtr (Ptr addr) off = Ptr (plusAddr addr off)

minusPtr :: Ptr a -> Int -> Ptr b
minusPtr (Ptr addr) off = Ptr (plusAddr addr (negate off))

castPtr :: Ptr a -> Ptr b
castPtr (Ptr addr) = Ptr addr

alignPtr :: Ptr a -> Int -> Ptr a
alignPtr = error "alignPtr"
--alignPtr addr@(Ptr a) (I# i)
--  = case remAddr# a i of {
--      0# -> addr;
--      n -> Ptr (plusAddr# a (i -# n)) }



nullFunPtr = FunPtr nullFunAddr
castFunPtr (FunPtr addr) = FunPtr addr

--castFunPtrToPtr :: FunPtr a -> Ptr b
--castFunPtrToPtr = unsafeCoerce

--castPtrToFunPtr :: Ptr a -> FunPtr b
--castPtrToFunPtr = unsafeCoerce


foreign import primitive "integralCast" castFunPtrToPtr :: FunPtr a -> Ptr b
foreign import primitive "integralCast" castPtrToFunPtr :: Ptr a -> FunPtr b


