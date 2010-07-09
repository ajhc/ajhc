m4_divert(-1)

m4_define(BITSINST,{{

instance Bits $1 where
    x .&. y = and$1 x y
    x .|. y = or$1 x y
    x `xor` y = xor$1 x y
    complement x = complement$1 x
    shiftL i x = shiftL$1 x i
    shiftR i x = shiftR$1 x i

foreign import primitive "And" and$1 :: $1 -> $1 -> $1
foreign import primitive "Or" or$1 :: $1 -> $1 -> $1
foreign import primitive "Xor" xor$1 :: $1 -> $1 -> $1
foreign import primitive "Shr$2" shiftR$1 :: Int -> $1 -> $1
foreign import primitive "Shl" shiftL$1 :: Int -> $1 -> $1
foreign import primitive "Com" complement$1 :: $1 -> $1

}})

m4_divert
