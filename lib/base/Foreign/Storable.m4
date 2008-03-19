

m4_define(INST_STORABLE,{{

instance Storable $1 where
    peek (Ptr (Addr addr)) = IO $ \w -> case peek$2 addr w of
        (# w', r #) -> (# w', $1 r #)
    poke (Ptr (Addr addr)) ($1 v) = IO $ \w -> case poke$2 v addr w of
        w' -> (# w', () #)
    sizeOf _ = boxInt (sizeOf$2 0#)
    alignment _ = boxInt (alignmentOf$2 0#)

ONCE({{
foreign import primitive "peek.$3" peek$2 :: Addr__ -> UIO $2
foreign import primitive "poke.$3" poke$2 :: $2 -> Addr__ -> UIO_
foreign import primitive "sizeOf.$3" sizeOf$2 :: $2 -> Int__
foreign import primitive "alignmentOf.$3" alignmentOf$2 :: $2 -> Int__
}})

}})

