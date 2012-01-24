m4_divert(-1)


m4_define(INST_STORABLE,{{

instance Storable $1 where
    peek (Ptr addr) = IO $ \w -> case peek$3 addr w of
        (# w', r #) -> (# w', $2 r #)
    poke (Ptr addr) ($2 v) = IO $ \w -> case poke$3 addr v w of
        w' -> (# w', () #)
    sizeOf _ = boxInt (sizeOf$3 0#)
    alignment _ = boxInt (alignmentOf$3 0#)

ONCE({{
foreign import primitive "peek.$4" peek$3 :: Addr__ -> UIO $3
foreign import primitive "poke.$4" poke$3 :: Addr__ -> $3 -> UIO_
foreign import primitive "sizeOf.$4" sizeOf$3 :: $3 -> Int__
foreign import primitive "alignmentOf.$4" alignmentOf$3 :: $3 -> Int__
}})

}})


m4_define(INST_STORABLE_XXX,{{

instance Storable $1 where
    peek (Ptr addr) = IO $ \w -> case peek$1 addr w of
        (# w', r #) -> (# w', box$1 r #)
    poke (Ptr addr) v = IO $ \w -> case poke$1 addr (unbox$1 v) w of
        w' -> (# w', () #)
    sizeOf _ = boxInt (sizeOf$1 0#)
    alignment _ = boxInt (alignmentOf$1 0#)

ONCE({{
foreign import primitive "peek.$3" peek$1 :: Addr__ -> UIO $2
foreign import primitive "poke.$3" poke$1 :: Addr__ -> $2 -> UIO_
foreign import primitive "sizeOf.$3" sizeOf$1 :: $2 -> Int__
foreign import primitive "alignmentOf.$3" alignmentOf$1 :: $2 -> Int__
}})

ONCE({{
foreign import primitive "box" box$1 :: $2 -> $1
foreign import primitive "unbox" unbox$1 :: $1 -> $2
}})

}})

m4_divert
