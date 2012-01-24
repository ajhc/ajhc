m4_divert(-1)

m4_define(INST_STORABLE_X,{{
instance Storable $1 where
    peek (Ptr addr) = IO $ \w -> case peek$2 addr w of
        (# w', r #) -> (# w', $2 r #)
    poke (Ptr addr) ($2 v) = IO $ \w -> case poke$2 addr v w of
        w' -> (# w', () #)
    sizeOf _ = Int (sizeOf$2 0#)
    alignment _ = Int (alignmentOf$2 0#)
ONCE({{
foreign import primitive "peek.$4" peek$2 :: Addr__ -> UIO $3
foreign import primitive "poke.$4" poke$2 :: Addr__ -> $3 -> UIO_
foreign import primitive "sizeOf.$4" sizeOf$2 :: Int__ -> Int__
foreign import primitive "alignmentOf.$4" alignmentOf$2 :: Int__ -> Int__
}})
}})

m4_define(INST_STORABLE,{{INST_STORABLE_X($1,$1,$2,$3)}})

m4_divert
