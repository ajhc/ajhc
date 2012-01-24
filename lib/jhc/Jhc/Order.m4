m4_divert(-1)
m4_dnl simple macros for defining instances for classes in Jhc.Order

m4_define(BOXBOOL,{{ONCE({{
foreign import primitive "box" boxBool :: Bool_ -> Bool
}})}})

m4_define(INST_EQ,{{
instance Eq $1 where
    $2 x == $2 y =  (equals$3 x y)
    $2 x /= $2 y =  (nequals$3 x y)
ONCE({{
foreign import primitive "Eq" equals$3 :: $3 -> $3 -> Bool
foreign import primitive "NEq" nequals$3 :: $3 -> $3 -> Bool
}})
BOXBOOL()
}})



m4_define(INST_ORDER,{{
instance Ord $1 where
    $2 x < $2 y =  (lt$4$3 x y)
    $2 x > $2 y =  (gt$4$3 x y)
    $2 x <= $2 y =  (lte$4$3 x y)
    $2 x >= $2 y =  (gte$4$3 x y)
ONCE({{
foreign import primitive "$4Lt" lt$4$3   :: $3 -> $3 -> Bool
foreign import primitive "$4Lte" lte$4$3 :: $3 -> $3 -> Bool
foreign import primitive "$4Gt" gt$4$3   :: $3 -> $3 -> Bool
foreign import primitive "$4Gte" gte$4$3 :: $3 -> $3 -> Bool
}})
BOXBOOL()
}})

m4_define(INST_EQORDER,{{INST_EQ($1,$2,$3)INST_ORDER($1,$2,$3,$4)}})

m4_define(BOUNDED,{{
instance Bounded $1 where
    maxBound = maxBound$1
    minBound = minBound$1

foreign import primitive "maxBound" maxBound$1 :: $1
foreign import primitive "minBound" minBound$1 :: $1

}})

m4_define(UBOUNDED,{{
instance Bounded $1 where
    maxBound = umaxBound$1
    minBound = zero$1

foreign import primitive "umaxBound" umaxBound$1 :: $1
foreign import primitive "zero" zero$1 :: $1

}})


m4_divert
