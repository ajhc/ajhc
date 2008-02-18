m4_divert(-1)
m4_dnl simple macros for defining instances for classes in Jhc.Order

m4_define(BOXBOOL,{{ONCE({{
foreign import primitive "box" boxBool :: Bool__ -> Bool
}})}})

m4_define(INST_EQ,{{
instance Eq $1 where
    $1 x == $1 y = boxBool (equals$2 x y)
    $1 x /= $1 y = boxBool (nequals$2 x y)
ONCE({{
foreign import primitive "Eq" equals$2 :: $2 -> $2 -> Bool__
foreign import primitive "NEq" nequals$2 :: $2 -> $2 -> Bool__
}})
BOXBOOL()
}})


m4_define(INST_ORDER,{{
instance Ord $1 where
    $1 x < $1 y = boxBool (lt$2 x y)
    $1 x > $1 y = boxBool (gt$2 x y)
    $1 x <= $1 y = boxBool (lte$2 x y)
    $1 x >= $1 y = boxBool (gte$2 x y)
ONCE({{
foreign import primitive "$3Lt" lt$3$2   :: $2 -> $2 -> Bool__
foreign import primitive "$3Lte" lte$3$2 :: $2 -> $2 -> Bool__
foreign import primitive "$3Gt" gt$3$2   :: $2 -> $2 -> Bool__
foreign import primitive "$3Gte" gte$3$2 :: $2 -> $2 -> Bool__
}})
BOXBOOL()
}})

m4_define(INST_EQORDER,{{INST_EQ($1,$2)INST_ORDER($1,$2,$3)}})

m4_divert


