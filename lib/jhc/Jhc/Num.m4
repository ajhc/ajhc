m4_divert(-1)
m4_dnl simple macros for defining instances for classes in Jhc.Enum


m4_define(MkNumPrim,{{
instance Num $1 where
    (+) = add$1
    (-) = sub$1
    (*) = mul$1
    negate = negate$1
    abs x = if x < 0 then negate$1 x else x
    signum x = if x == 0 then x else (if x > 0 then one$1 else negate one$1)
    fromInteger = fromInteger$1
    fromInt     = fromInt$1

foreign import primitive "$2{{}}2$2" fromInt$1 :: Int -> $1
foreign import primitive "$2{{}}2$2" fromInteger$1 :: Integer -> $1

foreign import primitive "one" one$1    :: $1
foreign import primitive "Add" add$1    :: $1 -> $1 -> $1
foreign import primitive "Sub" sub$1    :: $1 -> $1 -> $1
foreign import primitive "Mul" mul$1    :: $1 -> $1 -> $1
foreign import primitive "Neg" negate$1 :: $1 -> $1
}})

m4_define(MkRealPrim,{{
instance Real $1 where
    toRational x = toInteger x :% 1
    toDouble x   =  toDouble$1 x

foreign import primitive "$2{{}}2F" toDouble$1 :: $1 -> Double
}})

m4_define(MkIntegralPrim,{{
instance Integral $1 where
    div = div$1
    quot = quot$1
    mod = mod$1
    rem = rem$1
    toInt = toInt$1
    toInteger = toInteger$1

foreign import primitive "I2I" toInt$1 :: $1 -> Int
foreign import primitive "I2I" toInteger$1 :: $1 -> Integer
foreign import primitive "Div" div$1   :: $1 -> $1 -> $1
foreign import primitive "Quot" quot$1 :: $1 -> $1 -> $1
foreign import primitive "Mod" mod$1   :: $1 -> $1 -> $1
foreign import primitive "Rem" rem$1   :: $1 -> $1 -> $1
}})

m4_define(MkIntegralUPrim,{{
instance Integral $1 where
    div   = udiv$1
    quot  = udiv$1
    mod   = umod$1
    rem   = umod$1
    toInt = toInt$1
    toInteger = toInteger$1

foreign import primitive "U2U" toInt$1 :: $1 -> Int
foreign import primitive "U2U" toInteger$1 :: $1 -> Integer
foreign import primitive "UDiv" udiv$1   :: $1 -> $1 -> $1
foreign import primitive "UMod" umod$1   :: $1 -> $1 -> $1
}})


m4_divert
