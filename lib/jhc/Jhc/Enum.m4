m4_divert(-1)
m4_dnl simple macros for defining instances for classes in Jhc.Enum

m4_define(ENUMINST,{{
instance Enum $1 where
    toEnum = fromInt
    fromEnum = toInt
    succ = increment$1
    pred = decrement$1
    enumFrom c        = [c .. maxBound]
    enumFromThen c c' = last `seq` [c, c' .. last]
                      where last | c' < c    = minBound
                                 | True      = maxBound
    enumFromTo x y = f x where
        f x | x > y = []
            | True  = x:f (x + 1)
    enumFromThenTo x y z | y >= x = inc `seq` z `seq` f x where
        inc = y - x
        f x | x <= z = x:f (x + inc)
            | True   = []
    enumFromThenTo x y z  = dec `seq` z `seq` f x where
        dec = x - y
        f x | x >= z = x:f (x - dec)
            | True   = []

foreign import primitive "increment" increment$1 :: $1 -> $1
foreign import primitive "decrement" decrement$1 :: $1 -> $1

}})

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
