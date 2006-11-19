{-# OPTIONS_JHC -N -fffi #-}
module Jhc.Prim(
    World__(),
    Int__(),
    Addr__(),
    runRaw,
    unsafeCoerce__,
    dependingOn
    ) where


-- | this is treated very specially by the compiler. it is unboxed.
data World__ :: #

data Int__ :: #
data Addr__ :: #

-- | this is wrapped around arbitrary expressions and just evaluates them to whnf
foreign import primitive "seq" runRaw :: a -> World__ -> World__


foreign import primitive "unsafeCoerce" unsafeCoerce__ :: a -> b

-- throws away first argument. but causes second argument to artificially depend on it.
foreign import primitive drop__ :: forall a b. a -> b -> b

-- like 'const' but creates an artificial dependency on its second argument to guide optimization.
dependingOn :: b -> a -> b
dependingOn a b = drop__ b a
