{-# LANGUAGE UnboxedTuples, ForeignFunctionInterface, NoImplicitPrelude #-}
module Jhc.Prim(module Jhc.Prim.Bits, module Jhc.Prim, module Jhc.Prim.IO) where

import Jhc.Prim.Bits
import Jhc.Prim.IO

-- | this is wrapped around arbitrary expressions and just evaluates them to whnf
foreign import primitive "seq" runRaw :: a -> World__ -> World__
foreign import primitive "unsafeCoerce" unsafeCoerce__ :: a -> b

-- like 'const' but creates an artificial dependency on its second argument to guide optimization.
foreign import primitive dependingOn :: a -> b -> a
