{-# LANGUAGE UnboxedTuples, ForeignFunctionInterface, NoImplicitPrelude #-}
module Jhc.Prim(module Jhc.Prim.Bits, module Jhc.Prim, module Jhc.Prim.Prim, module Jhc.Prim.IO, module Jhc.Prim.Type.Basic, module Jhc.Prim.Type.Word) where

import Jhc.Prim.Bits
import Jhc.String
import Jhc.Prim.IO
import Jhc.Type.Word

-- | this is wrapped around arbitrary expressions and just evaluates them to whnf
foreign import primitive "seq" runRaw :: a -> World__ -> World__
foreign import primitive "unsafeCoerce" unsafeCoerce__ :: a -> b

-- like 'const' but creates an artificial dependency on its second argument to guide optimization.
foreign import primitive dependingOn :: a -> b -> a
