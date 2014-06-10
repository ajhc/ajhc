module Jhc.Prim.Basics where

import Jhc.Prim.Bits
import Jhc.Prim.Prim

infixr 9  .
infixr 0  $, $!, `seq`

-- These may be assumed to be exactly their definitions given here by the
-- compiler and specially optimized. Re-export them in any libraries rather than
-- redefining them.

{-# SUPERINLINE id, const, (.), ($), ($!), flip #-}

id x = x
const x _ = x
f . g = \x -> f (g x)
f $ x = f x
f $! x = x `seq` f x
flip f x y = f y x

-- we can desugar a bit more inteligently if we know this means True.
otherwise = True

--foreign import ccall unsafe "jhc_error"  error_ :: Addr_ -> a
foreign import primitive "error.Prelude.undefined" undefined :: a
foreign import primitive "unsafeCoerce" unsafeCoerce__ :: a -> b

-- like 'const' but creates an artificial dependency on its second argument to
-- guide optimization.

foreign import primitive dependingOn :: a -> b -> a

foreign import primitive seq :: a -> b -> b
