module Jhc.Prim.Wrapper where

import Jhc.Prim.IO

-- | when no exception wrapper is wanted
runNoWrapper :: IO a -> World__ -> World__
runNoWrapper (IO (ST run)) w = case run w of (# w, _ #) -> w

-- | this is wrapped around arbitrary expressions and just
-- evaluates them to whnf
foreign import primitive "seq" runRaw :: a -> World__ -> World__
