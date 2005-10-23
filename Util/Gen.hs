
-- | similar to GenUtil but can rely on non-haskell 98 features
module Util.Gen where

import Data.Monoid
import List

mconcatMap f xs = mconcat (map f xs)
mconcatInter x xs = mconcat (intersperse x xs)
