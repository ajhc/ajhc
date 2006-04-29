
-- | similar to GenUtil but can rely on non-haskell 98 features
module Util.Gen where

import Control.Monad.Writer
import Control.Monad.Identity
import Data.Monoid
import List

mconcatMap f xs = mconcat (map f xs)
mconcatInter x xs = mconcat (intersperse x xs)

mconcatMapM f xs = mapM f xs >>= return . mconcat



travCollect :: Monoid w => (forall m . Monad m => (a -> m a) -> a -> m a) -> (a -> w) -> a -> w
travCollect fn col x = execWriter (fn (\x -> tell (col x) >> return x) x)

