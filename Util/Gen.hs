
-- | similar to GenUtil but can rely on non-haskell 98 features
module Util.Gen(module Util.Gen, module GenUtil) where

import Control.Monad.Writer
import Control.Monad.Identity
import Data.Monoid
import List

import GenUtil hiding(replicateM)

mconcatMap f xs = mconcat (map f xs)
mconcatInter x xs = mconcat (intersperse x xs)

mconcatMapM f xs = mapM f xs >>= return . mconcat


runEither :: String -> Either String a -> a
runEither msg (Left fm) = error $ msg ++ " - " ++ fm
runEither _ (Right a) = a


travCollect :: Monoid w => (forall m . Monad m => (a -> m a) -> a -> m a) -> (a -> w) -> a -> w
travCollect fn col x = execWriter (fn (\x -> tell (col x) >> return x) x)

