module Util.ContextMonad where

import Control.Monad.Error
import Control.Monad.Identity
import Control.Monad


class Monad m => ContextMonad c m | m -> c where
    withContext :: c -> m a -> m a


instance Error [String] where
    noMsg = []
    strMsg s = [s]


instance ContextMonad String (Either [String]) where
    withContext s (Right x) = Right x
    withContext s (Left cs) = Left  (s:cs)


runSimpleContextMonad :: Either [String] a -> a
runSimpleContextMonad (Left ss) = error $ unlines ss
runSimpleContextMonad (Right x) = x


