module MonadUtil where

import Control.Monad.Error
import Control.Monad.Identity
import Control.Monad
import Data.Monoid


class Monad m => ContextMonad c m | m -> c where
    withContext :: c -> m a -> m a

--class Monad m => UniqueProducerMonad m where
--    newUniq :: m Int

instance Error [String] where
    noMsg = []
    strMsg s = [s]


instance ContextMonad String (Either [String]) where
    withContext s (Right x) = Right x
    withContext s (Left cs) = Left  (s:cs)
    

runSimpleContextMonad :: Either [String] a -> a
runSimpleContextMonad (Left ss) = error $ unlines ss
runSimpleContextMonad (Right x) = x



instance Show a => Show (Identity a) where
    show x = show $ runIdentity x

