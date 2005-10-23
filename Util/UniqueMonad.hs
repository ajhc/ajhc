module Util.UniqueMonad(UniqT,Uniq, runUniq, runUniqT, execUniq1, execUniq, execUniqT) where


import GenUtil
import Data.Unique
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Identity


instance UniqueProducer IO where
    newUniq = do
        u <- newUnique
        return $ hashUnique u

instance Monad m =>  UniqueProducer (UniqT m) where
    newUniq = UniqT $ do
        modify (+1)
        get

-- | Run the transformer version of the unique int generator.
runUniqT :: Monad m =>  UniqT m a -> Int -> m (a,Int)
runUniqT (UniqT sm) s  = runStateT sm s

-- | Run the bare version of the unique int generator.
runUniq :: Int -> Uniq a -> (a,Int)
runUniq x y = runIdentity $ runUniqT y x

-- | Execute the bare unique int generator starting with 1.
execUniq1 :: Uniq a -> a
execUniq1 x = fst $ runUniq 1 x

-- | Execute the bare unique int generator starting with the suplied number.
execUniq :: Int -> Uniq a -> a
execUniq st x = fst $ runUniq st x

-- | Execute the transformer version of the unique int generator starting with the suplied number.
execUniqT :: Monad m =>  Int -> UniqT m a -> m a
execUniqT s (UniqT sm)  = liftM fst $ runStateT sm s

instance (Monad m, Monad (t m), MonadTrans t, UniqueProducer m) => UniqueProducer (t m) where
    newUniq = lift newUniq

-- | Unique integer generator monad transformer.
newtype UniqT m a = UniqT (StateT Int m a)
    deriving(Monad,  MonadTrans, Functor, MonadFix, MonadPlus)

instance MonadReader s m => MonadReader s (UniqT m) where
    ask = UniqT $  ask
    local f (UniqT x) = UniqT $ local f x

-- | Unique integer generator monad.
type Uniq a = UniqT Identity a
