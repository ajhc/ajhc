module UniqueMonad(UniqT,Uniq, runUniq, runUniqT, execUniq1, execUniq, execUniqT) where


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

runUniqT :: Monad m =>  UniqT m a -> Int -> m (a,Int)
runUniqT (UniqT sm) s  = runStateT sm s

runUniq :: Int -> Uniq a -> (a,Int)
runUniq x y = runIdentity $ runUniqT y x

execUniq1 x = fst $ runUniq 1 x  
execUniq st x = fst $ runUniq st x  

execUniqT :: Monad m =>  Int -> UniqT m a -> m a
execUniqT s (UniqT sm)  = liftM fst $ runStateT sm s

instance (Monad m, Monad (t m), MonadTrans t, UniqueProducer m) => UniqueProducer (t m) where
    newUniq = lift newUniq

newtype UniqT m a = UniqT (StateT Int m a)
    deriving(Monad,  MonadTrans, Functor, MonadFix, MonadPlus)

instance MonadReader s m => MonadReader s (UniqT m) where
    ask = UniqT $  ask
    local f (UniqT x) = UniqT $ local f x
    
type Uniq a = UniqT Identity a
--newtype Uniq a = Uniq (UniqT Identity)
--    deriving(Monad,UniqueProducer)
