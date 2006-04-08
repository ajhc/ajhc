module Name.Id(
    Id(),
    idSetToList,
    runIdNameT',
    runIdNameT,
    IdNameT(),
    IdSet()
    )where

import Control.Monad
import Control.Monad.State
import qualified Data.IntSet as IS
import Data.Monoid
import Data.Typeable

import Util.SetLike as S
import Util.HasSize
import Util.NameMonad

-- TODO - make this a newtype
type Id = Int

newtype IdSet = IdSet IS.IntSet
    deriving(Typeable,Monoid,HasSize,SetLike)

instance BuildSet IdSet Id where
    fromList xs = IdSet $ IS.fromList (map idToInt xs)
    fromDistinctAscList xs = IdSet $ IS.fromDistinctAscList (map idToInt xs)
    member x (IdSet s) = IS.member (idToInt x) s
    insert x (IdSet s) = IdSet $ IS.insert (idToInt x) s
    delete x (IdSet s) = IdSet $ IS.delete (idToInt x) s
    singleton x = IdSet $ IS.singleton (idToInt x)

idSetToList :: IdSet -> [Id]
idSetToList (IdSet is) = IS.toList is

idToInt :: Id -> Int
idToInt = id

-- | Name monad transformer.
newtype IdNameT m a = IdNameT (StateT (IdSet, IdSet) m a)
    deriving(Monad, MonadTrans, Functor, MonadFix, MonadPlus, MonadIO)

-- | Run the name monad transformer.
runIdNameT :: (Monad m) => IdNameT m a -> m a
runIdNameT (IdNameT x) = liftM fst $ runStateT x (mempty,mempty)

runIdNameT' :: (Monad m) => IdNameT m a -> m (a,IdSet)
runIdNameT' (IdNameT x) = do
    (r,(used,bound)) <- runStateT x (mempty,mempty)
    return (r,bound)

fromIdNameT (IdNameT x) = x

instance Monad m => NameMonad Id (IdNameT m) where
    addNames ns = IdNameT $ do
        modify (\ (used,bound) -> (fromList ns `union` used, bound) )
    addBoundNames ns = IdNameT $ do
        let nset = fromList ns
        modify (\ (used,bound) -> (nset `union` used, nset `union` bound) )
    uniqueName n = IdNameT $ do
        (used,bound) <- get
        if n `member` bound then fromIdNameT newName else put (insert n used,insert n bound) >> return n
    newNameFrom vs = IdNameT $ do
        (used,bound) <- get
        let f (x:xs)
                | x `member` used = f xs
                | otherwise = x
            f [] = error "newNameFrom: finite list!"
            nn = f vs
        put (insert nn used, insert nn bound)
        return nn
    newName  = IdNameT $ do
        (used,bound) <- get
        let genNames i = [st, st + 2 ..]  where
                st = abs i + 2 + abs i `mod` 2
        fromIdNameT $ newNameFrom  (genNames (size used + size bound))


