module NameMonad(NameMonad(..), GenName(..), NameMT, runNameMT, runNameMT', freeNames) where

-- This may be horrid overdesign. I broke several principles I usually use to
-- prevent ones natural tendancy to overdesign.

import qualified Data.Set as Set
import Atom
import Control.Monad.State
import Control.Monad.Trans

-- | There are bound names and used names, the used names are always a superset of the bound names.
-- used names will not be chosen for any new bindings, bound names should be renamed if encountered.

class Monad m => NameMonad n m | m -> n  where
    -- | Add to list of used names
    addNames :: [n] -> m ()
    -- | Choose a new name, adding it to both bound and used sets.
    newName :: m n
    -- | choose the first available name from list
    newNameFrom :: [n] -> m n
    -- | choose a new name if n is bound, else return n adding n to the bound names list
    uniqueName :: n -> m n


    --  | get bound names
    -- getNames :: m [n]

-- | Generating names.

class GenName n where
    -- | Generate a list of candidate names given a seed
    genNames :: Int -> [n]

instance GenName Int where
    genNames i = [st, st + 2 ..]  where
        st = abs i + 2 + abs i `mod` 2

instance GenName Atom where
    genNames i = map (toAtom . show) [abs i..]

-- | Generate an infinite list of names not present in the given set.
freeNames :: (Ord n,GenName n) => Set.Set n -> [n]
freeNames s  = filter (not . (`Set.member` s)) (genNames (Set.size s))

instance (Monad m, Monad (t m), MonadTrans t, NameMonad n m) => NameMonad n (t m) where
    addNames n = lift $ addNames n
    newName = lift  newName
    newNameFrom y = lift $ newNameFrom y
    uniqueName y = lift $ uniqueName y

    --getNames = lift getNames

-- | Name monad transformer.
newtype NameMT n m a = NameMT (StateT (Set.Set n, Set.Set n) m a)
    deriving(Monad, MonadTrans, Functor, MonadFix, MonadPlus, MonadIO)

-- | Run the name monad transformer.
runNameMT :: (Monad m) => NameMT a1 m a -> m a
runNameMT (NameMT x) = liftM fst $ runStateT x (Set.empty,Set.empty)

runNameMT' :: (Monad m) => NameMT a1 m a -> m (a,Set.Set a1)
runNameMT' (NameMT x) = do
    (r,(used,bound)) <- runStateT x (Set.empty,Set.empty)
    return (r,bound)

fromNameMT :: NameMT n m a -> StateT (Set.Set n, Set.Set n) m a
fromNameMT (NameMT x) = x

instance (GenName n,Ord n,Monad m) => NameMonad n (NameMT n m) where
    addNames ns = NameMT $ do
        modify (\ (used,bound) -> (Set.fromList ns `Set.union` used, bound) )
    uniqueName n = NameMT $ do
        (used,bound) <- get
        if n `Set.member` bound then fromNameMT newName else put (Set.insert n used,Set.insert n bound) >> return n
    newNameFrom vs = NameMT $ do
        (used,bound) <- get
        let f (x:xs)
                | x `Set.member` used = f xs
                | otherwise = x
            f [] = error "newNameFrom: finite list!"
            nn = f vs
        put (Set.insert nn used, Set.insert nn bound)
        return nn
    newName  = NameMT $ do
        (used,bound) <- get
        fromNameMT $ newNameFrom  (genNames (Set.size used + Set.size bound))


