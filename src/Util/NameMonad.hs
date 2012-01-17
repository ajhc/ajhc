module Util.NameMonad(NameMonad(..),GenName(..),NameMT,runNameMT,runNameMT',freeNames,mixInt,mixInt3,hashInt) where

import Control.Monad.State
import Data.Bits
import Data.Word
import qualified Data.Set as Set

-- | There are bound names and used names, the used names are always a superset of the bound names.
-- used names will not be chosen for any new bindings, bound names should be renamed if encountered.

class Monad m => NameMonad n m | m -> n  where
    -- | Add to list of used names
    addNames :: [n] -> m ()
    -- | Add to list of bound names
    addBoundNames :: [n] -> m ()
    -- | Choose a new name, adding it to both bound and used sets.
    newName :: m n
    -- | choose the first available name from list
    newNameFrom :: [n] -> m n
    -- | choose a new name if n is bound, else return n adding n to the bound names list
    uniqueName :: n -> m n

    -- in case we only have a concept of bound names
    addNames = addBoundNames

-- | Generating names.

class GenName n where
    -- | Generate a list of candidate names given a seed
    genNames :: Int -> [n]

instance GenName Int where
    genNames i = [st, st + 2 ..]  where
        st = abs i + 2 + abs i `mod` 2

-- | Generate an infinite list of names not present in the given set.
freeNames :: (Ord n,GenName n) => Set.Set n -> [n]
freeNames s  = filter (not . (`Set.member` s)) (genNames (Set.size s))

instance (Monad m, Monad (t m), MonadTrans t, NameMonad n m) => NameMonad n (t m) where
    addNames n = lift $ addNames n
    addBoundNames n = lift $ addBoundNames n
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
    addBoundNames ns = NameMT $ do
        let nset = Set.fromList ns
        modify (\ (used,bound) -> (nset `Set.union` used, nset `Set.union` bound) )
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
        fromNameMT $ newNameFrom  (genNames (Set.size used `mixInt` Set.size bound))

hashInt :: Int -> Int
hashInt x = fromIntegral $ f (fromIntegral x) where
    f :: Word -> Word
    f a = a''''' where
        !a' = (a `xor` 61) `xor` (a `shiftR` 16)
        !a'' = a' + (a' `shiftL` 3)
        !a''' = a'' `xor` (a'' `shiftR` 4)
        !a'''' = a''' * 0x27d4eb2d
        !a''''' = a'''' `xor` (a'''' `shiftR` 15)

mixInt :: Int -> Int -> Int
mixInt x y = hashInt x - hashInt y

mixInt3 :: Int -> Int -> Int -> Int
mixInt3 x y z = (hashInt x - hashInt y) `xor` hashInt z

