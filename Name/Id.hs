module Name.Id(
    Id(),
    IdMap(),
    IdNameT(),
    IdSet(),
    addBoundNamesIdMap,
    addBoundNamesIdSet,
    addNamesIdSet,
    idMapToIdSet,
    idNameBoundNames,
    idNameUsedNames,
    etherialIds,
    isEtherialId,
    isInvalidId,
    idSetToIdMap,
    mapMaybeIdMap,
    idSetFromList,
    idSetFromDistinctAscList,
    idMapFromList,
    idMapFromDistinctAscList,
    idSetToList,
    idMapToList,
    emptyId,
    newIds,
    newId,
    runIdNameT',
    runIdNameT
    )where

import Control.Monad.State
import Control.Monad.Reader
import Data.Traversable
import Data.Foldable
import Data.Monoid
import Data.Typeable
import System.Random
import Data.Bits
import qualified Data.IntMap  as IM
import qualified Data.IntSet as IS

import StringTable.Atom
import Util.HasSize
import Util.Inst()
import Util.NameMonad
import Util.SetLike as S
import Name.Name

-- TODO - make this a newtype
type Id = Int


-- IdSet


newtype IdSet = IdSet IS.IntSet
    deriving(Typeable,Monoid,HasSize,SetLike,BuildSet Id,ModifySet Id,IsEmpty,Eq,Ord)


idSetToList :: IdSet -> [Id]
idSetToList (IdSet is) = IS.toList is

idMapToList :: IdMap a -> [(Id,a)]
idMapToList (IdMap is) = IM.toList is

idToInt :: Id -> Int
idToInt = id

mapMaybeIdMap :: (a -> Maybe b) -> IdMap a -> IdMap b
mapMaybeIdMap fn (IdMap m) = IdMap (IM.mapMaybe fn m)


-- IdMap

newtype IdMap a = IdMap (IM.IntMap a)
    deriving(Typeable,Monoid,HasSize,SetLike,BuildSet (Id,a),MapLike Id a,Functor,Traversable,Foldable,IsEmpty,Eq,Ord)


idSetToIdMap :: (Id -> a) -> IdSet -> IdMap a
idSetToIdMap f (IdSet is) = IdMap $ IM.fromDistinctAscList [ (x,f x) |  x <- IS.toAscList is]

idMapToIdSet :: IdMap a -> IdSet
idMapToIdSet (IdMap im) = IdSet $ IS.fromDistinctAscList (IM.keys im)


-- | Name monad transformer.
newtype IdNameT m a = IdNameT (StateT (IdSet, IdSet) m a)
    deriving(Monad, MonadTrans, Functor, MonadFix, MonadPlus, MonadIO)

instance (MonadReader r m) => MonadReader r (IdNameT m) where
	ask       = lift ask
	local f (IdNameT m) = IdNameT $ local f m

-- | Get bound and used names
idNameBoundNames :: Monad m => IdNameT m IdSet
idNameBoundNames = IdNameT $ do
    (_used,bound) <- get
    return bound
idNameUsedNames :: Monad m => IdNameT m IdSet
idNameUsedNames = IdNameT $  do
    (used,_bound) <- get
    return used

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

addNamesIdSet nset = IdNameT $ do
    modify (\ (used,bound) -> (nset `union` used, bound) )
addBoundNamesIdSet nset = IdNameT $ do
    modify (\ (used,bound) -> (nset `union` used, nset `union` bound) )

addBoundNamesIdMap nmap = IdNameT $ do
    modify (\ (used,bound) -> (nset `union` used, nset `union` bound) ) where
        nset = idMapToIdSet nmap

idSetFromDistinctAscList :: [Id] -> IdSet
idSetFromDistinctAscList ids = IdSet (IS.fromDistinctAscList ids)

idSetFromList :: [Id] -> IdSet
idSetFromList ids = IdSet (IS.fromList ids)

idMapFromList :: [(Id,a)] -> IdMap a
idMapFromList ids = IdMap (IM.fromList ids)

idMapFromDistinctAscList :: [(Id,a)] -> IdMap a
idMapFromDistinctAscList ids = IdMap (IM.fromDistinctAscList ids)


instance Show IdSet where
    showsPrec n is = showsPrec n $ map f (idSetToList is) where
        f n =  maybe (toAtom ('x':show n)) (toAtom . show) (fromId n)

instance Show v => Show (IdMap v) where
    showsPrec n is = showsPrec n $ map f (idMapToList is) where
        f (n,v) =  (maybe (toAtom ('x':show n)) (toAtom . show) (fromId n),v)

-- Id types
-- odd - an atom
-- 0 - special, indicating lack of binding
-- negative - etherial id, used as placeholder within algorithms
-- positive and even - arbitrary numbers.

etherialIds :: [Id]
etherialIds = [-2, -4 ..  ]

isEtherialId id = id < 0

isInvalidId id = id <= 0

emptyId :: Id
emptyId = 0


-- | find some temporary ids that are not members of the set,
-- useful for generating a small number of local unique names.

newIds :: IdSet -> [Id]
newIds ids = [ i | i <- [s, s + 2 ..] , i `notMember` ids ] where
    s = 2 + (2 * size ids)


newId :: Int           -- ^ a seed value, useful for speeding up finding a unique id
      -> (Id -> Bool)  -- ^ whether an Id is acceptable
      -> Id            -- ^ your new Id
newId seed check = head $ filter check ls where
    ls = map mask $ randoms (mkStdGen seed)
    mask x = x .&. 0x0FFFFFFE




