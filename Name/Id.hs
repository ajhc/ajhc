module Name.Id(
    Id(),
    IdMap(),
    IdNameT(),
    IdSet(),
    anonymous,
    addBoundNamesIdMap,
    addBoundNamesIdSet,
    addNamesIdSet,
    idMapToIdSet,
    idNameBoundNames,
    idNameUsedNames,
    etherealIds,
    isEtherealId,
    isInvalidId,
    isEmptyId,
    idSetToIdMap,
    mapMaybeIdMap,
    idSetFromList,
    idToInt,
    idSetFromDistinctAscList,
    idMapFromList,
    idMapFromDistinctAscList,
    idSetToList,
    idMapToList,
    emptyId,
    newIds,
    newId,
    toId,
    fromId,
    runIdNameT',
    runIdNameT
    )where

import Control.Monad.State
import Control.Monad.Reader
import Data.Binary(Binary())
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
import Doc.PPrint
import Doc.DocLike

-- TODO - make this a newtype
newtype Id = Id Int
    deriving(Eq,Ord,Enum,Binary)

anonymous :: Int -> Id
anonymous x = Id x

-- IdSet


newtype IdSet = IdSet IS.IntSet
    deriving(Typeable,Monoid,HasSize,SetLike,IsEmpty,Eq,Ord)

instance BuildSet Id IdSet where
    fromList = idSetFromList
    fromDistinctAscList = idSetFromDistinctAscList
    insert (Id x) (IdSet b) = IdSet $ IS.insert x b
    singleton (Id x) = IdSet $ IS.singleton x

instance ModifySet Id IdSet where
    toList = idSetToList
    delete (Id x) (IdSet b) = IdSet $ IS.delete x b
    member (Id x) (IdSet b) = IS.member x b
    sfilter f (IdSet s) = IdSet $ IS.filter (f . Id) s


idSetToList :: IdSet -> [Id]
idSetToList (IdSet is) = [ Id x | x <- IS.toList is ]

idMapToList :: IdMap a -> [(Id,a)]
idMapToList (IdMap is) = [ (Id x,y) | (x,y) <- IM.toList is ]

idToInt :: Id -> Int
idToInt (Id x) = x

mapMaybeIdMap :: (a -> Maybe b) -> IdMap a -> IdMap b
mapMaybeIdMap fn (IdMap m) = IdMap (IM.mapMaybe fn m)


-- IdMap

newtype IdMap a = IdMap (IM.IntMap a)
    deriving(Typeable,Monoid,HasSize,SetLike,Functor,Traversable,Foldable,IsEmpty,Eq,Ord)

instance BuildSet (Id,a) (IdMap a) where
    fromList = idMapFromList
    fromDistinctAscList = idMapFromDistinctAscList
    insert (Id x,y) (IdMap b) = IdMap $ IM.insert x y b
    singleton (Id x,y) = IdMap $ IM.singleton x y

instance MapLike Id a (IdMap a) where
    melems (IdMap m) = IM.elems m
    mdelete (Id x) (IdMap m) = IdMap $ IM.delete x m
    mmember (Id x) (IdMap m) = IM.member x m
    mlookup (Id x) (IdMap m) = IM.lookup x m
    massocs (IdMap m) = [ (Id x,y) | (x,y) <- IM.assocs m ]
    mkeys (IdMap m) = [ Id x | x <- IM.keys m ]
    mmapWithKey f (IdMap m) = IdMap $ IM.mapWithKey (\k v -> f (Id k) v) m
    mfilter f (IdMap m) = IdMap $ IM.filter f m
    mpartitionWithKey f (IdMap m) = case IM.partitionWithKey (\k v -> f (Id k) v) m of (x,y) -> (IdMap x,IdMap y)
    munionWith f (IdMap m1) (IdMap m2) = IdMap $ IM.unionWith f m1 m2
    mfilterWithKey f (IdMap m) = IdMap $ IM.filterWithKey (\k v -> f (Id k) v) m




idSetToIdMap :: (Id -> a) -> IdSet -> IdMap a
idSetToIdMap f (IdSet is) = IdMap $ IM.fromDistinctAscList [ (x,f (Id x)) |  x <- IS.toAscList is]

idMapToIdSet :: IdMap a -> IdSet
idMapToIdSet (IdMap im) = IdSet $ (IM.keysSet im)


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
        let genNames i = map Id [st, st + 2 ..]  where
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
idSetFromDistinctAscList ids = IdSet (IS.fromDistinctAscList [ x | Id x <- ids] )

idSetFromList :: [Id] -> IdSet
idSetFromList ids = IdSet (IS.fromList [ x | Id x <- ids])

idMapFromList :: [(Id,a)] -> IdMap a
idMapFromList ids = IdMap (IM.fromList [ (x,y) | (Id x,y) <- ids])

idMapFromDistinctAscList :: [(Id,a)] -> IdMap a
idMapFromDistinctAscList ids = IdMap (IM.fromDistinctAscList [ (x,y) | (Id x,y) <- ids ] )



instance Show Id where
        showsPrec _ n =  maybe (showString ('x':show (idToInt n))) shows (fromId n)
    

instance Show IdSet where
    showsPrec n is = showsPrec n $ map f (idSetToList is) where
        f n =  maybe (toAtom ('x':show (idToInt n))) (toAtom . show) (fromId n)

instance Show v => Show (IdMap v) where
    showsPrec n is = showsPrec n $ map f (idMapToList is) where
        f (n,v) =  (maybe (toAtom ('x':show (idToInt n))) (toAtom . show) (fromId n),v)

-- Id types
-- odd - an atom
-- 0 - special, indicating lack of binding
-- negative - etherial id, used as placeholder within algorithms
-- positive and even - arbitrary numbers.

etherealIds :: [Id]
etherealIds = map Id [-2, -4 ..  ]

isEtherealId id = id < emptyId

isInvalidId id = id <= emptyId

emptyId :: Id
emptyId = Id 0


-- | find some temporary ids that are not members of the set,
-- useful for generating a small number of local unique names.

newIds :: IdSet -> [Id]
newIds ids = [ Id i | i <- [s, s + 2 ..] , Id i `notMember` ids ] where
    s = 2 + (2 * size ids)


newId :: Int           -- ^ a seed value, useful for speeding up finding a unique id
      -> (Id -> Bool)  -- ^ whether an Id is acceptable
      -> Id            -- ^ your new Id
newId seed check = head $ filter check ls where
    ls = map mask $ randoms (mkStdGen seed)
    mask x = Id $ x .&. 0x0FFFFFFE



toId :: Name -> Id
toId x = Id $ fromAtom (toAtom x)

instance FromAtom Id where
    fromAtom x = Id $ fromAtom x

fromId :: Monad m => Id -> m Name
--fromId i | even i || i < 0 = fail $ "Name.fromId: not a name " ++ show i
--fromId i | not $ isValidAtom i = fail $ "Name.fromId: not a name " ++ show i
fromId (Id i) = case intToAtom i of
    Just a -> return $ fromAtom a
    Nothing -> fail $ "Name.fromId: not a name " ++ show i

isEmptyId x = x == emptyId

instance DocLike d => PPrint d Id where
    pprint x = tshow x

instance GenName Id where
    genNames i = map Id [st, st + 2 ..]  where
        st = abs i + 2 + abs i `mod` 2

