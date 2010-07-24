module Name.Id(
    Id(),
    IdMap(),
    IdNameT(),
    IdSet(),
    anonymous,
    va1,va2,va3,va4,va5,
    addBoundNamesIdMap,
    addBoundNamesIdSet,
    addNamesIdSet,
    idMapToIdSet,
    anonymousIds,
    sillyId,
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
    candidateIds,
    runIdNameT
    )where

import Control.Monad.Reader
import Control.Monad.State
import Data.Bits
import Data.Foldable
import Data.Int
import Data.Monoid
import Data.Traversable
import Data.Typeable
import System.Random
import qualified Data.Binary as B
import qualified Data.IntMap  as IM
import qualified Data.IntSet as IS

import Doc.DocLike
import Doc.PPrint
import Name.Name
import StringTable.Atom
import Util.HasSize
import Util.Inst()
import Util.NameMonad
import Util.SetLike as S
import Util.GMap()
--import Debug.Trace

{-
 - An Id is an opaque type with equality and ordering, Its range is split into the following categories
 - all the following categories are disjoint.
 -
 - the unique empty id, called 'emptyId'
 -
 - for every Atom there is a unique cooresponding Id.
 -
 - a set of anonymous ids, indexed by positive numbers.
 -
 - a set of epheremal Ids presented as the list 'epheremalIds'. these are
 - generally used as placeholders for unification algorithms.
 -
 - In general, only atomic and anonymous ids are used as values, and the empty id is used to indicate
 - an usused binding site. epheremal and silly ids are used internally in certain algorithms and have no
 - meaning outside of said context. They never escape the code that uses them.
 -
 -}



newtype Id = Id Int
    deriving(Eq,Ord)

anonymous :: Int -> Id
anonymous x | x <= 0 = error "invalid anonymous id"
            | otherwise = Id (2*x)

-- | some convinience anonymous ids
va1,va2,va3,va4,va5 :: Id
va1  = anonymous 1
va2  = anonymous 2
va3  = anonymous 3
va4  = anonymous 4
va5  = anonymous 5

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


--deriving instance MapLike Int a (IM.IntMap a) => MapLike Id a (IdMap a)


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

-- | Run the name monad transformer.
runIdNameT :: (Monad m) => IdNameT m a -> m (a,IdSet)
runIdNameT (IdNameT x) = do
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
        fromIdNameT $ newNameFrom (candidateIds (size used `xor` size bound))

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
        showsPrec _ (Id 0) =  showChar '_'
        showsPrec _ (Id x) =  maybe (showString ('x':show (x `div` 2))) shows (fromId $ Id x)

instance Show IdSet where
    showsPrec n is = showsPrec n (idSetToList is)

instance Show v => Show (IdMap v) where
    showsPrec n is = showsPrec n (idMapToList is)

anonymousIds :: [Id]
anonymousIds = map anonymous [1 .. ]



etherealIds :: [Id]
etherealIds = map Id [-4, -6 ..  ]

isEmptyId x = x == emptyId
isEtherealId id = id < emptyId

-- | id isn't anonymous or atom-mapped
isInvalidId id = id <= emptyId

-- | A occasionally useful random ethereal id
sillyId :: Id
sillyId = Id $ -2

emptyId :: Id
emptyId = Id 0


-- | find some temporary ids that are not members of the set,
-- useful for generating a small number of local unique names.

newIds :: IdSet -> [Id]
newIds (IdSet ids) = [ i | i <- candidateIds (size ids' `xor` IS.findMin ids' `xor` IS.findMax ids') , i `notMember` IdSet ids ] where
    ids' = if size ids == 0 then IS.insert 0 ids else ids


newId :: Int           -- ^ a seed value, useful for speeding up finding a unique id
      -> (Id -> Bool)  -- ^ whether an Id is acceptable
      -> Id            -- ^ your new Id
newId seed check = head $ filter check (candidateIds seed)

-- generate a list of candidate anonymous ids based on a seed value
candidateIds :: Int -> [Id]
candidateIds seed = map mask $ randoms (mkStdGen seed) where
    mask x = Id $ x .&. 0x0FFFFFFE
    --mask x = trace ("candidate " ++ show seed) $ Id $ x .&. 0x0FFFFFFE


toId :: Name -> Id
toId x = Id $ fromAtom (toAtom x)

instance FromAtom Id where
    fromAtom x = Id $ fromAtom x

fromId :: Monad m => Id -> m Name
fromId (Id i) = case intToAtom i of
    Just a -> return $ fromAtom a
    Nothing -> fail $ "Name.fromId: not a name " ++ show (Id i)


instance DocLike d => PPrint d Id where
    pprint x = tshow x

instance GenName Id where
    genNames = candidateIds

instance B.Binary Id where
    put (Id x) = case intToAtom x of
        Just a -> do B.putWord8 128 >> B.put a
        Nothing | x >= 0 && x < 128 -> B.putWord8 (fromIntegral x)
                | otherwise -> do
                    B.putWord8 129
                    B.put (fromIntegral x :: Int32)
    get = do
        x <- B.getWord8
        case x of
            128 -> do
                a <- B.get
                return (toId $ fromAtom a)
            129 -> do
                v <- B.get
                return (Id $ fromIntegral (v :: Int32))
            _ -> return (Id $ fromIntegral x)
