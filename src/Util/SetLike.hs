module Util.SetLike where

import Data.List(foldl')
import Data.Monoid
import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Data.Set as S
import qualified Data.IntSet as IS
import Util.HasSize

infixl 9 \\ --

(\\) :: Unionize s => s -> s -> s
m1 \\ m2 = difference m1 m2

class Monoid s => Unionize s where
    union :: s -> s -> s
    difference :: s -> s -> s
    intersection :: s -> s -> s
    unions :: [s] -> s
    sempty :: s

    sempty = mempty
    union = mappend
    unions = foldl' union mempty

type family Elem es
type family Key s :: *
type family Value m :: *

class Monoid s => Collection s where
    fromList :: [Elem s] -> s
    fromDistinctAscList :: [Elem s] -> s
    toList :: s -> [Elem s]
    singleton :: Elem s -> s
    singleton e = fromList [e]

    fromDistinctAscList = fromList

type instance Elem [e] = e

instance Collection [e] where
    fromList = id
    toList = id


class Collection s => SetLike s where
    keys :: s -> [Key s]
    member :: Key s -> s -> Bool
    delete :: Key s -> s -> s
    sfilter :: (Elem s -> Bool) -> s -> s
    insert :: Elem s -> s -> s
    spartition :: (Elem s -> Bool) -> s -> (s,s)

notMember :: SetLike s => Key s -> s -> Bool
notMember k s = not $ member k s

class SetLike m => MapLike m where
    mlookup :: Key m -> m -> Maybe (Value m)
    values :: m -> [Value m]
    unionWith :: (Value m -> Value m -> Value m) -> m -> m -> m

instance Unionize IS.IntSet where
    union = IS.union
    difference = IS.difference
    intersection = IS.intersection

type instance Elem IS.IntSet = Int

instance Collection IS.IntSet where
    fromList = IS.fromList
    toList = IS.toList
    singleton = IS.singleton

type instance  Key IS.IntSet = Int
instance SetLike IS.IntSet where
    keys = IS.toList
    member = IS.member
    sfilter = IS.filter
    delete = IS.delete
    insert = IS.insert
    spartition = IS.partition

instance Ord k => Unionize (S.Set k) where
    union = S.union
    intersection = S.intersection
    difference = S.difference

type instance  Elem (S.Set k) = k
instance Ord k => Collection (S.Set k) where
    fromList = S.fromList
    toList = S.toList
    singleton = S.singleton

type instance Key (S.Set k) = k
instance Ord k => SetLike (S.Set k) where
    keys = S.toList
    member = S.member
    sfilter = S.filter
    delete = S.delete
    insert = S.insert
    spartition = S.partition

instance Unionize (IM.IntMap v) where
    union = IM.union
    difference = IM.difference
    intersection = IM.intersection

type instance Elem (IM.IntMap v) = (Int,v)
instance Collection (IM.IntMap v) where
    fromList = IM.fromList
    toList = IM.toList
    singleton (k,v) = IM.singleton k v

type instance Key (IM.IntMap v) = Int
instance SetLike (IM.IntMap v) where
    keys = IM.keys
    member = IM.member
    sfilter f = IM.filterWithKey (\ k v -> f (k,v))
    delete = IM.delete
    insert (k,v) = IM.insert k v
    spartition f = IM.partitionWithKey (\ k v -> f (k,v))

type instance Value (IM.IntMap v) = v
instance MapLike (IM.IntMap v) where
    mlookup = IM.lookup
    values = IM.elems
    unionWith = IM.unionWith

instance Ord k => Unionize (M.Map k v) where
    union = M.union
    difference = M.difference
    intersection = M.intersection

type instance Elem (M.Map k v) = (k,v)
instance Ord k => Collection (M.Map k v) where
    fromList = M.fromList
    toList = M.toList
    singleton (k,v) = M.singleton k v

type instance Key (M.Map k v) = k
instance Ord k => SetLike (M.Map k v) where
    keys = M.keys
    member = M.member
    sfilter f = M.filterWithKey (\ k v -> f (k,v))
    delete = M.delete
    insert (k,v) = M.insert k v
    spartition f = M.partitionWithKey (\ k v -> f (k,v))

type instance Value (M.Map k v) = v
instance Ord k => MapLike (M.Map k v) where
    mlookup = M.lookup
    values = M.elems
    unionWith = M.unionWith

minsert :: (MapLike m, Elem m ~ (k,v)) => k -> v -> m -> m
minsert k v = insert (k,v)

msingleton :: (MapLike m, Elem m ~ (k,v)) => k -> v -> m
msingleton k v = singleton (k,v)

intersects x y = not $ isEmpty (x `intersection` y)

findWithDefault :: MapLike m => Value m -> Key m -> m -> Value m
findWithDefault d k m = case mlookup k m of
    Nothing -> d
    Just x -> x
{-
instance SetLike (GMap k v) where
    type Elem (GMap k v) = (k,v)
    type Key (GMap k v) = k

instance SetLike (GSet k) where
    type Elem (GSet k) = k
    type Key (GSet k) = k


class GMapKey k where
    data GMap k :: * -> *
    data GSet k :: *
--    fromList :: [(k,v)] -> GMap k v
--    fromDistinctAscList :: [(k,v)] -> GMap k v
--    fromBinDistinctAscList :: Int -> [(k,v)] -> GMap k v
--    insert :: k -> v -> GMap k v -> GMap k v
--    union :: GMap k v -> GMap k v -> GMap k v
--    toList :: GMap k v -> [(k,v)]
--    delete :: k -> GMap k v -> GMap k v
--    member :: k -> GMap k v -> Bool
--    lookup :: k -> GMap k v -> Maybe v

--    fromDistinctAscList = fromList
--    fromBinDistinctAscList _ = fromList
--
instance GMapKey Int where
    newtype GMap Int v = GMapInt (IM.IntMap v)
    fromList vs = GMapInt (IM.fromList vs)
    toList (GMapInt x) = IM.toList x
    insert k v (GMapInt m) = GMapInt (IM.insert k v m)
    union (GMapInt x) (GMapInt y) = GMapInt $  x `IM.union` y
    lookup k (GMapInt m) = IM.lookup k m

instance GMapKey a => GMapKey (Maybe a) where
    data GMap (Maybe a) v = GMapMaybe (Maybe v) (GMap a v)
-}


{-
class GMapKey k where
    type GMap k :: * -> *
    fromList :: [(k,v)] -> GMap k v
    fromDistinctAscList :: [(k,v)] -> GMap k v
    fromBinDistinctAscList :: Int -> [(k,v)] -> GMap k v
    insert :: k -> v -> GMap k v -> GMap k v
    union :: GMap k v -> GMap k v -> GMap k v
    toList :: GMap k v -> [(k,v)]
    delete :: k -> GMap k v -> GMap k v
    member :: k -> GMap k v -> Bool
    lookup :: k -> GMap k v -> Maybe v

    fromDistinctAscList = fromList
    fromBinDistinctAscList _ = fromList

instance GMapKey Int where
    type GMap Int v = (IM.IntMap v)
    fromList vs = (IM.fromList vs)
    toList x = IM.toList x
    insert k v m = (IM.insert k v m)
    union x y =  x `IM.union` y
    lookup k m = IM.lookup k m


data GMapMaybe k v = GMapMaybe (Maybe v) GMap k v)
instance GMapKey a => GMapKey (Maybe a) where
    data GMap (Maybe a) v = GMapMaybe (Maybe v) (GMap a v)


-}
