module Util.GMap where

import Data.Monoid
import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Data.Set as S
import qualified Data.IntSet as IS

class Monoid s => Collection s where
    type Elem s :: *
    fromList :: [Elem s] -> s
    toList :: s -> [Elem s]
    singleton :: Elem s -> s
    singleton e = fromList [e]

instance Collection [e] where
    type Elem [e] = e
    fromList = id
    toList = id

class Collection s => SetLike s where
    type Key s :: *
    keys :: s -> [Key s]
    member :: Key s -> s -> Bool
    delete :: Key s -> s -> s
    union :: s -> s -> s
    unions :: [s] -> s
    difference :: s -> s -> s
    filter :: (Elem s -> Bool) -> s -> s
    insert :: Elem s -> s -> s

    unions = mconcat
    union = mappend

class SetLike m => MapLike m where
    type Value m :: *
    lookup :: Key m -> m -> Maybe (Value m)
    values :: m -> [Value m]

instance Collection IS.IntSet where
    type Elem IS.IntSet = Int
    fromList = IS.fromList
    toList = IS.toList
    singleton = IS.singleton

instance SetLike IS.IntSet where
    type Key IS.IntSet = Int
    keys = IS.toList
    member = IS.member
    filter = IS.filter
    delete = IS.delete
    union = IS.union
    difference = IS.difference
    insert = IS.insert

instance Ord k => Collection (S.Set k) where
    type Elem (S.Set k) = k
    fromList = S.fromList
    toList = S.toList
    singleton = S.singleton

instance Ord k => SetLike (S.Set k) where
    type Key (S.Set k) = k
    keys = S.toList
    member = S.member
    filter = S.filter
    delete = S.delete
    union = S.union
    difference = S.difference
    insert = S.insert

instance Collection (IM.IntMap v) where
    type Elem (IM.IntMap v) = (Int,v)
    fromList = IM.fromList
    toList = IM.toList
    singleton (k,v) = IM.singleton k v

instance SetLike (IM.IntMap v) where
    type Key (IM.IntMap v) = Int
    keys = IM.keys
    member = IM.member
    filter f = IM.filterWithKey (\ k v -> f (k,v))
    delete = IM.delete
    union = IM.union
    difference = IM.difference
    insert (k,v) = IM.insert k v

instance MapLike (IM.IntMap v) where
    type Value (IM.IntMap v) = v
    lookup = IM.lookup
    values = IM.elems

instance Ord k => Collection (M.Map k v) where
    type Elem (M.Map k v) = (k,v)
    fromList = M.fromList
    toList = M.toList
    singleton (k,v) = M.singleton k v

instance Ord k => SetLike (M.Map k v) where
    type Key (M.Map k v) = k
    keys = M.keys
    member = M.member
    filter f = M.filterWithKey (\ k v -> f (k,v))
    delete = M.delete
    union = M.union
    difference = M.difference
    insert (k,v) = M.insert k v

instance Ord k => MapLike (M.Map k v) where
    type Value (M.Map k v) = v
    lookup = M.lookup
    values = M.elems

minsert :: (MapLike m, Elem m ~ (k,v)) => k -> v -> m -> m
minsert k v = insert (k,v)

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
