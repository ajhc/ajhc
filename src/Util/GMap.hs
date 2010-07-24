module Util.GMap where

import Data.Monoid
import Util.SetLike
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import Util.HasSize
import Data.Foldable hiding(toList)
import Data.Traversable


type family GMap k :: * -> *
type family GSet k :: *

type instance GMap Int = IM.IntMap
type instance GSet Int = IS.IntSet

type instance GSet Char = EnumSet Char
type instance GMap Char = EnumMap Char

newtype EnumSet a = EnumSet IS.IntSet
    deriving(Monoid,IsEmpty,HasSize,Unionize,Eq,Ord)

type instance Elem (EnumSet a) = a
type instance Key (EnumSet a) = a

instance Enum a => Collection (EnumSet a) where 
    singleton i = EnumSet $ singleton (fromEnum i)
    fromList ts = EnumSet $ fromList (map fromEnum ts)
    toList (EnumSet w) = map toEnum $ toList w

instance Enum a => SetLike (EnumSet a) where
    keys = toList
    delete (fromEnum -> i) (EnumSet v) = EnumSet $ delete i v
    member (fromEnum -> i) (EnumSet v) = member i v
    insert (fromEnum -> i) (EnumSet v) = EnumSet $ insert i v
    sfilter f (EnumSet v) = EnumSet $ sfilter (f . toEnum) v
    spartition f (EnumSet v) = case spartition (f . toEnum) v of
        (x,y) -> (EnumSet x,EnumSet y)

newtype EnumMap k v = EnumMap (IM.IntMap v)
    deriving(Monoid,IsEmpty,Functor,Foldable,Traversable,HasSize,Unionize,Eq,Ord)

type instance Elem (EnumMap k v) = (k,v)
type instance Key (EnumMap k v) = k
type instance Value (EnumMap k v) = v

instance Enum k => Collection (EnumMap k v) where 
    singleton (k,v) = EnumMap $ singleton (fromEnum k,v)
    fromList ts = EnumMap $ fromList [ (fromEnum k,v) | (k,v) <- ts ]
    toList (EnumMap kv) =  [ (toEnum k,v)  | (k,v) <-  toList kv]

instance Enum k => SetLike (EnumMap k v) where
    keys (EnumMap v) = map toEnum $ keys v
    delete (fromEnum -> i) (EnumMap v) = EnumMap $ delete i v
    member (fromEnum -> i) (EnumMap v) = member i v
    insert (fromEnum -> k,v) (EnumMap m) = EnumMap $ insert (k,v) m
    sfilter f (EnumMap v) = EnumMap $ sfilter (\ (k,v) -> f (toEnum k,v)) v
    spartition f (EnumMap v) = case spartition (\ (k,v) -> f (toEnum k,v)) v of
        (x,y) -> (EnumMap x,EnumMap y)

instance Enum k => MapLike (EnumMap k v) where
    mlookup (fromEnum -> i) (EnumMap v) = mlookup i v
    values (EnumMap v) = values v
    unionWith f (EnumMap x) (EnumMap y) = EnumMap $ unionWith f x y
    

class GMapSet k where
    toSet :: GMap k v -> GSet k
    toMap :: (k -> v) -> GSet k -> GMap k v

-- must be an injection into the integers
class Intjection a where
    fromIntjection :: a -> Int
    toIntjection :: Int -> a

newtype IntjectionSet a = IntjectionSet IS.IntSet
    deriving(Monoid,IsEmpty,HasSize,Unionize,Eq,Ord)

type instance Elem (IntjectionSet a) = a
type instance Key (IntjectionSet a) = a

instance Intjection a => Collection (IntjectionSet a) where 
    singleton i = IntjectionSet $ singleton (fromIntjection i)
    fromList ts = IntjectionSet $ fromList (map fromIntjection ts)
    toList (IntjectionSet w) = map toIntjection $ toList w

instance Intjection a => SetLike (IntjectionSet a) where
    keys = toList
    delete (fromIntjection -> i) (IntjectionSet v) = IntjectionSet $ delete i v
    member (fromIntjection -> i) (IntjectionSet v) = member i v
    insert (fromIntjection -> i) (IntjectionSet v) = IntjectionSet $ insert i v
    sfilter f (IntjectionSet v) = IntjectionSet $ sfilter (f . toIntjection) v
    spartition f (IntjectionSet v) = case spartition (f . toIntjection) v of
        (x,y) -> (IntjectionSet x,IntjectionSet y)

newtype IntjectionMap k v = IntjectionMap (IM.IntMap v)
    deriving(Monoid,IsEmpty,Functor,Foldable,Traversable,HasSize,Unionize,Eq,Ord)

type instance Elem (IntjectionMap k v) = (k,v)
type instance Key (IntjectionMap k v) = k
type instance Value (IntjectionMap k v) = v

instance Intjection k => Collection (IntjectionMap k v) where 
    singleton (k,v) = IntjectionMap $ singleton (fromIntjection k,v)
    fromList ts = IntjectionMap $ fromList [ (fromIntjection k,v) | (k,v) <- ts ]
    toList (IntjectionMap kv) =  [ (toIntjection k,v)  | (k,v) <-  toList kv]

instance Intjection k => SetLike (IntjectionMap k v) where
    keys (IntjectionMap v) = map toIntjection $ keys v
    delete (fromIntjection -> i) (IntjectionMap v) = IntjectionMap $ delete i v
    member (fromIntjection -> i) (IntjectionMap v) = member i v
    insert (fromIntjection -> k,v) (IntjectionMap m) = IntjectionMap $ insert (k,v) m
    sfilter f (IntjectionMap v) = IntjectionMap $ sfilter (\ (k,v) -> f (toIntjection k,v)) v
    spartition f (IntjectionMap v) = case spartition (\ (k,v) -> f (toIntjection k,v)) v of
        (x,y) -> (IntjectionMap x,IntjectionMap y)

instance Intjection k => MapLike (IntjectionMap k v) where
    mlookup (fromIntjection -> i) (IntjectionMap v) = mlookup i v
    values (IntjectionMap v) = values v
    unionWith f (IntjectionMap x) (IntjectionMap y) = IntjectionMap $ unionWith f x y

--type instance Elem  (GMap k v) = (k,v)
--type instance Key   (GMap k v) = k
--type instance Value (GMap k v) = k

--instance Collection (GMap k v) 
--instance SetLike (GMap k v) where

--instance SetLike (GSet k) where
--    type Elem (GSet k) = k
--    type Key (GSet k) = k


--class GMapKey k where
--    data GMap k :: * -> *
--    data GSet k :: *
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
{-
//instance GMapKey Int where
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
