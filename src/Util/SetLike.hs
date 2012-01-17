{-# OPTIONS -XTypeFamilies #-}
module Util.SetLike where

import Data.List(foldl')
import Data.Monoid
import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Data.Set as S
import qualified Data.IntSet as IS
import Util.HasSize
import Data.Foldable hiding(toList, foldl')
import Data.Traversable

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

type family Elem es :: *
type family Key s :: *
type family Value m :: *

class Monoid s => Collection s where
    fromList :: [Elem s] -> s
    fromDistinctAscList :: [Elem s] -> s
    toList :: s -> [Elem s]
    singleton :: Elem s -> s
    singleton e = fromList [e]

    fromDistinctAscList = fromList

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
    fromDistinctAscList = IS.fromDistinctAscList

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
    fromDistinctAscList = S.fromDistinctAscList

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
    fromDistinctAscList = IM.fromDistinctAscList

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
    fromDistinctAscList = M.fromDistinctAscList

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

-- must be an injection into the integers
class Intjection a where
    fromIntjection :: a -> Int
    toIntjection :: Int -> a

newtype IntjectionSet a = IntjectionSet IS.IntSet
    deriving(Monoid,IsEmpty,HasSize,Unionize,Eq,Ord)

instance (Intjection a,Show a) => Show (IntjectionSet a) where
    showsPrec n is = showsPrec n $ toList is

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

