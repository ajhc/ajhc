module Util.SetLike(
    EnumSet(),
    (\\),
    notMember,
    mnotMember,
    minsert,
    msingleton,
    intersects,
    mfindWithDefault,
    SetLike(..),
    ModifySet(..),
    MapLike(..),
    BuildSet(..)
    ) where


import Data.Monoid
import Data.Typeable
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Map as M
import qualified Data.Set as S

import Util.HasSize

infixl 9 \\ --

m1 \\ m2 = difference m1 m2

class (HasSize s,IsEmpty s) => SetLike s where
    difference :: s -> s -> s
    intersection :: s -> s -> s
    disjoint :: s -> s -> Bool
    isSubsetOf :: s -> s -> Bool

    union :: s -> s -> s
    unions :: [s] -> s
    sempty :: s

    disjoint x y = isEmpty (x `intersection` y)
    isSubsetOf x y = size x <= size y && (size (x `intersection` y) == size x)
    unions ss = foldr union sempty ss


-- you can't pull values out of the set with this, as it might store the
-- essence of a data type

class SetLike s => BuildSet t s | s -> t where
    fromList :: [t] -> s
    fromDistinctAscList :: [t] -> s
    insert :: t -> s -> s
    singleton :: t -> s

    singleton t = fromDistinctAscList [t]
    fromDistinctAscList = fromList

class BuildSet t s => ModifySet t s | s -> t where
    toList :: s -> [t]
    delete :: t -> s -> s
    member :: t -> s -> Bool
    sfilter :: (t -> Bool) -> s -> s

notMember x t = not $ member x t
mnotMember x t = not $ mmember x t

intersects x y = not $ disjoint x y


--  int set

instance SetLike IS.IntSet where
    difference = IS.difference
    intersection = IS.intersection
    isSubsetOf = IS.isSubsetOf
    union      = IS.union
    unions     = IS.unions
    sempty      = IS.empty

instance BuildSet Int IS.IntSet where
    fromList xs = IS.fromList xs
    fromDistinctAscList xs = IS.fromDistinctAscList xs
    insert x s = IS.insert x s
    singleton x = IS.singleton x

instance ModifySet Int IS.IntSet where
    toList s   = IS.toList s
    delete x s = IS.delete x s
    member x s = IS.member x s
    sfilter    = IS.filter

-- normal set

instance Ord a => SetLike (S.Set a) where
    difference = S.difference
    intersection = S.intersection
    isSubsetOf = S.isSubsetOf
    union      = S.union
    unions     = S.unions
    sempty      = S.empty

instance Ord a => BuildSet a (S.Set a) where
    fromList xs = S.fromList xs
    fromDistinctAscList xs = S.fromDistinctAscList xs
    insert x s = S.insert x s
    singleton x = S.singleton x

instance Ord a => ModifySet a (S.Set a) where
    toList s   = S.toList s
    member x s = S.member x s
    delete x s = S.delete x s
    sfilter    = S.filter

-- maps

instance SetLike (IM.IntMap a) where    -- SIC
    difference = IM.difference
    intersection = IM.intersection
    union      = IM.union
    unions     = IM.unions
    sempty     = IM.empty


instance BuildSet (Int,a) (IM.IntMap a) where
    fromList xs = IM.fromList xs
    fromDistinctAscList xs = IM.fromDistinctAscList xs
    insert (k,v) s = IM.insert k v s
    singleton (k,v) = IM.singleton k v


instance Ord a => SetLike (M.Map a b) where
    difference = M.difference
    intersection = M.intersection
    union      = M.union
    unions     = M.unions
    sempty     = M.empty

instance Ord a => BuildSet (a,b) (M.Map a b) where
    fromList xs = M.fromList xs
    fromDistinctAscList xs = M.fromDistinctAscList xs
    insert (k,v) s = M.insert k v s
    singleton (k,v) = M.singleton k v

minsert :: BuildSet (k,v) s => k -> v -> s -> s
minsert k v s = insert (k,v) s

msingleton :: BuildSet (k,v) s => k -> v -> s
msingleton k v = singleton (k,v)


class SetLike m => MapLike k v m | m -> k v where
    mdelete :: k -> m -> m
    mmember :: k -> m -> Bool
    mlookup :: k -> m -> Maybe v
    melems :: m -> [v]
    massocs :: m -> [(k,v)]
    mkeys :: m -> [k]
    mmapWithKey :: (k -> v -> v) -> m -> m
    mfilter :: (v -> Bool) -> m -> m
    mfilterWithKey :: (k -> v -> Bool) -> m -> m
    munionWith :: (v -> v -> v) -> m -> m -> m
    mpartitionWithKey :: (k -> v -> Bool) -> m -> (m,m)

    mkeys = map fst . massocs
    melems = map snd . massocs

instance MapLike Int a (IM.IntMap a) where
    mdelete = IM.delete
    mmember = IM.member
    mlookup k m = IM.lookup k m
    melems = IM.elems
    mkeys = IM.keys
    massocs = IM.toList
    mfilter = IM.filter
    mmapWithKey = IM.mapWithKey
    mfilterWithKey = IM.filterWithKey
    munionWith = IM.unionWith
    mpartitionWithKey = IM.partitionWithKey

instance Ord k => MapLike k v (M.Map k v) where
    mdelete = M.delete
    mmember = M.member
    mlookup k m = case M.lookup k m of
        Nothing -> fail "Map: mlookup can't find key"
        Just x -> return x
    melems = M.elems
    mkeys = M.keys
    massocs = M.toList
    mfilter = M.filter
    mmapWithKey = M.mapWithKey
    mfilterWithKey = M.filterWithKey
    munionWith = M.unionWith
    mpartitionWithKey = M.partitionWithKey

mfindWithDefault d k m = case mlookup k m of
    Nothing -> d
    Just x -> x

-- EnumSet

newtype EnumSet a = EnumSet IS.IntSet
    deriving(Typeable,Monoid,SetLike,HasSize,Eq,Ord,IsEmpty)

instance Enum a => BuildSet a (EnumSet a) where
    fromList xs = EnumSet $ IS.fromList (map fromEnum xs)
    fromDistinctAscList xs = EnumSet $ IS.fromDistinctAscList (map fromEnum xs)
    insert x (EnumSet s) = EnumSet $ IS.insert (fromEnum x) s
    singleton x = EnumSet $ IS.singleton (fromEnum x)

instance Enum a => ModifySet a (EnumSet a) where
    toList (EnumSet s) = map toEnum $ toList s
    member x (EnumSet s) = IS.member (fromEnum x) s
    delete x (EnumSet s) = EnumSet $ IS.delete (fromEnum x) s
    sfilter f (EnumSet s) = EnumSet $ IS.filter (f . toEnum)  s


