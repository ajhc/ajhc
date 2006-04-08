module Util.SetLike(
    EnumSet(),
    (\\),
    notMember,
    union,
    unions,
    SetLike(..),
    BuildSet(..)
    )
    where


import qualified Data.Set as S
import qualified Data.IntMap as IM
import qualified Data.Map as M
import qualified Data.IntSet as IS
import Data.Monoid
import Data.Typeable

import Util.HasSize

infixl 9 \\ --

m1 \\ m2 = difference m1 m2

class (Monoid s,HasSize s) => SetLike s where
    difference :: s -> s -> s
    intersection :: s -> s -> s
    disjoint :: s -> s -> Bool
    isSubsetOf :: s -> s -> Bool
    null :: s -> Bool

    disjoint x y = Util.SetLike.null (x `intersection` y)
    isSubsetOf x y = size x <= size y && (size (x `intersection` y) == size x)


-- you can't pull values out of the set with this, as it might store the
-- essence of a data type

class SetLike s => BuildSet s t where
    fromList :: [t] -> s
    fromDistinctAscList :: [t] -> s
    member :: t -> s -> Bool
    insert :: t -> s -> s
    delete :: t -> s -> s
    singleton :: t -> s

    singleton t = fromDistinctAscList [t]
    fromDistinctAscList = fromList


notMember x t = not $ member x t

union :: SetLike a => a -> a -> a
union = mappend

unions :: SetLike a => [a] -> a
unions = mconcat

--  int set

instance SetLike IS.IntSet where
    difference = IS.difference
    intersection = IS.intersection
    isSubsetOf = IS.isSubsetOf
    null = IS.null

instance BuildSet IS.IntSet Int where
    fromList xs = IS.fromList xs
    fromDistinctAscList xs = IS.fromDistinctAscList xs
    member x s = IS.member x s
    insert x s = IS.insert x s
    delete x s = IS.delete x s
    singleton x = IS.singleton x


-- normal set

instance Ord a => SetLike (S.Set a) where
    difference = S.difference
    intersection = S.intersection
    isSubsetOf = S.isSubsetOf
    null = S.null

instance Ord a => BuildSet (S.Set a) a where
    fromList xs = S.fromList xs
    fromDistinctAscList xs = S.fromDistinctAscList xs
    member x s = S.member x s
    insert x s = S.insert x s
    delete x s = S.delete x s
    singleton x = S.singleton x

-- maps

instance Ord a => SetLike (IM.IntMap a) where    -- SIC
    difference = IM.difference
    intersection = IM.intersection
    --isSubsetOf = IM.isSubsetOf
    null = IM.null

instance Ord a => SetLike (M.Map a b) where
    difference = M.difference
    intersection = M.intersection
    --isSubsetOf = M.isSubsetOf
    null = M.null

-- EnumSet

newtype EnumSet a = EnumSet IS.IntSet
    deriving(Typeable,Monoid,SetLike,HasSize,Eq,Ord)

instance Enum a => BuildSet (EnumSet a) a where
    fromList xs = EnumSet $ IS.fromList (map fromEnum xs)
    fromDistinctAscList xs = EnumSet $ IS.fromDistinctAscList (map fromEnum xs)
    member x (EnumSet s) = IS.member (fromEnum x) s
    insert x (EnumSet s) = EnumSet $ IS.insert (fromEnum x) s
    delete x (EnumSet s) = EnumSet $ IS.delete (fromEnum x) s
    singleton x = EnumSet $ IS.singleton (fromEnum x)


