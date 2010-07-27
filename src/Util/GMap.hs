module Util.GMap where

import Data.Monoid
import Util.SetLike
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Set as Set
import Util.HasSize
import Data.Foldable hiding(toList)
import Data.Traversable


data family GMap k :: * -> *
data family GSet k :: *

newtype instance GMap Int v = GMapInt (IM.IntMap v)
    deriving(Monoid,IsEmpty,HasSize,Collection,Unionize,SetLike,MapLike)
newtype instance GSet Int = GSetInt IS.IntSet
    deriving(Monoid,IsEmpty,HasSize,Collection,Unionize,SetLike)

instance Functor (GMap Int) where
    fmap f (GMapInt v) = GMapInt $ fmap f v

type instance Elem (GMap k v) = (k,v)
type instance Key (GMap k v) = k
type instance Value (GMap k v) = v
type instance Elem (GSet k) = k
type instance Key (GSet k) = k

newtype instance GSet Char = GSetChar (EnumSet Char)
    deriving(Monoid,IsEmpty,HasSize,Collection,Unionize,SetLike)
newtype instance GMap Char v = GMapChar (EnumMap Char v)
    deriving(Monoid,IsEmpty,HasSize,Collection,Unionize,SetLike,MapLike)

--newtype instance GSet (a,b) = GSetTup2 (GMap a (GSet b))

gsetToSet :: (Collection (GSet a), Ord a) => GSet a -> Set.Set a
gsetToSet gs = Set.fromDistinctAscList (toList gs)

class GMapSet k where
    toSet :: GMap k v -> GSet k
    toMap :: (k -> v) -> GSet k -> GMap k v

instance GMapSet Int where
    toSet (GMapInt im) = GSetInt (IM.keysSet im)
    toMap f (GSetInt is) = GMapInt $ IM.fromDistinctAscList [ (x,f x) | x <- IS.toAscList is]
