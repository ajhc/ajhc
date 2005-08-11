module DDataUtil(Elems(..),Member(..)) where

import qualified Data.IntSet as IS
import qualified Data.IntMap as IM
import qualified Data.Set as Set
import qualified Data.Map as Map



class Elems xs k v  | xs -> k v where
    elems :: xs -> [v]
    keys :: xs -> [k]
    assocs :: xs -> [(k,v)]

    assocs x = zip (keys x) (elems x)
    elems x = [ y | (_,y) <- assocs x]
    keys x =  [ x | (x,_) <- assocs x]

instance Elems (Set.Set x) x x where
    elems = Set.elems
    keys = Set.elems

instance Elems IS.IntSet Int Int where
    elems = IS.elems
    keys = IS.elems

instance Elems (Map.Map a b) a b where
    assocs = Map.assocs
    keys = Map.keys
    elems = Map.elems

instance Elems (IM.IntMap x) Int x where
    assocs = IM.assocs
    keys = IM.keys
    elems = IM.elems

instance Elems x y z => Elems (Maybe x) y z where
    keys Nothing = []
    keys (Just x) = keys x
    elems (Just x) = elems x
    elems Nothing = []
    assocs (Just x) = assocs x
    assocs Nothing = []

class Member m k | m -> k where
    member :: k -> m -> Bool
    notMember :: k -> m -> Bool

    notMember k m = not $ member k m
    member k m = not $ notMember k m

instance Ord a => Member (Set.Set a) a where
    member = Set.member
instance Ord k => Member (Map.Map k v) k where
    member = Map.member
instance Member (IM.IntMap v) Int where
    member = IM.member
instance Member IS.IntSet Int where
    member = IS.member



{-
instance Monad Set.Set where
    a >>= b = Set.unions (map b (Set.toList a))
    return x = Set.single x
    fail _ = Set.empty
-}
{-
instance Monoid IS.IntSet where
    mempty = IS.empty
    mappend = IS.union
    mconcat = IS.unions

instance Monoid (IM.IntMap a) where
    mempty = IM.empty
    mappend = IM.union
    mconcat = IM.unions

instance Ord a => Monoid (Set.Set a) where
    mempty = Set.empty
    mappend = Set.union
    mconcat = Set.unions

instance Ord k => Monoid (Map.Map k v ) where
    mempty = Map.empty
    mappend = Map.union
    mconcat = Map.unions



instance Functor IM.IntMap where
    fmap = IM.map

--instance Ord k => Functor (Map.Map k) where
--    fmap = Map.map


instance HasSize (Map.Map a b) where
    size = Map.size
instance HasSize (Set.Set a) where
    size = Set.size
instance HasSize IS.IntSet where
    size = IS.size
-}

