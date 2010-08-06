
-- | extend Data.Set with relation operations

module Util.Relation where

import Data.Monoid
import Data.Set as Set hiding(map)
import Util.SetLike
import qualified Data.Set as Set
import qualified Data.Map as Map

newtype Rel a b = Rel (Map.Map a (Set b))
    deriving(Eq)

instance (Ord a,Ord b) => Monoid (Rel a b) where
    mempty = Rel mempty
    mappend (Rel r1) (Rel r2) = Rel $ Map.unionWith Set.union r1 r2

instance (Ord a,Ord b) => Unionize (Rel a b) where
    difference (Rel r1) (Rel r2) = Rel $ Map.differenceWith f r1 r2 where
        f r1 r2 = if Set.null rs then Nothing else Just rs where
            rs = Set.difference r1 r2
    intersection (Rel r1) (Rel r2) = prune $ Map.intersectionWith Set.intersection r1 r2

instance (Ord a,Ord b) => Collection (Rel a b) where
    fromList xs = Rel $ Map.fromListWith Set.union [ (x,Set.singleton y) | (x,y) <- xs ]
    toList (Rel r) = [ (x,y) | (x,ys) <- Map.toList r, y <- Set.toList ys]

prune r = Rel $ Map.mapMaybe f r where
    f s = if Set.null s then Nothing else Just s


type instance Elem (Rel a b) = (a,b)
type instance Key (Rel a b) = (a,b)


domain :: (Ord a,Ord b) => Rel a b -> Set a
domain (Rel r) = Map.keysSet r

range :: (Ord a,Ord b) => Rel a b -> Set b
range (Rel r) = Set.unions $ Map.elems r

--flipRelation :: (Ord a, Ord b) => Rel a b -> Rel b a
--flipRelation (Rel r) = Rel $ Set.map (\ (x,y) -> (y,x)) r

restrictDomain :: (Ord a, Ord b) => (a -> Bool) -> Rel a b -> Rel a b
restrictDomain f (Rel r) = Rel $ Map.filterWithKey (\k _ -> f k) r

restrictDomainS :: (Ord a, Ord b) => a -> Rel a b -> Rel a b
restrictDomainS x (Rel r) = case Map.lookup x r of
    Nothing -> Rel mempty
    Just v -> Rel $ Map.singleton x v

restrictDomainSet :: (Ord a, Ord b) => Set a -> Rel a b -> Rel a b
restrictDomainSet s (Rel r) = Rel $ Map.filterWithKey (\k _ -> k `Set.member` s) r

restrictRange :: (Ord a, Ord b) => (b -> Bool) -> Rel a b -> Rel a b
restrictRange f (Rel r) = Rel $ Map.mapMaybe g r where
    g s = if Set.null ss then Nothing else Just ss where
        ss = Set.filter f s

mapDomain :: (Ord a, Ord b, Ord c) => (a -> c) -> Rel a b -> Rel c b
mapDomain f (Rel r) = Rel $ Map.mapKeys f r

mapRange :: (Ord a, Ord b, Ord c) => (b -> c) -> Rel a b -> Rel a c
mapRange f (Rel r) = Rel $ Map.map (Set.map f) r

partitionDomain f (Rel r) = case Map.partitionWithKey (\k _ -> f k) r of
    (x,y) -> (Rel x,Rel y)

--partitionRange f (Rel r) = Rel $ Set.partition (f . snd) r

--applyRelation :: (Ord a, Ord b) => Rel a b -> a -> [b]
--applyRelation r a = Prelude.map snd (Set.toList . unRel $ restrictDomain (== a) r)

unRel (Rel r) = r

toRelationList :: (Ord a, Ord b) => Rel a b -> [(a,[b])]
toRelationList (Rel r) = Map.toList (Map.map Set.toList r)
--toRelationList :: (Ord a, Ord b) => Rel a b -> [(a,[b])]
--toRelationList rel = [ (x, applyRelation rel x) | x <- Set.toList (domain rel)]
