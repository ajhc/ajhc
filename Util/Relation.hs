
-- | extend Data.Set with relation operations

module Util.Relation(module Util.Relation, module Set) where

import Data.Set as Set hiding(map)
import qualified Data.Set as Set (map)

type Rel a b = Set (a,b)


domain :: Ord a => Rel a b -> Set a
domain r = mapMonotonic fst r

range :: (Ord a,Ord b) => Rel a b -> Set b
range r = Set.map snd r

flipRelation :: (Ord a, Ord b) => Rel a b -> Rel b a
flipRelation = Set.map (\ (x,y) -> (y,x))

restrictDomain :: (Ord a, Ord b) => (a -> Bool) -> Rel a b -> Rel a b
restrictDomain f r = Set.filter (f . fst) r

restrictRange :: (Ord a, Ord b) => (b -> Bool) -> Rel a b -> Rel a b
restrictRange f r = Set.filter (f . snd) r


mapDomain :: (Ord a, Ord b, Ord c) => (a -> c) -> Rel a b -> Rel c b
mapDomain f r = Set.map (\ (x,y) -> (f x,y)) r

mapRange :: (Ord a, Ord b, Ord c) => (b -> c) -> Rel a b -> Rel a c
mapRange f r = Set.map (\ (x,y) -> (x,f y)) r

partitionDomain f r = Set.partition (f . fst) r

partitionRange f r = Set.partition (f . snd) r

applyRelation :: (Ord a, Ord b) => Rel a b -> a -> [b]
applyRelation r a = Prelude.map snd (toList $ restrictDomain (== a) r)

toRelationList :: (Ord a, Ord b) => Rel a b -> [(a,[b])]
toRelationList rel = [ (x, applyRelation rel x) | x <- toList (domain rel)]
