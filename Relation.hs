module Relation(module Relation, module Set) where

import Data.Set as Set hiding(map)

type Rel a b = Set (a,b)


--domain :: Rel a b -> Set a
--range :: Rel a b -> Set b

domain r = fromAscList (map fst (toAscList r)) 
range r = fromList [ y | (_,y) <- toList r ]


restrictDomain f r = Set.filter (f . fst) r 
restrictRange f r = Set.filter (f . snd) r 


mapDomain f r = fromList [ (f x,y)| (x,y) <- toList r ]
mapRange f r = fromList [ (x,f y)| (x,y) <- toList r ]

partitionDomain f r = Set.partition (f . fst) r
partitionRange f r = Set.partition (f . snd) r

applyRelation :: (Ord a, Ord b) => Rel a b -> a -> [b]
applyRelation r a = map snd (toList $ restrictDomain (== a) r) 

toRelationList :: (Ord a, Ord b) => Rel a b -> [(a,[b])]
toRelationList rel = [ (x, applyRelation rel x) | x <- toList (domain rel)] 
