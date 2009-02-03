module Util.Histogram(
    Histogram,
    singleton,
    insert,
    find,
    toList,
    satisfy,
    satisfyKey,
    Util.Histogram.filter,
    keys,
    elems,
    unions,
    union,
    fromList,
    Util.Histogram.map,
    Util.Histogram.mapM,
    Util.Histogram.mapM_
    ) where

import qualified Data.Map as Map
import Data.Monoid
import Data.Typeable

newtype Histogram a = Histogram (Map.Map a Int)
    deriving(Show,Typeable)

instance Ord a => Monoid (Histogram a) where
    mempty = Histogram Map.empty
    mappend (Histogram a) (Histogram b) = Histogram $ Map.unionWith (+) a b


singleton :: a -> Histogram a
singleton a = Histogram (Map.singleton a 1)

insert :: Ord a => a -> Histogram a -> Histogram a
insert a (Histogram m) = Histogram (Map.insertWith (+) a 1 m)

find :: (Ord a) => a -> Histogram a -> Int
find a (Histogram m) = Map.findWithDefault 0 a m

toList :: Histogram a -> [(a, Int)]
toList (Histogram m) = Map.toAscList m

satisfy :: (Int -> Bool) -> Histogram a -> [a]
satisfy f (Histogram m) = [ a | (a,i) <- Map.toAscList m, f i ]

satisfyKey :: (Int -> Bool) -> Histogram a -> [(a,Int)]
satisfyKey f (Histogram m) = [ (a,i) | (a,i) <- Map.toAscList m, f i ]

filter :: Ord a => (a -> Int -> Bool) -> Histogram a -> Histogram a
filter f (Histogram m) = Histogram (Map.filterWithKey f m)

keys :: Histogram a -> [a]
keys (Histogram m) = Map.keys m
elems :: Histogram a -> [Int]
elems (Histogram m) = Map.elems m

map :: Ord b => (a -> b) -> Histogram a -> Histogram b
map f (Histogram m) = Histogram $ Map.fromList [ (f k,i) | (k,i) <- Map.toList m ]

mapM :: (Monad m, Ord b) => (a -> m b) -> Histogram a -> m (Histogram b)
mapM f (Histogram m) = do
        ds <- sequence [ do f k >>= return . flip (,) i  | (k,i) <- Map.toList m ]
        return $ Histogram (Map.fromList ds)

mapM_ :: (Monad m) => (a -> m b) -> Histogram a -> m ()
mapM_ f (Histogram m) = sequence_ [ do f k >>= return . flip (,) i  | (k,i) <- Map.toList m ]

fromList :: Ord a => [a] -> Histogram a
fromList xs = foldr insert empty xs

empty :: Histogram a
empty = Histogram Map.empty

union :: Ord a => Histogram a -> Histogram a -> Histogram a
union = mappend
unions :: Ord a => [Histogram a] -> Histogram a
unions = mconcat



