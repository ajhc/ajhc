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


singleton a = Histogram (Map.singleton a 1)
insert a (Histogram m) = Histogram (Map.insertWith (+) a 1 m)
find a (Histogram m) = Map.findWithDefault 0 a m
toList (Histogram m) = Map.toAscList m
satisfy f (Histogram m) = [ a | (a,i) <- Map.toAscList m, f i ]
satisfyKey f (Histogram m) = [ (a,i) | (a,i) <- Map.toAscList m, f i ]
filter f (Histogram m) = Histogram (Map.filterWithKey f m)
keys (Histogram m) = Map.keys m
elems (Histogram m) = Map.elems m
map f (Histogram m) = Histogram $ Map.fromList [ (f k,i) | (k,i) <- Map.toList m ]
mapM f (Histogram m) = do
        ds <- sequence [ do f k >>= return . flip (,) i  | (k,i) <- Map.toList m ]
        return $ Histogram (Map.fromList ds)
mapM_ f (Histogram m) = sequence_ [ do f k >>= return . flip (,) i  | (k,i) <- Map.toList m ]
fromList :: Ord a => [a] -> Histogram a
fromList xs = foldr insert empty xs
empty = Histogram Map.empty

union :: Ord a => Histogram a -> Histogram a -> Histogram a
union = mappend
unions :: Ord a => [Histogram a] -> Histogram a
unions = mconcat

{-
instance Functor Histogram where
    fmap f (Histogram m) = Histogram $ Map.fromList [ (f k,i) | (k,i) <- Map.toList m ]

instance FunctorM Histogram where
    fmapM f (Histogram m) = do
        ds <- sequence [ do f k >>= return . flip (,) i  | (k,i) <- Map.toList m ]
        return $ Histogram (Map.fromList ds)
    fmapM_ f (Histogram m) = sequence_ [ do f k >>= return . flip (,) i  | (k,i) <- Map.toList m ]

-}


