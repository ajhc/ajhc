module Support.MapBinaryInstance where


import Data.Binary
import Data.Map as Map
import Data.Set as Set
import Control.Monad

putMap :: (Binary k,Ord k,Binary v) => Map.Map k v -> Put
putMap x = do
        put (Map.size x)
        mapM_ put (Map.toList x)
getMap :: (Binary k,Ord k,Binary v) => Get (Map.Map k v)
getMap = do
        (sz::Int) <- get
        ls <- replicateM sz get
        return (Map.fromList ls)


putSet :: (Binary a,Ord a) => Set.Set a -> Put
putSet x = do
        put (Set.size x)
        mapM_ put (Set.toList x)

getSet :: (Binary a,Ord a) => Get (Set.Set a)
getSet = do
        (sz::Int) <- get
        ls <- replicateM sz get
        return (Set.fromList ls)
