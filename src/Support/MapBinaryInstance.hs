module Support.MapBinaryInstance where


import Data.Binary
import Data.Map as Map
import Data.Set as Set
import Control.Monad

putMap :: (Binary k,Ord k,Binary v) => Map.Map k v -> Put
putMap x = do
        put (fromIntegral $ Map.size x :: Word32)
        mapM_ put (Map.toList x)
getMap :: (Binary k,Ord k,Binary v) => Get (Map.Map k v)
getMap = do
        sz <- get :: Get Word32
        ls <- replicateM (fromIntegral sz) get
        return (Map.fromList ls)


putSet :: (Binary a,Ord a) => Set.Set a -> Put
putSet x = do
        put (fromIntegral $ Set.size x :: Word32)
        mapM_ put (Set.toList x)

getSet :: (Binary a,Ord a) => Get (Set.Set a)
getSet = do
        sz <- get :: Get Word32
        ls <- replicateM (fromIntegral sz) get
        return (Set.fromList ls)
