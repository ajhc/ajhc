module Support.MapBinaryInstance where

import Control.Monad
import Data.Binary
import Data.Bits
import Data.Map as Map
import Data.Set as Set

putLEB128 :: Word -> Put
putLEB128 w = f w where
    f !w = do
        let !w' = w `shiftR` 7
        putWord8 $! fromIntegral $ (w .&. 0x7f) .|. (if w' /= 0 then 0x80 else 0)
        if w' /= 0 then f w' else return ()

getLEB128 :: Get Word
getLEB128 = f 0 0 where
    f !r !s = do
        byte <- getWord8
        let !r' = r .|. (fromIntegral (byte .&. 0x7f) `shiftL` s)
        if byte .&. 0x80 /= 0 then f r' (s + 7) else return r'

putList :: (Binary v) => [v] -> Put
putList xs = do
        putLEB128 (fromIntegral $ length xs)
        mapM_ put xs
getList :: (Binary v) => Get [v]
getList = do
        sz <- getLEB128
        ls <- replicateM (fromIntegral sz) get
        return ls

putMap :: (Binary k,Ord k,Binary v) => Map.Map k v -> Put
putMap x = do
        putLEB128 (fromIntegral $ Map.size x :: Word)
        mapM_ put (Map.toList x)
getMap :: (Binary k,Ord k,Binary v) => Get (Map.Map k v)
getMap = do
        sz <- getLEB128
        ls <- replicateM (fromIntegral sz) get
        return (Map.fromList ls)

putSet :: (Binary a,Ord a) => Set.Set a -> Put
putSet x = do
        putLEB128 (fromIntegral $ Set.size x)
        mapM_ put (Set.toList x)

getSet :: (Binary a,Ord a) => Get (Set.Set a)
getSet = do
        sz <- getLEB128
        ls <- replicateM (fromIntegral sz) get
        return (Set.fromList ls)
