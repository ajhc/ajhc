
module HasSize where

import qualified Data.Map(Map,size)
import qualified Data.Set(Set,size)
import qualified Data.IntMap(IntMap,size)
import qualified Data.IntSet(IntSet,size)

class HasSize a where
    size :: a -> Int
    sizeEQ :: Int -> a -> Bool
    sizeGT :: Int -> a -> Bool
    sizeLT :: Int -> a -> Bool
    sizeGTE :: Int -> a -> Bool
    sizeLTE :: Int -> a -> Bool
    sizeEQ s x = size x == s
    sizeGT s x = size x > s
    sizeLT s x = size x < s
    sizeGTE s x = not $ sizeLT s x
    sizeLTE s x = not $ sizeGT s x

genSize :: (Integral b,HasSize a) => a -> b
genSize = fromIntegral . HasSize.size

instance HasSize [x] where
    size = length
    sizeEQ 0 [] = True
    sizeEQ _ [] = False
    sizeEQ 0 _ = False
    sizeEQ n (_:xs) = sizeEQ (n - 1) xs



instance HasSize (Data.Map.Map a b) where
    size = Data.Map.size


instance HasSize (Data.Set.Set a) where
    size = Data.Set.size

instance HasSize (Data.IntMap.IntMap v) where
    size = Data.IntMap.size
instance HasSize Data.IntSet.IntSet where
    size = Data.IntSet.size

instance (HasSize a,HasSize b) => HasSize (Either a b) where
    size (Left x) = size x
    size (Right y) = size y
    sizeEQ s (Left x)  = sizeEQ s x
    sizeEQ s (Right x)  = sizeEQ s x
    sizeLT s (Left x)  = sizeLT s x
    sizeLT s (Right x)  = sizeLT s x
    sizeGT s (Left x)  = sizeGT s x
    sizeGT s (Right x)  = sizeGT s x

instance (HasSize a,HasSize b) => HasSize (a,b) where
    size (x,y) = size x + size y


