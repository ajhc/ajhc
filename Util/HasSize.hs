module Util.HasSize where

-- this point of this module is not only to share the 'size' syntax, but to
-- provide optimally lazy versions of size comparasin functions when dealing
-- with lazy structures. This is especially useful when having to compare the
-- size of possibly long lists.

-- it is up to each instance to decide what 'size' means

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
genSize = fromIntegral . Util.HasSize.size

instance HasSize [x] where
    size = length
    sizeEQ n _ | n < 0 = False
    sizeEQ n xs = f n xs where
        f 0 [] = True
        f _ [] = False
        f 0 _ = False
        f n (_:xs) = sizeEQ (n - 1) xs
    sizeGT n _ | n < 0 = True
    sizeGT n xs = f n xs where
        f 0 (_:_) = True
        f n [] = False
        f n (_:xs) = f (n - 1) xs
    sizeLT n _ | n <= 0 = False
    sizeLT n xs = f n xs where
        f 0 _ = False
        f _ [] = True
        f n (_:xs) = f (n - 1) xs



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

instance (HasSize a,HasSize b,HasSize c) => HasSize (a,b,c) where
    size (x,y,z) = size x + size y  + size z

