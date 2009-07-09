module Util.SameShape where

import Data.Tree



--class SameShape a b where
--    sameShape :: a -> b -> Bool

--instance (SameShape1 f) => SameShape (f a) (f b) where
--    sameShape x y = sameShape1 x y
--instance (SameShape2 f) => SameShape (f a b) (f c d) where
--    sameShape x y = sameShape2 x y

class SameShape1 f where
    sameShape1 :: f a -> f b -> Bool
class SameShape2 f where
    sameShape2 :: f a b -> f c d -> Bool


instance SameShape1 [] where
    sameShape1 [] [] = True
    sameShape1 (_:xs) (_:ys) = sameShape1 xs ys
    sameShape1 _ _ = False

instance SameShape1 Tree where
    sameShape1 (Node _ xs) (Node _ ys) = f xs ys where
        f [] [] = True
        f (x:xs) (y:ys) = sameShape1 x y && f xs ys
        f _ _ = False

instance SameShape1 Maybe where
    sameShape1 (Just _) (Just _) = True
    sameShape1 Nothing Nothing = True
    sameShape1 _ _ = False

instance SameShape2 Either where
    sameShape2 (Left _) (Left _) = True
    sameShape2 (Right _) (Right _) = True
    sameShape2 _ _ = False

instance SameShape1 IO where
    sameShape1 _ _ = True


