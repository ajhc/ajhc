module MonoidUtil where

import Data.Monoid
import List

infixr 6 <>

empty :: Monoid a => a
empty = mempty
(<>) :: Monoid a => a -> a -> a
(<>) = mappend


instance Monoid (IO ()) where
    mappend a b = a >> b
    mempty = return ()

--instance (Monoid a, Monoid b) => Monoid (a,b) where
--    mempty = (mempty,mempty)
--    mappend (a,b) (c,d) = (mappend a c, mappend b d) 
--    mconcat xs = case unzip xs of (a,b) -> (mconcat a,mconcat b) 
--
--instance (Monoid a, Monoid b, Monoid c) => Monoid (a,b,c) where
--    mempty = (mempty,mempty,mempty)
--    mappend (a,b,c) (a',b',c') = (mappend a a', mappend b b', mappend c c') 
--    mconcat xs = case unzip3 xs of (a,b,c) -> (mconcat a,mconcat b,mconcat c) 

instance Monoid Bool where
    mempty = False
    mappend a b = a || b
    mconcat = or

mconcatMap f xs = mconcat (map f xs)
mconcatInter x xs = mconcat (intersperse x xs)


class  QueryMonoid a where
    isEmpty :: a -> Bool


instance  QueryMonoid (Maybe a) where
    isEmpty Nothing = True
    isEmpty _ = False

instance  QueryMonoid [a] where
    isEmpty = null

instance QueryMonoid Bool where
    isEmpty = not 

instance QueryMonoid () where
    isEmpty _ = True
