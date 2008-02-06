
-- Useful instances that don't belong anywhere else.
module Util.Inst() where

import Control.Monad.Identity
import qualified Data.Map as Map
import qualified Data.IntMap as IM
import Data.Monoid
import List
import Data.Foldable  hiding(or)
import Data.Traversable


instance Monoid (IO ()) where
    mappend a b = a >> b
    mempty = return ()

instance Monoid Bool where
    mempty = False
    mappend a b = a || b
    mconcat = or


instance Show a => Show (Identity a) where
    show x = show $ runIdentity x


{-
instance Ord a => FunctorM (Map.Map a) where
    fmapM_ f mp = mapM_ f (Map.elems mp)
    fmapM f mp = sequence [ f y >>= return . (,) x | (x,y) <- Map.toAscList mp] >>= return . Map.fromDistinctAscList

instance FunctorM IM.IntMap where
    fmapM_ f mp = mapM_ f (IM.elems mp)
    fmapM f mp = sequence [ f y >>= return . (,) x | (x,y) <- IM.toAscList mp] >>= return . IM.fromDistinctAscList


instance Foldable IM.IntMap where
    foldMap f m = foldMap f (IM.elems m)

-}
instance Traversable IM.IntMap where
    traverse f mp = (IM.fromAscList . Map.toAscList) `fmap`  (traverse f . Map.fromAscList . IM.toAscList $ mp)
