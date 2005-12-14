
-- Useful instances that don't belong anywhere else.
module Util.Inst() where

import Control.Monad.Identity
import qualified Data.Map as Map
import Data.FunctorM
import Data.Monoid
import List


instance Monoid (IO ()) where
    mappend a b = a >> b
    mempty = return ()

instance Monoid Bool where
    mempty = False
    mappend a b = a || b
    mconcat = or


instance Show a => Show (Identity a) where
    show x = show $ runIdentity x


instance Ord a => FunctorM (Map.Map a) where
    fmapM_ f mp = mapM_ f (Map.elems mp)
    fmapM f mp = sequence [ f y >>= return . (,) x | (x,y) <- Map.toAscList mp] >>= return . Map.fromAscList
