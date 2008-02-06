
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


instance Traversable IM.IntMap where
    traverse f mp = (IM.fromAscList . Map.toAscList) `fmap`  (traverse f . Map.fromAscList . IM.toAscList $ mp)
