{-# OPTIONS_GHC -cpp #-}
{-# LANGUAGE CPP #-}

-- Useful instances that don't belong anywhere else.
module Util.Inst() where

import Control.Monad.Identity
import Data.Monoid
#if __GLASGOW_HASKELL__ <= 610
import qualified Data.Map as Map
import qualified Data.IntMap as IM
import List
import Data.Traversable
#endif


instance Monoid (IO ()) where
    mappend a b = a >> b
    mempty = return ()

instance Monoid Bool where
    mempty = False
    mappend a b = a || b
    mconcat = or


instance Show a => Show (Identity a) where
    show x = show $ runIdentity x


#if __GLASGOW_HASKELL__ <= 610
instance Traversable IM.IntMap where
    traverse f mp = (IM.fromAscList . Map.toAscList) `fmap`  (traverse f . Map.fromAscList . IM.toAscList $ mp)
#endif
