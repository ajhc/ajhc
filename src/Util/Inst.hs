{-# OPTIONS_GHC -cpp #-}
{-# LANGUAGE CPP #-}

#include "hs_src_config.h"

-- Useful instances that don't belong anywhere else.
module Util.Inst() where

import Control.Applicative
import Data.Foldable
import Data.Traversable
import Control.Monad.Identity
import Data.Monoid
#if !HAS_TRAVERSABLE_INTMAP
import qualified Data.Map as Map
import qualified Data.IntMap as IM
import Data.List
import Data.Traversable
#endif

instance Monoid (IO ()) where
    mappend a b = a >> b
    mempty = return ()

instance Monoid Bool where
    mempty = False
    mappend a b = a || b
    mconcat = or

#if !HAS_SHOW_IDENTITY
instance Show a => Show (Identity a) where
    show x = show $ runIdentity x
#endif

#if !HAS_TRAVERSABLE_INTMAP
instance Traversable IM.IntMap where
    traverse f mp = (IM.fromAscList . Map.toAscList) `fmap`  (traverse f . Map.fromAscList . IM.toAscList $ mp)
#endif
