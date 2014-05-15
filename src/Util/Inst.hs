{-# OPTIONS_GHC -cpp -fno-warn-unused-imports #-}
{-# LANGUAGE CPP #-}

#include "hs_src_config.h"

-- Useful instances that don't belong anywhere else.
module Util.Inst() where

import Control.Applicative
import Control.Monad.Identity(Identity(..))
import Data.Foldable(Foldable(..))
import Data.Monoid(Monoid(..))
import Data.Traversable(Traversable(..), foldMapDefault, fmapDefault)
import qualified Data.IntMap as IM
import qualified Data.Map as Map

#if !HAS_SHOW_IDENTITY
instance Show a => Show (Identity a) where
    show x = show $ runIdentity x
#endif

#if !HAS_TRAVERSABLE_INTMAP
instance Traversable IM.IntMap where
    traverse f mp = (IM.fromAscList . Map.toAscList) `fmap`  (traverse f . Map.fromAscList . IM.toAscList $ mp)
#endif


#if !HAS_FOLDABLE_TUPLE
instance Foldable ((,) a) where
    foldMap = foldMapDefault
instance Traversable  ((,) a) where
    traverse f (x,y) = (,) x <$> f y
#endif

#if !HAS_FUNCTOR_TUPLE3
instance Functor ((,,) a b) where
    fmap = fmapDefault
#endif

#if !HAS_FOLDABLE_TUPLE3
instance Foldable ((,,) a b) where
    foldMap = foldMapDefault
instance Traversable  ((,,) a b) where
    traverse f (x,y,z) = (,,) x y <$> f z
#endif
