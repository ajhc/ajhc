module Support.Inst where

import Control.Applicative
import Data.Foldable
import Data.Traversable

instance Foldable ((,) a) where
    foldMap = foldMapDefault
instance Traversable  ((,) a) where
    traverse f (x,y) = (,) x <$> f y

instance Functor ((,,) a b) where
    fmap = fmapDefault
instance Foldable ((,,) a b) where
    foldMap = foldMapDefault
instance Traversable  ((,,) a b) where
    traverse f (x,y,z) = (,,) x y <$> f z
