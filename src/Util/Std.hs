-- standard modules we almost always want
module Util.Std(
        module Control.Applicative,
        module Control.Monad,
        module Control.Monad.Identity,
        module Data.List,
        module Data.Maybe,
        module Data.Monoid,
        module Data.Traversable,
        module Data.Foldable
        )where

import Control.Applicative
import Control.Monad
import Control.Monad.Identity
import Data.List
import Data.Maybe
import Data.Monoid(Monoid(..),(<>))
-- we want the names for deriving
import Data.Traversable(Traversable())
import Data.Foldable(Foldable())
