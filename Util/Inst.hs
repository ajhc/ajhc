
-- Useful instances that don't belong anywhere else.
module Util.Inst() where

import Control.Monad.Identity
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

