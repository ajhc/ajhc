
module Support.FreeVars where

import Data.Monoid

class Monoid b => FreeVars a b where
    freeVars ::  a -> b

instance  Monoid x => FreeVars () x where
    freeVars () = mempty


instance (FreeVars x b, FreeVars y b) => FreeVars (x,y) b where
    freeVars (x,y) = freeVars x `mappend` freeVars y

instance (FreeVars x b, FreeVars y b, FreeVars z b) => FreeVars (x,y,z) b where
    freeVars (x,y,z) = freeVars x `mappend` freeVars y `mappend` freeVars z

instance FreeVars a b => FreeVars [a] b where
    freeVars as = mconcat (map freeVars as)

instance FreeVars a b => FreeVars (Maybe a) b where
    freeVars (Just x) = freeVars x
    freeVars Nothing = mempty

instance (FreeVars x b, FreeVars y b) => FreeVars (Either x y) b where
    freeVars (Left x) = freeVars x
    freeVars (Right y) = freeVars y

