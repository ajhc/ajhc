module Ty.Level where

import Util.Std

newtype TyLevel = TyLevel Int
    deriving(Eq,Ord,Enum)

-- inhabits constructor
-- level(y) = level(x) + 1
data x ::: y = x ::: y
    deriving(Eq,Ord,Functor,Traversable,Foldable,Show)

instance Show TyLevel where
    showsPrec _ (TyLevel 0) = ('V':)
    showsPrec _ (TyLevel 1) = ('T':)
    showsPrec _ (TyLevel 2) = ('*':)
    showsPrec _ (TyLevel 3) = ('*':) . ('*':)
    showsPrec _ (TyLevel n) | n > 0 && n <= 10 = f (n - 3) where
        f 0 s = s
        f n s = '□':f (n - 1) s
    showsPrec _ (TyLevel n) = showString "(TyLevel:" . shows n . showChar ')'

-- tyLevelOf is possibly not total
class HasTyLevel a where
    getTyLevel :: a -> Maybe TyLevel
    tyLevelOf  :: a -> TyLevel

    getTyLevel x = Just (tyLevelOf x)
    tyLevelOf x = case getTyLevel x of
        Just t -> t
        Nothing -> error "tyLevelOf: Does not have a TyLevel"

instance HasTyLevel TyLevel where
    tyLevelOf t = t

-- we subtract one from the level of the type as the
-- term itself may not carry a level with it.
instance HasTyLevel b => HasTyLevel (a ::: b) where
    getTyLevel (_ ::: y) = fmap pred (getTyLevel y)

termLevel = TyLevel 0
typeLevel = TyLevel 1
kindLevel = TyLevel 2
