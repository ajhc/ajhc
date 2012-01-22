module Support.CanType where

import Control.Monad.Error()

-- This is a simple routine meant to do the minimum amount of work to get the type of something
class CanType a where
    type TypeOf a
    getType :: a -> (TypeOf a)

instance CanType e => CanType [e] where
    type TypeOf [e] = [TypeOf e]
    getType es = map getType es

instance CanType e => CanType (Maybe e) where
    type TypeOf (Maybe e) = Maybe (TypeOf e)
    getType Nothing = Nothing
    getType (Just x) = Just (getType x)

instance (CanType e1, CanType e2) => CanType (Either e1 e2) where
    type TypeOf (Either e1 e2) = Either (TypeOf e1) (TypeOf e2)
    getType (Left x) = Left $ getType x
    getType (Right x) = Right $ getType x
