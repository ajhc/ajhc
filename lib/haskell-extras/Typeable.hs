module Typeable where

class Typeable a where
    typeOf :: a -> String
    typeOf _ = "Unknown"

instance Typeable Char where
    typeOf _ = "Char"

instance Typeable Bool where
    typeOf _ = "Bool"

instance Typeable a => Typeable [a] where
    typeOf x = "[" ++ typeOf (head x) ++ "]"
