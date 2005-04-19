module CanType where


class CanType a e | a -> e where
    getType :: a -> e


