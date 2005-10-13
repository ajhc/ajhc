module CanType where


class CanType a e | a -> e where
    getType :: a -> e


-- typechecking
class CanTypeCheck env a ty | a -> ty env where
    typecheck :: Monad m => env -> a -> m ty
    tc :: Monad m => env -> a -> m ty
    tc = typecheck
