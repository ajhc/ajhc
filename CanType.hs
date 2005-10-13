module CanType where


-- This is a simple routine meant to do the minimum amount of work to get the type of something
class CanType a e | a -> e where
    getType :: a -> e

-- This should perform a full typecheck and may take any extra information needed as an extra parameter
class CanTypeCheck env a ty | a -> ty env where
    typecheck :: Monad m => env -> a -> m ty
