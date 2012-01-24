{-# OPTIONS_JHC -fffi -funboxed-values  #-}
module Data.Typeable(TypeRep(),Typeable(..),Typeable1(..),Typeable2(..)) where

import Jhc.Prim
import Jhc.String

type String_ = BitsPtr_

data TypeRep = TypeRep String_ [TypeRep]

showsAddr__ :: String_ -> [Char] -> [Char]
showsAddr__ a xs = unpackStringFoldr a (:) xs

instance Show TypeRep where
    showsPrec _ (TypeRep a []) = showsAddr__ a
    showsPrec n (TypeRep a xs) = showParen (n > 9) $ spacesep (showsAddr__ a:map (showsPrec 10) xs) where
        spacesep [] = id
        spacesep [x] = x
        spacesep (x:xs) = x . showChar ' ' . spacesep xs

instance Eq TypeRep where
    TypeRep a xs == TypeRep b ys = case c_strcmp (Addr_ a) (Addr_ b) of
        0 -> xs == ys
        _ -> False

foreign import ccall "strcmp" c_strcmp :: Addr_ -> Addr_ -> Int

{-
foreign import primitive ptypeOf :: a -> TypeRep
foreign import primitive ptypeOf1 :: t a -> TypeRep
foreign import primitive ptypeOf2 :: t a b -> TypeRep
foreign import primitive ptypeOf3 :: t a b c -> TypeRep
foreign import primitive ptypeOf4 :: t a b c d -> TypeRep
foreign import primitive ptypeOf5 :: t a b c d e -> TypeRep
foreign import primitive ptypeOf6 :: t a b c d e f -> TypeRep
foreign import primitive ptypeOf7 :: t a b c d e f g -> TypeRep
foreign import primitive typeRepEq :: TypeRep -> TypeRep -> Bool
-}

class Typeable a where
    typeOf :: a -> TypeRep

class Typeable1 f where
    typeOf1 :: f a -> TypeRep

class Typeable2 f where
    typeOf2 :: f a b -> TypeRep

instance Typeable1 [] where
    typeOf1 _ = TypeRep "[]"# []

instance Typeable a => Typeable [a] where
    typeOf x = typeOfDefault x

{-
instance (Typeable a,Typeable b) => Typeable (a -> b) where
    typeOf x = (typeOf2 x `mkAppTy` arg1 x) `mkAppTy` arg2 x where
        arg1 :: (x -> y) -> x
        arg2 :: (x -> y) -> y
        arg1 = undefined
        arg2 = undefined

instance (Typeable a) => Typeable1 ((->) a) where
    typeOf1 x = typeOf1Default x

instance Typeable2 (->) where
    typeOf2 _ = TypeRep "->"# []
-}

instance Typeable2 (,) where
    typeOf2 _ = TypeRep "(,)"# []

instance Typeable a => Typeable1 ((,) a) where
    typeOf1 x = typeOf1Default x

instance (Typeable b,Typeable a) => Typeable (a,b) where
    typeOf x = typeOfDefault x

instance Typeable Char where
    typeOf _ = TypeRep "Char"# []

instance Typeable () where
    typeOf _ = TypeRep "()"# []

instance Typeable Int where
    typeOf _ = TypeRep "Int"# []

--instance (Typeable1 f,Typeable a) => Typeable (f a) where
--    typeOf x = typeOf1 x `mkAppTy` typeOf (argType x) where
--        argType :: a b -> b
--        argType = undefined

mkAppTy :: TypeRep -> TypeRep -> TypeRep
mkAppTy (TypeRep x xs) tr = TypeRep x (xs ++ [tr])

-------------------------------------------------------------
--
--		Type-safe cast
--
-------------------------------------------------------------

unsafeCoerce :: a -> b
unsafeCoerce = unsafeCoerce__

-- | The type-safe cast operation
cast :: (Typeable a, Typeable b) => a -> Maybe b
cast x = r where
    fromJust (Just x) = x
    r = if typeOf x == typeOf (fromJust r)
               then Just $ unsafeCoerce x
	       else Nothing

{-
-- | A flexible variation parameterised in a type constructor
gcast :: (Typeable a, Typeable b) => c a -> Maybe (c b)
gcast x = r
 where
  r = if typeOf (getArg x) == typeOf (getArg (fromJust r))
        then Just $ unsafeCoerce x
        else Nothing
  getArg :: c x -> x
  getArg = undefined

-- | Cast for * -> *
gcast1 :: (Typeable1 t, Typeable1 t') c (t a) -> Maybe (c (t' a))
gcast1 x = r
 where
  r = if typeOf1 (getArg x) == typeOf1 (getArg (fromJust r))
       then Just $ unsafeCoerce x
       else Nothing
  getArg :: c x -> x
  getArg = undefined

-- | Cast for * -> * -> *
gcast2 :: (Typeable2 t, Typeable2 t') c (t a b) -> Maybe (c (t' a b))
gcast2 x = r
 where
  r = if typeOf2 (getArg x) == typeOf2 (getArg (fromJust r))
       then Just $ unsafeCoerce x
       else Nothing
  getArg :: c x -> x
  getArg = undefined
  -}

-- | For defining a 'Typeable' instance from any 'Typeable1' instance.
typeOfDefault :: (Typeable1 t, Typeable a) => t a -> TypeRep
typeOfDefault x = typeOf1 x `mkAppTy` typeOf (argType x)
 where
   argType :: t a -> a
   argType =  undefined

-- | For defining a 'Typeable1' instance from any 'Typeable2' instance.
typeOf1Default :: (Typeable2 t, Typeable a) => t a b -> TypeRep
typeOf1Default x = typeOf2 x `mkAppTy` typeOf (argType x)
 where
   argType :: t a b -> a
   argType =  undefined
