module Data.Typeable(TypeRep,typeOf) where


data TypeRep

instance Eq TypeRep where
    (==) = primTypeRepEq

foreign import primitive typeOf :: a -> TypeRep
foreign import primitive typeOf1 :: t a -> TypeRep
foreign import primitive typeOf2 :: t a b -> TypeRep
foreign import primitive typeOf3 :: t a b c -> TypeRep
foreign import primitive typeOf4 :: t a b c d -> TypeRep
foreign import primitive typeOf5 :: t a b c d e -> TypeRep
foreign import primitive typeOf6 :: t a b c d e f -> TypeRep
foreign import primitive typeOf7 :: t a b c d e f g -> TypeRep
foreign import primitive typeRepEq :: TypeRep -> TypeRep -> Bool


-------------------------------------------------------------
--
--		Type-safe cast
--
-------------------------------------------------------------

-- | The type-safe cast operation
cast ::  a -> Maybe b
cast x = r
       where
	 r = if typeOf x == typeOf (fromJust r)
               then Just $ unsafeCoerce x
	       else Nothing

-- | A flexible variation parameterised in a type constructor
gcast :: c a -> Maybe (c b)
gcast x = r
 where
  r = if typeOf (getArg x) == typeOf (getArg (fromJust r))
        then Just $ unsafeCoerce x
        else Nothing
  getArg :: c x -> x
  getArg = undefined

-- | Cast for * -> *
gcast1 ::  c (t a) -> Maybe (c (t' a))
gcast1 x = r
 where
  r = if typeOf1 (getArg x) == typeOf1 (getArg (fromJust r))
       then Just $ unsafeCoerce x
       else Nothing
  getArg :: c x -> x
  getArg = undefined

-- | Cast for * -> * -> *
gcast2 ::  c (t a b) -> Maybe (c (t' a b))
gcast2 x = r
 where
  r = if typeOf2 (getArg x) == typeOf2 (getArg (fromJust r))
       then Just $ unsafeCoerce x
       else Nothing
  getArg :: c x -> x
  getArg = undefined
