{-# OPTIONS_JHC -fffi #-}
module Data.Dynamic(Dynamic,toDyn,fromDyn,fromDynamic,dynApply,dynApp) where


import Data.Typeable

data Obj

data Dynamic = Dynamic TypeRep Obj

instance Show Dynamic where
    showsPrec _ x s = "<Dynamic>" ++ s

-- | Converts an arbitrary value into an object of type 'Dynamic'.
--
-- The type of the object must be an instance of 'Typeable', which
-- ensures that only monomorphically-typed objects may be converted to
-- 'Dynamic'.  To convert a polymorphic object into 'Dynamic', give it
-- a monomorphic type signature.  For example:
--
-- >    toDyn (id :: Int -> Int)
--
toDyn :: Typeable a => a -> Dynamic
toDyn v = Dynamic (typeOf v) (unsafeCoerce v)

-- | Converts a 'Dynamic' object back into an ordinary Haskell value of
-- the correct type.  See also 'fromDynamic'.
fromDyn :: Typeable a
 	=> Dynamic 	-- ^ the dynamically-typed object
	-> a		-- ^ a default value
	-> a		-- ^ returns: the value of the first argument, if
			-- it has the correct type, otherwise the value of
			-- the second argument.
fromDyn (Dynamic t v) def
  | typeOf def == t = unsafeCoerce v
  | otherwise       = def

-- | Converts a 'Dynamic' object back into an ordinary Haskell value of
-- the correct type.  See also 'fromDyn'.
fromDynamic
	:: Typeable a
	=> Dynamic	-- ^ the dynamically-typed object
	-> Maybe a	-- ^ returns: @'Just' a@, if the dynamically-typed
			-- object has the correct type (and @a@ is its value),
			-- or 'Nothing' otherwise.
fromDynamic (Dynamic t v) =
  case unsafeCoerce v of
    r | t == typeOf r -> Just r
      | otherwise     -> Nothing

-- (f::(a->b)) `dynApply` (x::a) = (f a)::b
dynApply :: Dynamic -> Dynamic -> Maybe Dynamic
dynApply (Dynamic t1 f) (Dynamic t2 x) =
  case funResultTy t1 t2 of
    Just t3 -> Just (Dynamic t3 ((unsafeCoerce f) x))
    Nothing -> Nothing

dynApp :: Dynamic -> Dynamic -> Dynamic
dynApp f x = case dynApply f x of
             Just r -> r
             Nothing -> error ("Type error in dynamic application.\n" ++
                               "Can't apply function " ++ show f ++
                               " to argument " ++ show x)


foreign import primitive "unsafeCoerce" unsafeCoerce :: a -> b
