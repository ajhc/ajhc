{-# LANGUAGE UnboxedTuples, ForeignFunctionInterface #-}
module Jhc.Prim.IO where

data State_ :: * -> #
data RealWorld :: *

type World__ = State_ RealWorld

-- Aliases for common State_ related types. Useful in foreign imports.
type UST s a = State_ s -> (# State_ s, a #)
type UST_ s = State_ s -> State_ s
-- Aliases specialized for the world.
type UIO a = UST RealWorld a
type UIO_ = UST_ RealWorld

newtype ST s a = ST (UST s a)
newtype IO a = IO (ST RealWorld a)
newtype ACIO a = ACIO (IO a)

-- Note the implicit unsafeCoerce__ here!
-- We currently don't allow exeptions in general ST monads as a design choice.
foreign import primitive catch__ :: UIO a -> (e -> UIO a) -> UIO a
foreign import primitive raiseIO__ :: e -> UIO_
