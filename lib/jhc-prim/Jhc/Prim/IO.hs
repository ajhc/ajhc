module Jhc.Prim.IO where

data State_ s :: #
data RealWorld

type STRep s a = State_ s -> (# State_ s, a #)
type World__ = State_ RealWorld
type UIO a = STRep RealWorld a
type UIO_ = World__ -> World__

newtype IO a = IO (STRep RealWorld a)
newtype ST s a = ST (STRep s a)

-- | note the implicit unsafeCoerce__ here!
foreign import primitive catch__ :: UIO a -> (e -> UIO a) -> UIO a
foreign import primitive raiseIO__ :: e -> UIO_
