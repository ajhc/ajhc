module Jhc.Prim.IO where

data State_ :: * -> #
data RealWorld :: *

type World__ = State_ RealWorld

type UST s a = State_ s -> (# State_ s, a #)
type UST_ s = State_ s -> State_ s
type UIO a = UST RealWorld a
type UIO_ = World__ -> World__

newtype IO a = IO (UST RealWorld a)
newtype ST s a = ST (UST s a)
newtype ACIO a = ACIO (IO a)

-- | note the implicit unsafeCoerce__ here!
foreign import primitive catch__ :: UIO a -> (e -> UIO a) -> UIO a
foreign import primitive raiseIO__ :: e -> UIO_
