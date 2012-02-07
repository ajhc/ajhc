module Jhc.Prim.Array where

import Jhc.Prim.IO
import Jhc.Prim.Bits

data MutArray_ :: * -> #
newtype Array_ m = Array_ (MutArray_ m)

foreign import primitive newArray__      :: Word_ -> a -> UST s (MutArray_ a)
foreign import primitive newBlankArray__ :: Word_ -> UST s (MutArray_ a)
foreign import primitive copyArray__     :: Word_ -> Word_ -> Word_ -> MutArray_ a -> MutArray_ a -> UST_ s
foreign import primitive readArray__     :: MutArray_ a -> Word_ -> UST s a
foreign import primitive writeArray__    :: MutArray_ a -> Word_ -> a -> UST_ s
foreign import primitive indexArray__    :: Array_ a -> Word_ -> (# a #)
