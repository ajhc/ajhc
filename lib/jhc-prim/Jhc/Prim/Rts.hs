-- allowing introspection into the rts
module Jhc.Prim.Rts where

import Jhc.Prim.Bits

-- A Bang_ is always in WHNF. The mnemonic is 'Bang_ Char ~ !Char'
-- Bang_ is also an FFI-able type that turns into a raw haskell object pointer.
data Bang_ a :: #

-- safe
foreign import primitive toBang_     :: a -> Bang_ a
foreign import primitive fromBang_   :: Bang_ a -> a
-- unwise
foreign import primitive isWHNF      :: a -> Bool_
foreign import primitive isInHeap    :: Bang_ a -> Bool_
foreign import primitive bangPtr     :: Bang_ a -> Addr_
foreign import primitive bangToRaw   :: Bang_ a -> BitsPtr_
-- unsafe
foreign import primitive bangFromRaw :: BitsPtr_ -> Bang_ a
