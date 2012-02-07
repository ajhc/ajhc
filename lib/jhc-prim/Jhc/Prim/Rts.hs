-- allowing introspection into the rts
module Jhc.Prim.Rts where

import Jhc.Prim.Bits

--data Void

-- A Bang_ is always in WHNF. The mnemonic is 'Bang_ Char ~ !Char'
-- toBang_ will seq the object if needed.
data Bang_ a :: #  -- TODO(jwm): should be '!'

foreign import primitive toBang_ :: a -> Bang_ a
foreign import primitive fromBang_ :: Bang_ a -> a
foreign import primitive isWHNF :: a -> Bool_
foreign import primitive isInHeap :: Bang_ a -> Bool_
foreign import primitive bangBits :: Bang_ a -> BitsPtr_
foreign import primitive bangPtr  :: Bang_ a -> Addr_

--isWHNF   :: a -> Bool_
--isInHeap :: Bang_ a -> Bool_
--bangBits :: Bang_ a -> BitsPtr_
