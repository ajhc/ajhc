module Jhc.Prim.Words where

import Jhc.Prim.Bits

-- define the lifted form of the basic
-- numeric types.

data Word = Word Bits32_
data Word8 = Word8 Bits8_
data Word16 = Word16 Bits16_
data Word32 = Word32 Bits32_
data Word64 = Word64 Bits64_
data Word128 = Word128 Bits128_
data WordPtr = WordPtr BitsPtr_
data WordMax = WordMax BitsMax_

data Int = Int Bits32_
data Int8 = Int8 Bits8_
data Int16 = Int16 Bits16_
data Int32 = Int32 Bits32_
data Int64 = Int64 Bits64_
data Int128 = Int128 Bits128_
data IntPtr = IntPtr BitsPtr_
data IntMax = IntMax BitsMax_
