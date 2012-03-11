module Jhc.Type.Word(module Jhc.Type.Word, module Jhc.Prim.Bits) where

import Jhc.Prim.Bits

-- define the lifted form of the basic
-- numeric types.

data {-# CTYPE "unsigned"  #-} Word = Word Bits32_
data {-# CTYPE "uint8_t"   #-} Word8 = Word8 Bits8_
data {-# CTYPE "uint16_t"  #-} Word16 = Word16 Bits16_
data {-# CTYPE "uint32_t"  #-} Word32 = Word32 Bits32_
data {-# CTYPE "uint64_t"  #-} Word64 = Word64 Bits64_
data {-# CTYPE "uint128_t" #-} Word128 = Word128 Bits128_
data {-# CTYPE "uintptr_t" #-} WordPtr = WordPtr BitsPtr_
data {-# CTYPE "uintmax_t" #-} WordMax = WordMax BitsMax_

data {-# CTYPE "int"      #-} Int = Int Bits32_
data {-# CTYPE "int8_t"   #-} Int8 = Int8 Bits8_
data {-# CTYPE "int16_t"  #-} Int16 = Int16 Bits16_
data {-# CTYPE "int32_t"  #-} Int32 = Int32 Bits32_
data {-# CTYPE "int64_t"  #-} Int64 = Int64 Bits64_
data {-# CTYPE "int128_t" #-} Int128 = Int128 Bits128_
data {-# CTYPE "intptr_t" #-} IntPtr = IntPtr BitsPtr_
data {-# CTYPE "intmax_t" #-} IntMax = IntMax BitsMax_
