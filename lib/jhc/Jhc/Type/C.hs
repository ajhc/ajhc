module Jhc.Type.C where

import Data.Word
import Data.Int

import Jhc.Type.Float

-- | Haskell representation for @errno@ values.
-- The implementation is deliberately exposed, to allow users to add
-- their own definitions of 'Errno' values.
newtype Errno = Errno CInt

newtype CChar    = CChar Int8
newtype CSChar   = CSChar Int8
newtype CUChar   = CUChar Word8
newtype CShort   = CShort Int16
newtype CUShort  = CUShort Word16
newtype CInt     = CInt Int
newtype CUInt    = CUInt Word
newtype CLong    = CLong IntPtr
newtype CULong   = CULong WordPtr
newtype CLLong   = CLLong IntMax
newtype CULLong  = CULLong WordMax

newtype CIntMax  = CIntMax IntMax
newtype CUIntMax = CUIntMax WordMax
newtype CIntPtr  = CIntPtr IntPtr
newtype CUIntPtr = CUIntPtr WordPtr

newtype CFloat   = CFloat Float
newtype CDouble  = CDouble Double
newtype CLDouble = CLDouble Double

newtype CWchar   = CWchar Word32
newtype CWint    = CWint Int32
newtype CClock   = CClock IntMax
newtype CPtrdiff = CPtrdiff IntPtr
newtype CSize    = CSize WordPtr
newtype CTime    = CTime IntMax

data CFile
data CJmpBuf
data CFpos
data CSigAtomic
