-- Haskell 2010 compliant
module Foreign.C.Types (
    CChar,  CSChar,  CUChar,  CShort,  CUShort,  CInt,  CUInt,  CLong,  CULong,
    CPtrdiff,  CSize,  CWchar,  CSigAtomic,  CLLong,  CULLong,  CIntPtr,
    CUIntPtr,  CIntMax,  CUIntMax,  CClock,  CTime,  CFloat,  CDouble,  CFile,
    CFpos,  CJmpBuf
  ) where

import Data.Word
import Jhc.Type.C

data CChar
data CSChar
data CUChar

data CInt
data CUInt

data CPtrdiff
data CSize
data CSigAtomic
data CClock
data CTime
data CFloat
data CDouble
data CLDouble
