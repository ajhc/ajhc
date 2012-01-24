module Jhc.Type.C where

import Data.Word
import Data.Int

newtype CLLong = CLLong IntMax
newtype CULLong = CULLong WordMax
newtype CLong = CLong IntPtr
newtype CULong = CULong WordPtr
newtype CShort = CShort Int16
newtype CUShort = CUShort Word16
newtype CWchar = CWchar Word32
newtype CWint = CWint Int32
newtype CIntMax = CIntMax IntMax
newtype CUIntMax = CUIntMax WordMax
newtype CIntPtr = CIntPtr IntPtr
newtype CUIntPtr = CUIntPtr WordPtr

data CFile
data CJmpBuf
data CFpos
