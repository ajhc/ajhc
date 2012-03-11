module Jhc.Type.C where

import Jhc.Type.Word
import Jhc.Type.Float

-- | Haskell representation for @errno@ values.
-- The implementation is deliberately exposed, to allow users to add
-- their own definitions of 'Errno' values.
newtype Errno = Errno CInt

newtype {-# CTYPE "char" #-}           CChar    = CChar Int8
newtype {-# CTYPE "signed char" #-}    CSChar   = CSChar Int8
newtype {-# CTYPE "unsigned char" #-}  CUChar   = CUChar Word8
newtype {-# CTYPE "short" #-}          CShort   = CShort Int16
newtype {-# CTYPE "unsigned short" #-} CUShort  = CUShort Word16
newtype {-# CTYPE "int" #-}            CInt     = CInt Int
newtype {-# CTYPE "unsigned" #-}       CUInt    = CUInt Word
newtype {-# CTYPE "long" #-}           CLong    = CLong IntPtr
newtype {-# CTYPE "unsigned long" #-}  CULong   = CULong WordPtr
newtype {-# CTYPE "long long" #-}      CLLong   = CLLong IntMax
newtype {-# CTYPE "unsigned long long" #-} CULLong  = CULLong WordMax

newtype CIntMax  = CIntMax IntMax
newtype CUIntMax = CUIntMax WordMax
newtype CIntPtr  = CIntPtr IntPtr
newtype CUIntPtr = CUIntPtr WordPtr

newtype CFloat   = CFloat Float
newtype CDouble  = CDouble Double
newtype {-# CTYPE "long double" #-} CLDouble = CLDouble Double

newtype {-# CTYPE "wchar_t" #-}   CWchar   = CWchar Word32
newtype {-# CTYPE "wint_t" #-}    CWint    = CWint Int32
newtype {-# CTYPE "clock_t" #-}   CClock   = CClock IntMax
newtype {-# CTYPE "ptrdiff_t" #-} CPtrdiff = CPtrdiff IntPtr
newtype {-# CTYPE "size_t" #-}    CSize    = CSize WordPtr
newtype {-# CTYPE "time_t" #-}    CTime    = CTime IntMax

data {-# CTYPE "FILE" #-}        CFile
data {-# CTYPE "jmp_buf" #-}     CJmpBuf
data {-# CTYPE "fpos_t" #-}      CFpos
data {-# CTYPE "sigatomic_t" #-} CSigAtomic
