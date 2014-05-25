{-# OPTIONS_JHC -fffi #-}
module Data.Unicode(
    CType(),
    ctype,
    isCType,
    t_alnum,
    t_alpha,
    t_blank,
    t_cntrl,
    t_digit,
    t_graph,
    t_lower,
    t_print,
    t_punct,
    t_space,
    t_upper,
    t_xdigit,
    t_none
    )where

import Foreign.C.String
import Foreign.C.Types
import System.IO.Unsafe

newtype CType = CType CInt

-- | Get a ctype other than one of the defaults.

ctype :: String -> IO CType
ctype s = withCString s c_wctype

isCType :: Char -> CType -> Bool
isCType ch ct = unsafePerformIO (c_iswctype ch ct) /= 0

t_alnum, t_alpha, t_blank, t_cntrl,
 t_digit, t_graph, t_lower, t_print,
 t_punct, t_space, t_upper, t_xdigit, t_none :: CType

t_alnum = unsafePerformIO (ctype "alnum")
t_alpha = unsafePerformIO (ctype "alpha")
t_blank = unsafePerformIO (ctype "blank")
t_cntrl = unsafePerformIO (ctype "cntrl")
t_digit = unsafePerformIO (ctype "digit")
t_graph = unsafePerformIO (ctype "graph")
t_lower = unsafePerformIO (ctype "lower")
t_print = unsafePerformIO (ctype "print")
t_punct = unsafePerformIO (ctype "punct")
t_space = unsafePerformIO (ctype "space")
t_upper = unsafePerformIO (ctype "upper")
t_xdigit = unsafePerformIO (ctype "xdigit")
t_none = CType 0

{-# NOINLINE
 t_alnum, t_alpha, t_blank, t_cntrl,
 t_digit, t_graph, t_lower, t_print,
 t_punct, t_space, t_upper, t_xdigit
 #-}

foreign import ccall "wctype.h iswctype" c_iswctype :: Char -> CType -> IO Int
foreign import ccall "wctype.h wctype" c_wctype :: CString -> IO CType
