{-# OPTIONS_JHC -fffi #-}
module System.Info(compilerName,compilerVersion,os,arch) where

import Foreign.C.String
import System.IO.Unsafe
import Foreign

compilerName = "jhc"
compilerVersion = "0"

os = unsafePerformIO $ peekCAString =<< peek options_os
arch = unsafePerformIO $ peekCAString =<< peek options_arch


foreign import ccall "&jhc_options_os"   options_os   :: Ptr CString
foreign import ccall "&jhc_options_arch" options_arch :: Ptr CString
