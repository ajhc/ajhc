{-# OPTIONS_JHC -fm4 -fffi #-}
module System.Info(compilerName,compilerVersion,os,arch) where

import Foreign.C.String
import Foreign
import Data.Version ( Version(..) )

compilerName = "jhc"

compilerVersion :: Version
compilerVersion = Version (splitup "__JHC_VERSION__") []
    where splitup s = case reads s of
                        [(v,s')] -> v : splitup s'
                        _ -> []

os = unsafePerformIO $ peekCAString =<< peek options_os
arch = unsafePerformIO $ peekCAString =<< peek options_arch

foreign import ccall "&jhc_options_os"   options_os   :: Ptr CString
foreign import ccall "&jhc_options_arch" options_arch :: Ptr CString
