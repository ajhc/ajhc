{-# OPTIONS_JHC -N -fffi -fcpp -funboxed-values #-}
{-# LANGUAGE CPP #-}

module Jhc.Options(
#ifdef __JHC__
    isWindows,
    isPosix,
    target,
    isBigEndian,
    isLittleEndian,
#endif
    Target(..)
    ) where

import Jhc.Order
import Jhc.Enum
import Jhc.Prim
import Jhc.Types
import Jhc.Basics

data Target = Grin | GhcHs | DotNet | Java
    deriving(Eq,Ord,Enum)




#ifdef __JHC__

isBigEndian,isLittleEndian :: Bool
isLittleEndian = not isBigEndian

foreign import primitive "box" boxTarget :: Enum__ -> Target
foreign import primitive "box" boxBool   :: Enum__ -> Bool


target = boxTarget    (options_target      ())
isWindows = boxBool   (options_isWindows   ())
isPosix = boxBool     (options_isPosix     ())
isBigEndian = boxBool (options_isBigEndian ())

foreign import primitive options_target      :: () -> Enum__
foreign import primitive options_isWindows   :: () -> Bool__
foreign import primitive options_isPosix     :: () -> Bool__
foreign import primitive options_isBigEndian :: () -> Bool__

#endif

