{-# OPTIONS_JHC -fno-prelude -fffi -fcpp -funboxed-values #-}
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

import Jhc.Basics
import Jhc.Enum
import Jhc.Order
import Jhc.Prim.Options

data Target = Grin | GhcHs | DotNet | Java
    deriving(Eq,Ord,Enum)

#ifdef __JHC__

foreign import primitive "box" boxTarget :: Enum__ -> Target

target      = boxTarget (options_target      ())

foreign import primitive options_target      :: () -> Enum__

#endif
