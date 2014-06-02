module Jhc.Prim.Options(
    isWindows,
    isPosix,
    isBigEndian,
    isLittleEndian,
    targetIsC,
    targetIsDotNet,
    targetIsJava
    ) where

import Jhc.Prim.Prim

isBigEndian,isLittleEndian :: Bool
isLittleEndian = case isBigEndian  of
    False -> True
    True -> False

foreign import primitive "const.JHC_targetIsC" targetIsC           :: Bool
foreign import primitive "const.JHC_targetIsJava" targetIsJava     :: Bool
foreign import primitive "const.JHC_targetIsDotNet" targetIsDotNet :: Bool
foreign import primitive "const.JHC_isWindows" isWindows           :: Bool
foreign import primitive "const.JHC_isPosix" isPosix               :: Bool
foreign import primitive "const.JHC_isBigEndian" isBigEndian       :: Bool
foreign import primitive "const.JHC_isConsole" isConsole           :: Bool
