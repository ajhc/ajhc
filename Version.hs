module Version(
    versionContext,
    versionSimple,
    versionString
    ) where


import Data.Version
import System.Info

import Version.Ctx
import Version.Raw

{-# NOINLINE versionSimple #-}
versionSimple = concat ["jhc ", jhcVersion, " ", compileDate, " (", darcsTag, "+",darcsPatches, ")"]

{-# NOINLINE versionString #-}
versionString = concat [versionSimple, "\n", "compiled by ",compilerName,"-",showVersion compilerVersion," on a ",arch," running ",os]

{-# NOINLINE versionContext #-}
versionContext = changes_txt
