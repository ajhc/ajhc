module Version(
    versionContext,
    versionString
    ) where


import Data.Version
import System.Info

import Version.Ctx
import Version.Raw

{-# NOINLINE versionString #-}
versionString = concat ["jhc ", jhcVersion, " ", compileDate, " (", darcsTag, "+",darcsPatches, ")\n",
                        "compiled by ",compilerName,"-",showVersion compilerVersion," on a ",arch," running ",os]

{-# NOINLINE versionContext #-}
versionContext = changes_txt
