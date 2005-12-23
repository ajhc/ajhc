module Version
    (module Version.Ctx,
     module Version.Raw,
     versionString) where

import Data.Version
import System.Info

import Version.Ctx
import Version.Raw

versionString = concat ["jhc ", jhcVersion, " ", compileDate, " (", darcsTag, "+",darcsPatches, ")\n",
                        "compiled by ",compilerName,"-",showVersion compilerVersion," on a ",arch," running ",os]
