module Version.Version(
    versionContext,
    versionSimple,
    versionString
    ) where


import Data.Version
import System.Info
import qualified Data.ByteString.UTF8 as BS

import Version.Config
import RawFiles

{-# NOINLINE versionSimple #-}
versionSimple = concat [package, " ", version, " (", BS.toString shortchange_txt, ")"]

{-# NOINLINE versionString #-}
versionString = concat [versionSimple, "\n", "compiled by ",compilerName,"-",showVersion compilerVersion," on a ",arch," running ",os]

{-# NOINLINE versionContext #-}
versionContext = changelog
