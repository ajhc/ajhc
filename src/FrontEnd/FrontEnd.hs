module FrontEnd.FrontEnd(
    doModules,
    Tc.TiData(..)
    ) where

import Control.Monad
import qualified Data.Map as Map

import Doc.DocLike
import FrontEnd.Exports
import FrontEnd.HsSyn
import FrontEnd.Rename
import Ho.Type
import Options
import qualified FlagDump as FD
import qualified FrontEnd.Tc.Module as Tc

-- Process modules found by Ho
doModules :: HoTcInfo -> [HsModule] -> IO  (HoTcInfo,Tc.TiData)
doModules htc ms  = do
    ms <- mapM modInfo ms
    when (dump FD.Defs) $ flip mapM_ ms $ \m -> do
         putStrLn $ " ---- Definitions for" <+> show (modInfoName m) <+> "----"
         mapM_ print (modInfoDefs m)
    ms <- determineExports [ (x,y,z) |
        (x,(y,z)) <- Map.toList $ hoDefs htc] (Map.toList $ hoExports htc) ms
    Tc.tiModules htc ms

modInfo m = do
    let (xs,ys) = collectDefsHsModule m
    return ModInfo {
        modInfoName = hsModuleName m,
        modInfoDefs = xs,
        modInfoHsModule = m,
        modInfoConsArity = ys,
        modInfoExport = error "modInfoExport",
        modInfoImport = error "modInfoImport",
        modInfoReverseMap = error "modInfoReverseMap",
        modInfoOptions = hsModuleOpt m
        }
