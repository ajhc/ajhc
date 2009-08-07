module FrontEnd.FrontEnd(
--    parseFiles,
    makeLibrary,
    doModules',
    Tc.TiData(..)
    ) where

import Monad
import Data.Monoid
import qualified Data.Map as Map

import Doc.DocLike
import FrontEnd.Exports
import FrontEnd.Rename
import FrontEnd.SrcLoc
import GenUtil
import Ho.Collected
import Ho.Type
import FrontEnd.HsSyn
import Options
import FrontEnd.Warning
import qualified FlagDump as FD
import qualified FrontEnd.Tc.Module as Tc


--makeLibrary ifunc func hl = do buildLibrary ifunc (doModules func) hl
makeLibrary ifunc func hl = undefined -- do buildLibrary ifunc (doModules func) hl

-- | Main entry point to front end

{-
parseFiles :: [Either Module String]      -- ^ List of files or modules to read
               -> (CollectedHo -> Ho -> IO CollectedHo) -- ^ Process initial data loaded from ho files
               -> (CollectedHo -> Ho -> Tc.TiData -> IO (CollectedHo,Ho))  -- ^ routine which takes the global ho, the partial local ho and the output of the front end, and returns the completed ho.
               -> IO CollectedHo          -- ^ (the final combined ho,all the loaded ho data)
parseFiles fs ifunc func = do
    wdump FD.Progress $ do
        putErrLn $ "Compiling " ++ show fs
    compileModules fs ifunc (doModules func)
-}

-- Process modules found by Ho
doModules :: (CollectedHo -> Ho -> Tc.TiData -> IO (CollectedHo,Ho)) -> CollectedHo -> [HsModule] -> IO (CollectedHo,Ho)
doModules func ho ms  = do
    ms <- mapM modInfo ms
    when (dump FD.Defs) $ flip mapM_ ms $ \m -> do
         putStrLn $ " ---- Definitions for" <+> show (modInfoName m) <+> "----";
         mapM_ print ( modInfoDefs m)
    ms <- determineExports [ (x,y,z) | (x,(y,z)) <- Map.toList $ hoDefs $ hoTcInfo $ choHo ho] (Map.toList $ hoExports $ hoTcInfo $ choHo ho) ms
    --(ho',tiData) <- Tc.tiModules' ho ms
    (htc,tiData) <- Tc.tiModules (hoTcInfo (choHo ho)) ms
    func ho mempty { hoTcInfo = htc } tiData

-- Process modules found by Ho
doModules' :: HoTcInfo -> [HsModule] -> IO  (HoTcInfo,Tc.TiData)
doModules' htc ms  = do
    ms <- mapM modInfo ms
    when (dump FD.Defs) $ flip mapM_ ms $ \m -> do
         putStrLn $ " ---- Definitions for" <+> show (modInfoName m) <+> "----";
         mapM_ print ( modInfoDefs m)
    ms <- determineExports [ (x,y,z) | (x,(y,z)) <- Map.toList $ hoDefs htc] (Map.toList $ hoExports htc) ms
    Tc.tiModules htc ms

modInfo m = do
    opt <- case fileOptions (hsModuleOptions m) of
        Just o -> return o
        Nothing -> warn (srcLoc m) "unknown-option" ("Unknown OPTIONS in pragma module" <+> fromModule (hsModuleName m) <+>  show (hsModuleOptions m)) >> return options
    let (xs,ys) = collectDefsHsModule m
    return ModInfo {
        modInfoName = hsModuleName m,
        modInfoDefs = xs,
        modInfoHsModule = m,
        modInfoConsArity = ys,
        modInfoExport = error "modInfoExport",
        modInfoImport = error "modInfoImport",
        modInfoOptions = opt
        }


