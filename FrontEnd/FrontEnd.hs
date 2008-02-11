module FrontEnd.FrontEnd(
    parseFiles,
    makeLibrary,
    Tc.TiData(..)
    ) where

import Monad
import qualified Data.Map as Map

import Doc.DocLike
import FrontEnd.Exports
import FrontEnd.Rename
import FrontEnd.SrcLoc
import GenUtil
import Ho.Build
import HsSyn
import Options
import FrontEnd.Warning
import qualified FlagDump as FD
import qualified FrontEnd.Tc.Module as Tc


makeLibrary ifunc func hl = do buildLibrary ifunc (doModules func) hl

-- | Main entry point to front end

parseFiles :: [Either Module String]      -- ^ List of files or modules to read
               -> (CollectedHo -> Ho -> IO CollectedHo) -- ^ Process initial data loaded from ho files
               -> (CollectedHo -> Ho -> Tc.TiData -> IO (CollectedHo,Ho))  -- ^ routine which takes the global ho, the partial local ho and the output of the front end, and returns the completed ho.
               -> IO CollectedHo          -- ^ (the final combined ho,all the loaded ho data)
parseFiles fs ifunc func = do
    wdump FD.Progress $ do
        putErrLn $ "Compiling " ++ show fs
    (res,_,_) <- findModule fs ifunc (doModules func)
    return res

-- Process modules found by Ho
doModules :: (CollectedHo -> Ho -> Tc.TiData -> IO (CollectedHo,Ho)) -> CollectedHo -> [HsModule] -> IO (CollectedHo,Ho)
doModules func ho ms  = do
    ms <- mapM modInfo ms
    when (dump FD.Defs) $ flip mapM_ ms $ \m -> do
         putStrLn $ " ---- Definitions for" <+> show (modInfoName m) <+> "----";
         mapM_ print ( modInfoDefs m)
    ms <- determineExports [ (x,y,z) | (x,(y,z)) <- Map.toList $ hoDefs $ hoExp $ choHo ho] (Map.toList $ hoExports $ hoExp $ choHo ho) ms
    (ho',tiData) <- Tc.tiModules' ho ms
    func ho ho' tiData

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


