module FrontEnd.FrontEnd(
    parseFiles,
    makeLibrary,
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
import Ho.Build
import HsSyn
import Options
import FrontEnd.Warning
import qualified FlagDump as FD
import qualified FrontEnd.Tc.Module as Tc


makeLibrary processInitialHo processDecls hl = createLibrary hl buildLibrary where
    buildLibrary [] = do putErrLn "WARNING: building empty library" >> return mempty
    buildLibrary mods = do
        putVerboseLn $ "Building library containing: " ++ show mods
        -- TODO - remove hidden exports
        parseFiles (map Left mods) processInitialHo processDecls

-- | Main entry point to front end

parseFiles :: [Either Module String]      -- ^ List of files or modules to read
               -> (CollectedHo -> Ho -> IO CollectedHo) -- ^ Process initial data loaded from ho files
               -> (CollectedHo -> Ho -> Tc.TiData -> IO (CollectedHo,Ho))  -- ^ routine which takes the global ho, the partial local ho and the output of the front end, and returns the completed ho.
               -> IO (CollectedHo,Ho)     -- ^ (the final combined ho,all the loaded ho data)
parseFiles fs ifunc func = do
    wdump FD.Progress $ do
        putErrLn $ "Compiling " ++ show fs
    res <- findModule fs ifunc (doModules func)
    processIOErrors
    return res

-- Process modules found by Ho
doModules :: (CollectedHo -> Ho -> Tc.TiData -> IO (CollectedHo,Ho)) -> CollectedHo -> [HsModule] -> IO (CollectedHo,Ho)
doModules func ho ms  = do
    ms <- mapM modInfo ms
    when (dump FD.Defs) $ flip mapM_ ms $ \m -> do
         putStrLn $ " ---- Definitions for" <+> show (modInfoName m) <+> "----";
         mapM_ print ( modInfoDefs m)
    ms <- determineExports [ (x,y,z) | (x,(y,z)) <- Map.toList $ hoDefs $ choHo ho] (Map.toList $ hoExports $ choHo ho) ms
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


