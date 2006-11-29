module FrontEnd.FrontEnd(
    parseFiles,
    Tc.TiData(..)
    ) where

import Monad
import Data.Monoid
import qualified Data.Map as Map
import qualified Text.PrettyPrint.HughesPJ as PPrint

import Doc.DocLike
import Doc.PPrint
import FrontEnd.Exports
import FrontEnd.Rename
import FrontEnd.SrcLoc
import GenUtil
import Ho.Build
import Ho.Library(loadLibraries)
import HsSyn
import Options
import Util.SetLike
import Warning
import qualified Doc.PPrint as PPrint
import qualified FlagDump as FD
import qualified FlagOpts as FO
import qualified FrontEnd.Tc.Module as Tc



-- | Main entry point to front end

parseFiles :: [String]      -- ^ List of files to read
               -> [Module]  -- ^ List of modules to find
               -> (CollectedHo -> Ho -> IO CollectedHo) -- ^ Process initial data loaded from ho files
               -> (CollectedHo -> Ho -> Tc.TiData -> IO (CollectedHo,Ho))  -- ^ routine which takes the global ho, the partial local ho and the output of the front end, and returns the completed ho.
               -> IO (CollectedHo,Ho)     -- ^ (the final combined ho,all the loaded ho data)
parseFiles fs deps ifunc func = do
    wdump FD.Progress $ do
        putErrLn $ "Compiling " ++ show fs
    libraries <- loadLibraries
    initialHo <- ifunc mempty (initialHo `mappend` libraries)
    let xs = snub $ map Right fs ++ map Left deps
        f accumHo ho [] = return (accumHo,ho)
        f accumHo ho (x:xs) = do
            (accumHo,ho') <- findModule accumHo x ifunc (doModules func)
            f accumHo (ho `mappend` ho') xs
    (initialHo,ho) <- f initialHo mempty  xs
    processIOErrors
    return (initialHo,ho)

-- Process modules found by Ho
doModules :: (CollectedHo -> Ho -> Tc.TiData -> IO (CollectedHo,Ho)) -> CollectedHo -> [HsModule] -> IO (CollectedHo,Ho)
doModules func ho ms  = do
    ms <- mapM modInfo ms
    --putErrLn $ show (hoExports ho)
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


