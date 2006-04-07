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
import qualified Doc.PPrint as PPrint
import qualified FlagDump as FD
import qualified FlagOpts as FO
import qualified FrontEnd.Tc.Module as Tc
import Warning



-- | Main entry point to front end

parseFiles :: [String]      -- ^ List of files to read
               -> [Module]  -- ^ List of modules to find
               -> (Ho -> IO Ho) -- ^ Process initial data loaded from ho files
               -> (Ho -> Ho -> Tc.TiData -> IO Ho)  -- ^ routine which takes the global ho, the partial local ho and the output of the front end, and returns the completed ho.
               -> IO (Ho,Ho)     -- ^ (the libraries and predifiend ho,the final combined ho of loaded code)
parseFiles fs deps ifunc func = do
    wdump FD.Progress $ do
        putErrLn $ "Compiling " ++ show fs
    initialHo <- loadLibraries
    initialHo <- ifunc initialHo
    let xs = snub $ map Right fs ++ map Left deps
        f ho [] = return ho
        f ho (x:xs) = do
            ho' <- findModule initialHo ho x ifunc (doModules func)
            f ho' xs
    ho <- f mempty  xs
    processIOErrors
    return (initialHo,ho)

-- Process modules found by Ho
doModules :: (Ho -> Ho -> Tc.TiData -> IO Ho) -> Ho -> [HsModule] -> IO Ho
doModules func ho ms  = do
    ms <- mapM modInfo ms
    --putErrLn $ show (hoExports ho)
    when (dump FD.Defs) $ flip mapM_ ms $ \m -> do
         putStrLn $ " ---- Definitions for" <+> show (modInfoName m) <+> "----";
         mapM_ print ( modInfoDefs m)
    ms <- determineExports [ (x,y,z) | (x,(y,z)) <- Map.toList $ hoDefs ho] (Map.toList $ hoExports ho) ms
    (ho',tiData) <- Tc.tiModules' ho ms
    ho'' <- func ho ho' tiData
    return ho''
    --me <- foldM tiModules emptyModEnv mss

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


