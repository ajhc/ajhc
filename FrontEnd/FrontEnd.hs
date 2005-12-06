module FrontEnd.FrontEnd(
    parseFiles,
    TiData(..)
    ) where

import Doc.DocLike
import Doc.PPrint
import FrontEnd.Exports
import FrontEnd.Rename
import GenUtil
import Ho
import HsSyn
import Monad
import MultiModuleBasics
import Options
import qualified Data.Map as Map
import qualified Doc.PPrint as PPrint
import qualified FlagDump as FD
import qualified Text.PrettyPrint.HughesPJ as PPrint
import TIModule
import Warning



-- | Main entry point to front end

parseFiles :: [String]      -- ^ List of files to read
               -> [Module]  -- ^ List of modules to find
               -> (Ho -> IO Ho) -- ^ Process initial data loaded from ho files
               -> (Ho -> Ho -> TiData -> IO Ho)  -- ^ routine which takes the global ho, the partial local ho and the output of the front end, and returns the completed ho.
               -> IO Ho     -- ^ the final combined ho.
parseFiles fs deps ifunc func = do
    wdump FD.Progress $ do
        putErrLn $ "Compiling " ++ show fs
    let xs = snub $ map Right fs ++ map Left deps
        f ho [] = return ho
        f ho (x:xs) = do
            ho' <- findModule ho x ifunc (doModules func)
            f ho' xs
    initialHo <- loadLibraries
    ho <- f initialHo xs
    processIOErrors
    when (dump FD.AllKind) $
         do {putStrLn " ---- kind information ---- \n";
             putStr $ PPrint.render $ pprint (hoKinds ho)}
    --when  (dump FD.AllDcons) $
    --    do {putStr " ---- data constructor assumptions ---- \n";
    --         putStrLn $ PPrint.render $ pprintEnv (hoDConsAssumptions ho)}
    return ho

-- Process modules found by Ho
doModules :: (Ho -> Ho -> TiData -> IO Ho) -> Ho -> [HsModule] -> IO Ho
doModules func ho ms  = do
    ms <- mapM modInfo ms
    --putErrLn $ show (hoExports ho)
    when (dump FD.Defs) $ flip mapM_ ms $ \m -> do
         putStrLn $ " ---- Definitions for" <+> show (modInfoName m) <+> "----";
         mapM_ print ( modInfoDefs m)
    ms <- determineExports [ (x,y,z) | (x,(y,z)) <- Map.toList $ hoDefs ho] (Map.toList $ hoExports ho) ms
    (ho',tiData) <- tiModules' ho ms
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


