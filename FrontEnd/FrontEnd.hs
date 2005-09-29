module FrontEnd.FrontEnd(
    parseFiles,
    TiData(..)
    ) where

import Doc.DocLike
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
import Utils
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
             putStr $ PPrint.render $ pprintEnvMap (hoKinds ho)}
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
        Right o -> return o
        Left s -> warn (srcLoc m) "unknown-option" ("Unknown OPTIONS in pragma module" <+> fromModule (hsModuleName m) <+>  s) >> return options
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





--modInfoDeps m = snub $ map hsImportDeclModule $ modInfoModImports m


{-
doTime str action = do
    start <- getCPUTime
    x <- action
    end <- getCPUTime
    putStrLn $ "Timing: " ++ str ++ " " ++ show ((end - start) `div` cpuTimePrecision)
    return x

parseHsSource :: String -> String -> IO HsModule
--parseHsSource fn s = case parse s' (SrcLoc fn 1 1) 0 [] of
parseHsSource fn s = case runParserWithMode ParseMode { parseFilename = fn } parse  s'  of
                      ParseOk e -> return e
                      ParseFailed sl err -> putErrDie $ show sl ++ err
    where
    s' = if "shl." `isPrefixOf` reverse fn  then unlit fn s else s
                      -- warnF fn "parse-error" err >> return emptyHsModule

satisfyDeps :: [String] -> [String] -> IO [HsModule]
satisfyDeps have [] = return []
satisfyDeps have (n:ns) | n `elem` have = satisfyDeps have ns
satisfyDeps have (n:ns) = do
    let fns n = concatMap (\i -> [i ++ "/" ++ n ++ ".hs",i ++ "/" ++ n ++ ".lhs"]) (optIncdirs options)
    (fn,fc) <- catch (msum (map (\n -> CharIO.readFile n >>= return . (,) n) (fns n))) (\_ -> putErrDie ("Module not found: " ++ n))
    wdump FD.Progress $ do
        putErrLn $ "Found dependency:" <+> n <+> "at" <+> fn
    hm <- parseHsSource fn fc
    rm <- satisfyDeps (n:have) (ns ++ hsModuleRequires hm)
    return (hm:rm)

readFiles :: [String] -> IO [HsModule]
readFiles fs = do
    ss <- fmap (zip fs) $ mapM CharIO.readFile fs
    mapM (uncurry parseHsSource) ss

-}


{-
parseFiles :: [String] -> [String] -> IO ModEnv
parseFiles fs deps = do
    wdump FD.Progress $ do
        putErrLn $ "Compiling " ++ show fs
    ms <- readFiles fs
    let mh = [(fromModule (hsModuleName hsm)) | hsm <- ms ]
        mn = concat [ hsModuleRequires x | x <- ms ]
    ms' <- satisfyDeps mh (mn ++ deps)
    ms <- return $ ms ++ ms'
    ms <- mapM modInfo ms
    --wdump FD.Progress $ do
    --    putErrLn $ "Determining exports and imports"
    --mis <- determineExports ms -- (map modInfo ms)
    --processIOErrors
--    let me = M.fromList [( (modInfoName m), m) | m <- mis ]
--        --ps = [ (fromModule (hsModuleName hsm), (if optPrelude options then ("Prelude":) else id) [fromModule (hsImportDeclModule i) | i <- hsModuleImports hsm] ) | hsm <-  ms]
--        ps = [ (modInfoName m, modInfoDeps m)  | m <- mis ]
--        nodes   = map fst ps
--        targets = concat (map snd ps)
--    unless (all (`elem` nodes) targets) $
--        putErrDie $ "Modules not found!\n" ++ show ps
--    let ps' = Scc.scc ps
--        ps'' = map (map (me M.!)) ps'
    let mss' = stronglyConnComp [ (m,toAtom (modInfoName m), map toAtom (modInfoDeps m))  | m <- ms]
        mss = map f mss'
        f (AcyclicSCC x) = [x]
        f (CyclicSCC xs) = xs
    when (dump FD.SccModules) $ putStrLn $ "scc modules:\n" ++ unlines (map  (show . map (fromModule . modInfoName) ) mss)
    mss <- doExports [] mss []
    me <- foldM tiModules emptyModEnv mss
    when (dump FD.AllKind) $
         do {putStrLn " ---- kind information ---- \n";
             putStr $ PPrint.render $ pprintEnvMap (modEnvKinds me)}
    when  (dump FD.AllDcons) $
         do {putStr " ---- data constructor assumptions ---- \n";
             putStrLn $ PPrint.render $ pprintEnv (modEnvDConsAssumptions me)}
    processIOErrors
    return me
-}


{-

type Entity = Name
--exports ModInfo { modInfoHsModule = m@HsModule { hsModuleExports = Nothing } } _ =
--        case namesHsModule m of { (xs,ts) -> R.fromList $ [ ((False,n),n) | (n,_) <- xs] ++ [ ((True,n),n) | (n,_) <- ts];   }
exports :: ModInfo -> Rel (Name) Entity -> Rel (Name) Entity
exports mi@ModInfo { modInfoHsModule = m@HsModule { hsModuleExports = Nothing } } _ = defsToRel $ modInfoDefs mi
exports mi is | HsModule { hsModuleExports = Just es } <- modInfoHsModule mi = mapDomain h (R.unions $ map f es) where
    f (HsEModuleContents m) = mapDomain g unqs `R.intersection` qs where
        (qs,unqs) = partitionDomain (isJust . getModule ) is
        --g (x,UnQual i) = (x,Qual m i)
        g x = Name.qualifyName m x
    f z = entSpec False is z
    h n = toUnqualified n

imports :: ModInfo -> (Module -> Rel (Name) Entity) -> Rel (Name) Entity -> Rel (Name) Entity
imports mi em rel = mconcatMap f is where
    f x = rel `mappend` z where
        z = (mapDomain (\n -> (Name.qualifyName as n)) es `mappend` if hsImportDeclQualified x then mempty else es)
        Just as = hsImportDeclAs x `mplus` Just (hsImportDeclModule x)
        es = em (hsImportDeclModule x)

    is = modInfoModImports mi
    --is' = hsModuleImports $ modInfoHsModule mi
    --is = is' ++ if any ( (== Module "Prelude") . hsImportDeclModule) is' then [] else [prelude]
    --prelude = HsImportDecl { hsImportDeclSrcLoc = bogusASrcLoc, hsImportDeclModule = Module "Prelude", hsImportDeclSpec = Nothing, hsImportDeclAs = Nothing, hsImportDeclQualified = False }

--mEntSpec isHiding rel es
 determineExports ::  MonadWarn m => (Map.Map Module  (Rel Name Name) ) -> [ModInfo] -> m [ModInfo]
determineExports soFar mi = mapM g [ (m,i,o) | (i,o) <- lfp start | m <- mi] where
    start = [(h m,mempty) |  m <- mi]
    f xs = [ (imports m mp i, o `mappend` exports m i) | (m,i,o) <- z] where
        z = [ (m,i,o) | m <- mi | (i,o) <- xs]
        mp :: Module -> Rel Name Entity
        mp m = case M.lookup m (soFar `mappend` M.fromList [ (modInfoName m,o)  | (m,_,o) <- z ]) of
            Nothing -> error $ "Could not find Module Exports for: " ++ show m
            Just x -> x
    lfp x = let fx = f x in if fx == x then fx else lfp fx
    g (m,i,o) = ce m o >>= \o' -> ci i >>= \i' -> return m { modInfoExport = o', modInfoImport = i' }
    h m = R.fromList $  concat [ [(toUnqualified z,z),(z,z)]| (z, _, _) <- modInfoDefs m]
    ce m x = mapM f (toRelationList x) where
        f (x,[y]) = return y
        f (_,[]) = error "can't happen"
        f (x,ys) = warn bogusASrcLoc "ambiguous-export" ("module " <> fromModule (modInfoName m) <> " has ambiguous exports: " ++ show ys) >> return (head ys)
    ci x = mapM f (toRelationList x) where
        f (x,[]) = error "can't happen"
        f (x,ys) = return (x,ys)

hsModuleRequires x = (if optPrelude options then ("Prelude":) else id) [ fromModule $ hsImportDeclModule y | y <- hsModuleImports x]

    {-
parseFile verb mi fn = do
    src <- readFile fn
    moduleSyntax <- parseHsSource fn src
    x <- tiModule (if verb then ["all"] else []) moduleSyntax mi
    return $ x `joinModuleInfo` mi
-}
-}
