module FrontEnd.Tc.Module (tiModules,TiData(..)) where

import Control.Monad.Writer
import Data.Char
import Text.PrettyPrint.HughesPJ as PPrint
import qualified Data.Foldable as T
import qualified Data.Map as Map
import qualified Data.Set as Set

import DataConstructors
import DerivingDrift.Drift
import Doc.PPrint as PPrint
import FrontEnd.Class
import FrontEnd.DataConsAssump     (dataConsEnv)
import FrontEnd.DeclsDepends       (getDeclDeps, debugDeclBindGroups)
import FrontEnd.DependAnalysis     (getBindGroups)
import FrontEnd.Desugar
import FrontEnd.Exports
import FrontEnd.HsSyn
import FrontEnd.Infix
import FrontEnd.KindInfer
import FrontEnd.Rename
import FrontEnd.Tc.Main
import FrontEnd.Tc.Monad
import FrontEnd.Tc.Type
import FrontEnd.TypeSigs           (collectSigs, listSigsToSigEnv)
import FrontEnd.TypeSynonyms
import FrontEnd.TypeSyns
import FrontEnd.Utils
import FrontEnd.Warning
import Ho.Type
import Info.Types
import Name.Name as Name
import Name.Names
import Options
import Util.Gen
import Util.Inst()
import Util.SetLike
import qualified FlagDump as FD
import qualified FrontEnd.HsPretty as HsPretty

trimEnv env = Map.filterWithKey (\k _ -> isGlobal k) env

getDeclNames ::  HsDecl -> [Name]
getDeclNames (HsTypeSig _ ns _ ) =  map (toName Val) ns
getDeclNames d = maybeGetDeclName d

-- Extra data produced by the front end, used to fill in the Ho file.
data TiData = TiData {
    tiDataDecls      :: [HsDecl],
    tiDataModules    :: [(Module,HsModule)],
    tiModuleOptions  :: [(Module,Opt)],
    tiCheckedRules   :: [Rule],
    tiCoerce         :: Map.Map Name CoerceTerm,
    tiProps          :: Map.Map Name Properties,
    tiAllAssumptions :: Map.Map Name Type
}

isGlobal x |  (_,(_::String,(h:_))) <- fromName x =  not $ isDigit h
isGlobal _ = error "isGlobal"

buildFieldMap :: [ModInfo] -> FieldMap
buildFieldMap ms = FieldMap ans' ans where
        allDefs = [ (x,z) | (x,_,z) <- concat $ map modInfoDefs ms, nameType x == DataConstructor ]
        ans = Map.fromList $ sortGroupUnderFG fst snd $ concat [ [ (y,(x,i)) |  y <- ys | i <- [0..] ]  | (x,ys) <-  allDefs ]
        ans' = Map.fromList $ concatMap modInfoConsArity ms

{-
buildFieldMap :: Ho -> [ModInfo] -> FieldMap
buildFieldMap ho ms = (ans',ans) where
        theDefs = [ (x,z) | (x,_,z) <- concat $ map modInfoDefs ms, nameType x == DataConstructor ]
        allDefs = theDefs ++ [ (x,z) | (x,(_,z)) <- Map.toList (hoDefs $ hoExp ho), nameType x == DataConstructor ]
        ans = Map.fromList $ sortGroupUnderFG fst snd $ concat [ [ (y,(x,i)) |  y <- ys | i <- [0..] ]  | (x,ys) <-  allDefs ]
        ans' = Map.fromList $ concatMap modInfoConsArity ms ++ getConstructorArities (hoDataTable $ hoBuild ho)
-}

processModule :: FieldMap -> ModInfo -> IO (ModInfo,[Warning])
processModule defs m = do
    when (dump FD.Parsed) $ do
        putStrLn " \n ---- parsed code ---- \n";
        putStrLn $ HsPretty.render
            $ HsPretty.ppHsModule
                $ modInfoHsModule m
    -- driftDerive only uses IO to print the derived instances.
    zmod' <-  driftDerive (modInfoHsModule m)
    let mod = desugarHsModule (zmod')
    let (mod',errs) = runWriter $ renameModule (modInfoOptions m) defs (modInfoImport m)  mod
    when (dump FD.Renamed) $ do
        putStrLn " \n ---- renamed code ---- \n"
        putStrLn $ HsPretty.render $ HsPretty.ppHsModule $  mod'
    return $ (modInfoHsModule_s mod' m,errs)

-- type check a set of mutually recursive modules.
-- assume all dependencies are met in the
-- ModEnv parameter and export lists have been calculated.

or' :: [(a -> Bool)] -> a -> Bool
or' fs x = or [ f x | f <- fs ]

-- Very broad information on a data type.
data DatDesc
    = DatEnum [Name]
    | DatMany [(Name,Int)]
    | DatNewT Name

dtDataDesc :: Monad m => DataTable -> Name -> m DatDesc
dtDataDesc dt n = maybe (fail "dtDataDesc") return $ do
    c <- getConstructor n dt
    let datEnum = conVirtual c >>= return . DatEnum
        datNewT = do ErasedAlias <- return $ conAlias c; DataNormal [n] <- return $ conChildren c; return $ DatNewT n
        datMany = do DataNormal ns <- return $ conChildren c ; return $ DatMany [ (n,-1) | n <- ns ]
    datEnum `mplus` datNewT `mplus` datMany

getDataDesc :: Monad m => HsDecl -> m (Name,DatDesc)
getDataDesc d = g d where
    g desc = do
        r <- f d
        return (hsDeclName desc,r)
    f HsNewTypeDecl { hsDeclCon = (hsConDeclName -> cn)  } = return $ DatNewT cn
    f HsDataDecl { hsDeclCons = cs }
        | all null $ map hsConDeclArgs cs = return $ DatEnum (map hsConDeclName cs)
    f HsDataDecl { hsDeclCons = cs } = return $ DatMany [ (hsConDeclName c, (length . hsConDeclArgs) c) | c <- cs]
    f _ = fail "getDataDesc: not a data declaration"

-- FIXME: Use an warnings+writer+error monad instead of IO.
tiModules ::  HoTcInfo -> [ModInfo] -> IO (HoTcInfo,TiData)
tiModules htc ms = do
--    let importVarEnv = Map.fromList [ (x,y) | (x,y) <- Map.toList $ hoAssumps me, nameType x == Name.Val ]
--        importDConsEnv = Map.fromList [ (x,y) | (x,y) <- Map.toList $ hoAssumps me, nameType x ==  Name.DataConstructor ]
    let importClassHierarchy = hoClassHierarchy htc
        importKindEnv = hoKinds htc
    --wdump FD.Progress $ do
    --    putErrLn $ "Typing: " ++ show ([ m | Module m <- map modInfoName ms])
    -- 'processModule' doesn't need IO. We can use a plain writer+error monad.
    let nfm = buildFieldMap ms `mappend` hoFieldMap htc
    mserrs <- mapM (processModule nfm) ms
    let ms = fsts mserrs
    let thisFixityMap = buildFixityMap (concat [ filter isHsInfixDecl (hsModuleDecls $ modInfoHsModule m) | m <- ms])
    let fixityMap = thisFixityMap  `mappend` hoFixities htc
    --let thisTypeSynonyms =  (declsToTypeSynonyms $ concat [ filter isHsTypeDecl (hsModuleDecls $ modInfoHsModule m) | m <- ms])
    thisTypeSynonyms <- declsToTypeSynonyms (hoTypeSynonyms htc) $ concat [ filter isHsTypeDecl (hsModuleDecls $ modInfoHsModule m) | m <- ms]
--    putStrLn "Synonyms"
--    putStrLn $ HsPretty.render $ showSynonyms pprint thisTypeSynonyms

    let ts = thisTypeSynonyms `mappend` hoTypeSynonyms htc
    -- 'expandTypeSyns' is in the Warning monad and doesn't require IO.
    let f x = expandTypeSyns ts (modInfoHsModule x) >>= return . FrontEnd.Infix.infixHsModule fixityMap >>= \z -> return (modInfoHsModule_s ( z) x)
    ms <- mapM f ms
    processIOErrors
    let ds = concat [ hsModuleDecls $ modInfoHsModule m | m <- ms ]

    wdump FD.Decls $ do
        putStrLn "  ---- processed decls ---- "
        putStrLn $ HsPretty.render (HsPretty.ppHsDecls ds)

    -- kind inference for all type constructors type variables and classes in the module
    let classAndDataDecls = filter (or' [isHsDataDecl, isHsNewTypeDecl, isHsClassDecl, isHsClassAliasDecl]) ds  -- rDataDecls ++ rNewTyDecls ++ rClassDecls
    kindInfo <- kiDecls importKindEnv classAndDataDecls

    when (dump FD.Kind) $
         do {putStrLn " \n ---- kind information ---- \n";
             putStrLn $ PPrint.render $ pprint kindInfo}

    -- collect types for data constructors

    let localDConsEnv =  dataConsEnv (error "modName") kindInfo classAndDataDecls -- (rDataDecls ++ rNewTyDecls)

    wdump FD.Dcons $ do
        putStr "\n ---- data constructor assumptions ---- \n"
        mapM_ putStrLn [ show n ++  " :: " ++ prettyPrintType s |  (n,s) <- Map.toList localDConsEnv]

    --let globalDConsEnv = localDConsEnv `Map.union` importDConsEnv

    let smallClassHierarchy = foldr addInstanceToHierarchy (makeClassHierarchy importClassHierarchy kindInfo ds) dinsts
        cHierarchyWithInstances = scatterAliasInstances $ smallClassHierarchy `mappend` importClassHierarchy
        derivingClauses = collectDeriving ds
        dataInfo = Map.fromList $ concatMap getDataDesc ds
        --dataTable = hoDataTable htc
        dinsts = concatMap g derivingClauses where
            g r@(_,c,t) | c `elem` enumDerivableClasses, Just (DatEnum (_:_:_)) <- Map.lookup t dataInfo = [f r]
                        | c `elem` enumDerivableClasses, t `elem` [tc_Bool, tc_Ordering, tc_IOErrorType, tc_IOMode] = [f r]
            --g r@(_,c,t) | c `notElem` noNewtypeDerivable, Just (DatMany True [_]) <- Map.lookup t dataInfo = [f r]
            g _ = []
            f (sl,c,t) = emptyInstance { instSrcLoc = sl, instDerived = True, instHead = [] :=> IsIn c (TCon (Tycon t kindStar)) }

    when (dump FD.ClassSummary) $ do
        putStrLn "  ---- class summary ---- "
        printClassSummary cHierarchyWithInstances

    when (dump FD.Class) $
         do {putStrLn "  ---- class hierarchy ---- ";
             printClassHierarchy smallClassHierarchy}

    -- lift the instance methods up to top-level decls
    let cDefBinds = concat [ [ z | z <- ds] | HsClassDecl _ _ ds <- ds]
    let myClassAssumps = concat  [ classAssumps as | as <- classRecords cHierarchyWithInstances ]
        instanceEnv   = Map.fromList instAssumps
        classDefs = snub (concatMap getDeclNames cDefBinds)
        classEnv  = Map.fromList $ [ (x,y) | (x,y) <- myClassAssumps, x `elem` classDefs  ]
        (liftedInstances,instAssumps) =  mconcatMap (instanceToTopDecls kindInfo cHierarchyWithInstances) ds -- rInstDecls

    when (not (null liftedInstances) && (dump FD.Instance) ) $ do
        putStrLn "  ---- lifted instance declarations ---- "
        putStr $ unlines $ map (HsPretty.render . HsPretty.ppHsDecl) liftedInstances
        putStrLn $ PPrint.render $ pprintEnvMap instanceEnv

    let funPatBinds =  [ d | d <- ds, or' [isHsFunBind, isHsPatBind, isHsForeignDecl, isHsActionDecl] d]
    let rTySigs =  [ d | d <- ds, or' [isHsTypeSig] d]

    -- build an environment of assumptions for all the type signatures
    let allTypeSigs = collectSigs (funPatBinds ++ liftedInstances) ++ rTySigs

    when (dump FD.Srcsigs) $
         do {putStrLn " ---- type signatures from source code (after renaming) ---- ";
             putStr $ unlines $ map (HsPretty.render . HsPretty.ppHsDecl) allTypeSigs}

    let sigEnv = Map.unions [listSigsToSigEnv kindInfo allTypeSigs,instanceEnv, classEnv]
    when (dump FD.Sigenv) $
         do {putStrLn "  ---- initial sigEnv information ---- ";
             putStrLn $ PPrint.render $ pprintEnvMap sigEnv}
    let bindings = (funPatBinds ++  liftedInstances)
        --classDefaults  = snub [ getDeclName z | z <- cDefBinds, isHsFunBind z || isHsPatBind z ]
        classNoDefaults = snub (concat [ getDeclNames z | z <- cDefBinds ]) -- List.\\ classDefaults
        noDefaultSigs = Map.fromList [ (n,maybe (error $ "sigEnv:"  ++ show n) id $ Map.lookup n sigEnv) | n <- classNoDefaults ]
    --when verbose2 $ putStrLn (show bindings)
    let programBgs = getBindGroups bindings (nameName . getDeclName) getDeclDeps

    when (dump FD.Bindgroups) $
         do {putStrLn " \n ---- toplevel variable binding groups ---- ";
             putStrLn " ---- Bindgroup # = [members] [vars depended on] [missing vars] ---- \n";
             putStr $ debugDeclBindGroups programBgs}

    let program = makeProgram sigEnv programBgs
    when (dump FD.Program) $ do
        putStrLn " ---- Program ---- "
        mapM_ putStrLn $ map (PPrint.render . PPrint.pprint) $  program

    when (dump FD.AllTypes) $ do
        putStrLn "  ---- all types ---- "
        putStrLn $ PPrint.render $ pprintEnvMap (sigEnv `mappend` localDConsEnv `mappend` hoAssumps htc)

    let moduleName = modInfoName tms
        (tms:_) = ms
    let tcInfo = tcInfoEmpty {
        tcInfoEnv = hoAssumps htc `mappend` localDConsEnv, -- (importVarEnv `mappend` globalDConsEnv),
        tcInfoSigEnv = sigEnv,
        tcInfoModName = moduleName,
        tcInfoKindInfo = kindInfo,
        tcInfoClassHierarchy = cHierarchyWithInstances
        }

    processErrors (concatMap snd mserrs)
    (localVarEnv,checkedRules,coercions,tcDs) <- withOptionsT (modInfoOptions tms) $ runTc tcInfo $ do
        --mapM_ addWarning (concatMap snd mserrs)
        (tcDs,out) <- listen (tiProgram program ds)
        env <- getCollectedEnv
        cc <- getCollectedCoerce
        let cc' = Map.union cc $ Map.fromList [ (as,lup v) | (as,v) <- outKnots out ]
            lup v = case Map.lookup v cc of
                Just (CTAbs xs) -> ctAp (map TVar xs)
                _ -> ctId
        return (env,T.toList $ checkedRules out,cc',tcDs)

    when (dump FD.Decls) $ do
        putStrLn " \n ---- typechecked code ---- \n"
        mapM_ (putStrLn . HsPretty.render . HsPretty.ppHsDecl) tcDs

    when (dump FD.Types) $ do
        putStrLn " ---- the types of identifiers ---- "
        mapM_ putStrLn [ show n ++  " :: " ++ prettyPrintType s |  (n,s) <- Map.toList (if verbose2 then localVarEnv else trimEnv localVarEnv)]
    when (dump FD.Types) $ do
        putStrLn " ---- the coersions of identifiers ---- "
        mapM_ putStrLn [ show n ++  " --> " ++ show s |  (n,s) <- Map.toList coercions]

    localVarEnv <- return $  localVarEnv `Map.union` noDefaultSigs

    let pragmaProps = fromList $ Map.toList $ Map.fromListWith mappend [ (toName Name.Val x,fromList $ readProp w) |  HsPragmaProps _ w xs <- ds, x <- xs ]

    let allAssumps = localDConsEnv `Map.union` localVarEnv
        allExports = Set.fromList (concatMap modInfoExport ms)
        externalKindEnv = restrictKindEnv (\ x  -> isGlobal x && (getModule x `elem` map (Just . modInfoName) ms)) kindInfo
    let hoEx = HoTcInfo {
            hoExports = Map.fromList [ (modInfoName m,modInfoExport m) | m <- ms ],
            hoDefs =  Map.fromList [ (x,(y,filter (`member` allExports) z)) | (x,y,z) <- concat $ map modInfoDefs ms, x `member` allExports],
            hoAssumps = Map.filterWithKey (\k _ -> k `member` allExports) allAssumps,
            hoFixities = restrictFixityMap (`member` allExports) thisFixityMap,
            -- TODO - this contains unexported names, we should filter these before writing to disk.
            --hoKinds = restrictKindEnv (`member` allExports) kindInfo,
            hoKinds = externalKindEnv,
            hoClassHierarchy = smallClassHierarchy,
            hoFieldMap = buildFieldMap ms,
            hoTypeSynonyms = restrictTypeSynonyms (`member` allExports) thisTypeSynonyms
        }
        tiData = TiData {
            tiDataDecls = tcDs ++ filter isHsClassDecl ds,
            tiDataModules = [ (modInfoName m, modInfoHsModule m) |  m <- ms],
            tiModuleOptions = [ (modInfoName m, modInfoOptions m) |  m <- ms],
            tiCheckedRules = checkedRules,
            tiCoerce       = coercions,
            tiProps        = pragmaProps,
            tiAllAssumptions = allAssumps
        }
    return (hoEx,tiData)
