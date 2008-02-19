module FrontEnd.Tc.Module (tiModules',TiData(..)) where

import Char
import Control.Monad.Writer
import IO
import List
import Maybe
import Monad
import Text.PrettyPrint.HughesPJ as PPrint
import qualified Data.Map as Map
import qualified Data.Set as Set

import DataConsAssump     (dataConsEnv)
import DataConstructors
import DeclsDepends       (getDeclDeps, debugDeclBindGroups)
import DependAnalysis     (getBindGroups)
import DerivingDrift.Drift
import Doc.PPrint as PPrint
import FrontEnd.Class
import FrontEnd.Desugar
import FrontEnd.Exports
import FrontEnd.Infix
import FrontEnd.KindInfer
import FrontEnd.Rename
import FrontEnd.Tc.Main
import FrontEnd.Tc.Monad
import FrontEnd.Tc.Type
import FrontEnd.Utils
import FrontEnd.Warning
import Ho.Type
import Ho.Collected
import HsSyn
import Info.Types
import Name.Name as Name
import Options
import TypeSigs           (collectSigs, listSigsToSigEnv)
import TypeSynonyms
import TypeSyns
import Util.Gen
import Util.Inst()
import Util.SetLike
import qualified FlagDump as FD
import qualified HsPretty

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




buildFieldMap :: Ho -> [ModInfo] -> FieldMap
buildFieldMap ho ms = (ans',ans) where
        theDefs = [ (x,z) | (x,_,z) <- concat $ map modInfoDefs ms, nameType x == DataConstructor ]
        allDefs = theDefs ++ [ (x,z) | (x,(_,z)) <- Map.toList (hoDefs $ hoExp ho), nameType x == DataConstructor ]
        ans = Map.fromList $ sortGroupUnderFG fst snd $ concat [ [ (y,(x,i)) |  y <- ys | i <- [0..] ]  | (x,ys) <-  allDefs ]
        ans' = Map.fromList $ concatMap modInfoConsArity ms ++ getConstructorArities (hoDataTable $ hoBuild ho)


processModule :: FieldMap -> ModInfo -> IO ModInfo
processModule defs m = do
    when (dump FD.Parsed) $ do
        putStrLn " \n ---- parsed code ---- \n";
        putStrLn $ HsPretty.render
            $ HsPretty.ppHsModule
                $ modInfoHsModule m
    zmod' <-  driftDerive (modInfoHsModule m)
    let mod = desugarHsModule (zmod')
    let (mod',errs) = runWriter $ renameModule defs (modInfoImport m)  mod
    when (dump FD.Renamed) $ do
        putStrLn " \n ---- renamed code ---- \n"
        putStrLn $ HsPretty.render $ HsPretty.ppHsModule $  mod'
    processErrors errs
    return $ modInfoHsModule_s mod' m


-- type check a set of mutually recursive modules.
-- assume all dependencies are met in the
-- ModEnv parameter and export lists have been calculated.

or' :: [(a -> Bool)] -> a -> Bool
or' fs x = or [ f x | f <- fs ]

tiModules' ::  CollectedHo -> [ModInfo] -> IO (Ho,TiData)
tiModules' cho ms = do
    let me = choHo cho
        hoB = hoBuild me
        hoE = hoExp me
--    let importVarEnv = Map.fromList [ (x,y) | (x,y) <- Map.toList $ hoAssumps me, nameType x == Name.Val ]
--        importDConsEnv = Map.fromList [ (x,y) | (x,y) <- Map.toList $ hoAssumps me, nameType x ==  Name.DataConstructor ]
    let importClassHierarchy = hoClassHierarchy hoB
        importKindEnv = hoKinds hoB
    wdump FD.Progress $ do
        putErrLn $ "Typing: " ++ show ([ m | Module m <- map modInfoName ms])
    let fieldMap = buildFieldMap me ms
    ms <- mapM (processModule fieldMap) ms
    let thisFixityMap = buildFixityMap (concat [ filter isHsInfixDecl (hsModuleDecls $ modInfoHsModule m) | m <- ms])
    let fixityMap = thisFixityMap `mappend` hoFixities hoB
    let thisTypeSynonyms =  (declsToTypeSynonyms $ concat [ filter isHsTypeDecl (hsModuleDecls $ modInfoHsModule m) | m <- ms])
    let ts = thisTypeSynonyms  `mappend` hoTypeSynonyms hoB
    let f x = expandTypeSyns ts (modInfoHsModule x) >>= FrontEnd.Infix.infixHsModule fixityMap >>= \z -> return (modInfoHsModule_s ( z) x)
    ms <- mapM f ms
    processIOErrors
    let ds = concat [ hsModuleDecls $ modInfoHsModule m | m <- ms ]

    wdump FD.Decls $ do
        putStrLn "  ---- processed decls ---- "
        putStrLn $ HsPretty.render (HsPretty.ppHsDecls ds)


    -- kind inference for all type constructors type variables and classes in the module
    let classAndDataDecls = filter (or' [isHsDataDecl, isHsNewTypeDecl, isHsClassDecl]) ds  -- rDataDecls ++ rNewTyDecls ++ rClassDecls

    wdump FD.Progress $ do
        putErrLn $ "Kind inference"
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


    smallClassHierarchy <- makeClassHierarchy importClassHierarchy kindInfo ds
    cHierarchyWithInstances <- return $ smallClassHierarchy `mappend` importClassHierarchy

    when (dump FD.ClassSummary) $ do
        putStrLn "  ---- class summary ---- "
        printClassSummary cHierarchyWithInstances

    when (dump FD.Class) $
         do {putStrLn "  ---- class hierarchy ---- ";
             printClassHierarchy smallClassHierarchy}

    -- lift the instance methods up to top-level decls

    let cDefBinds = concat [ [ z | z <- ds] | HsClassDecl _ _ ds <- ds]
    let myClassAssumps = concat  [ classAssumps as | as <- (classRecords cHierarchyWithInstances)]
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

    -- type inference/checking for all variables

    when (dump FD.AllTypes) $ do
        putStrLn "  ---- all types ---- "
        putStrLn $ PPrint.render $ pprintEnvMap (sigEnv `mappend` localDConsEnv `mappend` hoAssumps hoB)

    wdump FD.Progress $ do
        putErrLn $ "Type inference"
    let moduleName = modInfoName tms
        (tms:_) = ms
    let tcInfo = tcInfoEmpty {
        tcInfoEnv = hoAssumps hoB `mappend` localDConsEnv, -- (importVarEnv `mappend` globalDConsEnv),
        tcInfoSigEnv = sigEnv,
        tcInfoModName =  show moduleName,
        tcInfoKindInfo = kindInfo,
        tcInfoClassHierarchy = cHierarchyWithInstances
        }

    (localVarEnv,checkedRules,coercions,tcDs) <- withOptionsT (modInfoOptions tms) $ runTc tcInfo $ do
        (tcDs,out) <- listen (tiProgram program ds)
        env <- getCollectedEnv
        cc <- getCollectedCoerce
        let cc' = Map.union cc $ Map.fromList [ (as,lup v) | (as,v) <- outKnots out ]
            lup v = case Map.lookup v cc of
                Just (CTAbs xs) -> ctAp (map TVar xs)
                _ -> ctId
        return (env,checkedRules out,cc',tcDs)

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
    let hoBld = mempty {
            hoAssumps = Map.filterWithKey (\k _ -> k `member` allExports) allAssumps,
            hoFixities = restrictFixityMap (`member` allExports) thisFixityMap,
            -- TODO - this contains unexported names, we should filter these before writing to disk.
            hoKinds = externalKindEnv,
            --hoKinds = restrictKindEnv (`member` allExports) kindInfo,
            hoClassHierarchy = smallClassHierarchy,
            hoTypeSynonyms = restrictTypeSynonyms (`member` allExports) thisTypeSynonyms
        }
        hoEx = mempty {
            hoExports = Map.fromList [ (modInfoName m,modInfoExport m) | m <- ms ],
            hoDefs =  Map.fromList [ (x,(y,filter (`member` allExports) z)) | (x,y,z) <- concat $ map modInfoDefs ms, x `member` allExports]
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
    return (mempty { hoBuild = hoBld, hoExp = hoEx },tiData)

