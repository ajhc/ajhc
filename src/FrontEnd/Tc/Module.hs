module FrontEnd.Tc.Module (tiModules,TiData(..)) where

import Control.Monad.Writer
import Data.Char
import Text.PrettyPrint.HughesPJ as PPrint
import qualified Data.Foldable as T
import qualified Data.Map as Map
import qualified Data.Set as Set

import DataConstructors
import DerivingDrift.Drift
import Doc.PPrint
import Doc.PPrint as PPrint
import FrontEnd.Class
import FrontEnd.DataConsAssump(dataConsEnv)
import FrontEnd.DependAnalysis(debugDeclBindGroups)
import FrontEnd.Exports
import FrontEnd.HsErrors
import FrontEnd.Infix
import FrontEnd.KindInfer
import FrontEnd.Rename
import FrontEnd.Syn.Traverse hiding(getDeclNames)
import FrontEnd.Tc.Main
import FrontEnd.Tc.Monad
import FrontEnd.Tc.Type
import FrontEnd.TypeSigs(collectSigs, listSigsToSigEnv)
import FrontEnd.TypeSynonyms
import FrontEnd.Warning
import Ho.Type
import Info.Types
import Name.Name as Name
import Name.Names
import Options
import Util.Gen
import Util.Graph
import Util.Inst()
import Util.SetLike
import qualified Doc.DocLike as D
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
        allDefs = [ (x,z) | (x,_,z) <- concat $ map modInfoDefs ms,
            nameType x == DataConstructor ]
        ans = Map.fromList $ sortGroupUnderFG fst snd $ concat
            [ [ (y,(x,i)) |  y <- ys | i <- [0..] ]  | (x,ys) <-  allDefs ]
        ans' = Map.fromList $ [ (x,fromIntegral y) | (x,y) <- concatMap modInfoConsArity ms]

processModule :: FieldMap -> ModInfo -> IO (ModInfo,[Warning])
processModule defs m = do
    when (dump FD.Parsed) $ do
        putStrLn " \n ---- parsed code ---- \n";
        putStrLn $ HsPretty.render
            $ HsPretty.ppHsModule
                $ modInfoHsModule m
    let mod = modInfoHsModule m
    let imp = modInfoImport m
    let (((mod',inst),rmap),errs) = runWriter $ renameModule (modInfoOptions m) defs imp mod
    when (dump FD.Derived && (not $ null inst)) $ do
        putStrLn " \n ---- derived instances ---- \n"
        putStrLn $ HsPretty.render $ HsPretty.ppHsDecls $ inst
    wdump FD.Renamed $ do
        putStrLn " \n ---- renamed code ---- \n"
        putStrLn $ HsPretty.render $ HsPretty.ppHsModule $  mod'
    return (m { modInfoReverseMap = rmap,
                modInfoImport = imp ++ driftResolvedNames,
                modInfoHsModule = mod' }, errs)

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

getDataDesc :: Monad m => HsDecl -> m (Name,DatDesc)
getDataDesc d = g d where
    g desc = do
        r <- f d
        return (hsDeclName desc,r)
    f HsDataDecl { hsDeclDeclType = DeclTypeNewtype, hsDeclCons = (hsConDeclName . head -> cn)  } = return $ DatNewT cn
    f HsDataDecl { hsDeclCons = cs }
        | all null $ map hsConDeclArgs cs = return $ DatEnum (map hsConDeclName cs)
    f HsDataDecl { hsDeclCons = cs } = return $
        DatMany [ (hsConDeclName c, (length . hsConDeclArgs) c) | c <- cs]
    f _ = fail "getDataDesc: not a data declaration"

{-# NOINLINE tiModules #-}
tiModules ::  HoTcInfo -> [ModInfo] -> IO (HoTcInfo,TiData)
tiModules htc ms = do
    let importClassHierarchy = hoClassHierarchy htc
        importKindEnv = hoKinds htc
    let nfm = buildFieldMap ms `mappend` hoFieldMap htc
    mserrs <- mapM (processModule nfm) ms
    processErrors (concatMap snd mserrs)
    let ms = fsts mserrs
    let thisFixityMap = buildFixityMap . concat $
            [filter isHsInfixDecl (hsModuleDecls $ modInfoHsModule m) | m <- ms]
    let fixityMap = thisFixityMap  `mappend` hoFixities htc
    wdump FD.Fixity $ do
       dumpFixityMap fixityMap
    thisTypeSynonyms <- declsToTypeSynonyms (hoTypeSynonyms htc) $ concat
        [ filter isHsTypeDecl (hsModuleDecls $ modInfoHsModule m) | m <- ms]
    let ts = thisTypeSynonyms `mappend` hoTypeSynonyms htc

    -- expand synonyms and refix expressions
    let f x = expandTypeSyns ts (modInfoHsModule x) >>=
            return . FrontEnd.Infix.infixHsModule fixityMap >>=
            \z -> return x { modInfoHsModule = z }
    ms <- mapM f ms
    let ds = concat [ hsModuleDecls $ modInfoHsModule m | m <- ms ]
    wdump FD.Decls $ do
        putStrLn "  ---- processed decls ---- "
        putStrLn $ HsPretty.render (HsPretty.ppHsDecls ds)
    runSLM $ preTypecheckChecks ds
    processIOErrors "synonym expansion and fixity processing"

    -- kind inference for all type constructors type variables and classes in the module
    let classAndDataDecls = filter (or' [isHsDataDecl, isHsClassDecl, isHsClassAliasDecl]) ds
    kindInfo <- kiDecls importKindEnv ds

    when (dump FD.Kind) $
         do {putStrLn " \n ---- kind information ---- \n";
             putStrLn $ PPrint.render $ pprint kindInfo}

    processIOErrors "kind inference"

    let localDConsEnv = dataConsEnv kindInfo classAndDataDecls
    wdump FD.Dcons $ do
        putStr "\n ---- data constructor assumptions ---- \n"
        mapM_ putStrLn [ show n ++  " :: " ++ prettyPrintType s |
            (n,s) <- Map.toList localDConsEnv]

    cHierarchy <- makeClassHierarchy importClassHierarchy kindInfo ds
    let smallClassHierarchy = foldr addInstanceToHierarchy cHierarchy dinsts where
            derivingClauses = collectDeriving ds
            dataInfo = Map.fromList $ concatMap getDataDesc ds
            dinsts = concatMap g derivingClauses where
                g r@(_,c,t) | c `elem` enumDerivableClasses,
                    Just (DatEnum (_:_:_)) <- Map.lookup t dataInfo = [f r]
                            | c `elem` enumDerivableClasses,
                    t `elem` [tc_Bool, tc_Ordering, tc_IOErrorType, tc_IOMode] = [f r]
                --g r@(_,c,t) | c `notElem` noNewtypeDerivable, Just (DatMany True [_]) <- Map.lookup t dataInfo = [f r]
                g _ = []
                f (sl,c,t) = emptyInstance {
                    instSrcLoc = sl,
                    instDerived = True,
                    instHead = [] :=> IsIn c (TCon (Tycon t kindStar))
                    }
    smallClassHierarchy <- checkForDuplicateInstaces importClassHierarchy smallClassHierarchy

    let cHierarchyWithInstances = scatterAliasInstances $
            smallClassHierarchy `mappend` importClassHierarchy

    checkForCircularDeps cHierarchyWithInstances

    when (dump FD.ClassSummary) $ do
        putStrLn "  ---- class summary ---- "
        printClassSummary cHierarchyWithInstances
    when (dump FD.Class) $
         do {putStrLn "  ---- class hierarchy ---- ";
             printClassHierarchy smallClassHierarchy}
    -- lift the instance methods up to top-level decls
    let cDefBinds = concat [ [ z | z <- ds] | HsClassDecl _ _ ds <- ds]
    let myClassAssumps = concat [ classAssumps as | as <- classRecords cHierarchyWithInstances ]
        instanceEnv = Map.fromList instAssumps
        classDefs = snub (concatMap getDeclNames cDefBinds)
        classEnv = Map.fromList $ [ (x,y) | (x,y) <- myClassAssumps, x `elem` classDefs  ]
        (liftedInstances,instAssumps) = mconcatMap
            (instanceToTopDecls kindInfo cHierarchyWithInstances) ds

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
    let programBgs = easySCC bindings getDeclName getDeclDeps

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
        putStrLn $ PPrint.render $ pprintEnvMap
            (sigEnv `mappend` localDConsEnv `mappend` hoAssumps htc)

    let moduleName = modInfoName tms
        (tms:_) = ms
    let tcInfo = tcInfoEmpty {
        tcInfoEnv = hoAssumps htc `mappend` localDConsEnv,
        tcInfoSigEnv = sigEnv,
        tcInfoModName = moduleName,
        tcInfoKindInfo = kindInfo,
        tcInfoClassHierarchy = cHierarchyWithInstances
        }

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
        mapM_ putStrLn [ show n ++  " :: " ++ prettyPrintType s |
            (n,s) <- Map.toList (if verbose2 then localVarEnv else trimEnv localVarEnv)]
    when (dump FD.Types) $ do
        putStrLn " ---- the coersions of identifiers ---- "
        mapM_ putStrLn [ show n ++  " --> " ++ show s |  (n,s) <- Map.toList coercions]

    localVarEnv <- return $  localVarEnv `Map.union` noDefaultSigs

    let pragmaProps = fromList $ Map.toList $ Map.fromListWith mappend
            [(toName Name.Val x,fromList $ readProp w) | HsPragmaProps _ w xs <- ds, x <- xs]

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
    processIOErrors "module typechecking"
    return (hoEx,tiData)

pprintEnvMap :: (PPrint d k, PPrint d a) => Map.Map k a -> d
pprintEnvMap m = D.vcat [ pprint x D.<+> D.text "::" D.<+> pprint y | (x,y) <- Map.toList m ]
