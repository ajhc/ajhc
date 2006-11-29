module FrontEnd.Tc.Module (tiModules',TiData(..)) where

import Char
import Control.Monad.Writer
import IO
import List
import Maybe
import Monad
import qualified Data.Map as Map
import Text.PrettyPrint.HughesPJ as PPrint

import Atom
import FrontEnd.Class
import DataConsAssump     (dataConsEnv)
import DataConstructors
import DeclsDepends       (getDeclDeps, debugDeclBindGroups)
import DependAnalysis     (getBindGroups)
import DerivingDrift.Drift
import Doc.PPrint as PPrint
import FrontEnd.Desugar
import FrontEnd.Infix
import FrontEnd.KindInfer
import FrontEnd.Rename
import FrontEnd.SrcLoc
import FrontEnd.Tc.Monad
import FrontEnd.Tc.Main
import FrontEnd.Tc.Type
import FrontEnd.Utils
import FrontEnd.Exports
import GenUtil
import Info.Properties
import Ho.Type
import HsSyn
import Name.Name as Name
import Options
import qualified FlagDump as FD
import qualified HsPretty
import Util.SetLike
import TypeSigs           (collectSigs, listSigsToSigEnv)
import TypeSynonyms
import TypeSyns
import Util.Gen
import Util.Inst()
import Warning

trimEnv env = Map.filterWithKey (\k _ -> isGlobal k) env


getDeclNames ::  HsDecl -> [Name]
getDeclNames (HsTypeSig _ ns _ ) =  map (toName Val) ns
getDeclNames d = maybeGetDeclName d

-- Extra data produced by the front end, used to fill in the Ho file.
data TiData = TiData {
    tiDataLiftedInstances :: Map.Map Name HsDecl,
    tiDataDecls      :: [HsDecl],
    tiDataModules    :: [(Module,HsModule)],
    tiModuleOptions  :: [(Module,Opt)],
    tiCheckedRules   :: [Rule],
    tiCoerce         :: Map.Map Name CoerceTerm,
    tiAllAssumptions :: Map.Map Name Type
}

isGlobal x |  (_,(_::String,(h:_))) <- fromName x =  not $ isDigit h
isGlobal _ = error "isGlobal"

modInfoDecls = hsModuleDecls . modInfoHsModule

getImports ModInfo { modInfoHsModule = mod }  = [  (hsImportDeclModule x) | x <-  hsModuleImports mod]

pprintEnv :: PPrint Doc a => Map.Map Name a -> Doc
pprintEnv env = pl global $+$ pl local_norm $+$ pl local_sys  where
    es = Map.toList env
    (local,global) = partition (\ (x,_) -> not (isGlobal x)) es -- isDigit $ head (hsIdentString (hsNameIdent x)) ) es
    (local_sys,local_norm) = partition (\(x,_) -> last (show x) == '@' ) local
    pl es = vcat [((pprint a) <+> (text "::") <+> (pprint b)) | (a, b) <- es]

buildFieldMap :: Ho -> [ModInfo] -> FieldMap
buildFieldMap ho ms = (ans',ans) where
        theDefs = [ (x,z) | (x,_,z) <- concat $ map modInfoDefs ms, nameType x == DataConstructor ]
        allDefs = theDefs ++ [ (x,z) | (x,(_,z)) <- Map.toList (hoDefs ho), nameType x == DataConstructor ]
        ans = Map.fromList $ sortGroupUnderFG fst snd $ concat [ [ (y,(x,i)) |  y <- ys | i <- [0..] ]  | (x,ys) <-  allDefs ]
        ans' = Map.fromList $ concatMap modInfoConsArity ms ++ getConstructorArities (hoDataTable ho)


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
tiModules' (CollectedHo me) ms = do
--    let importVarEnv = Map.fromList [ (x,y) | (x,y) <- Map.toList $ hoAssumps me, nameType x == Name.Val ]
--        importDConsEnv = Map.fromList [ (x,y) | (x,y) <- Map.toList $ hoAssumps me, nameType x ==  Name.DataConstructor ]
    let importClassHierarchy = hoClassHierarchy me
        importKindEnv = hoKinds me
    wdump FD.Progress $ do
        putErrLn $ "Typing: " ++ show ([ m | Module m <- map modInfoName ms])
    let fieldMap = buildFieldMap me ms
    ms <- mapM (processModule fieldMap) ms
    let thisFixityMap = buildFixityMap (concat [ filter isHsInfixDecl (hsModuleDecls $ modInfoHsModule m) | m <- ms])
    let fixityMap = thisFixityMap `mappend` hoFixities me
    let thisTypeSynonyms =  (declsToTypeSynonyms $ concat [ filter isHsTypeDecl (hsModuleDecls $ modInfoHsModule m) | m <- ms])
    let ts = thisTypeSynonyms  `mappend` hoTypeSynonyms me
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
        classDefaults  = snub [ getDeclName z | z <- cDefBinds, isHsFunBind z || isHsPatBind z ]
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
        putStrLn $ PPrint.render $ pprintEnvMap (sigEnv `mappend` localDConsEnv `mappend` hoAssumps me)

    wdump FD.Progress $ do
        putErrLn $ "Type inference"
    let moduleName = modInfoName tms
        (tms:_) = ms
    let tcInfo = tcInfoEmpty {
        tcInfoEnv = hoAssumps me `mappend` localDConsEnv, -- (importVarEnv `mappend` globalDConsEnv),
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

    let getMod x = case getModule x of
                     Just m  -> m
                     Nothing -> error ("getModule "++show x++" => Nothing")
        interesting x = isGlobal x && nameType x /= FfiExportName
    let externalEnv = Map.filterWithKey (\ x _ -> interesting x && (getMod x `elem` map modInfoName ms)) localVarEnv `Map.union` noDefaultSigs
    localVarEnv <- return $  localVarEnv `Map.union` noDefaultSigs
    let externalKindEnv = restrictKindEnv (\ x  -> interesting x && (getMod x `elem` map modInfoName ms)) kindInfo

    let pragmaProps = fromList $ Map.toList $ Map.fromListWith mappend [ (toId $ toName Name.Val x,fromList $ readProp w) |  HsPragmaProps _ w xs <- ds, x <- xs ]

    let allAssumps = localDConsEnv `Map.union` localVarEnv
        expAssumps = localDConsEnv `Map.union` externalEnv
    let ho = mempty {
        hoExports = Map.fromList [ (modInfoName m,modInfoExport m) | m <- ms ],
        hoDefs =  Map.fromList [ (x,(y,z)) | (x,y,z) <- concat $ map modInfoDefs ms],
        hoAssumps = expAssumps,
        hoFixities = thisFixityMap,
        hoKinds = externalKindEnv,
        hoClassHierarchy = smallClassHierarchy,
        hoProps = pragmaProps,
        hoTypeSynonyms = thisTypeSynonyms

        }
        tiData = TiData {
            --tiDataLiftedInstances = Map.fromList [ (getDeclName d,d) | d <- liftedInstances],
            tiDataLiftedInstances = error "tiDataLiftedInstances not used", -- Map.fromList [ (getDeclName d,d) | d <- ds],
            tiDataDecls = tcDs ++ filter isHsClassDecl ds,
            tiDataModules = [ (modInfoName m, modInfoHsModule m) |  m <- ms],
            tiModuleOptions = [ (modInfoName m, modInfoOptions m) |  m <- ms],
            tiCheckedRules = checkedRules,
            tiCoerce       = coercions,
            tiAllAssumptions = allAssumps
        }
    return (ho,tiData)

