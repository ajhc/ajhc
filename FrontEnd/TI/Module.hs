module FrontEnd.TI.Module (tiModules') where

import Char
import Control.Monad.Writer
import IO
import List
import Maybe
import Monad
import qualified Data.Map as Map
import Text.PrettyPrint.HughesPJ as PPrint

import Atom
import Class
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
import FrontEnd.Tc.Monad()
import FrontEnd.TiData
import FrontEnd.Utils
import GenUtil
import Ho.Type
import HsSyn
import MultiModuleBasics
import Name.Name as Name
import Options
import qualified FlagDump as FD
import qualified HsPretty
import Representation
import FrontEnd.TI.Main
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
    let mod = desugarHsModule (zmod') -- only needed for pattern bindings.. or is it?
        --is = getImports m ++ if optPrelude options then [Module "Prelude"] else []
        --(es,ts) = mconcat [ modInfoExports (lookupMod me i) | i <- is]
        --(mod', errs) =  renameTidyModule (modEnvTypeSynonyms me) (fsts es) (fsts ts)  mod
        -- we pass in the imported infix decls and also the ones from the local module
        --renamedTidyModule'' = Infix.infixer (tidyInFixDecls (tidyModule renamedTidyModule') ++ modEnvFixities me) (tidyModule renamedTidyModule')
        --(mod',errs) = runWriter $ renameModule (modEnvTypeSynonyms me) (modInfoImport m)  mod
    let (mod',errs) = runWriter $ renameModule defs (modInfoImport m)  mod
        mod'' = mod'
        --mod'' = Infix.infixHsModule ( [ d | d <- hsModuleDecls mod', isHsInfixDecl d] ++ modEnvFixities me) ( mod')

    when (dump FD.Renamed) $
         do {putStrLn " \n ---- renamed code ---- \n";
             putStrLn $ HsPretty.render
                      $ HsPretty.ppHsModule
                      $  mod''}
    processErrors errs
    return $ modInfoHsModule_s mod'' m


shwartz f xs = [ (f x, x) | x <- xs ]

-- type check a set of mutually recursive modules.
-- assume all dependencies are met in the
-- ModEnv parameter and export lists have been calculated.

or' :: [(a -> Bool)] -> a -> Bool
or' fs x = or [ f x | f <- fs ]

tiModules' ::  Ho -> [ModInfo] -> IO (Ho,TiData)
tiModules' me ms = do
    let importVarEnv = Map.fromList [ (x,y) | (x,y) <- Map.toList $ hoAssumps me, nameType x == Name.Val ]
        importDConsEnv = Map.fromList [ (x,y) | (x,y) <- Map.toList $ hoAssumps me, nameType x ==  Name.DataConstructor ]
        importClassHierarchy = hoClassHierarchy me
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
    --print (importKindEnv, classAndDataDecls)

    wdump FD.Progress $ do
        putErrLn $ "Kind inference"
    kindInfo <- kiDecls importKindEnv classAndDataDecls

    when (dump FD.Kind) $
         do {putStrLn " \n ---- kind information ---- \n";
             --mapM_ (putStrLn . show) (envToList kindInfo);
             putStr $ PPrint.render $ pprint kindInfo}

    -- collect types for data constructors

    let localDConsEnv = dataConsEnv (error "modName") kindInfo classAndDataDecls -- (rDataDecls ++ rNewTyDecls)

    when  (dump FD.Dcons) $
         do {putStr "\n ---- data constructor assumptions ---- \n";
             putStrLn $ PPrint.render $ pprint localDConsEnv}


    let globalDConsEnv = localDConsEnv `Map.union` importDConsEnv

    -- generate the class hierarchy skeleton

    --classHierarchy  <- foldM (flip (addClassToHierarchy kindInfo)) importClassHierarchy ds -- ds -- rClassDecls
    --cHierarchyWithInstances <- addInstancesToHierarchy kindInfo classHierarchy ds -- (rInstDecls ++ rDataDecls)

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
        --ca = listToEnv $ [ (x,y) | (x :>: y) <- myClassAssumps  ++ instAssumps ]
        --ca' = listToEnv $ [ (x,y) | (x :>: y) <- myClassAssumps  ]
        instanceEnv   = Map.fromList $ [ (x,y) | (x :>: y) <-  instAssumps ]
        classDefs = snub (concatMap getDeclNames cDefBinds)
        classEnv  = Map.fromList $ [ (x,y) | (x :>: y) <- myClassAssumps, x `elem` classDefs  ]
        (liftedInstances,instAssumps) =  mconcatMap (instanceToTopDecls kindInfo cHierarchyWithInstances) ds -- rInstDecls


    when (not (null liftedInstances) && (dump FD.Instance) ) $ do
        putStrLn "  ---- lifted instance declarations ---- "
        putStr $ unlines $ map (HsPretty.render . HsPretty.ppHsDecl) liftedInstances
        putStrLn $ PPrint.render $ pprintEnvMap instanceEnv


    let funPatBinds =  [ d | d <- ds, or' [isHsFunBind, isHsPatBind, isHsForeignDecl] d]
    let rTySigs =  [ d | d <- ds, or' [isHsTypeSig] d]

    -- build an environment of assumptions for all the type signatures
    let allTypeSigs = collectSigs (funPatBinds ++ liftedInstances) ++ rTySigs

    when (dump FD.Srcsigs) $
         do {putStrLn " ---- type signatures from source code (after renaming) ---- ";
             putStr $ unlines $ map (HsPretty.render . HsPretty.ppHsDecl) allTypeSigs}

    let sigEnv = Map.unions [listSigsToSigEnv kindInfo allTypeSigs,instanceEnv, classEnv]
    when (dump FD.Sigenv) $
         do {putStrLn "  ---- initial sigEnv information ---- ";
             --mapM_ (putStrLn . show) (envToList kindInfo);
             putStrLn $ PPrint.render $ pprintEnvMap sigEnv}
    let bindings = (funPatBinds ++ [ z | z <- cDefBinds, isHsFunBind z || isHsPatBind z] ++ liftedInstances)
        classDefaults  = snub [ getDeclName z | z <- cDefBinds, isHsFunBind z || isHsPatBind z ]
        classNoDefaults = snub (concat [ getDeclNames z | z <- cDefBinds ])  List.\\ classDefaults
        noDefaultSigs = Map.fromList [ (n,maybe (error $ "sigEnv:"  ++ show n) id $ Map.lookup n sigEnv) | n <- classNoDefaults ]
        fakeForeignDecls = [ [HsForeignDecl bogusASrcLoc (Import "" [] []) Primitive Safe (nameName x) (HsQualType [] $ HsTyTuple []) ] | (x,_) <- Map.toList noDefaultSigs]
    --when verbose2 $ putStrLn (show bindings)
    let programBgs = getBindGroups bindings (nameName . getDeclName) getDeclDeps

    when (dump FD.Bindgroups) $
         do {putStrLn " \n ---- toplevel variable binding groups ---- ";
             putStrLn " ---- Bindgroup # = [members] [vars depended on] [missing vars] ---- \n";
             putStr $ debugDeclBindGroups programBgs}

    let program = makeProgram sigEnv ( fakeForeignDecls ++ programBgs )
    when (dump FD.Program) $ do
        putStrLn " ---- Program ---- "
        mapM_ putStrLn $ map (PPrint.render . PPrint.pprint) $  program

    -- type inference/checking for all variables

    wdump FD.Progress $ do
        putErrLn $ "Type inference"
    let moduleName = modInfoName tms
        (tms:_) = ms
    localVarEnv <- tiProgram
                (modInfoOptions tms)           -- choose options from one of recursive group
                moduleName                     -- name of the module
                sigEnv                         -- environment of type signatures
                kindInfo                       -- kind information about classes and type constructors
                cHierarchyWithInstances        -- class hierarchy with instances
                globalDConsEnv                 -- data constructor type environment
                importVarEnv                   -- type environment
                program                        -- binding groups
                ds                             -- all decls



    when (dump FD.Types) $
         do {putStrLn " ---- the types of identifiers ---- ";
             putStrLn $ PPrint.render $ pprintEnv (if verbose2 then localVarEnv else trimEnv localVarEnv) }

    --let externalEnv = Map.fromList [ v | v@(x@(Qual m i) ,s) <- Map.toList localVarEnv, isGlobal x, m `elem` map modInfoName ms ]  `Map.union` noDefaultSigs
    let externalEnv = Map.filterWithKey (\ x _ -> isGlobal x && (fromJust (getModule x) `elem` map modInfoName ms)) localVarEnv `Map.union` noDefaultSigs
    localVarEnv <- return $  localVarEnv `Map.union` noDefaultSigs
    --let externalKindEnv = Map.fromList [ v | v@(x@(Qual m i) ,s) <- Map.toList kindInfo, isGlobal x, m `elem` map modInfoName ms ]
    let externalKindEnv = restrictKindEnv (\ x  -> isGlobal x && (fromJust (getModule x) `elem` map modInfoName ms)) kindInfo

    let pragmaProps = Map.fromListWith (\a b -> snub $ a ++ b ) [ (toName Name.Val x,[toAtom w]) |  HsPragmaProps _ w xs <- ds, x <- xs ]

    let allAssumps = localDConsEnv `Map.union` localVarEnv -- Map.fromList $ [ (toName Name.DataConstructor x,y) | (x,y) <- Map.toList localDConsEnv ] ++ [ (toName Name.Val x,y) | (x,y) <- Map.toList localVarEnv ]
        --expAssumps = M.fromList $ [ (toName Name.DataConstructor x,y) | (x,y) <- Env.toList localDConsEnv ] ++ [ (toName Name.Val x,y) | (x,y) <- Env.toList $ trimEnv localVarEnv ]
        --expAssumps = Map.fromList $ [ (toName Name.DataConstructor x,y) | (x,y) <- Map.toList localDConsEnv ] ++ [ (toName Name.Val x,y) | (x,y) <- Map.toList $ externalEnv ]
        expAssumps = localDConsEnv `Map.union` externalEnv -- Map.fromList $ [ (toName Name.DataConstructor x,y) | (x,y) <- Map.toList localDConsEnv ] ++ [ (toName Name.Val x,y) | (x,y) <- Map.toList $ externalEnv ]
    let ho = mempty {
        hoExports = Map.fromList [ (modInfoName m,modInfoExport m) | m <- ms ],
        hoDefs =  Map.fromList [ (x,(y,z)) | (x,y,z) <- concat $ map modInfoDefs ms],
        hoAssumps = expAssumps,
        hoFixities = thisFixityMap,
        --hoKinds = trimMapEnv kindInfo,
        hoKinds = externalKindEnv,
        --hoClassHierarchy = cHierarchyWithInstances,
        hoClassHierarchy = smallClassHierarchy,
        hoProps = pragmaProps,
        hoTypeSynonyms = thisTypeSynonyms

        }
        tiData = TiData {
            tiDataLiftedInstances = Map.fromList [ (getDeclName d,d) | d <- liftedInstances],
            tiDataModules = [ (modInfoName m, modInfoHsModule m) |  m <- ms],
            tiModuleOptions = [ (modInfoName m, modInfoOptions m) |  m <- ms],
            tiCheckedRules = [],
            tiAllAssumptions = allAssumps
        }
    return (ho,tiData)

