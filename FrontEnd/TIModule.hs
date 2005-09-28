module TIModule (tiModules', TiData(..)) where

import Char
import Control.Monad.Writer
import IO
import List
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
import FrontEnd.Rename
import GenUtil
import Ho
import HsSyn
import KindInfer
import MonoidUtil
import MultiModuleBasics
import Name
import Options
import qualified FlagDump as FD
import qualified HsPretty
import Representation
import TIMain
import TypeSigs           (collectSigs, listSigsToSigEnv)
import TypeSynonyms
import TypeSyns
import Utils
import Warning

trimEnv env = (Map.fromList [ n | n@(name,_) <- Map.toList env,  isGlobal name ])
trimMapEnv env = (Map.fromAscList [ n | n@(name,_) <- Map.toAscList env,  isGlobal name ])
--------------------------------------------------------------------------------

getDeclNames ::  HsDecl -> [HsName]
getDeclNames (HsTypeSig _ ns _ ) =  ns
getDeclNames d = maybeGetDeclName d

-- Extra data produced by the front end, used to fill in the Ho file.
data TiData = TiData {
    tiDataLiftedInstances :: Map.Map HsName HsDecl,
    tiDataModules :: [(Module,HsModule)],
    tiModuleOptions :: [(Module,Opt)],
    tiAllAssumptions :: Map.Map Name Scheme
}

isGlobal (Qual _ x) =  not $ isDigit $ head (hsIdentString x)
isGlobal _ = error "isGlobal"

modInfoDecls = hsModuleDecls . modInfoHsModule

getImports ModInfo { modInfoHsModule = mod }  = [  (hsImportDeclModule x) | x <-  hsModuleImports mod]

--lookupMod ModEnv { modEnvModules  = m } s =  case  M.lookup s m of
--    Just z -> z
--    Nothing -> error $ "lookupMod: " ++ show s

pprintEnv :: PPrint Doc a => Map.Map HsName a -> Doc
pprintEnv env = pl global $+$ pl local_norm $+$ pl local_sys  where
    es = Map.toList env
    (local,global) = partition (\(x,_) -> isDigit $ head (hsIdentString (hsNameIdent x)) ) es
    (local_sys,local_norm) = partition (\(x,_) -> last (hsIdentString (hsNameIdent x)) == '@' ) local
    pl es = vcat [((pprint a) <+> (text "::") <+> (pprint b)) | (a, b) <- es]

--buildFieldLabelMap ::  Map.Map Name (SrcLoc,[Name]) -> Map.Map Name [(Name,Int,Int)]
--buildFieldLabelMap fm = Map.fromList $ sortGroupUnderF fst $ concat [ [ (y,(x,i,length ys)) |  y <- ys | i <- [0..] ]  | (x,(_,ys)) <- Map.toList fm, nameType x == DataConstructor ]

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
    let importVarEnv = Map.fromList [ (n,y) | (x,y) <- Map.toList $ hoAssumps me, let (t,n) = fromName x, t == Name.Val ]
        importDConsEnv = Map.fromList [ (n,y) | (x,y) <- Map.toList $ hoAssumps me, let (t,n) = fromName x, t == Name.DataConstructor ]
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
             putStr $ PPrint.render $ pprintEnvMap kindInfo}

    -- collect types for data constructors

    let localDConsEnv = dataConsEnv (error "modName") kindInfo classAndDataDecls -- (rDataDecls ++ rNewTyDecls)

    when  (dump FD.Dcons) $
         do {putStr "\n ---- data constructor assumptions ---- \n";
             putStrLn $ PPrint.render $ pprintEnv localDConsEnv}


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
        fakeForeignDecls = [ [HsForeignDecl bogusASrcLoc ForeignPrimitive "" x (HsUnQualType $ HsTyTuple []) ] | (x,_) <- Map.toList noDefaultSigs]
    --when verbose2 $ putStrLn (show bindings)
    let programBgs
           = getBindGroups bindings getDeclName getDeclDeps

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
    let moduleName = modInfoName (head ms)
    localVarEnv <- tiProgram
                moduleName                     -- name of the module
                sigEnv                         -- environment of type signatures
                kindInfo                       -- kind information about classes and type constructors
                cHierarchyWithInstances        -- class hierarchy with instances
                globalDConsEnv                 -- data constructor type environment
                (importVarEnv  )               -- type environment
                program                        -- binding groups


    when (dump FD.Types) $
         do {putStrLn " ---- the types of identifiers ---- ";
             putStrLn $ PPrint.render $ pprintEnv (if verbose2 then localVarEnv else trimEnv localVarEnv) }

    let externalEnv = Map.fromList [ v | v@(x@(Qual m i) ,s) <- Map.toList localVarEnv, isGlobal x, m `elem` map modInfoName ms ]  `Map.union` noDefaultSigs
    localVarEnv <- return $  localVarEnv `Map.union` noDefaultSigs
    let externalKindEnv = Map.fromList [ v | v@(x@(Qual m i) ,s) <- Map.toList kindInfo, isGlobal x, m `elem` map modInfoName ms ]

    let pragmaProps = Map.fromListWith (\a b -> snub $ a ++ b ) [ (toName Name.Val x,[toAtom w]) |  HsPragmaProps _ w xs <- ds, x <- xs ]

    let allAssumps = Map.fromList $ [ (toName Name.DataConstructor x,y) | (x,y) <- Map.toList localDConsEnv ] ++ [ (toName Name.Val x,y) | (x,y) <- Map.toList localVarEnv ]
        --expAssumps = M.fromList $ [ (toName Name.DataConstructor x,y) | (x,y) <- Env.toList localDConsEnv ] ++ [ (toName Name.Val x,y) | (x,y) <- Env.toList $ trimEnv localVarEnv ]
        expAssumps = Map.fromList $ [ (toName Name.DataConstructor x,y) | (x,y) <- Map.toList localDConsEnv ] ++ [ (toName Name.Val x,y) | (x,y) <- Map.toList $ externalEnv ]
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
            tiAllAssumptions = allAssumps
        }
    return (ho,tiData)
    {-
    let me''' =
            addItems modEnvVarAssumptions_u (trimEnv localVarEnv) .
            addItems modEnvDConsAssumptions_u localDConsEnv .
            addItems modEnvAllAssumptions_u allAssumps .
            addItems modEnvKinds_u (trimMapEnv kindInfo) .
            modEnvTypeSynonyms_s ts . --  (++ [ d | d <- ds, isHsTypeDecl d ]) .
            modEnvClassHierarchy_s cHierarchyWithInstances .
            modEnvLiftedInstances_u (M.union $ M.fromList [ (getDeclName d,d) | d <- liftedInstances]) .
            --modEnvFixities_u (++ [ d | d <- ds, isHsInfixDecl d ])
            modEnvFixities_s fixityMap
            $ me''
    --let mi = ModuleInfo { varAssumps = localVarEnv, dconsAssumps = localDConsEnv,
    --                    classHierarchy = cHierarchyWithInstances, kinds = kindInfo, infixDecls = getInfixDecls mod,
    --                    tyconsMembers = getTyconsMembers mod, synonyms = tidyTyDecls tidyMod,
    --                    renamedModule =  [addDecls mod liftedInstances]}
    return me'''

tiModules ::  ModEnv -> [ModInfo] -> IO ModEnv
tiModules me ms = do
    let importVarEnv = modEnvVarAssumptions me
        importDConsEnv = modEnvDConsAssumptions me
        importClassHierarchy = modEnvClassHierarchy me
        importKindEnv = modEnvKinds me
    wdump FD.Progress $ do
        putErrLn $ "Typing: " ++ show ([ m | Module m <- map modInfoName ms])

    let me' = modEnvModules_u (M.union (M.fromList (shwartz modInfoName ms))) me
    ms <- mapM (processModule me') ms
    let fixityMap = buildFixityMap (concat [ filter isHsInfixDecl (hsModuleDecls $ modInfoHsModule m) | m <- ms]) `mappend` modEnvFixities me
    let ts = (declsToTypeSynonyms $ concat [ filter isHsTypeDecl (hsModuleDecls $ modInfoHsModule m) | m <- ms])  `mappend` modEnvTypeSynonyms me
    let f x = expandTypeSyns ts (modInfoHsModule x) >>= FrontEnd.Infix.infixHsModule fixityMap >>= \z -> return (modInfoHsModule_s ( z) x)
    ms <- mapM f ms
    let me'' = modEnvModules_u (M.union (M.fromList (shwartz modInfoName ms))) me'
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
             putStr $ PPrint.render $ pprintEnvMap kindInfo}

    -- collect types for data constructors

    let localDConsEnv = dataConsEnv (error "modName") kindInfo classAndDataDecls -- (rDataDecls ++ rNewTyDecls)

    when  (dump FD.Dcons) $
         do {putStr "\n ---- data constructor assumptions ---- \n";
             putStrLn $ PPrint.render $ pprintEnv localDConsEnv}


    let globalDConsEnv = localDConsEnv `joinEnv` importDConsEnv

    -- generate the class hierarchy skeleton

    classHierarchy  <- foldM (flip (addClassToHierarchy kindInfo)) importClassHierarchy ds -- ds -- rClassDecls
    cHierarchyWithInstances <- addInstancesToHierarchy kindInfo classHierarchy ds -- (rInstDecls ++ rDataDecls)
    when (dump FD.ClassSummary) $ do
        putStrLn "  ---- class summary ---- "
        printClassSummary cHierarchyWithInstances

    when (dump FD.Class) $
         do {putStrLn "  ---- class hierarchy ---- ";
             printClassHierarchy cHierarchyWithInstances}

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
        noDefaultSigs = Env.fromList [ (n,Map.find n sigEnv) | n <- classNoDefaults ]
        fakeForeignDecls = [ [HsForeignDecl bogusASrcLoc ForeignPrimitive "" x (HsUnQualType $ HsTyTuple []) ] | (x,_) <- Env.toList noDefaultSigs]
    --when verbose2 $ putStrLn (show bindings)
    let programBgs
           = getBindGroups bindings getDeclName getDeclDeps

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
    let moduleName = modInfoName (head ms)
    localVarEnv <- tiProgram
                moduleName                     -- name of the module
                sigEnv                         -- environment of type signatures
                kindInfo                       -- kind information about classes and type constructors
                cHierarchyWithInstances        -- class hierarchy with instances
                globalDConsEnv                 -- data constructor type environment
                (importVarEnv  )               -- type environment
                program                        -- binding groups


    when (dump FD.Types) $
         do {putStrLn " ---- the types of identifiers ---- ";
             putStrLn $ PPrint.render $ pprintEnv (if verbose2 then localVarEnv else trimEnv localVarEnv) }

    localVarEnv <- return $  localVarEnv `joinFM` noDefaultSigs

    let allAssumps = M.fromList $ [ (toName Name.DataConstructor x,y) | (x,y) <- Env.toList localDConsEnv ] ++ [ (toName Name.Val x,y) | (x,y) <- Env.toList localVarEnv ]
    let me''' =
            addItems modEnvVarAssumptions_u (trimEnv localVarEnv) .
            addItems modEnvDConsAssumptions_u localDConsEnv .
            addItems modEnvAllAssumptions_u allAssumps .
            addItems modEnvKinds_u (trimMapEnv kindInfo) .
            modEnvTypeSynonyms_s ts . --  (++ [ d | d <- ds, isHsTypeDecl d ]) .
            modEnvClassHierarchy_s cHierarchyWithInstances .
            modEnvLiftedInstances_u (M.union $ M.fromList [ (getDeclName d,d) | d <- liftedInstances]) .
            --modEnvFixities_u (++ [ d | d <- ds, isHsInfixDecl d ])
            modEnvFixities_s fixityMap
            $ me''
    --let mi = ModuleInfo { varAssumps = localVarEnv, dconsAssumps = localDConsEnv,
    --                    classHierarchy = cHierarchyWithInstances, kinds = kindInfo, infixDecls = getInfixDecls mod,
    --                    tyconsMembers = getTyconsMembers mod, synonyms = tidyTyDecls tidyMod,
    --                    renamedModule =  [addDecls mod liftedInstances]}
    return me'''

addItems mu env = mu (mappend env)
    -}

{-
{-# NOINLINE tiModule #-}
tiModule dumps modSyntax imports = do
    let importVarEnv = varAssumps imports
        importDConsEnv = dconsAssumps imports
        importClassHierarchy = classHierarchy imports
        importKindEnv = kinds imports
        importSynonyms = synonyms imports
        importTyconMembers = tyconsMembers imports

    let moduleName = hsModuleName modSyntax
    let tidyMod = tidyModule modSyntax
    -- make all pattern bindings simple and remove type synonyms, convert do-notation into expression form
    let desugaredTidyModule = desugarTidyModule importSynonyms tidyMod
    when (doDump dumps "desugar") $
         do {putStrLn "\n\n ---- desugared code ---- \n\n";
             putStrLn $ HsPretty.render
                      $ HsPretty.ppHsModule
                      $ tidyModuleToHsModule desugaredTidyModule}
    -- uniquely rename variables and generate a table of information about identifiers

        -- TODO: we probably need to worry about synonyms and
        --       the like as well but at the moment we can live
        --       with vars and datacons only.
    let
        importVarEnv' = trimEnv $ importVarEnv
        isGlobal (Qual _ x) =  not $ isDigit $ head (hsIdentString x)
        isGlobal _ = error "isGlobal"
    let importedNames = getNamesFromEnv importVarEnv'
                     ++ getNamesFromEnv importDConsEnv
                     ++ getNamesFromTycons importTyconMembers
                     ++ getNamesFromEnv importClassHierarchy
                     ++ [ n | (n :>: _) <- classAssumps ]
                     ++ getNamesFromEnv importKindEnv
                    --  ++ getNamesFromInfix  -- shouldn't need this as we get
                    -- them as part of getting their types in the varEnv
        -- because we need to know to rename True to Prelude.True
        -- as well, and this is a convenient way to do it:
        classAssumps = concat  [ as | (_,_,as) <- (eltsFM importClassHierarchy)]
        getNamesFromTycons :: [(HsName, [HsName])] -> [HsName]
        getNamesFromTycons = concatMap snd

    putVerbose $ show (namesHsModule (tidyModuleToHsModule desugaredTidyModule))
    let (renamedTidyModule', errs) =  renameTidyModule importSynonyms (filter isGlobal importedNames) (filter isGlobal importedNames) (tidyModuleToHsModule desugaredTidyModule)
        -- we pass in the imported infix decls and also the ones from the local module
        renamedTidyModule'' = Infix.infixer (tidyInFixDecls (tidyModule renamedTidyModule') ++ infixDecls imports) (tidyModule renamedTidyModule')

    let renamedTidyModule =  renamedTidyModule''

    when (doDump dumps "desugar") $
         do {putStrLn "\n\n ---- desugared code ---- \n\n";
             putStrLn $ HsPretty.render
                      $ HsPretty.ppHsModule
                      $ tidyModuleToHsModule desugaredTidyModule}

    -- All the names are getting qualified but they are unqualified by fromHsModule
    processErrors errs

    when (doDump dumps "renamed") $
         do {putStrLn " \n\n ---- renamed code ---- \n\n";
             putStrLn $ HsPretty.render
                      $ HsPretty.ppHsModule
                      $ tidyModuleToHsModule renamedTidyModule}


    -- separate the renamed decls apart
    let --rTyDecls    = tidyTyDecls    renamedTidyModule
        rDataDecls  = tidyDataDecls  renamedTidyModule
        rNewTyDecls = tidyNewTyDecls renamedTidyModule
        rClassDecls = tidyClassDecls renamedTidyModule
        rInstDecls  = tidyInstDecls  renamedTidyModule
        rTySigs     = tidyTySigs     renamedTidyModule
        rFunBinds   = tidyFunBinds   renamedTidyModule
        rPatBinds   = tidyPatBinds   renamedTidyModule


    -- collect all the type signatures from the module (this must be done after renaming)

        --   = getBindGroups (rFunBinds ++ rPatBinds ++ cDefBinds ++ liftedInstances) getDeclName getDeclDeps

    -- kind inference for all type constructors type variables and classes in the module

    let classAndDataDecls = rDataDecls ++ rNewTyDecls ++ rClassDecls

    let kindInfo = kiModule (trimEnv importKindEnv) classAndDataDecls

    when (doDump dumps "kinds") $
         do {putStrLn " \n\n ---- kind information ---- \n\n";
             putStr $ PPrint.render $ pprintEnv kindInfo}


-- collect types for data constructors

    let localDConsEnv = dataConsEnv moduleName kindInfo (rDataDecls ++ rNewTyDecls)

    when (doDump dumps "dconstypes") $
         do {putStr "\n\n ---- data constructor assumptions ---- \n\n";
             putStrLn $ PPrint.render $ pprintEnv localDConsEnv}


    let globalDConsEnv = localDConsEnv `joinEnv` importDConsEnv

-- generate the class hierarchy skeleton

    let classHierarchy = foldl (flip (addClassToHierarchy moduleName kindInfo)) importClassHierarchy rClassDecls
    let cHierarchyWithInstances
            = addInstancesToHierarchy kindInfo classHierarchy (rInstDecls ++ rDataDecls)
    when (doDump dumps "classes") $
         do {putStrLn " \n\n ---- class hierarchy ---- \n\n";
             printClassHierarchy cHierarchyWithInstances}

 -- lift the instance methods up to top-level decls

    let myClassAssumps = concat  [ as | (_,_,as) <- (eltsFM cHierarchyWithInstances)]
        ca = listToEnv $ [ (x,y) | (x :>: y) <- myClassAssumps  ++ instAssumps ]
    --print ca
        (liftedInstances,instAssumps) = unzip $ concatMap (instanceToTopDecls kindInfo cHierarchyWithInstances) rInstDecls


    when (not (null liftedInstances) &&  doDump dumps "instances") $
       do {putStrLn " \n\n ---- lifted instance declarations ---- \n\n";
           putStr $ unlines $
              map (HsPretty.render . HsPretty.ppHsDecl) liftedInstances}


-- build an environment of assumptions for all the type signatures
    let cDefBinds = concat [ [ z | z <- ds] | HsClassDecl _ _ ds <- rClassDecls]
    let allTypeSigs = (collectSigs (rFunBinds ++ rPatBinds {- ++ cDefBinds -} ++ liftedInstances)) ++ rTySigs

    when (doDump dumps "srcsigs") $
         do {putStrLn " \n\n ---- type signatures from source code (after renaming) ---- \n\n";
             putStr $ unlines $ map (HsPretty.render . HsPretty.ppHsDecl) allTypeSigs}

    let sigEnv = listSigsToSigEnv kindInfo allTypeSigs `joinEnv` ca

-- binding groups for top-level variables
    let programBgs
           = getBindGroups (rFunBinds ++ rPatBinds ++ [ z | z <- cDefBinds, isHsFunBind z || isHsPatBind z] ++ liftedInstances) getDeclName getDeclDeps


    when (doDump dumps "varbindgroups") $
         do {putStrLn " \n\n ---- toplevel variable binding groups ---- ";
             putStrLn " ---- Bindgroup # = [members] [vars depended on] [missing vars] ---- \n";
             putStr $ debugDeclBindGroups programBgs}

    let program = makeProgram sigEnv programBgs

-- type inference/checking for all variables

    when (doDump dumps "types") $
         do {putStr "\n\n ---- the types of identifiers assumed... ---- \n\n";
             putStrLn $ PPrint.render $ pprintEnv (importVarEnv' `joinEnv` ca )}




    let localVarEnv = tiProgram
                moduleName                     -- name of the module
                sigEnv                         -- environment of type signatures
                kindInfo                       -- kind information about classes and type constructors
                cHierarchyWithInstances        -- class hierarchy with instances
                globalDConsEnv                 -- data constructor type environment
                (importVarEnv' `joinEnv` ca )  -- type environment
                program                        -- binding groups


    when (doDump dumps "types") $
         do {putStr "\n\n ---- the types of identifiers ---- \n\n";
             putStrLn $ PPrint.render $ pprintEnv localVarEnv}

    let mod = tidyModuleToHsModule renamedTidyModule

    let mi = ModuleInfo { varAssumps = localVarEnv, dconsAssumps = localDConsEnv,
                        classHierarchy = cHierarchyWithInstances, kinds = kindInfo, infixDecls = getInfixDecls mod,
                        tyconsMembers = getTyconsMembers mod, synonyms = tidyTyDecls tidyMod,
                        renamedModule =  [addDecls mod liftedInstances]}

    return mi
 -}
