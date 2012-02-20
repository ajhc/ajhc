-- this file contains the main driver for the core->core optimizations

module E.Main(processInitialHo,processDecls,compileWholeProgram,dumpRules) where

import Control.Exception
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import System.Mem
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified List

import Data.List
import DataConstructors
import Doc.PPrint
import E.Annotate(annotateDs,annotateCombs,annotateProgram)
import E.E
import E.Eta
import E.FromHs
import E.Inline
import E.LambdaLift
import E.LetFloat
import E.Lint
import E.Program
import E.Rules
import E.Traverse
import E.TypeAnalysis
import E.TypeCheck
import E.Values
import E.WorkerWrapper
import FrontEnd.Class(augmentClassHierarchy)
import FrontEnd.HsSyn
import FrontEnd.KindInfer
import FrontEnd.Tc.Module
import FrontEnd.Warning
import Grin.Show(render)
import Ho.Build
import Ho.Collected
import Info.Types
import Name.Id
import Name.Name
import Options
import Support.CanType(getType)
import Support.FreeVars
import Support.TempDir
import Support.Transform
import Util.Gen
import Util.Graph
import Util.Progress
import Util.SetLike as S
import qualified E.CPR
import qualified E.Demand as Demand(analyzeProgram)
import qualified E.SSimplify as SS
import qualified FlagDump as FD
import qualified FlagOpts as FO
import qualified Info.Info as Info
import qualified Stats

lamann _ nfo = return nfo
letann e nfo = return (annotateArity e nfo)
idann ps i nfo = return (props ps i nfo) where
    props :: IdMap Properties -> Id -> Info.Info -> Info.Info
    props ps i = case mlookup i ps of
        Just ps ->  modifyProperties (mappend ps)
        Nothing ->  id

processInitialHo ::
    CollectedHo       -- ^ current accumulated ho
    -> Ho             -- ^ new ho, freshly read from file
    -> IO CollectedHo -- ^ final combined ho data.
processInitialHo accumho aho = withStackStatus "processInitialHo" $ do
    let Rules rm = hoRules $ hoBuild aho
        newTVrs = fsts $ hoEs (hoBuild aho)
        (_,orphans) = spartition (\ (k,_) -> k `elem` map tvrIdent newTVrs) rm

    let fakeEntry = emptyComb { combRules = map ruleUpdate . concat $ values orphans }
        combs =  fakeEntry:[combRules_s (map ruleUpdate $ findWithDefault [] (tvrIdent t) rm) (bindComb (t,e))  | (t,e) <- hoEs (hoBuild aho) ]

    -- extract new combinators and processed rules
    let choCombinators' = fromList [ (combIdent c,c) | c <- runIdentity $ annotateCombs (choVarMap accumho) (\_ -> return) letann lamann combs]
        nrules = map ruleUpdate . combRules $ findWithDefault emptyComb emptyId choCombinators'
        reRule :: Comb -> Comb
        reRule comb = combRules_u f comb where
            f rs = List.union  rs [ x | x <- nrules, ruleHead x == combHead comb]

    let choCombs = sfilter (\(k,_) -> k /= emptyId) choCombinators'
    return $ updateChoHo mempty {
        choExternalNames = choExternalNames accumho `mappend` (fromList . map tvrIdent $ newTVrs),
        choCombinators = choCombs `mappend` fmap reRule (choCombinators accumho),
        choHoMap = Map.singleton (hoModuleGroup aho) aho `mappend` choHoMap accumho
        }

processDecls ::
    CollectedHo             -- ^ Collected ho
    -> Ho                   -- ^ preliminary haskell object  data
    -> TiData               -- ^ front end output
    -> IO (CollectedHo,Ho)  -- ^ (new accumulated ho, final ho for this modules)
processDecls cho ho' tiData = withStackStatus "processDecls" $  do
    -- some useful values
    let ho = choHo cho
        -- XXX typechecker drops foreign exports!
        decls  = tiDataDecls tiData ++ [ x | x@HsForeignExport {} <- originalDecls ]
        originalDecls =  concat [ hsModuleDecls  m | (_,m) <- tiDataModules tiData ]

    -- build datatables
    let derives = (collectDeriving originalDecls)
    let dataTable = toDataTable (getConstructorKinds (hoKinds $ hoTcInfo ho'))
            (tiAllAssumptions tiData) originalDecls (hoDataTable $ hoBuild ho)
        classInstances = deriveClasses (choCombinators cho) fullDataTable derives
        fullDataTable = dataTable `mappend` hoDataTable (hoBuild ho)
    wdump FD.Datatable $ putErrLn (render $ showDataTable dataTable)
    wdump FD.Derived $ do
        mapM_ print derives
        mapM_ (\ (v,lc) -> printCheckName'' fullDataTable v lc) classInstances
    -- initial program
    let prog = program {
            progDataTable = fullDataTable,
            progExternalNames = choExternalNames cho,
            progModule = head (fsts $ tiDataModules tiData)
            }
    -- Convert Haskell decls to E
    let allAssumps = (tiAllAssumptions tiData `mappend` hoAssumps (hoTcInfo ho))
        theProps = fromList [ (toId x,y) | (x,y) <- Map.toList $ tiProps tiData]
    ds' <- convertDecls tiData theProps
        (hoClassHierarchy $ hoTcInfo ho') allAssumps  fullDataTable decls
    processIOErrors
    let ds = classInstances ++ [ (v,lc) | (n,v,lc) <- ds', v `notElem` fsts classInstances ]
    -- Build rules from instances, specializations, and user specified rules and catalysts
    let augmentedClassHierarchy = hoClassHierarchy (hoTcInfo ho) `augmentClassHierarchy` hoClassHierarchy (hoTcInfo ho')
    instanceRules <- createInstanceRules fullDataTable augmentedClassHierarchy (ds `mappend` hoEs (hoBuild ho))
    userRules <- convertRules (progModule prog) tiData (hoClassHierarchy  $ hoTcInfo ho') allAssumps fullDataTable decls
    (nds,specializeRules) <- procAllSpecs fullDataTable (tiCheckedRules tiData) ds

    ds <- return $ ds ++ nds
    wdump FD.CoreInitial $
        mapM_ (\(v,lc) -> printCheckName'' fullDataTable v lc) ds
    ds <- annotateDs mempty (\_ nfo -> return nfo) (\_ nfo -> return nfo) (\_ nfo -> return nfo) ds
    wdump FD.CoreInitial $
        mapM_ (\(v,lc) -> printCheckName'' fullDataTable v lc) ds

    let rules@(Rules rules') = instanceRules `mappend` userRules `mappend` specializeRules

    dumpRules rules

    let seasoning = freeVars [ rs | (k,rs) <- toList rules', k `notMember` defined ] `intersection` defined
        defined = fromList $ map (tvrIdent . fst) ds :: IdSet

    -- our initial program
    prog <- return prog { progSeasoning = seasoning }
    Identity prog <- return $ programMapDs (\ (t,e) -> return (shouldBeExported (getExports $ hoTcInfo ho') t,e)) $ atomizeApps False (programSetDs ds prog)

    -- now we must attach rules to the existing chos, as well as the current ones
    let addRule c = case mlookup (combIdent c) rules' of
            Nothing -> c
            Just rs -> combRules_u (map ruleUpdate . List.union rs) c
    prog <- return $ progCombinators_u (map addRule) prog
    cho <- return $ updateChoHo $ choCombinators_u (fmap addRule) cho

    -- Here we substitute in all the original types, with rules and properties defined in the current module included
    prog <- return $ runIdentity $ annotateProgram (choVarMap cho) (idann theProps) letann lamann prog

    lintCheckProgram (putErrLn "LintPostProcess") prog

    let entryPoints = fromList . execWriter $ programMapDs_ (\ (t,_) -> when
            (getProperty prop_EXPORTED t || getProperty prop_INSTANCE t || getProperty prop_SPECIALIZATION t)  (tell [tvrIdent t])) prog
    prog <- return $ prog { progEntry = entryPoints `mappend` progSeasoning prog }

    lintCheckProgram (putErrLn "InitialLint") prog

    prog <- programPrune prog

    -- initial pass, performs
    -- eta expansion of definitons
    -- simplify
    -- type analysis
    -- floating outward
    -- simplify
    -- floating inward

    let sopt = SS.cacheSimpOpts SS.emptySimplifyOpts {
            SS.so_boundVars = choCombinators cho,
            SS.so_forwardVars = progSeasoning prog
            }
    let tparms = transformParms {
            transformPass = "PreInit",
            transformDumpProgress = verbose
            }

    -- quick float inward pass to inline once used functions and prune unused ones
    prog <- transformProgram tparms {
        transformCategory = "FloatInward",
        transformOperation = programFloatInward
        } prog

    putProgress "<"
    pr_r <- progressIONew (length $ programDecomposedDs prog) 25 '.'

    let fint mprog = do
        let names = pprint [ n | (n,_) <- programDs mprog]
        withStackStatus ("fint: " ++ names) $ do
        when coreMini $ putErrLn ("----\n" ++ names)
        let tparms = transformParms { transformPass = "Init", transformDumpProgress = coreMini }

        lintCheckProgram onerrNone mprog
        mprog <- evaluate $ etaAnnotateProgram mprog
        lintCheckProgram onerrNone mprog

        mprog <- simplifyProgram sopt "Init-One" coreMini mprog

        -- | this catches more static arguments if we wait until after the initial normalizing simplification pass
        mprog <- transformProgram tparms { transformSkipNoStats = True, transformCategory = "SimpleRecursive"
                                         , transformOperation = return . staticArgumentTransform } mprog

        mprog <- transformProgram tparms { transformCategory = "FloatOutward", transformOperation = floatOutward } mprog
        mprog <- transformProgram tparms { transformCategory = "typeAnalyze", transformPass = "PreInit"
                                         , transformOperation = typeAnalyze True } mprog

        mprog <- transformProgram tparms { transformCategory = "FloatOutward", transformOperation = floatOutward } mprog

        -- perform another supersimplify in order to substitute the once used
        -- variables back in and replace the variable of case of variables with
        -- the default binding of the case statement.
        mprog <- simplifyProgram sopt "Init-Two-FloatOutCleanup" coreMini mprog
        mprog <- transformProgram tparms { transformCategory = "typeAnalyze", transformOperation = typeAnalyze True } mprog

        mprog <- transformProgram tparms { transformCategory = "FloatInward", transformOperation = programFloatInward } mprog
        mprog <- Demand.analyzeProgram mprog
        lintCheckProgram onerrNone mprog
        mprog <- simplifyProgram sopt "Init-Three-AfterDemand" False mprog
        when miniCorePass $ printProgram mprog -- mapM_ (\ (v,lc) -> printCheckName'' fullDataTable v lc) (programDs mprog)
        when miniCoreSteps $ Stats.printLStat (optStatLevel options) ("InitialOptimize:" ++ names) (progStats mprog)
        wdump FD.Progress $ let SubProgram isRec = progType mprog in  progressIOSteps pr_r (if isRec then "*" else ".")
        return mprog
    lintCheckProgram onerrNone prog
    prog <- programMapProgGroups mempty fint prog

    wdump FD.Stats $
        Stats.printLStat (optStatLevel options) "Initial Pass Stats" (progStats prog)
    lintCheckProgram onerrNone prog

    prog <- etaExpandProg "Init-Big-One" prog { progStats = mempty }
    prog <- transformProgram tparms {
        transformPass = "Init-Big-One",
        transformCategory = "FloatInward",
        transformOperation = programFloatInward
        } prog

    prog <- Demand.analyzeProgram prog
    prog <- simplifyProgram' sopt "Init-Big-One" verbose (IterateMax 4) prog

    wdump FD.Stats $
        Stats.printLStat (optStatLevel options) "Init-Big-One Stats" (progStats prog)

    pr_r <- progressIONew (length $ programDecomposedCombs prog) 25 '.'

    -- This is the main function that optimizes the routines before writing them out
    let optWW mprog = do
        let names = pprint [ n | (n,_) <- programDs mprog]
        liftIO $ when coreMini $ putErrLn ("----\n" ++ names)
        smap <- get
        let tparms = transformParms { transformPass = "OptWW", transformDumpProgress = coreMini }
            sopt = SS.cacheSimpOpts SS.emptySimplifyOpts {
                SS.so_boundVars = smap,
                SS.so_forwardVars = progSeasoning mprog
                }

        mprog <- simplifyProgram sopt "Simplify-One" coreMini mprog
        mprog <- transformProgram tparms { transformCategory = "FloatInward", transformOperation = programFloatInward } mprog
        mprog <- Demand.analyzeProgram mprog
        mprog <- simplifyProgram sopt "Simplify-Two" coreMini mprog
        mprog <- transformProgram tparms { transformCategory = "FloatInward", transformOperation = programFloatInward } mprog
        mprog <- Demand.analyzeProgram mprog
        mprog <- return $ E.CPR.cprAnalyzeProgram mprog
        mprog' <- transformProgram tparms { transformSkipNoStats = True, transformCategory = "WorkWrap", transformOperation = return . workWrapProgram } mprog
        let wws = length (programDs mprog') - length (programDs mprog)
--        liftIO $ wdump FD.Progress $ putErr (replicate wws 'w')
        mprog <- return mprog'

        mprog <- simplifyProgram sopt "Simplify-Three" coreMini mprog

        --mprog <- transformProgram tparms { transformCategory = "FloatInward", transformOperation = programFloatInward } mprog
        --mprog <- Demand.analyzeProgram mprog
        --mprog <- return $ E.CPR.cprAnalyzeProgram mprog
        --mprog <- transformProgram tparms { transformSkipNoStats = True, transformCategory = "WorkWrap2", transformOperation = return . workWrapProgram } mprog
        --mprog <- simplifyProgram sopt "Simplify-Four" coreMini mprog

        -- annotate our bindings for further passes
        mprog <- return $ etaAnnotateProgram mprog
        mprog <- Demand.analyzeProgram mprog
        mprog <- return $ E.CPR.cprAnalyzeProgram mprog

        put $ fromList [ (combIdent c,c) | c <- progCombinators mprog] `S.union` smap

        --liftIO $ wdump FD.Progress $ let SubProgram rec = progType mprog in  putErr (if rec then "*" else ".")
        liftIO $ wdump FD.Progress $ let SubProgram isRec = progType mprog in  progressIOSteps pr_r (if wws > 0 then "w" else if isRec then "*" else ".")
        return mprog

    prog <- evalStateT (programMapProgGroups mempty optWW prog { progStats = mempty }) (SS.so_boundVars sopt)
    putProgressLn ">"
    wdump FD.Stats $
        Stats.printLStat (optStatLevel options) "MainPass Stats" (progStats prog)

    processIOErrors

    lintCheckProgram (putErrLn "After the workwrap/CPR") prog

    prog <- programPrune prog

    lintCheckProgram (putErrLn "After the Opimization") prog
    wdump FD.Core $ printProgram prog

    let newHoBuild = (hoBuild ho') {
        hoDataTable = dataTable,
        hoEs = programDs prog,
        hoRules = hoRules (hoBuild ho') `mappend` rules
        }
        newMap = fmap (\c -> Just (EVar $ combHead c)) $ progCombMap prog
    return (updateChoHo $ mempty {
        choHoMap = Map.singleton (hoModuleGroup ho') ho' { hoBuild = newHoBuild},
        choCombinators = fromList $ [ (combIdent c,c) | c <- progCombinators prog ],
        choExternalNames = idMapToIdSet newMap
        } `mappend` cho,ho' { hoBuild = newHoBuild })

coreMini = dump FD.CoreMini
corePass = dump FD.CorePass
coreSteps = dump FD.CoreSteps
miniCorePass = coreMini && corePass
miniCoreSteps = coreMini && coreSteps

dumpRules rules = do
    wdump FD.Rules $ putStrLn "  ---- user rules ---- " >> printRules RuleUser rules
    wdump FD.Rules $ putStrLn "  ---- user catalysts ---- " >> printRules RuleCatalyst rules
    wdump FD.RulesSpec $ putStrLn "  ---- specializations ---- " >> printRules RuleSpecialization rules

programPruneUnreachable :: Program -> Program
programPruneUnreachable prog = progCombinators_s ds' prog where
    ds' = reachableFrom combIdent freeVars (progCombinators prog) (toList $ progEntry prog)

programPrune :: Program -> IO Program
programPrune prog = transformProgram transformParms { transformCategory = "PruneUnreachable"
                                                    , transformDumpProgress  = miniCorePass
                                                    , transformOperation = evaluate . programPruneUnreachable } prog

etaExpandProg :: String -> Program -> IO Program
etaExpandProg pass prog = do
    let f prog = prog' { progStats = progStats prog `mappend` stats } where
        (prog',stats) = Stats.runStatM $  etaExpandProgram prog
    transformProgram transformParms { transformPass = pass, transformCategory = "EtaExpansion"
                                    , transformDumpProgress = miniCorePass,  transformOperation = evaluate . f } prog

getExports ho =  Set.fromList $ map toId $ concat $  Map.elems (hoExports ho)
shouldBeExported exports tvr
    | tvrIdent tvr `Set.member` exports || getProperty prop_SRCLOC_ANNOTATE_FUN tvr  = setProperty prop_EXPORTED tvr
    | otherwise = tvr

transTypeAnalyze = transformParms { transformCategory = "typeAnalyze",  transformOperation = typeAnalyze True }

simplifyProgram sopt name dodump prog = liftIO $ do
    let istat = progStats prog
    let g prog = do
            let nprog = SS.programPruneOccurance prog
            when (corePass && dodump) $ do
                putStrLn "-- After Occurance Analysis"
                printProgram nprog
            lintCheckProgram (putErrLn "AfterOccurance") nprog
            return $ SS.programSSimplify sopt nprog
    prog <- transformProgram transformParms { transformCategory = "Simplify"
                                            , transformPass = name
                                            , transformIterate = IterateDone
                                            , transformDumpProgress = dodump
                                            , transformOperation = g } prog { progStats = mempty }
    when (dodump && (dump FD.Progress || coreSteps)) $
        Stats.printLStat (optStatLevel options) ("Total: " ++ name) (progStats prog)
    return prog { progStats = progStats prog `mappend` istat }

{-
simplifyProgramPStat sopt name dodump prog = do
    let istat = progStats prog
    let g =  SS.programSSimplifyPStat sopt { SS.so_dataTable = progDataTable prog } . SS.programPruneOccurance
    prog <- transformProgram ("PS:" ++ name) IterateDone dodump g prog  { progStats = mempty }
    when ((dodump && dump FD.Progress) || dump FD.CoreSteps) $ Stats.printStat ("Total: " ++ name) (progStats prog)
    return prog { progStats = progStats prog `mappend` istat }
-}
simplifyProgram' sopt name dodump iterate prog = do
    let istat = progStats prog
    let g =  return . SS.programSSimplify sopt . SS.programPruneOccurance
    prog <- transformProgram transformParms { transformCategory = "Simplify"
                                            , transformPass = name
                                            , transformIterate = iterate
                                            , transformDumpProgress = dodump
                                            , transformOperation = g } prog { progStats = mempty }
    when (dodump && (dump FD.Progress || coreSteps)) $ Stats.printLStat (optStatLevel options) ("Total: " ++ name) (progStats prog)
    return prog { progStats = progStats prog `mappend` istat }

{-# NOINLINE compileWholeProgram #-}
compileWholeProgram prog = do
    performGC

    when verbose $ do
        Stats.print "PassStats" Stats.theStats
        Stats.clear Stats.theStats

    esmap <- programEsMap prog
    let mainFunc = parseName Val (maybe "Main.main" snd (optMainFunc options))
        dataTable = progDataTable prog
        ffiExportNames = [ tv | tv <- map combHead $ progCombinators prog, name <- tvrName tv, "FE@" `isPrefixOf` show name ]
    (main,mainv) <- getMainFunction dataTable mainFunc esmap
    prog <- return $ programUpdate prog {
        progMain   = tvrIdent main,
        progEntry = fromList $ map tvrIdent (main:ffiExportNames),
        progCombinators = emptyComb { combHead = main, combBody = mainv }:map (unsetProperty prop_EXPORTED) (progCombinators prog)
        }
    prog <- transformProgram transformParms {
        transformCategory = "PruneUnreachable",
        transformOperation = evaluate . programPruneUnreachable
        } prog

    prog <- if (fopts FO.TypeAnalysis) then do
      transformProgram transformParms { transformCategory = "TypeAnalyzeMethods",
                                        transformOperation = typeAnalyze False,
                                        transformDumpProgress = dump FD.Progress }
                       prog
            else return prog

    when verbose $ do
        putStrLn "Type analyzed methods"
        flip mapM_ (sortUnder (show . fst) (programDs prog)) $ \ (t,e) -> do
        let (_,ts) = fromLam e
            ts' = takeWhile (sortKindLike . getType) ts
        when (not (null ts')) $ putStrLn $ (pprint t) ++ " \\" ++
            concat [ "(" ++ show  (Info.fetch (tvrInfo t) :: Typ) ++ ")" | t <- ts' ]
    lintCheckProgram onerrNone prog
    prog <- programPrune prog

    cmethods <- do
        let es' = concatMap expandPlaceholder (progCombinators prog)
        es' <- return [ combBody_u floatInward e |  e <- es' ]
        wdump FD.Class $ do
            sequence_ [ printCheckName'' dataTable (combHead x) (combBody x) |  x <- es']
        return es'

    prog <- evaluate $ progCombinators_s ([ p | p <- progCombinators prog,
        combHead p `notElem` map combHead cmethods] ++ cmethods) prog
    prog <- annotateProgram mempty (\_ nfo -> return $ unsetProperty prop_INSTANCE nfo)
        letann (\_ nfo -> return nfo) prog

    if not (fopts FO.GlobalOptimize) then do
        prog <- programPrune prog
        wdump FD.CoreBeforelift $ printProgram prog
        prog <- transformProgram transformParms {
            transformCategory = "LambdaLift",
            transformDumpProgress = dump FD.Progress,
            transformOperation = lambdaLift } prog
        wdump FD.CoreAfterlift $ printProgram prog
        prog <- return $ atomizeApps True prog
        wdump FD.CoreMangled $ dumpCore "mangled" prog
        return prog
     else do
    prog <- transformProgram transTypeAnalyze {
        transformPass = "Main-AfterMethod",
        transformDumpProgress = verbose } prog
    prog <- simplifyProgram SS.emptySimplifyOpts "Main-One" verbose prog
    prog <- etaExpandProg "Main-AfterOne" prog
    wdump FD.CorePass $ dumpCore "before-TypeAnalyze-Main-AfterSimp" prog
    prog <- transformProgram transTypeAnalyze {
        transformPass = "Main-AfterSimp", transformDumpProgress = verbose } prog
    prog <- simplifyProgram SS.emptySimplifyOpts "Main-Two" verbose prog

    -- run optimization again with no rules enabled
    prog <- return $ runIdentity $ annotateProgram mempty (\_ nfo -> return $
        modifyProperties (flip (foldr S.delete) [prop_HASRULE,prop_WORKER]) nfo)
        letann (\_ -> return) prog
    --prog <- transformProgram "float inward" DontIterate True programFloatInward prog

    prog <- simplifyProgram SS.emptySimplifyOpts { SS.so_finalPhase = True }
        "SuperSimplify no rules" verbose prog

    -- We should float inward right before lambda lifting so that when a case statement is lifted out, it takes any local definitions with it.
--    prog <- transformProgram transformParms {
--        transformCategory = "FloatInward",
--        transformDumpProgress = dump FD.Progress,
--        transformOperation = programFloatInward
--        } prog
    -- perform lambda lifting
--    prog <- denewtypeProgram prog

    prog <- transformProgram transformParms {
        transformCategory = "BoxifyProgram",
        transformDumpProgress = dump FD.Progress,
        transformOperation = boxifyProgram } prog
    prog <- programPrune prog

    prog <- Demand.analyzeProgram prog
    prog <- return $ E.CPR.cprAnalyzeProgram prog
    prog <- transformProgram transformParms {
        transformCategory = "Boxy WorkWrap",
        transformDumpProgress = dump FD.Progress,
        transformOperation = evaluate . workWrapProgram } prog
    prog <- simplifyProgram SS.emptySimplifyOpts { SS.so_finalPhase = True }
        "SuperSimplify after Boxy WorkWrap" verbose prog
    prog <- return $ runIdentity $ programMapBodies (return . cleanupE) prog

    wdump FD.CoreBeforelift $ dumpCore "before-lift" prog
    prog <- transformProgram transformParms {
        transformCategory = "LambdaLift",
        transformDumpProgress = dump FD.Progress,
        transformOperation = lambdaLift } prog

    wdump FD.CoreAfterlift $ dumpCore "after-lift" prog

    finalStats <- Stats.new

    -- final optimization pass to clean up lambda lifting droppings
--    prog <- flip programMapBodies prog $ \ e -> do
--        let cm stats e = do
--            let sopt = mempty {  SS.so_dataTable = dataTable }
--            let (stat, e') = SS.simplifyE sopt e
--            Stats.tickStat stats stat
--            return e'
--        doopt (mangle' Nothing dataTable) False finalStats "PostLambdaLift"  cm e
--    wdump FD.Progress $ Stats.print "PostLifting" finalStats

    lintCheckProgram (putErrLn "LintPostLifting") prog

--    wdump FD.Progress $ printEStats (programE prog)

    prog <- Demand.analyzeProgram prog
    prog <- return $ E.CPR.cprAnalyzeProgram prog
    prog <- simplifyProgram SS.emptySimplifyOpts {
        SS.so_postLift = True, SS.so_finalPhase = True } "PostLiftSimplify" verbose prog
--    prog <- programFloatInward prog

    when verbose $ do
        Stats.print "PassStats" Stats.theStats
        Stats.clear Stats.theStats

    prog <- return $ atomizeApps True prog
    wdump FD.CoreMangled $ dumpCore "mangled" prog
    return prog

-- | this gets rid of all type variables, replacing them with boxes that can hold any type.
-- The program is still type-safe, but all polymorphism has been removed in favor of
-- implicit coercion to a universal type in preparation for the grin transformation.

boxifyProgram :: Program -> IO Program
boxifyProgram prog = ans where
    ans = do programMapDs f (progCombinators_u (map $ combRules_s []) prog)
    f (t,e) = do
        e <- return $ runReader (g e) Set.empty
        tt <- return $ runReader (boxify (tvrType t)) Set.empty
        return (t {tvrType = tt},e)
    _tv t = t { tvrType = boxify (tvrType t) }
    g e = do
        emapEG g (boxify) e -- (\e -> do putStrLn ("box: " ++ pprint e) ; return $ boxify e) e
--    boxify t | Just e <- followAlias (progDataTable prog) t = boxify e
    boxify (from_unsafeCoerce -> Just (e,t)) = do
        t' <- boxify t
        e' <- boxify e
        case typesCompatable t' (getType e') of
            Just () -> return e
            _ -> return $ prim_unsafeCoerce e' t'
    boxify (EPi t e) = local (Set.insert (tvrIdent t)) $ do
        nt <- boxify $ tvrType t
        ne <- boxify e
        return $ EPi t { tvrType = nt } ne
    boxify v@(EVar vr) | canBeBox v = do
        s <- ask
        if tvrIdent vr `Set.member` s then return v else return $ mktBox (tvrType vr)
    boxify (ELit lc) = do
        na <- mapM boxify (litArgs lc)
        return $ ELit lc { litArgs = na }
--    boxify v@(EAp _ _) | canBeBox v = mktBox (getType v)
--    boxify (EAp (ELam t b) e) = boxify (subst t e b)
 --   boxify (EAp a b) = EAp (boxify a) b -- TODO there should be no applications at the type level by now (boxify b)
    boxify (EAp a b) = liftM2 eAp (boxify a) (boxify b)
    boxify s@ESort {} = return s
    boxify x = error $ "boxify: " ++ show x

-- | get rid of unused bindings
cleanupE :: E -> E
cleanupE e = runIdentity (f e) where
    f (ELam t@TVr { tvrIdent = v } e) | v /= emptyId, v `notMember` freeIds e = f (ELam t { tvrIdent = emptyId } e)
    f (EPi t@TVr { tvrIdent = v } e) | v /= emptyId, v `notMember` freeIds e = f (EPi t { tvrIdent = emptyId } e)
    f ec@ECase { eCaseBind = t@TVr { tvrIdent = v } } | v /= emptyId, v `notMember` (freeVars (caseBodies ec)::IdSet) = f ec { eCaseBind = t { tvrIdent = emptyId } }
    f e = emapEG f f e
