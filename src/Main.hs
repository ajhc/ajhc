module Main(main) where

import Control.Exception
import Control.Monad.Identity
import Control.Monad.Writer
import Control.Monad.State
import Directory
import IO(hFlush,stderr,stdout)
import Prelude hiding(putStrLn, putStr,print)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified System
import List as L
import System.Mem

import Doc.PPrint
import Util.Util
import CharIO
import DataConstructors
import E.Annotate(annotateDs,annotateCombs,annotateProgram)
import E.Diff
import E.E
import E.Eta
import E.Lint
import E.FreeVars
import E.FromHs
import E.Inline
import E.LambdaLift
import E.LetFloat
import E.Program
import E.Rules
import E.Subst(subst)
import E.Traverse
import E.TypeAnalysis
import E.TypeCheck
import E.WorkerWrapper
import FrontEnd.Class
import FrontEnd.FrontEnd
import FrontEnd.KindInfer(getConstructorKinds)
import GenUtil hiding(replicateM,putErrLn,putErr,putErrDie)
import Grin.DeadCode
import Grin.Devolve(twiddleGrin,devolveTransform)
import Grin.EvalInline(createEvalApply)
import Grin.FromE
import Grin.Grin
import Grin.Lint
import Grin.NodeAnalyze
import Grin.Optimize
import Grin.Show
import Ho.Build
import Ho.Library
import Ho.Collected
import FrontEnd.HsSyn
import Info.Types
import Name.Id
import Name.Name
import Options
import Support.FreeVars
import Support.CanType(getType)
import Support.Transform
import Util.Graph
import Util.Progress
import Util.SetLike as S
import Version.Version(versionString,versionContext,versionSimple)
import qualified Version.Config as VC
import qualified C.FromGrin2 as FG2
import qualified E.CPR
import qualified E.Demand as Demand(analyzeProgram)
import qualified E.SSimplify as SS
import qualified FlagDump as FD
import qualified FlagOpts as FO
import qualified Grin.Simplify
import qualified Grin.SSimplify
import qualified Info.Info as Info
import qualified Interactive
import qualified Stats
import qualified IO


---------------
-- ∀α∃β . α → β
---------------

progressM c  = wdump FD.Progress $ (c >>= putErrLn) >> hFlush stderr


collectPassStats = verbose

bracketHtml action = do
    (argstring,_) <- getArgString
    wdump FD.Html $ putStrLn $ "<html><head><title>" ++ argstring ++ "</title><meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\"></head><body style=\"background: black; color: lightgrey\"><pre>"
    action `finally` (wdump FD.Html $ putStrLn "</pre></body></html>")


main =  runMain $ bracketHtml $ do
    o <- processOptions
    progressM $ do
        (argstring,_) <- getArgString
        return (argstring ++ "\n" ++ versionSimple)
    case optMode o of
        BuildHl hl    -> makeLibrary processInitialHo processDecls hl
        ListLibraries -> do
            when (optVerbose options > 0) $ do
                putStrLn "Search path:"
                mapM_ putStrLn (optHlPath options)
                putStrLn "Libraries found:"
            ll <- libraryList
            sequence_ [ putStrLn name | (name,_) <- ll ]
        ShowHo ho       -> dumpHoFile ho
        Version         -> putStrLn versionString
        PrintHscOptions -> putStrLn $ "-I" ++ VC.datadir ++ "/" ++ VC.package ++ "-" ++ VC.shortVersion ++ "/include"
        DependencyTree  -> doDependency (optArgs o)
        VersionCtx      -> putStrLn (versionString ++ versionContext)
        _               -> processFiles  (optArgs o)


processFiles :: [String] -> IO ()
processFiles cs = f cs (optMainFunc options) where
    f [] Nothing  = do
        int <- Interactive.isInteractive
        when (not int) $ putErrDie "jhc: no input files"
        g [Left (Module "Prelude")]
    f [] (Just (b,m)) = do
        m <- getModule (parseName Val m)
        g [Left m]
    f cs _ = g (map fileOrModule cs)
    g fs =  processCollectedHo =<< parseFiles fs processInitialHo processDecls
    fileOrModule f = case reverse f of
       ('s':'h':'.':_)     -> Right f
       ('s':'h':'l':'.':_) -> Right f
       _                   -> Left $ Module f



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
processInitialHo accumho aho = do
    let Rules rm = hoRules $ hoBuild aho
        newTVrs = fsts $ hoEs (hoBuild aho)
        (_,orphans) = mpartitionWithKey (\k _ -> k `elem` map tvrIdent newTVrs) rm

    let fakeEntry = emptyComb { combRules = map ruleUpdate . concat $ melems orphans }
        combs =  fakeEntry:[combRules_s (map ruleUpdate $ mfindWithDefault [] (tvrIdent t) rm) (bindComb (t,e))  | (t,e) <- hoEs (hoBuild aho) ]

    -- extract new combinators and processed rules
    let choCombinators' = fromList [ (combIdent c,c) | c <- runIdentity $ annotateCombs (choVarMap accumho) (\_ -> return) letann lamann combs]
        nrules = map ruleUpdate . combRules $ mfindWithDefault emptyComb emptyId choCombinators'
        reRule :: Comb -> Comb
        reRule comb = combRules_u f comb where
            f rs = L.union  rs [ x | x <- nrules, ruleHead x == combHead comb]

    let finalVarMap = mappend (fromList [(tvrIdent tvr,Just $ EVar tvr) | tvr <- map combHead $ melems choCombs ]) (choVarMap accumho)
        choCombs = mfilterWithKey (\k _ -> k /= emptyId) choCombinators'
        --(mod:_) = Map.keys $ hoExports $ hoTcInfo aho
    return $ mempty {
        choVarMap = finalVarMap,
        choExternalNames = choExternalNames accumho `mappend` (fromList . map tvrIdent $ newTVrs),
        choCombinators = choCombs `mappend` fmap reRule (choCombinators accumho),
        choHoMap = Map.singleton (show $ hoModuleGroup aho) aho `mappend` choHoMap accumho
        }




-- | this is called on parsed, typechecked haskell code to convert it to the internal representation

coreMini = dump FD.CoreMini
corePass = dump FD.CorePass
coreSteps = dump FD.CoreSteps
miniCorePass = coreMini && corePass
miniCoreSteps = coreMini && coreSteps


processDecls ::
    CollectedHo          -- ^ Collected ho
    -> Ho                   -- ^ preliminary haskell object  data
    -> TiData               -- ^ front end output
    -> IO (CollectedHo,Ho)  -- ^ (new accumulated ho, final ho for this modules)
processDecls cho ho' tiData = do
    -- some useful values
    let allHo = ho `mappend` ho'
        ho = choHo cho
        htc = hoTcInfo ho
        -- XXX typechecker drops foreign exports!
        decls  = tiDataDecls tiData ++ [ x | x@HsForeignExport {} <- originalDecls ]
        originalDecls =  concat [ hsModuleDecls  m | (_,m) <- tiDataModules tiData ]

    -- build datatables
    let dataTable = toDataTable (getConstructorKinds (hoKinds $ hoTcInfo ho')) (tiAllAssumptions tiData) originalDecls (hoDataTable $ hoBuild ho)
        classInstances = deriveClasses (choCombinators cho) dataTable
        fullDataTable = dataTable `mappend` hoDataTable (hoBuild ho)
    wdump FD.Datatable $ putErrLn (render $ showDataTable dataTable)

    wdump FD.Derived $
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
    ds' <- convertDecls tiData theProps (hoClassHierarchy $ hoTcInfo ho') allAssumps  fullDataTable decls
    let ds = [ (v,e) | (v,e) <- classInstances ] ++  [ (v,lc) | (n,v,lc) <- ds', v `notElem` fsts classInstances ]
 --   sequence_ [lintCheckE onerrNone fullDataTable v e | (_,v,e) <- ds ]

    -- Build rules from instances, specializations, and user specified rules and catalysts
    instanceRules <- createInstanceRules fullDataTable (hoClassHierarchy $ hoTcInfo ho')  (ds `mappend` hoEs (hoBuild ho))
    userRules <- convertRules (progModule prog) tiData (hoClassHierarchy  $ hoTcInfo ho') allAssumps fullDataTable decls
    (nds,specializeRules) <- procAllSpecs fullDataTable (tiCheckedRules tiData) ds

    ds <- return $ ds ++ nds
    wdump FD.CoreInitial $
        mapM_ (\(v,lc) -> printCheckName'' fullDataTable v lc) ds
    ds <- annotateDs mempty (\_ nfo -> return nfo) (\_ nfo -> return nfo) (\_ nfo -> return nfo) ds
    wdump FD.CoreInitial $
        mapM_ (\(v,lc) -> printCheckName'' fullDataTable v lc) ds

    let rules@(Rules rules') = instanceRules `mappend` userRules `mappend` specializeRules

    wdump FD.Rules $ putStrLn "  ---- user rules ---- " >> printRules RuleUser rules
    wdump FD.Rules $ putStrLn "  ---- user catalysts ---- " >> printRules RuleCatalyst rules
    wdump FD.RulesSpec $ putStrLn "  ---- specializations ---- " >> printRules RuleSpecialization rules

    let seasoning = freeVars [ rs | (k,rs) <- massocs rules', k `notMember` defined ] `intersection` defined
        defined = fromList $ map (tvrIdent . fst) ds :: IdSet

    -- our initial program
    prog <- return prog { progSeasoning = seasoning }
    Identity prog <- return $ programMapDs (\ (t,e) -> return (shouldBeExported (getExports $ hoTcInfo ho') t,e)) $ atomizeApps False (programSetDs ds prog)

    -- now we must attach rules to the existing chos, as well as the current ones
    let addRule c = case mlookup (combIdent c) rules' of
            Nothing -> c
            Just rs -> combRules_u (map ruleUpdate . L.union rs) c
    prog <- return $ progCombinators_u (map addRule) prog
    cho <- return $ choCombinators_u (fmap addRule) cho

    -- Here we substitute in all the original types, with rules and properties defined in the current module included
    prog <- return $ runIdentity $ annotateProgram (choVarMap cho) (idann theProps) letann lamann prog

    lintCheckProgram (putErrLn "LintPostProcess") prog



    let entryPoints = fromList . execWriter $ programMapDs_ (\ (t,_) -> when (getProperty prop_EXPORTED t || getProperty prop_INSTANCE t || getProperty prop_SPECIALIZATION t)  (tell [tvrIdent t])) prog
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
        when coreMini $ putErrLn ("----\n" ++ names)
        let tparms = transformParms { transformPass = "Init", transformDumpProgress = coreMini }


        mprog <- evaluate $ etaAnnotateProgram mprog

        mprog <- simplifyProgram sopt "Init-One" coreMini mprog

        -- | this catches more static arguments if we wait until after the initial normalizing simplification pass
        mprog <- transformProgram tparms { transformSkipNoStats = True, transformCategory = "SimpleRecursive"
                                         , transformOperation = return . staticArgumentTransform } mprog

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
        --wdump FD.Progress $ let SubProgram rec = progType mprog in  putErr (if rec then "*" else ".")
        wdump FD.Progress $ let SubProgram rec = progType mprog in  progressIOSteps pr_r (if rec then "*" else ".")
        return mprog
    lintCheckProgram onerrNone prog
    --putProgressLn "Initial optimization pass"

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

    pr_r <- progressIONew (length $ programDecomposedDs prog) 25 '.'

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
        liftIO $ wdump FD.Progress $ let SubProgram rec = progType mprog in  progressIOSteps pr_r (if wws > 0 then "w" else if rec then "*" else ".")
        return mprog

    prog <- evalStateT (programMapProgGroups mempty optWW prog { progStats = mempty }) (SS.so_boundVars sopt)
    putProgressLn ">"
    wdump FD.Stats $
        Stats.printLStat (optStatLevel options) "MainPass Stats" (progStats prog)

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
    return (mempty {
        choHoMap = Map.singleton (show $ hoModuleGroup ho') ho' { hoBuild = newHoBuild},
        choCombinators = fromList $ [ (combIdent c,c) | c <- progCombinators prog ],
        choExternalNames = idMapToIdSet newMap,
        choVarMap = newMap
        } `mappend` cho,ho' { hoBuild = newHoBuild })

programPruneUnreachable :: Program -> Program
programPruneUnreachable prog = progCombinators_s ds' prog where
    ds' = reachable (newGraph (progCombinators prog) combIdent freeVars) (toList $ progEntry prog)

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


--idHistogram e = execWriter $ annotate mempty (\id nfo -> tell (Histogram.singleton id) >> return nfo) (\_ -> return) (\_ -> return) e

transTypeAnalyze = transformParms { transformCategory = "typeAnalyze",  transformOperation = typeAnalyze True }

processCollectedHo cho = do
    if optMode options == CompileHo then return () else do
    putProgressLn "Collected Compilation..."

    when (dump FD.ClassSummary) $ do
        putStrLn "  ---- class summary ---- "
        printClassSummary (choClassHierarchy cho)
    when (dump FD.Class) $ do
        putStrLn "  ---- class hierarchy ---- "
        printClassHierarchy (choClassHierarchy cho)

    let dataTable = choDataTable cho
        combinators = melems $ choCombinators cho

    evaluate dataTable
    evaluate combinators

    let prog = programUpdate program {
            progCombinators = combinators,
            progDataTable = dataTable
            }
        rules' = Rules $ fromList [ (combIdent x,combRules x) | x <- combinators, not $ null (combRules x) ]
    -- dump final version of various requested things
    wdump FD.Datatable $ putErrLn (render $ showDataTable dataTable)
    wdump FD.DatatableBuiltin $ putErrLn (render $ showDataTable samplePrimitiveDataTable)
    wdump FD.Rules $ putStrLn "  ---- user rules ---- " >> printRules RuleUser rules'
    wdump FD.Rules $ putStrLn "  ---- user catalysts ---- " >> printRules RuleCatalyst rules'
    wdump FD.RulesSpec $ putStrLn "  ---- specializations ---- " >> printRules RuleSpecialization rules'

    -- enter interactive mode
    int <- Interactive.isInteractive
    if int then Interactive.interact cho else compileWholeProgram prog


{-# NOINLINE compileWholeProgram #-}
compileWholeProgram prog = do
    performGC

    let dataTable = progDataTable prog

    when collectPassStats $ do
        Stats.print "PassStats" Stats.theStats
        Stats.clear Stats.theStats


    let mainFunc = parseName Val (maybe "Main.main" snd (optMainFunc options))
    esmap <- programEsMap prog
    (main,mainv) <- getMainFunction dataTable mainFunc esmap
    let ffiExportNames = [tv | tv <- map combHead $  progCombinators prog,
                               name <- tvrName tv,
                               "FE@" `isPrefixOf` show name]
    prog <- return prog { progMain   = tvrIdent main,
                          progEntry = fromList $ map tvrIdent (main:ffiExportNames),
                          progCombinators = emptyComb { combHead = main, combBody = mainv }:map (unsetProperty prop_EXPORTED) (progCombinators prog)
                        }
    prog <- transformProgram transformParms { transformCategory = "PruneUnreachable", transformOperation = evaluate . programPruneUnreachable } prog

--    (viaGhc,fn,_,_) <- determineArch
--    wdump FD.Progress $ putStrLn $ "Arch: " ++ fn

    --wdump FD.Core $ printProgram prog

    prog <- if (fopts FO.TypeAnalysis) then do
      transformProgram transformParms { transformCategory = "typeAnalyzeMethods",
                                        transformOperation = typeAnalyze False,
                                        transformDumpProgress = dump FD.Progress }
                       prog
            else return prog

    when (verbose) $ do putStrLn "Type analyzed methods"
                        flip mapM_ (programDs prog) $ \ (t,e) -> do
                        let (_,ts) = fromLam e
                            ts' = takeWhile (sortKindLike . getType) ts
                        when (not (null ts')) $ putStrLn $ (pprint t) ++ " \\" ++ concat [ "(" ++ show  (Info.fetch (tvrInfo t) :: Typ) ++ ")" | t <- ts' ]
    lintCheckProgram onerrNone prog
    prog <- programPrune prog
    --wdump FD.Core $ printProgram prog

    cmethods <- do
        let es' = concatMap expandPlaceholder (progCombinators prog)
        es' <- return [ combBody_u floatInward e |  e <- es' ]
        wdump FD.Class $ do
            sequence_ [ printCheckName'' dataTable (combHead x) (combBody x) |  x <- es']
        return es'

    prog <- evaluate $ progCombinators_s ([ p | p <- progCombinators prog, combHead p `notElem` map combHead cmethods] ++ cmethods) prog


    prog <- annotateProgram mempty (\_ nfo -> return $ unsetProperty prop_INSTANCE nfo) letann (\_ nfo -> return nfo) prog


    unless (fopts FO.GlobalOptimize) $ do
        prog <- programPrune prog
        wdump FD.CoreBeforelift $ printProgram prog
        prog <- transformProgram transformParms { transformCategory = "LambdaLift", transformDumpProgress = dump FD.Progress, transformOperation = lambdaLift } prog
        wdump FD.CoreAfterlift $ printProgram prog -- printCheckName dataTable (programE prog)
        compileToGrin prog
        exitSuccess


    prog <- transformProgram transTypeAnalyze { transformPass = "Main-AfterMethod", transformDumpProgress = verbose } prog


    prog <- simplifyProgram SS.emptySimplifyOpts "Main-One" verbose prog


    prog <- etaExpandProg "Main-AfterOne" prog
    prog <- transformProgram transTypeAnalyze { transformPass = "Main-AfterSimp", transformDumpProgress = verbose } prog


    prog <- simplifyProgram SS.emptySimplifyOpts "Main-Two" verbose prog


    -- run optimization again with no rules enabled

    -- delete rules
    prog <- return $ runIdentity $ annotateProgram mempty (\_ nfo -> return $ modifyProperties (flip (foldr S.delete) [prop_HASRULE,prop_WORKER]) nfo) letann (\_ -> return) prog
    --prog <- transformProgram "float inward" DontIterate True programFloatInward prog

    prog <- simplifyProgram SS.emptySimplifyOpts { SS.so_finalPhase = True } "SuperSimplify no rules" verbose prog


    -- We should float inward right before lambda lifting so that when a case statement is lifted out, it takes any local definitions with it.
--    prog <- transformProgram transformParms {
--        transformCategory = "FloatInward",
--        transformDumpProgress = dump FD.Progress,
--        transformOperation = programFloatInward
--        } prog
    -- perform lambda lifting
--    prog <- denewtypeProgram prog

    prog <- transformProgram transformParms { transformCategory = "BoxifyProgram", transformDumpProgress = dump FD.Progress, transformOperation = boxifyProgram } prog
    prog <- programPrune prog

    prog <- Demand.analyzeProgram prog
    prog <- return $ E.CPR.cprAnalyzeProgram prog
    prog <- transformProgram transformParms { transformCategory = "Boxy WorkWrap", transformDumpProgress = dump FD.Progress, transformOperation = evaluate . workWrapProgram } prog
    prog <- simplifyProgram SS.emptySimplifyOpts { SS.so_finalPhase = True } "SuperSimplify after Boxy WorkWrap" verbose prog
    prog <- return $ runIdentity $ programMapBodies (return . cleanupE) prog

--    when viaGhc $ do
 --       wdump FD.Core $ printProgram prog
 --       fail "Compiling to GHC currently disabled"
        --compileToHs prog
 --       exitSuccess

    wdump FD.CoreBeforelift $ dumpCore "before-lift" prog
    prog <- transformProgram transformParms { transformCategory = "LambdaLift", transformDumpProgress = dump FD.Progress, transformOperation = lambdaLift } prog

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

    wdump FD.Progress $ printEStats (programE prog)

    prog <- Demand.analyzeProgram prog
    prog <- return $ E.CPR.cprAnalyzeProgram prog
    prog <- simplifyProgram SS.emptySimplifyOpts { SS.so_postLift = True, SS.so_finalPhase = True } "PostLiftSimplify" verbose prog
--    prog <- programFloatInward prog

    when collectPassStats $ do
        Stats.print "PassStats" Stats.theStats
        Stats.clear Stats.theStats

    compileToGrin prog


-- | this gets rid of all type variables, replacing them with boxes that can hold any type
-- the program is still type-safe, but all polymorphism has been removed in favor of
-- implicit coercion to a universal type.
--
-- also, all rules are deleted.

boxifyProgram :: Program -> IO Program
boxifyProgram prog = ans where
    ans = do programMapDs f (progCombinators_u (map $ combRules_s []) prog)
    f (t,e) = do
--        putStrLn $ ">>> " ++ pprint t
        e <- g e
        return (tv t,e)
    tv t = t { tvrType = boxify (tvrType t) }
    g e = do
  --      putStrLn $ "g: " ++ pprint e
        emapEG g (return . boxify) e -- (\e -> do putStrLn ("box: " ++ pprint e) ; return $ boxify e) e
    boxify t | Just e <- followAlias (progDataTable prog) t = boxify e
    boxify (EPi t e) = EPi t { tvrType = boxify (tvrType t) } (boxify e)
    boxify v@EVar {} | canBeBox v = mktBox (getType v)
    boxify (ELit lc) = ELit lc { litArgs = map boxify (litArgs lc) }
--    boxify v@(EAp _ _) | canBeBox v = mktBox (getType v)
    boxify (EAp (ELam t b) e) = boxify (subst t e b)
 --   boxify (EAp a b) = EAp (boxify a) b -- TODO there should be no applications at the type level by now (boxify b)
    boxify (EAp a b) = EAp (boxify a) (boxify b)
    boxify s@ESort {} = s
    boxify x = error $ "boxify: " ++ show x

-- | get rid of unused bindings
cleanupE :: E -> E
cleanupE e = runIdentity (f e) where
    f (ELam t@TVr { tvrIdent = v } e) | v /= emptyId, v `notMember` freeIds e = f (ELam t { tvrIdent = emptyId } e)
    f (EPi t@TVr { tvrIdent = v } e) | v /= emptyId, v `notMember` freeIds e = f (EPi t { tvrIdent = emptyId } e)
    f ec@ECase { eCaseBind = t@TVr { tvrIdent = v } } | v /= emptyId, v `notMember` (freeVars (caseBodies ec)::IdSet) = f ec { eCaseBind = t { tvrIdent = emptyId } }
    f e = emapEG f f e

simplifyParms = transformParms {
    transformDumpProgress = verbose,
    transformCategory = "Simplify",
    transformPass = "Grin",
    transformOperation = Grin.SSimplify.simplify,
    transformIterate = IterateDone
    }

compileToGrin prog = do
    stats <- Stats.new
    putProgressLn "Converting to Grin..."
    prog <- return $ atomizeApps True prog
    --wdump FD.CoreMangled $ printProgram prog
    wdump FD.CoreMangled $ dumpCore "mangled" prog
    x <- Grin.FromE.compile prog
    when verbose $ Stats.print "Grin" Stats.theStats
    wdump FD.GrinInitial $ do dumpGrin "initial" x
    --x <- return $ normalizeGrin x
    x <- transformGrin simplifyParms x
    wdump FD.GrinNormalized $ do dumpGrin "normalized" x
    lintCheckGrin x
    let pushGrin grin = do
            grin <- transformGrin simplifyParms grin
            nf   <- mapMsnd (grinPush undefined) (grinFuncs grin)
            return $ setGrinFunctions nf grin

        opt s grin = do
            stats' <- Stats.new
            let fop grin = do Grin.Simplify.simplify stats' grin
                tparms = transformParms {
                    transformDumpProgress = verbose,
                    transformCategory = s,
                    transformPass = "Grin",
                    transformOperation = fop
                    }
            grin <- transformGrin tparms grin
            t' <- Stats.isEmpty stats'
            wdump FD.Progress $ Stats.print s stats'
            Stats.combine stats stats'
            case t' of
                True -> return grin
                False -> opt s grin

    x <- deadCode stats (grinEntryPointNames x) x  -- XXX
    x <- Grin.SSimplify.simplify x
    --x <- transformGrin simplifyParms x

    x <- pushGrin x
    x <- opt "Optimization" x
    lintCheckGrin x
    x <- grinSpeculate x
    lintCheckGrin x

    x <- deadCode stats (grinEntryPointNames x) x  -- XXX
    --x <- transformGrin simplifyParms x

    x <- opt "Optimization" x
    --lintCheckGrin x
    x <- Grin.SSimplify.simplify x

    wdump FD.OptimizationStats $ Stats.print "Optimization" stats
    wdump FD.GrinPreeval $ dumpGrin "preeval" x
    x <- nodeAnalyze x
    lintCheckGrin x
    x <- createEvalApply x
    lintCheckGrin x
    x <- Grin.SSimplify.simplify x
    lintCheckGrin x
    wdump FD.GrinFinal $ dumpGrin "predevolve" x
    x <- transformGrin devolveTransform x
    x <- opt "After Devolve Optimization" x
    x <- transformGrin simplifyParms x
    x <- return $ twiddleGrin x
    dumpFinalGrin x
    compileGrinToC x


dumpFinalGrin grin = do
    wdump FD.GrinGraph $ do
        let dot = graphGrin grin
            fn = optOutName options
        writeFile (fn ++ "_grin.dot") dot
    wdump FD.GrinFinal $ dumpGrin "final" grin



compileGrinToC grin | optMode options == Interpret = fail "Interpretation currently not supported."
compileGrinToC grin | optMode options /= CompileExe = return ()
compileGrinToC grin = do
    let (cg,rls) = FG2.compileGrin grin
        fn = optOutName options ++ lup "executable_extension"
        cf = (fn ++ "_code.c")
        lup k = maybe "" id $ Map.lookup k (optInis options)
    (argstring,sversion) <- getArgString
    let
        boehmOpts | fopts FO.Boehm || lup "gc" == "boehm"  = ["-D_JHC_GC=_JHC_GC_BOEHM", "-lgc"]
                  | fopts FO.Jgc || lup "gc" == "jgc"  = ["-D_JHC_GC=_JHC_GC_JGC"]
                  | otherwise = []
        profileOpts | fopts FO.Profile || lup "profile" == "true" = ["-D_JHC_PROFILE=1"]
                    | otherwise = []
        comm = shellQuote $ [lup "cc"] ++ words (lup "cflags") ++ ["-o", fn, cf] ++
                            (map ("-l" ++) rls) ++ debug ++ optCCargs options  ++ boehmOpts ++ profileOpts
        debug = if fopts FO.Debug then words (lup "cflags_debug") else words (lup "cflags_nodebug")
        globalvar n c = "char " ++ n ++ "[] = \"" ++ c ++ "\";"
    putProgressLn ("Writing " ++ show cf)
    writeFile cf $ unlines [globalvar "jhc_c_compile" comm, globalvar "jhc_command" argstring,globalvar "jhc_version" sversion,"",cg]
    putProgressLn ("Running: " ++ comm)
    r <- System.system comm
    when (r /= System.ExitSuccess) $ fail "C code did not compile."
    unless (dump FD.C) $ removeFile cf
    return ()



simplifyProgram sopt name dodump prog = liftIO $ do
    let istat = progStats prog
    let g prog = do
            let nprog = SS.programPruneOccurance prog
            when (corePass && dodump) $ do
                putStrLn "-- After Occurance Analysis"
                printProgram nprog
            return $ SS.programSSimplify sopt  nprog
    prog <- transformProgram transformParms { transformCategory = "Simplify"
                                            , transformPass = name
                                            , transformIterate = IterateDone
                                            , transformDumpProgress = dodump
                                            , transformOperation = g } prog { progStats = mempty }
    when (dodump && (dump FD.Progress || coreSteps)) $ Stats.printLStat (optStatLevel options) ("Total: " ++ name) (progStats prog)
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






