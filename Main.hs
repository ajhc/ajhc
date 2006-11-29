
module Main(main) where

import Control.Exception
import Control.Monad.Identity
import Control.Monad.Writer
import Control.Monad.State
import Data.Monoid
import IO(hFlush,stderr,stdout,openFile,hClose,IOMode(..))
import List hiding(group,union,delete)
import Maybe
import Prelude hiding(putStrLn, putStr,print)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified List(group)
import qualified System

import Atom
import C.FromGrin
import CharIO
import FrontEnd.Class
import DataConstructors
import Doc.DocLike
import Doc.PPrint
import Doc.Pretty
import E.Annotate(annotate,annotateDs,annotateProgram)
import E.Diff
import E.E
import E.Eta
import E.FromHs
import E.Inline
import E.LambdaLift
import E.ToHs
import E.FreeVars
import E.LetFloat
import E.Program
import E.Rules
import E.Show hiding(render)
import E.Subst(subst)
import E.Traverse
import E.TypeAnalysis
import E.TypeCheck
import E.WorkerWrapper
import FrontEnd.FrontEnd
import FrontEnd.KindInfer(getConstructorKinds)
import GenUtil hiding(replicateM,putErrLn,putErr,putErrDie)
import Grin.DeadCode
import Grin.Devolve(devolveGrin)
import Grin.EvalInline(createEvalApply)
import Grin.FromE
import Grin.Grin
import Grin.Optimize
import Grin.Show
import Grin.Unboxing
import Grin.Whiz
import Ho.Build
import Ho.Library
import Ho.LibraryMap
import HsSyn
import Info.Types
import Name.Id
import Name.Name
import Name.Names
import Name.VConsts
import Options
import SelfTest(selfTest)
import Support.CanType(getType)
import Support.FreeVars
import Support.ShowTable
import Util.Graph
import Util.NameMonad
import Util.SetLike as S
import Version(versionString,versionContext)
import qualified E.CPR
import qualified E.Demand as Demand(analyzeProgram,solveDs)
import qualified E.SSimplify as SS
import qualified FlagDump as FD
import qualified FlagOpts as FO
import qualified FrontEnd.Tc.Type as Type
import qualified Grin.PointsToAnalysis
import qualified Grin.Simplify
import qualified Info.Info as Info
import qualified Interactive
import qualified Stats

---------------
-- ∀α∃β . α → β
---------------

progress str = wdump FD.Progress $  (putErrLn str) >> hFlush stderr
progressM c  = wdump FD.Progress $ (c >>= putErrLn) >> hFlush stderr


collectPassStats = True

bracketHtml action = do
    pn <- System.getProgName
    as <- getArguments
    wdump FD.Html $ putStrLn $ "<html><head><title>" ++ (unwords (pn:as)) ++ "</title><meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\"></head><body style=\"background: black; color: lightgrey\"><pre>"
    action `finally` (wdump FD.Html $ putStrLn "</pre></body></html>")

main = runMain $ bracketHtml $ do
    o <- processOptions
    progressM $ do
        name <- System.getProgName
        args <- getArguments
        return (simpleQuote (name:args))
    case optMode o of
        BuildHl hl    -> createLibrary hl buildLibrary
        ListLibraries -> do
            when (optVerbose options > 0) $ do
                putStrLn "Search path:"
                mapM_ putStrLn (optHlPath options)
                putStrLn "Libraries found:"
            sequence_ [ putStrLn name | (name,_) <- libraryList ]
        SelfTest      -> do
            putStrLn "Starting self testing..."
            SelfTest.selfTest (optArgs o)
        ShowHo ho     -> dumpHoFile ho
        Version       -> putStrLn versionString
        VersionCtx    -> putStrLn versionContext
        _             -> processFiles  (optArgs o)


buildLibrary [] = do
    putStrLn "WARNING: building empty library"
    return mempty
buildLibrary mods = do
    putVerboseLn $ "Building library containing: " ++ show mods
    (_,ho) <- parseFiles [] mods processInitialHo processDecls
    -- TODO optimize, leaving out hidden module exports
    return ho



processFiles [] | Nothing <- optMainFunc options = do
    int <- isInteractive
    when (not int) $ putErrDie "jhc: no input files"
    processFilesModules [] [Module "Prelude"]
processFiles [] | Just (b,m) <- optMainFunc options = do
    m <- return $ parseName Val m
    Module m <- getModule m
    processFilesModules [] [Module m]
processFiles cs = do
    (ms,fs) <- return $ splitEither $ map fileOrModule cs
    processFilesModules fs ms

processFilesModules fs ms = do
    compileModEnv' =<< parseFiles fs ms processInitialHo processDecls

fileOrModule f = case reverse f of
                   ('s':'h':'.':_)     -> Right f
                   ('s':'h':'l':'.':_) -> Right f
                   _                   -> Left $ Module f


barendregt e = fst $ renameE mempty mempty e -- runIdentity  (renameTraverse' e)

barendregtProg prog = transformProgram transBarendregt prog

transBarendregt = transformParms {
        transformCategory = "Barendregt",
        transformIterate = DontIterate,
        transformDumpProgress = corePass,
        transformOperation =  return . barendregtProgram
        } where
    barendregtProgram prog | null $ progCombinators prog = prog
    barendregtProgram prog = programSetDs ds' prog where
        (ELetRec ds' Unknown,_) = renameE mempty mempty (ELetRec (programDs prog) Unknown)
          {-
denewtypeProgram prog = transformProgram transDenewtype prog
transDenewtype = transformParms {
        transformCategory = "DeNewtype",
        transformIterate = DontIterate,
        transformDumpProgress = corePass,
        transformOperation =  return . denewtype
        } where
        -}
denewtype prog | null $ progCombinators prog = prog
denewtype prog = prog' where
    ELetRec ds _ = removeNewtypes (progDataTable prog) (programE prog)
    prog' = programSetDs ds prog
--    Identity prog' = annotateProgram mempty (\_ nfo -> return nfo)  (\_ nfo -> return nfo) (\_ nfo -> return nfo) (programSetDs ds prog)

lamann _ nfo = return nfo
letann e nfo = return (annotateArity e nfo)
idann rs ps i nfo = return (rules rs i (props ps i nfo)) where
    props :: IdMap Properties -> Id -> Info.Info -> Info.Info
    props ps i = case mlookup i ps of
        Just ps ->  modifyProperties (mappend ps)
        Nothing ->  id
    rules rs i = case getARules rs i of
        Nothing -> id
        Just x -> \nfo -> Info.insert (x `mappend` Info.fetch nfo) nfo

collectIdAnn r p id nfo = do
    tell $ singleton id
    idann r p id nfo

processInitialHo ::
    CollectedHo       -- ^ current accumulated ho
    -> Ho    -- ^ new ho, freshly read from file
    -> IO CollectedHo -- ^ final combined ho data.
processInitialHo (CollectedHo accumho) ho = do
    let (ds,uids) = runWriter $ annotateDs imap (collectIdAnn rules' (hoProps ho) ) letann lamann (Map.elems $ hoEs ho)
        rules' = runIdentity $ mapBodies (annotate imapRules (\_ nfo -> return nfo) (\_ -> return) (\_ -> return)) (hoRules ho)
        prog = etaAnnotateProgram (denewtype $ programSetDs ds program { progDataTable = hoDataTable accumho `mappend` hoDataTable ho })
        imap = fromList [ (tvrIdent v,Just (EVar v))| (v,_) <- Map.elems (hoEs accumho)]
        imapRules = fromList [ (tvrIdent v,Just (EVar v))| (v,_) <- Map.elems (hoEs accumho' `mappend` hoEs ho)]
        accumho' = reprocessHo rules' (hoProps ho) accumho

    --lintCheckProgram (putStrLn "processInitialHo") prog
    return $ CollectedHo $ accumho' `mappend` ho { hoUsedIds = uids, hoEs = programEsMap prog }

reprocessHo :: Rules -> IdMap Properties -> Ho -> Ho
reprocessHo rules ps ho = ho { hoEs = Map.map f (hoEs ho) } where
    f (t,e) = (tvrInfo_u (g (tvrIdent t)) t,e)
    g id = runIdentity . idann rules ps id



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
processDecls (CollectedHo ho) ho' tiData = do
    -- some useful values
    let allHo = ho `mappend` ho'
        decls | fopts FO.Boxy = tiDataDecls tiData
              | otherwise = concat [ hsModuleDecls  m | (_,m) <- tiDataModules tiData ] ++ Map.elems (tiDataLiftedInstances tiData)
        originalDecls =  concat [ hsModuleDecls  m | (_,m) <- tiDataModules tiData ]

    -- build datatables
    let dataTable = toDataTable (getConstructorKinds (hoKinds ho')) (tiAllAssumptions tiData) originalDecls (hoDataTable ho)
        classInstances = deriveClasses dataTable
    let fullDataTable = (dataTable `mappend` hoDataTable ho)
    wdump FD.Datatable $ putErrLn (render $ showDataTable dataTable)

    wdump FD.Derived $
        mapM_ (\ (v,lc) -> printCheckName'' fullDataTable v lc) classInstances
    -- initial program
    let prog = program {
            progClassHierarchy = hoClassHierarchy allHo,
            progDataTable = fullDataTable,
            progExternalNames = fromList [ tvrIdent n | (n,_) <- Map.elems $ hoEs ho ],
            progModule = head (fsts $ tiDataModules tiData)
            }

    -- Convert Haskell decls to E
    let allAssumps = (tiAllAssumptions tiData `mappend` hoAssumps ho)
    ds' <- convertDecls tiData (hoClassHierarchy ho') allAssumps  fullDataTable decls
    let ds = [ (v,e) | (v,e) <- classInstances ] ++  [ (v,lc) | (n,v,lc) <- ds', v `notElem` fsts classInstances ]
 --   sequence_ [lintCheckE onerrNone fullDataTable v e | (_,v,e) <- ds ]

    -- Build rules from instances, specializations, and user specified rules and catalysts
    rules' <- createInstanceRules fullDataTable (hoClassHierarchy ho')   (Map.fromList [ (runIdentity $ fromId (tvrIdent y),(y,z)) | (y,z) <- ds] `mappend` hoEs ho)
    nrules <- convertRules (progModule prog) tiData (hoClassHierarchy ho') allAssumps fullDataTable decls
    (nds,srules) <- procAllSpecs (tiCheckedRules tiData) ds

    ds <- return $ ds ++ nds
    ds <- annotateDs mempty (\_ nfo -> return nfo) (\_ nfo -> return nfo) (\_ nfo -> return nfo) ds
    wdump FD.CoreInitial $
        mapM_ (\(v,lc) -> printCheckName'' fullDataTable v lc) ds

    let rules = rules' `mappend` nrules `mappend` srules

    wdump FD.Rules $ putStrLn "  ---- user rules ---- " >> printRules RuleUser rules
    wdump FD.Rules $ putStrLn "  ---- user catalysts ---- " >> printRules RuleCatalyst rules
    wdump FD.RulesSpec $ putStrLn "  ---- specializations ---- " >> printRules RuleSpecialization rules

    let allRules = hoRules ho `mappend` rules

    -- some more useful values.
    --let  namesInscope = fromList $ [ tvrIdent n | (n,_) <- Map.elems $ hoEs ho ] ++ [tvrIdent n | (n,_) <- ds ]

    let prog' = programSetDs ds prog
    let Identity prog = programMapDs (\ (t,e) -> return (shouldBeExported (getExports ho') t,e)) $ atomizeApps False prog'
    prog <- barendregtProg prog
    let allProps = munionWith mappend (hoProps ho')  (idSetToIdMap (const (singleton prop_HASRULE)) (ruleHeadFreeVars allRules))

    ho <- return $ reprocessHo rules allProps ho

    let brum = fromList [ (tvrIdent n,Just (EVar n)) | (n,_) <- Map.elems $ hoEs ho ] where
    -- Here we substitute in all the original types, with rules and properties defined in the current module included
    prog <- return $ runIdentity $ annotateProgram brum (idann allRules allProps) letann lamann prog

    lintCheckProgram (putErrLn "LintPostProcess") prog


    --let entryPoints = execWriter $ programMapDs_ (\ (t,_) -> when (getProperty prop_EXPORTED t || member (tvrIdent t) rfreevars) (tell [t])) prog
    --    rfreevars = ruleAllFreeVars rules
    let entryPoints = execWriter $ programMapDs_ (\ (t,_) -> when (getProperty prop_EXPORTED t || getProperty prop_INSTANCE t)  (tell [t])) prog
    prog <- return $ prog { progEntryPoints = entryPoints }

    lintCheckProgram (putErrLn "InitialLint") prog

    prog <- programPrune prog

    let initMap = fromList [ (tvrIdent t, Just (EVar t)) | (t,_) <- (Map.elems (hoEs ho))]

    -- initial pass, performs
    -- eta expansion of definitons
    -- simplify
    -- type analysis
    -- floating outward
    -- simplify
    -- floating inward

    let sopt = mempty {
            SS.so_boundVars = fromList [ (tvrIdent v,(v,e)) | (v,e) <- Map.elems (hoEs ho)],
            SS.so_dataTable = fullDataTable
            }
    let tparms = transformParms {
            transformPass = "PreInit",
            transformDumpProgress = True
            }

    -- quick float inward pass to inline once used functions and prune unused ones
    prog <- transformProgram tparms {
        transformCategory = "FloatInward",
        transformOperation = programFloatInward
        } prog
    prog <- transformProgram tparms { transformCategory = "typeAnalyze", transformOperation = typeAnalyze True } prog

    let fint mprog = do
        let names = pprint [ n | (n,_) <- programDs mprog]
        when coreMini $ putErrLn ("----\n" ++ names)
        let tparms = transformParms { transformPass = "Init", transformDumpProgress = coreMini }

        mprog <- return $ etaAnnotateProgram mprog

        mprog <- simplifyProgram sopt "Init-One" (dump FD.CoreMini) mprog
        mprog <- barendregtProg mprog

        -- | this catches more static arguments if we wait until after the initial normalizing simplification pass
        mprog <- transformProgram tparms { transformSkipNoStats = True, transformCategory = "SimpleRecursive", transformOperation = return . staticArgumentTransform } mprog


        mprog <- transformProgram tparms { transformCategory = "FloatOutward", transformOperation = floatOutward } mprog
        -- perform another supersimplify in order to substitute the once used
        -- variables back in and replace the variable of case of variables with
        -- the default binding of the case statement.

        mprog <- simplifyProgram sopt "Init-Two-FloatOutCleanup" (dump FD.CoreMini) mprog
        mprog <- barendregtProg mprog
        mprog <- transformProgram tparms { transformCategory = "typeAnalyze", transformOperation = typeAnalyze True } mprog

        mprog <- transformProgram tparms { transformCategory = "FloatInward", transformOperation = programFloatInward } mprog
        mprog <- Demand.analyzeProgram mprog
        lintCheckProgram onerrNone mprog
        mprog <- simplifyProgram sopt "Init-Three-AfterDemand" False mprog
        mprog <- barendregtProg mprog
        when miniCorePass $ printProgram mprog -- mapM_ (\ (v,lc) -> printCheckName'' fullDataTable v lc) (programDs mprog)
        when miniCoreSteps $ Stats.printLStat (optStatLevel options) ("InitialOptimize:" ++ names) (progStats mprog)
        wdump FD.Progress $ let SubProgram rec = progType mprog in  putErr (if rec then "*" else ".")
        return mprog
    lintCheckProgram onerrNone prog
    progress "Initial optimization pass"

    prog <- programMapProgGroups initMap  fint prog
    progress "!"
    hFlush stdout >> hFlush stderr

    wdump FD.Progress $
        Stats.printLStat (optStatLevel options) "Initial Pass Stats" (progStats prog)
    lintCheckProgram onerrNone prog

    prog <- barendregtProg prog { progStats = mempty }
    prog <- etaExpandProg "Init-Big-One" prog
    prog <- transformProgram tparms {
        transformPass = "Init-Big-One",
        transformCategory = "FloatInward",
        transformOperation = programFloatInward
        } prog

    prog <- Demand.analyzeProgram prog
    prog <- simplifyProgram' sopt "Init-Big-One" True (IterateMax 4) prog

    wdump FD.Progress $
        Stats.printLStat (optStatLevel options) "Init-Big-One Stats" (progStats prog)

    -- This is the main function that optimizes the routines before writing them out
    let optWW mprog = do
        let names = pprint [ n | (n,_) <- programDs mprog]
        liftIO $ when coreMini $ putErrLn ("----\n" ++ names)
        smap <- get
        let tparms = transformParms { transformPass = "OptWW", transformDumpProgress = coreMini }
            sopt = mempty {  SS.so_boundVars = smap, SS.so_dataTable = progDataTable mprog }

        mprog <- simplifyProgram sopt "Simplify-One" coreMini mprog
        mprog <- barendregtProg mprog
        mprog <- transformProgram tparms { transformCategory = "FloatInward", transformOperation = programFloatInward } mprog
        mprog <- Demand.analyzeProgram mprog
        mprog <- simplifyProgram sopt "Simplify-Two" coreMini mprog
        mprog <- transformProgram tparms { transformCategory = "FloatInward", transformOperation = programFloatInward } mprog
        mprog <- Demand.analyzeProgram mprog
        mprog <- return $ E.CPR.cprAnalyzeProgram mprog
        mprog' <- transformProgram tparms { transformSkipNoStats = True, transformCategory = "WorkWrap", transformOperation = return . workWrapProgram } mprog
        let wws = length (programDs mprog') - length (programDs mprog)
        liftIO $ wdump FD.Progress $ putErr (replicate wws 'w')
        mprog <- return mprog'

        --smap <- return $ fromList [ (tvrIdent v,(v,lc)) | (v,lc) <- programDs mprog] `union` smap
        --sopt <- return $ sopt { SS.so_boundVars = smap }

        mprog <- simplifyProgram sopt "Simplify-Three" coreMini mprog


        -- annotate our bindings for further passes
        mprog <- return $ etaAnnotateProgram mprog
        mprog <- Demand.analyzeProgram mprog
        mprog <- return $ E.CPR.cprAnalyzeProgram mprog

        put $ fromList [ (tvrIdent v,(v,lc)) | (v,lc) <- programDs mprog] `union` smap

        liftIO $ wdump FD.Progress $ let SubProgram rec = progType mprog in  putErr (if rec then "*" else ".")
        return mprog

    prog <- barendregtProg prog { progStats = mempty }
    prog <- evalStateT (programMapProgGroups mempty optWW prog) (SS.so_boundVars sopt)
    progress "!"
    hFlush stdout >> hFlush stderr
    wdump FD.Progress $
        Stats.printLStat (optStatLevel options) "MainPass Stats" (progStats prog)

    lintCheckProgram (putErrLn "After the workwrap/CPR") prog

    prog <- programPrune prog


    lintCheckProgram (putErrLn "After the Opimization") prog
    wdump FD.Core $ printProgram prog

    let newHo = ho' {
        hoDataTable = dataTable,
        hoEs = programEsMap prog,
        hoRules = hoRules ho' `mappend` rules,
        hoUsedIds = collectIds (ELetRec (programDs prog) Unknown)
        }
    return (CollectedHo (newHo `mappend` ho),newHo)

programPruneUnreachable :: Program -> Program
programPruneUnreachable prog = programSetDs ds' prog where
    ds' = reachable (newGraph (programDs prog) (tvrIdent . fst) (\ (t,e) -> idSetToList $ bindingFreeVars t e)) (map tvrIdent $ progEntryPoints prog)

programPrune :: Program -> IO Program
programPrune prog = transformProgram transformParms { transformCategory = "PruneUnreachable", transformDumpProgress  = miniCorePass, transformOperation = return . programPruneUnreachable } prog

etaExpandProg :: String -> Program -> IO Program
etaExpandProg pass prog = do
    let f prog = prog' { progStats = progStats prog `mappend` stats } where
        (prog',stats) = Stats.runStatM $  etaExpandProgram prog
    transformProgram transformParms { transformPass = pass, transformCategory = "EtaExpansion", transformDumpProgress = miniCorePass,  transformOperation = return . f } prog


getExports ho =  Set.fromList $ map toId $ concat $  Map.elems (hoExports ho)
shouldBeExported exports tvr
    | tvrIdent tvr `Set.member` exports || getProperty prop_SRCLOC_ANNOTATE_FUN tvr  = setProperty prop_EXPORTED tvr
    | otherwise = tvr


collectIds e = execWriter $ annotate mempty (\id nfo -> tell (singleton id) >> return nfo) (\_ -> return) (\_ -> return) e
--idHistogram e = execWriter $ annotate mempty (\id nfo -> tell (Histogram.singleton id) >> return nfo) (\_ -> return) (\_ -> return) e

isInteractive :: IO Bool
isInteractive = do
    pn <- System.getProgName
    return $ (optMode options == Interactive)
          || "ichj" `isPrefixOf` reverse pn
          || not (null $ optStmts options)

transTypeAnalyze = transformParms { transformCategory = "typeAnalyze",  transformOperation = typeAnalyze True }

compileModEnv' (CollectedHo ho,_) = do
    if optMode options == CompileHo then return () else do

    let dataTable = progDataTable prog
        rules = hoRules ho
        prog = hoToProgram ho

    -- dump final version of various requested things
    wdump FD.Datatable $ putErrLn (render $ showDataTable dataTable)
    when (dump FD.ClassSummary) $ do
        putStrLn "  ---- class summary ---- "
        printClassSummary (hoClassHierarchy ho)
    when (dump FD.Class) $ do
        putStrLn "  ---- class hierarchy ---- "
        printClassHierarchy (hoClassHierarchy ho)
    wdump FD.Rules $ putStrLn "  ---- user rules ---- " >> printRules RuleUser rules
    wdump FD.Rules $ putStrLn "  ---- user catalysts ---- " >> printRules RuleCatalyst rules
    wdump FD.RulesSpec $ putStrLn "  ---- specializations ---- " >> printRules RuleSpecialization rules

    -- enter interactive mode
    int <- isInteractive
    if int then Interactive.interact ho else do

    when collectPassStats $ do
        Stats.print "PassStats" Stats.theStats
        Stats.clear Stats.theStats


    let mainFunc = parseName Val (maybe "Main.main" snd (optMainFunc options))
    (_,main,mainv) <- getMainFunction dataTable mainFunc (programEsMap prog)
    let ffiExportNames = filter (\t -> (tvrName t >>= return . nameType) == Just FfiExportName) $ map (\(x,_,_) -> x) $ progCombinators prog
    prog <- return prog { progMainEntry   = main,
                          progEntryPoints = (main:ffiExportNames),
                          progCombinators = (main,[],mainv):[ (unsetProperty prop_EXPORTED t,as,e) | (t,as,e) <- progCombinators prog]
                        }
    prog <- transformProgram transformParms { transformCategory = "PruneUnreachable", transformOperation = return . programPruneUnreachable } prog
    prog <- barendregtProg prog


    let theTarget = ELit litCons { litName = dc_Target, litArgs = [ELit (LitInt targetIndex tIntzh)], litType = ELit litCons { litName = tc_Target, litArgs = [], litType = eStar } }
        targetIndex = if fopts FO.ViaGhc then 1 else 0
    prog <- return $ runIdentity $ flip programMapDs prog $ \(t,e) -> return $ if tvrIdent t == toId v_target then (t { tvrInfo = setProperty prop_INLINE mempty },theTarget) else (t,e)

    --wdump FD.Core $ printProgram prog
    prog <- if (fopts FO.TypeAnalysis) then do typeAnalyze False prog else return prog
    putStrLn "Type analyzed methods"
    flip mapM_ (programDs prog) $ \ (t,e) -> do
        let (_,ts) = fromLam e
            ts' = takeWhile (sortKindLike . getType) ts
        when (not (null ts')) $ putStrLn $ (pprint t) ++ " \\" ++ concat [ "(" ++ show  (Info.fetch (tvrInfo t) :: Typ) ++ ")" | t <- ts' ]
    lintCheckProgram onerrNone prog
    prog <- programPrune prog
    --wdump FD.Core $ printProgram prog

    cmethods <- do
        let es' = concatMap expandPlaceholder (programDs prog)
        es' <- return [ (y,floatInward z) |  (y,z) <- es' ]
        wdump FD.Class $ do
            sequence_ [ printCheckName' dataTable y z |  (y,z) <- es']
        return es'

    prog <- return $ programSetDs ([ (t,e) | (t,e) <- programDs prog, t `notElem` fsts cmethods] ++ cmethods) prog

--    prog <- denewtypeProgram prog

    prog <- annotateProgram mempty (\_ nfo -> return $ unsetProperty prop_INSTANCE nfo) letann (\_ nfo -> return nfo) prog


    unless (fopts FO.GlobalOptimize) $ do
        prog <- programPrune prog
        prog <- barendregtProg prog
        wdump FD.CoreBeforelift $ printProgram prog
        prog <- transformProgram transformParms { transformCategory = "LambdaLift", transformDumpProgress = dump FD.Progress, transformOperation = lambdaLift } prog
        wdump FD.CoreAfterlift $ printProgram prog -- printCheckName dataTable (programE prog)
        compileToGrin prog
        exitSuccess


    prog <- transformProgram transTypeAnalyze { transformPass = "Main-AfterMethod", transformDumpProgress = True } prog
    prog <- barendregtProg prog

--    prog <- denewtypeProgram prog

    prog <- simplifyProgram mempty "Main-One" True prog
    prog <- barendregtProg prog


    prog <- etaExpandProg "Main-AfterOne" prog
    prog <- barendregtProg prog
    prog <- transformProgram transTypeAnalyze { transformPass = "Main-AfterSimp", transformDumpProgress = True } prog


    prog <- barendregtProg prog
    prog <- simplifyProgram mempty "Main-Two" True prog
    prog <- barendregtProg prog


    -- run optimization again with no rules enabled

    -- delete rules
    prog <- return $ runIdentity $ annotateProgram mempty (\_ nfo -> return $ modifyProperties (flip (foldr delete) [prop_HASRULE,prop_WORKER,prop_WRAPPER]) nfo) letann (\_ -> return) prog
    --prog <- transformProgram "float inward" DontIterate True programFloatInward prog

    prog <- simplifyProgram mempty { SS.so_finalPhase = True } "SuperSimplify no rules" True prog
    prog <- barendregtProg prog


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
    prog <- transformProgram transformParms { transformCategory = "Boxy WorkWrap", transformDumpProgress = dump FD.Progress, transformOperation = return . workWrapProgram } prog
    prog <- simplifyProgram mempty { SS.so_finalPhase = True } "SuperSimplify after Boxy WorkWrap" True prog
    prog <- barendregtProg prog
    prog <- return $ runIdentity $ programMapBodies (return . cleanupE) prog

    when (fopts FO.ViaGhc) $ do
        wdump FD.Core $ printProgram prog
        compileToHs prog
        exitSuccess

    wdump FD.CoreBeforelift $ printProgram prog
    prog <- transformProgram transformParms { transformCategory = "LambdaLift", transformDumpProgress = dump FD.Progress, transformOperation = lambdaLift } prog

    finalStats <- Stats.new
    -- final optimization pass to clean up lambda lifting droppings
    rs' <- flip mapM (progCombinators prog) $ \ (t,ls,e) -> do
        let cm stats e = do
            let sopt = mempty {  SS.so_dataTable = dataTable }
            let (stat, e') = SS.simplifyE sopt e
            Stats.tickStat stats stat
            return e'
        e' <- doopt (mangle' Nothing dataTable) False finalStats ("SuperSimplify: " ++ pprint t)  cm e
        --e'' <- atomizeAp True dataTable stats e'
        return (t,ls,e')
    wdump FD.Progress $ Stats.print "PostLifting" finalStats

    prog <- return $ prog { progCombinators = rs' }


    wdump FD.CoreAfterlift $ printProgram prog -- printCheckName dataTable (programE prog)

    wdump FD.Progress $ printEStats (programE prog)

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
    ans = do
        prog' <- programMapDs f prog
        return $ runIdentity $ annotateProgram mempty (\_ nfo -> return $ unsetProperty prop_HASRULE $ Info.delete (undefined :: ARules) nfo) (\_ nfo -> return nfo)  (\_ nfo -> return nfo) prog'
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
    boxify v@EVar {} | canBeBox v = tBox
    boxify (ELit lc) = ELit lc { litArgs = map boxify (litArgs lc) }
    boxify v@(EAp _ _) | canBeBox v = tBox
    boxify (EAp (ELam t b) e) = boxify (subst t e b)
    boxify (EAp a b) = EAp (boxify a) b -- TODO there should be no applications at the type level by now (boxify b)
    boxify s@ESort {} = s
    boxify x = error $ "boxify: " ++ show x

-- | get rid of unused bindings
cleanupE :: E -> E
cleanupE e = runIdentity (f e) where
    f (ELam t@TVr { tvrIdent = v } e) | v /= 0, v `notMember` freeIds e = f (ELam t { tvrIdent = 0 } e)
    f (EPi t@TVr { tvrIdent = v } e) | v /= 0, v `notMember` freeIds e = f (EPi t { tvrIdent = 0 } e)
    f ec@ECase { eCaseBind = t@TVr { tvrIdent = v } } | v /= 0, v `notMember` (freeVars (caseBodies ec)::IdSet) = f ec { eCaseBind = t { tvrIdent = 0 } }
    f e = emapEG f f e

compileToGrin prog = do
    stats <- Stats.new
    progress "Converting to Grin..."
    prog <- return $ atomizeApps True prog
    wdump FD.CoreMangled $ printProgram prog
    x <- Grin.FromE.compile prog
    Stats.print "Grin" Stats.theStats
    wdump FD.GrinInitial $ do
        putErrLn "v-- Initial Grin"
        printGrin x
        putErrLn "^-- Initial Grin"
    x <- return $ normalizeGrin x
    wdump FD.GrinNormalized $ do
        putErrLn "v-- Normalized Grin"
        printGrin x
        putErrLn "^-- Normalized Grin"
    lintCheckGrin x
    let opt s  x = do
        stats' <- Stats.new
        nf <- mapMsnd (grinPush stats') (grinFuncs x)
        x <- return $ setGrinFunctions nf x
        wdump FD.GrinPass $ printGrin x
        x <- Grin.Simplify.simplify stats' x
        t' <- Stats.getTicks stats'
        wdump FD.Progress $ Stats.print s stats'
        Stats.combine stats stats'
        lintCheckGrin x
        case t' of
            0 -> return x
            _ -> opt s x
    x <- deadCode stats (grinEntryPointNames x) x  -- XXX
    lintCheckGrin x
    x <- opt "Optimization" x
    lintCheckGrin x
    x <- grinSpeculate x
    lintCheckGrin x
    x <- deadCode stats (grinEntryPointNames x) x  -- XXX
    lintCheckGrin x
    x <- opt "Optimization" x
    lintCheckGrin x
    x <- return $ normalizeGrin x

    wdump FD.OptimizationStats $ Stats.print "Optimization" stats

    if fopts FO.EvalOptimize then do
        lintCheckGrin x
        wdump FD.GrinPreeval $ printGrin x
        progress "Points-to analysis..."
        stats <- Stats.new
        x <- Grin.PointsToAnalysis.grinInlineEvalApply stats x
        wdump FD.Progress $ Stats.print "EvalInline" stats
        lintCheckGrin x
        wdump FD.GrinPosteval $ printGrin x
        stats <- Stats.new
        x <- opt "AE Optimization 1" x
        x <- unboxReturnValues x
        lintCheckGrin x
        x <- deadCode stats (grinEntryPointNames x) x
        lintCheckGrin x
        x <- return $ normalizeGrin x
        lintCheckGrin x
        x <- opt "AE Optimization 2" x
        x <- unboxReturnValues x
        lintCheckGrin x
        x <- deadCode stats (grinEntryPointNames x) x
        lintCheckGrin x
        x <- opt "AE Optimization 3" x
        wdump FD.OptimizationStats $ Stats.print "AE Optimization" stats
        x <- return $ normalizeGrin x
        lintCheckGrin x

        printTable "Return points-to" (grinReturnTags x)
        printTable "Argument points-to" (grinArgTags x)
        x <- devolveGrin x
        x <- opt "After Devolve Optimization" x
        x <- return $ normalizeGrin x
        x <- devolveGrin x
        x <- opt "After Devolve Optimization 2" x
        x <- return $ normalizeGrin x
        x <- devolveGrin x
        x <- return $ normalizeGrin x
        dumpFinalGrin x
        when (optMode options == CompileExe) $ compileGrinToC x
     else do
        x <- createEvalApply x
        x <- return $ normalizeGrin x
        lintCheckGrin x
        x <- devolveGrin x
        x <- opt "After Devolve Optimization" x
        x <- return $ normalizeGrin x
        dumpFinalGrin x
        when (optMode options == CompileExe) $ compileGrinToC x

dumpFinalGrin grin = do
    let fn = optOutName options
    wdump FD.GrinGraph $ do
        let dot = graphGrin grin
        writeFile (fn ++ "_grin.dot") dot
    h <- openFile (fn ++ "_grin.txt") WriteMode
    hPrintGrin h grin
    hClose h
    wdump FD.Grin $ printGrin grin


compileGrinToC grin = do
    when (optMode options == Interpret) $ do
        putErrLn "Interpreting not supported."
        --progress "Interpreting..."
        --(v,stats) <- Grin.Interpret.evaluate grin
        --CharIO.putStrLn $ render $ Grin.Show.prettyVal v
        --wdump FD.Stats $  Stats.print "Stats" stats
        return ()
    let (cg,rls) = compileGrin grin
    let fn = optOutName options
    let cf = (fn ++ "_code.c")
    progress ("Writing " ++ show cf)
    name <- System.getProgName
    args <- getArguments
    let argstring = simpleQuote (name:args)
        boehmOpts | fopts FO.Boehm = ["-DUSE_BOEHM_GC", "-lgc"]
                  | otherwise = []
        profileOpts | fopts FO.Profile = ["-D_JHC_PROFILE"]
                  | otherwise = []
        comm = shellQuote $ [optCC options, "-std=gnu99", "-foptimize-sibling-calls", "-O", {- "-funit-at-a-time", -} "-g", "-Wall", "-o", fn, cf ] ++ (map ("-l" ++) rls) ++ optCCargs options  ++ boehmOpts ++ profileOpts
        globalvar n c = "char " ++ n ++ "[] = \"" ++ c ++ "\";"
    writeFile cf $ unlines [globalvar "jhc_c_compile" comm, globalvar "jhc_command" argstring,globalvar "jhc_version" (head $ lines versionString),"",cg]
    progress ("Running: " ++ comm)
    r <- System.system comm
    when (r /= System.ExitSuccess) $ fail "C code did not compile."
    return ()

dereferenceItem (HeapValue hvs) | not $ Set.null hvs = combineItems (map f $ Set.toList hvs) where
    f (HV _ (Right v)) = valToItem v
    f (HV _ (Left (_,i))) = i
dereferenceItem x = x

buildShowTableLL xs = buildTableLL [ (show x,show y) | (x,y) <- xs ]

simplifyProgram sopt name dodump prog = liftIO $ do
    let istat = progStats prog
    let g prog = do
            let nprog = SS.programPruneOccurance prog
            when (corePass && dodump) $ do
                putStrLn "-- After Occurance Analysis"
                printProgram nprog
            return $ SS.programSSimplify sopt { SS.so_dataTable = progDataTable prog } nprog
    prog <- transformProgram transformParms { transformCategory = "Simplify", transformPass = name, transformIterate = IterateDone, transformDumpProgress = dodump, transformOperation = g } prog { progStats = mempty }
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
    let g =  return . SS.programSSimplify sopt { SS.so_dataTable = progDataTable prog } . SS.programPruneOccurance
    prog <- transformProgram transformParms { transformCategory = "Simplify", transformPass = name, transformIterate = iterate, transformDumpProgress = dodump, transformOperation = g } prog { progStats = mempty }
    when (dodump && (dump FD.Progress || coreSteps)) $ Stats.printLStat (optStatLevel options) ("Total: " ++ name) (progStats prog)
    return prog { progStats = progStats prog `mappend` istat }

-- all transformation routines assume they are being passed a correct program, and only check the output

data Iterate = DontIterate | IterateMax !Int | IterateExactly !Int | IterateDone
    deriving(Eq)

doIterate (IterateMax _) stat | stat /= mempty = True
doIterate IterateDone stat | stat /= mempty = True
doIterate IterateExactly {} _ = True
doIterate _ _ = False

iterateStep (IterateMax n) = IterateMax (n - 1)
iterateStep (IterateExactly n) = IterateExactly (n - 1)
iterateStep x = x

data TransformParms = TransformParms {
    transformIterate :: Iterate,
    transformDumpProgress :: Bool,
    transformSkipNoStats  :: Bool,
    transformOperation :: Program -> IO Program,
    transformCategory :: String,   -- ^ general name of transformation
    transformPass :: String,       -- ^ what pass we are in
    transformName :: String        -- ^ name of what we are working on
    }

transformParms = TransformParms {
    transformIterate = DontIterate,
    transformDumpProgress = False,
    transformSkipNoStats = False,
    transformCategory = "Unknown",
    transformPass = "",
    transformOperation = return,
    transformName = ""
    }

transformProgram :: MonadIO m => TransformParms -> Program -> m Program

transformProgram TransformParms { transformIterate = IterateMax n } prog | n <= 0 = return prog
transformProgram TransformParms { transformIterate = IterateExactly n } prog | n <= 0 = return prog
transformProgram tp prog = liftIO $ do
    let dodump = transformDumpProgress tp
        name = transformCategory tp ++ pname (transformPass tp) ++ pname (transformName tp)
        scname = transformCategory tp ++ pname (transformPass tp)
        pname "" = ""
        pname xs = '-':xs
        iterate = transformIterate tp
    when dodump $ putErrLn $ "-- " ++ name
    when (dodump && dump FD.CorePass) $ printProgram prog
    wdump FD.ESize $ printESize ("Before "++name) prog
    let istat = progStats prog
    let ferr e = do
        putErrLn $ "\n>>> Exception thrown"
        putErrLn $ "\n>>> Before " ++ name
        printProgram prog
        putErrLn $ "\n>>>"
        putErrLn (show e)
        maybeDie
        return prog
    prog' <- Control.Exception.catch (transformOperation tp prog { progStats = mempty }) ferr
    let estat = progStats prog'
        onerr = do
            putErrLn $ "\n>>> Before " ++ name
            printProgram prog
            Stats.printStat name estat
            putErrLn $ "\n>>> After " ++ name
    if transformSkipNoStats tp && estat == mempty then do
        when dodump $ putErrLn "program not changed"
        return prog
     else do
    when (dodump && dump FD.CoreSteps && estat /= mempty) $ Stats.printLStat (optStatLevel options) name estat
    when collectPassStats $ do
        Stats.tick Stats.theStats scname
        Stats.tickStat Stats.theStats (Stats.prependStat scname estat)
    wdump FD.ESize $ printESize ("After  "++name) prog'
    lintCheckProgram onerr prog'
    if doIterate iterate estat then transformProgram tp { transformIterate = iterateStep iterate } prog' { progStats = istat `mappend` estat } else
        return prog' { progStats = istat `mappend` estat, progPasses = name:progPasses prog' }



-- these are way too complicated and should be simplified

doopt mangle dmp stats name func lc = do
    stats' <- Stats.new
    lc <- mangle (Stats.print "stats" stats') dmp name (func stats') lc
    t' <- Stats.getTicks stats'
    case t'  of
        0 -> return lc
        _ -> do
            when ((dmp && dump FD.Progress) || dmp && coreSteps) $ Stats.print "Optimization" stats'
            Stats.combine stats stats'
            doopt mangle dmp stats name func lc


mangle' ::
    Maybe IdSet  -- ^ Acceptable free variables
    -> DataTable        -- ^ The datatable needed for typechecking
    -> IO ()            -- ^ run on error
    -> Bool             -- ^ Whether to dump progress
    -> String           -- ^ Name of pass
    -> (E -> IO E)      -- ^ Mangling function
    -> E                -- ^ What to mangle
    -> IO E             -- ^ Out it comes
mangle'  fv dataTable erraction b  s action e = do
    when ((b && dump FD.Progress) || (b && dump FD.CorePass)) $ putErrLn $ "-- " ++ s
    e' <- action e
    if not flint then return e' else do
        let ufreevars e | Just as <- fv = filter ( not . (`member` as) . tvrIdent) (freeVars e)
            ufreevars e = []
        case inferType dataTable [] e' of
        -- temporarily disabled due to newtypes of functions
--            Right _ |  xs@(_:_) <- ufreevars e' -> do
--                putErrLn $ "\n>>> internal error: Unaccountable Free Variables\n" ++ render (pprint (xs:: [TVr]))
--                putErrLn $ "\n>>>Before" <+> s
--                printEStats e
--                putDocM CharIO.putErr (ePretty e)
--                putErrLn $ "\n>>>After" <+> s
--                printEStats e'
--                erraction
--                --let (_,e'') = E.Diff.diff e e'
--                let e''' = findOddFreeVars xs e'
--                putDocM CharIO.putErr (ePrettyEx e''')
--                putErrLn $ "\n>>> internal error: Unaccountable Free Variables\n" ++ render (pprint (xs:: [TVr]))
--                case optKeepGoing options of
--                    True -> return e'
--                    False -> putErrDie "Unusual free vars in E"
            Left ss -> do
                putErrLn "Type Error..."
                putErrLn $ "\n>>>Before" <+> s
                printEStats e
                putDocM CharIO.putErr (ePretty e)
                putErrLn $ "\n>>>After" <+> s
                printEStats e'
                erraction
                let (_,e'') = E.Diff.diff e e'
                putDocM CharIO.putErr (ePretty e'')
                putErrLn $ "\n>>> internal error:\n" ++ unlines (tail ss)
                maybeDie
                return e'
            Right _ -> wdump FD.Stats (printEStats e') >>  return e'


typecheck dataTable e = case inferType dataTable [] e of
    Left ss -> do
        putErrLn (render $ ePretty e)
        putErrLn $ "\n>>> internal error:\n" ++ unlines (intersperse "----" $ tail ss)
        maybeDie
        return Unknown
    Right v -> return v

maybeDie = case optKeepGoing options of
    True -> return ()
    False -> putErrDie "Internal Error"

onerrNone :: IO ()
onerrNone = return ()


lintCheckGrin grin = when flint $ typecheckGrin grin

lintCheckE onerr dataTable tvr e | flint = case inferType dataTable [] e of
    Left ss -> do
        onerr
        putErrLn ">>> Type Error"
        putErrLn  ( render $ hang 4 (pprint tvr <+> equals <+> pprint e))
        putErrLn $ "\n>>> internal error:\n" ++ unlines (intersperse "----" $ tail ss)
        maybeDie
    Right v -> return ()
lintCheckE _ _ _ _ = return ()

lintCheckProgram onerr prog | flint = do
    when (hasRepeatUnder fst (programDs prog)) $ do
        onerr
        let repeats = [ x | x@(_:_:_) <- List.group $ sort (map fst (programDs prog))]
        putErrLn $ ">>> Repeated top level decls: " ++ pprint repeats
        printProgram prog
        putErrLn $ ">>> program has repeated toplevel definitions" ++ pprint repeats
        maybeDie
    let f (tvr@TVr { tvrIdent = n },e) | even n = do
            onerr
            putErrLn $ ">>> non-unique name at top level: " ++ pprint tvr
            printProgram prog
            putErrLn $ ">>> non-unique name at top level: " ++ pprint tvr
            maybeDie
        f (tvr,e) = lintCheckE onerr (progDataTable prog) tvr e
    mapM_ f (programDs prog)
    let ids = progExternalNames prog `mappend` fromList (map tvrIdent $ fsts (programDs prog))
        fvs = Set.fromList $ melems (freeVars $ snds $ programDs prog :: IdMap TVr)
        unaccounted = Set.filter (not . (`member` ids) . tvrIdent) fvs
    unless (Set.null unaccounted) $ do
        onerr
        putErrLn ("\n>>> Unaccounted for free variables: " ++ render (pprint $ Set.toList $ unaccounted))
        printProgram prog
        putErrLn (">>> Unaccounted for free variables: " ++ render (pprint $ Set.toList $ unaccounted))
        --putErrLn (show ids)
        maybeDie
lintCheckProgram _ _ = return ()



dumpTyEnv (TyEnv tt) = mapM_ putStrLn $ sort [ show n <+> hsep (map show as) <+> "::" <+> show t |  (n,(as,t)) <- Map.toList tt]

printCheckName dataTable e = do
    putErrLn  ( render $ hang 4 (pprint e <+> text "::") )
    ty <- typecheck dataTable e
    putErrLn  ( render $ indent 4 (pprint ty))

printCheckName' dataTable tvr e = do
    putErrLn (show $ tvrInfo tvr)
    putErrLn  ( render $ hang 4 (pprint tvr <+> equals <+> pprint e <+> text "::") )
    ty <- typecheck dataTable e
    putErrLn  ( render $ indent 4 (pprint ty))




printESize :: String -> Program -> IO ()
printESize str prog = putErrLn $ str ++ " program e-size: " ++ show (eSize (programE prog))









