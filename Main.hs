
module Main(main) where

import Control.Exception
import Control.Monad.Identity
import Control.Monad.Writer
import Data.Monoid
import List hiding(group,union)
import Maybe
import Prelude hiding(putStrLn, putStr,print)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified System
import IO(hFlush,stderr,stdout)

import C.FromGrin
import CharIO
import Class
import DataConstructors
import Doc.DocLike
import Doc.PPrint
import Doc.Pretty
import E.Annotate(annotate,annotateDs,annotateProgram)
import E.Diff
import E.E
import E.Eta
import E.Inline
import E.FromHs
import E.LambdaLift
import E.Program
import E.LetFloat
import E.Show hiding(render)
import E.Rules
import E.Strictness
import E.Traverse
import E.TypeAnalysis
import E.TypeCheck
import E.WorkerWrapper
import FrontEnd.FrontEnd
import qualified FrontEnd.Tc.Type as Type
import FrontEnd.KindInfer(getConstructorKinds)
import GenUtil hiding(replicateM,putErrLn,putErr,putErrDie)
import Grin.DeadCode
import Grin.EvalInline(createEvalApply)
import Grin.FromE
import Grin.Grin
import Grin.Show
import Grin.Unboxing
import Grin.Optimize
import qualified Grin.MangleE as Mangle(mangle)
import Grin.Whiz
import Ho.Build
import Ho.Library
import Ho.LibraryMap
import HsSyn
import Info.Types
import Name.Name
import Options
import SelfTest(selfTest)
import Support.CanType(getType)
import Support.FreeVars
import Support.ShowTable
import Util.Graph
import Name.Id
import Util.NameMonad
import Util.SetLike as S
import Version(versionString,versionContext)
import qualified E.CPR
import qualified E.SSimplify as SS
import qualified FlagDump as FD
import qualified FlagOpts as FO
import qualified Grin.Interpret
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
    s <- Stats.new
    (_,ho) <- parseFiles [] mods processInitialHo (processDecls s)
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
    s <- Stats.new
    compileModEnv' s =<< parseFiles fs ms processInitialHo (processDecls s)

fileOrModule f = case reverse f of
                   ('s':'h':'.':_)     -> Right f
                   ('s':'h':'l':'.':_) -> Right f
                   _                   -> Left $ Module f


barendregt e = runIdentity  (renameTraverse' e)

barendregtProgram prog | null $ progCombinators prog = prog
barendregtProgram prog = programSetDs ds' prog where
    Identity (ELetRec ds' Unknown) = renameTraverse' (ELetRec (programDs prog) Unknown)


barendregtProg prog = do
    transformProgram "Barendregt" DontIterate (dump FD.CorePass) (return . barendregtProgram) prog


lamann _ nfo = return nfo
letann e nfo = return (annotateArity e nfo)
idann rs ps i nfo = return (rules rs i (props ps i nfo)) where
    props ps i = case tvrName (tvr { tvrIdent = i }) of
        Just n -> case Map.lookup n ps of
            Just ps ->  setProperties ps
            Nothing ->  id
        Nothing -> id
    rules rs i = case getARules rs i of
        Nothing -> id
        Just x -> \nfo -> Info.insert (x `mappend` Info.fetch nfo) nfo

collectIdAnn r p id nfo = do
    tell $ singleton id
    idann r p id nfo

processInitialHo :: Ho -> IO Ho
processInitialHo ho = do
    let (ds,uids) = runWriter $ annotateDs mempty (collectIdAnn (hoRules ho) (hoProps ho) ) letann lamann (Map.elems $ hoEs ho)
        ds' = programDs $ etaAnnotateProgram (programSetDs ds program)
    return ho { hoUsedIds = uids, hoEs = Map.fromList [ (runIdentity $ fromId (tvrIdent v),d) |  d@(v,_) <- ds' ] }

procSpecs :: Monad m => (Map.Map Name [Type.Rule]) -> (TVr,E) -> m ([(TVr,E)],[Rule])
procSpecs specMap (t,e) | Just n <- fromId (tvrIdent t), Just rs <- Map.lookup n specMap = do
    hs <- mapM (makeSpec (t,e)) rs
    let (defs,rls) = unzip hs
        crules = Info.fetch (tvrInfo t)
    return $ ((t { tvrInfo = Info.insert (mappend crules (arules rls)) $ tvrInfo t},e):defs,rls)
procSpecs _specMap d = return ([d],[])

-- | this is called on parsed, typechecked haskell code to convert it to the internal representation

coreMini = dump FD.CoreMini
corePass = dump FD.CorePass
coreSteps = dump FD.CoreSteps
miniCorePass = coreMini && corePass
miniCoreSteps = coreMini && coreSteps

processDecls ::
    Stats.Stats   -- ^ statistics
    -> Ho     -- ^ Collected ho
    -> Ho     -- ^ preliminary haskell object  data
    -> TiData -- ^ front end output
    -> IO Ho  -- ^ final haskell object file
processDecls stats ho ho' tiData = do
    -- some useful values
    let allHo = ho `mappend` ho'
        decls | fopts FO.Boxy = tiDataDecls tiData
              | otherwise = concat [ hsModuleDecls  m | (_,m) <- tiDataModules tiData ] ++ Map.elems (tiDataLiftedInstances tiData)
        originalDecls =  concat [ hsModuleDecls  m | (_,m) <- tiDataModules tiData ]

    -- build datatables
    let dataTable = toDataTable (getConstructorKinds (hoKinds ho')) (tiAllAssumptions tiData) originalDecls
    let fullDataTable = (dataTable `mappend` hoDataTable ho)
    wdump FD.Datatable $ putErrLn (render $ showDataTable dataTable)

    -- initial program
    let prog = program {
            progClassHierarchy = hoClassHierarchy allHo,
            progDataTable = fullDataTable,
            progClosed = False,
            progExternalNames = fromList [ tvrIdent n | (n,_) <- Map.elems $ hoEs ho ],
            progModule = head (fsts $ tiDataModules tiData)
            }

    -- Convert Haskell decls to E
    let allAssumps = (tiAllAssumptions tiData `mappend` hoAssumps ho)
    ds <- convertDecls tiData (hoClassHierarchy ho') allAssumps  fullDataTable decls
    wdump FD.InitialCore $
        mapM_ (\(_,v,lc) -> printCheckName'' fullDataTable v lc) ds
    sequence_ [lintCheckE onerrNone fullDataTable v e | (_,v,e) <- ds ]

    -- Build rules
    rules' <- createInstanceRules (hoClassHierarchy ho')   (Map.fromList [ (x,(y,z)) | (x,y,z) <- ds] `mappend` hoEs ho)
    rawRules <- convertRules tiData (hoClassHierarchy ho') allAssumps fullDataTable decls
    let nrules = fromRules [ makeRule n (progModule prog,i) vs head args e2 | (n,vs,e1,e2) <- rawRules, let (EVar head,args) = fromAp e1 | i <- [1..] ]
    let rules = rules' `mappend` nrules
    wdump FD.Rules $ printRules rules
    let allRules = hoRules allHo `mappend` rules

    -- some more useful values.
    let inscope =  [ tvrIdent n | (n,_) <- Map.elems $ hoEs ho ] ++ [tvrIdent n | (_,n,_) <- ds ]
        mangle = mangle' (Just $ fromList inscope) fullDataTable
        namesInscope = fromList inscope

    -- initial pass over functions to put them into a normalized form
    let procE (ds,usedIds) (n,v,lc) = do
        lc <- atomizeAp False fullDataTable stats (progModule prog) lc -- doopt mangle False stats "FixupLets..." (\stats x -> atomizeAp False fullDataTable stats (progModule prog) x >>= coalesceLets stats)  lc
        lc <- coalesceLets stats lc
        nfo <- idann  allRules (hoProps ho') (tvrIdent v) (tvrInfo v)
        v <- return $ v { tvrInfo = Info.insert LetBound nfo }
        let used' = collectIds lc
        return ((n, shouldBeExported (getExports ho') v,lc):ds,usedIds `mappend` used')
    Stats.clear stats
    (ds,_allIds) <- foldM procE ([],hoUsedIds ho) ds
    Stats.print "PostProcess" stats
    Stats.clear stats

    prog <- return $ programSetDs [ (t,e) | (_,t,e) <- ds] prog
    let entries = execWriter $ programMapDs_ (\ (t,_) -> when (getProperty prop_EXPORTED t) (tell [t])) prog
    prog <- return $ prog { progEntryPoints = entries }
    prog <- programPrune prog

    let specMap = Map.fromListWith (++) [ (n,[r]) | r@Type.RuleSpec { Type.ruleName = n } <- tiCheckedRules tiData]
    nds <- mapM (procSpecs specMap) (programDs prog)
    prog <- return $ programSetDs (concat (fsts nds)) prog
    let specRules = fromRules $ concat $ snds nds
    wdump FD.Rules $ printRules specRules
    rules <- return $ specRules `mappend` rules
    allRules <- return $ allRules `mappend` rules

    let initMap = fromList [ (tvrIdent t, Just (EVar t)) | (t,_) <- (Map.elems (hoEs ho))]

    -- initial pass, performs
    -- eta expansion of definitons
    -- simplify
    -- type analysis
    -- floating outward
    -- simplify
    -- floating inward

    initialPassStats <- Stats.new
    let sopt = mempty {
            SS.so_boundVars = fromList [ (tvrIdent v,(v,e)) | (v,e) <- Map.elems (hoEs ho)],
            SS.so_dataTable = fullDataTable
            }

    let fint (rec,ns) = do
        let names = [ n | (n,_) <- ns]
        when coreMini $ putErrLn ("----\n" ++ pprint names)
        mstats <- Stats.new
        let mprog = programSetDs ns prog {
            progStats = mempty,
            progClosed = True,
            progEntryPoints = fsts ns,
            progExternalNames = progExternalNames prog `mappend` (fromList $ map tvrIdent $ fsts (programDs prog))
            }
        --progress "eta annotating"
        mprog <- return $ etaAnnotateProgram mprog
        mprog <- simplifyProgram sopt "SuperSimplify" (dump FD.CoreMini) mprog
        mprog <- barendregtProg mprog
        mprog <- transformProgram "floatOutward" DontIterate (dump FD.CoreMini) floatOutward mprog
        -- perform another supersimplify in order to substitute the once used
        -- variables back in and replace the variable of case of variables with
        -- the default binding of the case statement.

        mprog <- simplifyProgram sopt "Simplify FloatOutCleanup" (dump FD.CoreMini) mprog
        mprog <- barendregtProg mprog
        mprog <- transformProgram "float inward" DontIterate (dump FD.CoreMini) (programMapBodies (return . floatInward)) mprog
        let ns = programDs mprog
        ns <- E.Strictness.solveDs ns
        mprog <- return $ programSetDs ns mprog
        lintCheckProgram onerrNone mprog
        --mprog <- simplifyProgram sopt "SuperSimplify" False mprog
        --mprog <- barendregtProg mprog
        Stats.tickStat mstats (progStats mprog)
        Stats.combine initialPassStats mstats
        when miniCorePass $ mapM_ (\ (v,lc) -> printCheckName'' fullDataTable v lc) (programDs mprog)
        when miniCoreSteps $ Stats.print ("InitialOptimize:" ++ pprint names) mstats
        wdump FD.Progress $ putErr (if rec then "*" else ".")
        return (programDs mprog)
    lintCheckProgram onerrNone prog
    progress "Initial optimization pass"


    prog <- programMapRecGroups initMap (const return) (const return) (const return) fint prog
    progress "!"
    hFlush stdout >> hFlush stderr

    wdump FD.Progress $
        Stats.print "Initial Pass Stats" initialPassStats
    lintCheckProgram onerrNone prog
    progress "Big Simplify"
    prog <- barendregtProg prog
    prog <- simplifyProgram' sopt "SuperSimplify" True (IterateMax 4) prog
    prog <- barendregtProg prog
    progress "Big Type Anasysis"
    prog <- transformProgram "typeAnalyze" DontIterate True (typeAnalyze True) prog
    prog <- barendregtProg prog
    --prog <- simplifyProgram sopt "SuperSimplify" True prog

    progress "Big Eta Expansion"
    prog <- etaExpandProg prog

    -- This is the main function that optimizes the routines before writing them out
    let f (retds,(smap,annmap,idHist')) (rec,ns) = do
        let names = [ n | (n,_) <- ns]
        let namesInscope' = fromDistinctAscList (mkeys smap) `union` namesInscope
        when coreMini $ putErrLn ("----\n" ++ pprint names)
        cds <- annotateDs annmap (idann allRules mempty) letann lamann [ (t,e) | (t,e) <- ns]
        --putStrLn "*** After annotate"
        when miniCorePass $ mapM_ (\ (v,lc) -> printCheckName'' fullDataTable v lc) cds
        let cm stats e = do
            let sopt = mempty {  SS.so_boundVars = smap, SS.so_dataTable = fullDataTable }
            let (e',_) = SS.collectOccurance' e
            let (stat, e'') = SS.simplifyE sopt e'
            when miniCorePass  $ printCheckName fullDataTable e''
            Stats.tickStat stats stat
            return e''
        let mangle = mangle' (Just $ namesInscope' `union` fromList (map (tvrIdent . fst) cds)) fullDataTable
        cds <- flip mapM cds $ \ (v,lc) -> do
            lintCheckE onerrNone fullDataTable v lc
            (v,lc) <- Stats.runStatIO stats (runNameMT $ etaExpandDef' fullDataTable v lc)
            lc <- doopt mangle coreMini stats ("SuperSimplify 1: " ++ pprint v) cm lc
            lc <- mangle (return ()) coreMini ("Barendregt: " ++ pprint v) (return . barendregt) lc
            lc <- doopt mangle coreMini stats "Float Inward..." (\stats x -> return (floatInward x)) lc
            lintCheckE onerrNone fullDataTable v lc
            return (v,lc)
        wdump FD.Lambdacube $ mapM_ (\ (v,lc) -> printCheckName'' fullDataTable v lc) cds
        cds <- E.Strictness.solveDs cds
        cds <- flip mapM cds $ \ (v,lc) -> do
            lintCheckE onerrNone fullDataTable v lc
            (v,lc) <- Stats.runStatIO stats (runNameMT $ etaExpandDef' fullDataTable v lc)
            lc <- doopt mangle coreMini stats ("SuperSimplify 2: " ++ pprint v) cm lc
            lc <- mangle (return ()) coreMini ("Barendregt: " ++ pprint v) (return . barendregt) lc
            lintCheckE onerrNone fullDataTable v lc
            return (v,lc)

        -- cds <- E.Strictness.solveDs cds
        cds <- return (E.CPR.cprAnalyzeDs fullDataTable cds)
        --cds' <- return $ concatMap (uncurry (workWrap fullDataTable)) cds
        when miniCorePass  $ mapM_ (\ (v,lc) -> printCheckName' fullDataTable v lc) cds
        let (cds',st) = performWorkWrap fullDataTable cds
        Stats.tickStat stats st
        let wws = length cds' - length cds
        wdump FD.Progress $ putErr (replicate wws 'w')

        let graph = (newGraph cds' (\ (b,_) -> tvrIdent b) (\ (b,c) -> idSetToList $ bindingFreeVars b c))
            (lb,os) = findLoopBreakers (const 1) nogood graph
            nogood (b,_) = not $ getProperty prop_PLACEHOLDER b || getProperty prop_WRAPPER b
            cds = [ if x `elem` fsts lb then (setProperty prop_NOINLINE x,y) else (x,y) | (x,y) <- os  ]
        sequence_ [lintCheckE onerrNone fullDataTable v e | (v,e) <- cds ]
        cds <- annotateDs annmap (\_ -> return) letann lamann cds
        sequence_ [lintCheckE onerrNone fullDataTable v e | (v,e) <- cds ]

        let mangle = mangle' (Just $ namesInscope' `union` fromList (map (tvrIdent . fst) cds')) fullDataTable
        let dd  (ds,used) (v,lc) = do
                let cm stats e = do
                    let sopt = mempty {  SS.so_boundVars = fromList [ (tvrIdent v,(v,lc)) | (v,lc) <- ds] `union` smap,  SS.so_dataTable = fullDataTable }
                    let (e',_) = SS.collectOccurance' e
                    let (stat, e'') = SS.simplifyE sopt e'
                    when miniCorePass  $ printCheckName fullDataTable e''
                    Stats.tickStat stats stat
                    return e''
                let (lc', _) = runRename used lc
                lc <- doopt mangle False stats ("SuperSimplify PostPWW: " ++ pprint v) cm lc'
                let (lc', used') = runRename used lc
                return ((v,lc'):ds,used' `mappend` used)
        (cds,usedids) <- foldM dd ([],fromDistinctAscList $ Set.toList $ hoUsedIds ho) cds
        cds <- E.Strictness.solveDs cds
        cds <- return (E.CPR.cprAnalyzeDs fullDataTable cds)
        cds <- annotateDs annmap (\_ -> return) letann lamann cds
        wdump FD.Lambdacube $ mapM_ (\ (v,lc) -> printCheckName' fullDataTable v lc) cds
        let toName t
                | Just n <- fromId (tvrIdent t) = n
                | otherwise = error $ "toName: " ++ tvrShowName t
        let nvls = [ (t,e)  | (t,e) <- cds ]

        wdump FD.Progress $ putErr (if rec then "*" else ".")
        return (nvls ++ retds, (fromList [ (tvrIdent v,(v,lc)) | (v,lc) <- nvls] `union` smap, fromList [ (tvrIdent v,(Just (EVar v))) | (v,_) <- nvls] `union` annmap , idHist' ))



    let graph =  (newGraph (programDs prog) (\ (b,_) -> tvrIdent b) (\ (b,c) -> idSetToList $ bindingFreeVars b c))
        fscc (Left n) = (False,[n])
        fscc (Right ns) = (True,ns)
    progress "Optimization pass with workwrapping/CPR"
    (ds,_) <- foldM f ([],(fromList [ (tvrIdent v,(v,e)) | (v,e) <- Map.elems (hoEs ho)], initMap, Set.empty)) (map fscc $ scc graph)
    progress "!"
    prog <- return $ programSetDs ds prog
    prog <- programPrune prog
    Stats.print "Optimization" stats

    --prog <- if (fopts FO.TypeAnalysis) then do typeAnalyze True prog else return prog
    prog <- transformProgram "typeAnalyze" DontIterate True (typeAnalyze True) prog


    prog <- if True then do
        prog <- barendregtProg prog
        prog <- return $ etaAnnotateProgram prog
        progress "Post typeanalyis/etaexpansion pass"
        let graph =  (newGraph (programDs prog) (\ (b,_) -> tvrIdent b) (\ (b,c) -> idSetToList $ bindingFreeVars b c))
            fscc (Left n) = (False,[n])
            fscc (Right ns) = (True,ns)
        (ds,_) <- foldM f ([],(fromList [ (tvrIdent v,(v,e)) | (v,e) <- Map.elems (hoEs ho)], initMap, Set.empty)) (map fscc $ scc graph)
        progress "!"
        return $ programSetDs ds prog
      else return prog
    prog <- programPrune prog

    wdump FD.Lambdacube $ printProgram prog

    Stats.print "Optimization" stats
    return ho' { hoDataTable = dataTable, hoEs = programEsMap prog , hoRules = hoRules ho' `mappend` rules, hoUsedIds = collectIds (ELetRec (programDs prog) Unknown) }

programPruneUnreachable :: Program -> Program
programPruneUnreachable prog = programSetDs ds' prog where
    ds' = reachable (newGraph (programDs prog) (tvrIdent . fst) (\ (t,e) -> idSetToList $ bindingFreeVars t e)) (map tvrIdent $ progEntryPoints prog)

programPrune :: Program -> IO Program
programPrune prog = transformProgram "Prune Unreachable" DontIterate (dump FD.CorePass || coreMini) (return . programPruneUnreachable) prog

etaExpandProg :: Program -> IO Program
etaExpandProg prog = do
    let (prog',stats) = Stats.runStatM $  etaExpandProgram prog
    transformProgram "eta expansion" DontIterate (dump FD.CorePass || coreMini) (const $ return prog' { progStats = progStats prog' `mappend` stats }) prog

getExports ho =  Set.fromList $ map toId $ concat $  Map.elems (hoExports ho)
shouldBeExported exports tvr
    | tvrIdent tvr `Set.member` exports || getProperty prop_SRCLOC_ANNOTATE_FUN tvr  = setProperty prop_EXPORTED tvr
    | otherwise = tvr


collectIds e = execWriter $ annotate mempty (\id nfo -> tell (Set.singleton id) >> return nfo) (\_ -> return) (\_ -> return) e
--idHistogram e = execWriter $ annotate mempty (\id nfo -> tell (Histogram.singleton id) >> return nfo) (\_ -> return) (\_ -> return) e

isInteractive :: IO Bool
isInteractive = do
    pn <- System.getProgName
    return $ (optMode options == Interactive)
          || "ichj" `isPrefixOf` reverse pn
          || not (null $ optStmts options)


compileModEnv' stats (initialHo,finalHo) = do
    let ho = initialHo `mappend` finalHo

    let dataTable = progDataTable prog
        rules = hoRules ho
        prog = (hoToProgram ho) { progClosed = True }

    -- dump final version of various requested things
    wdump FD.Datatable $ putErrLn (render $ showDataTable dataTable)
    when (dump FD.ClassSummary) $ do
        putStrLn "  ---- class summary ---- "
        printClassSummary (hoClassHierarchy ho)
    when (dump FD.Class) $ do
        putStrLn "  ---- class hierarchy ---- "
        printClassHierarchy (hoClassHierarchy ho)
    wdump FD.Rules $ printRules rules

    -- enter interactive mode
    int <- isInteractive
    if int then Interactive.interact ho else do
    if optMode options == CompileHo then return () else do

    let mainFunc = parseName Val (maybe "Main.main" snd (optMainFunc options))
    (_,main,mainv) <- getMainFunction dataTable mainFunc (programEsMap prog)
    prog <- return prog { progMainEntry = main, progEntryPoints = [main], progCombinators = (main,[],mainv):[ (unsetProperty prop_EXPORTED t,as,e) | (t,as,e) <- progCombinators prog] }
    prog <- transformProgram "Initial Prune Unreachable" DontIterate False (return . programPruneUnreachable) prog
    prog <- barendregtProg prog

    --wdump FD.Lambdacube $ printProgram prog
    prog <- if (fopts FO.TypeAnalysis) then do typeAnalyze False prog else return prog
    putStrLn "Type analyzed methods"
    flip mapM_ (programDs prog) $ \ (t,e) -> do
        let (_,ts) = fromLam e
            ts' = takeWhile (sortStarLike . getType) ts
        when (not (null ts')) $ putStrLn $ (pprint t) ++ " \\" ++ concat [ "(" ++ show  (Info.fetch (tvrInfo t) :: Typ) ++ ")" | t <- ts' ]
    lintCheckProgram onerrNone prog
    prog <- programPrune prog
    --wdump FD.Lambdacube $ printProgram prog

    cmethods <- do
        let es' = concatMap expandPlaceholder (programDs prog)
        es' <- return [ (y,floatInward z) |  (y,z) <- es' ]
        wdump FD.Class $ do
            sequence_ [ printCheckName' dataTable y z |  (y,z) <- es']
        return es'

    prog <- return $ programSetDs ([ (t,e) | (t,e) <- programDs prog, t `notElem` fsts cmethods] ++ cmethods) prog
    prog <- annotateProgram mempty (\_ nfo -> return $ unsetProperty prop_INSTANCE nfo) letann (\_ nfo -> return nfo) prog


    unless (fopts FO.GlobalOptimize) $ do
        prog <- programPrune prog
        prog <- barendregtProg prog
        wdump FD.CoreBeforelift $ printProgram prog
        finalStats <- Stats.new
        prog <- transformProgram "lambda lift" DontIterate (dump FD.Progress) (lambdaLift finalStats) prog
        wdump FD.Progress $ Stats.print "PostLifting" finalStats
        wdump FD.CoreAfterlift $ printProgram prog -- printCheckName dataTable (programE prog)
        compileToGrin prog
        exitSuccess

    st <- Stats.new

    let mangle = mangle'  (Just mempty)
    let opt = doopt (mangle dataTable) True stats
    let showTVr t = prettyE (EVar t) <> show (tvrInfo t)


    prog <- transformProgram "typeAnalyze after method" DontIterate True (typeAnalyze True) prog
    prog <- barendregtProg prog


    prog <- simplifyProgram mempty "SuperSimplify pass 1" True prog
    prog <- barendregtProg prog


    st <- Stats.new
    prog <- etaExpandProg prog
    prog <- barendregtProg prog
    prog <- transformProgram "typeAnalyze" DontIterate True (typeAnalyze True) prog


    prog <- barendregtProg prog
    prog <- simplifyProgram mempty "SuperSimplify pass 2" True prog
    prog <- barendregtProg prog


    -- run optimization again with no rules enabled

    -- delete rules
    prog <- return $ runIdentity $ annotateProgram mempty (\_ nfo -> return $ Info.delete (mempty :: ARules) nfo) letann (\_ -> return) prog

    prog <- simplifyProgram mempty { SS.so_finalPhase = True } "SuperSimplify no rules" True prog
    prog <- barendregtProg prog

    -- perform lambda lifting
    wdump FD.CoreBeforelift $ printProgram prog
    finalStats <- Stats.new
    prog <- transformProgram "lambda lift" DontIterate (dump FD.Progress) (lambdaLift finalStats) prog

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

    wdump FD.OptimizationStats $ Stats.print "Optimization" stats
    wdump FD.Progress $ printEStats (programE prog)
    compileToGrin prog


compileToGrin prog = do
    stats <- Stats.new
    progress "Converting to Grin..."
    prog <- Mangle.mangle prog
    wdump FD.CoreMangled $ printUntypedProgram prog -- printCheckName dataTable (programE prog)
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
        nf <- mapMsnd (grinPush stats') (grinFunctions x)
        x <- return x { grinFunctions = nf }
        wdump FD.GrinPass $ printGrin x
        x <- Grin.Simplify.simplify stats' x
        lintCheckGrin x
        t' <- Stats.getTicks stats'
        wdump FD.Progress $ Stats.print s stats'
        Stats.combine stats stats'
        case t' of
            0 -> return x
            _ -> opt s x
    x <- deadCode stats (grinEntryPoints x) x  -- XXX
    lintCheckGrin x
    x <- opt "Optimization" x
    lintCheckGrin x
    x <- grinSpeculate x
    lintCheckGrin x
    x <- deadCode stats (grinEntryPoints x) x  -- XXX
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
        x <- deadCode stats (grinEntryPoints x) x
        lintCheckGrin x
        x <- return $ normalizeGrin x
        lintCheckGrin x
        x <- opt "AE Optimization 2" x
        x <- unboxReturnValues x
        lintCheckGrin x
        x <- deadCode stats (grinEntryPoints x) x
        lintCheckGrin x
        x <- opt "AE Optimization 3" x
        wdump FD.OptimizationStats $ Stats.print "AE Optimization" stats
        x <- return $ normalizeGrin x
        lintCheckGrin x

        printTable "Return points-to" (grinReturnTags x)
        printTable "Argument points-to" (grinArgTags x)
        wdump FD.Grin $ printGrin x
        when (optMode options == CompileExe) $ compileGrinToC x
     else do
        x <- createEvalApply x
        x <- return $ normalizeGrin x
        lintCheckGrin x
        wdump FD.Grin $ printGrin x
        when (optMode options == CompileExe) $ compileGrinToC x

compileGrinToC grin = do
    when (optMode options == Interpret) $ do
        progress "Interpreting..."
        (v,stats) <- Grin.Interpret.evaluate grin
        CharIO.putStrLn $ render $ Grin.Show.prettyVal v
        wdump FD.Stats $  Stats.print "Stats" stats
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

simplifyProgram sopt name dodump prog = do
    let istat = progStats prog
    let g prog = do
            let nprog = SS.programPruneOccurance prog
            when (corePass && dodump) $ do
                putStrLn "-- After Occurance Analysis"
                printProgram nprog
            return $ SS.programSSimplify sopt { SS.so_dataTable = progDataTable prog } nprog
    prog <- transformProgram name IterateDone dodump g prog  { progStats = mempty }
    when (dodump && (dump FD.Progress || coreSteps)) $ Stats.printStat ("Total: " ++ name) (progStats prog)
    return prog { progStats = progStats prog `mappend` istat }

simplifyProgramPStat sopt name dodump prog = do
    let istat = progStats prog
    let g =  SS.programSSimplifyPStat sopt { SS.so_dataTable = progDataTable prog } . SS.programPruneOccurance
    prog <- transformProgram ("PS:" ++ name) IterateDone dodump g prog  { progStats = mempty }
    when ((dodump && dump FD.Progress) || dump FD.CoreSteps) $ Stats.printStat ("Total: " ++ name) (progStats prog)
    return prog { progStats = progStats prog `mappend` istat }

simplifyProgram' sopt name dodump iterate prog = do
    let istat = progStats prog
    let g =  return . SS.programSSimplify sopt { SS.so_dataTable = progDataTable prog } . SS.programPruneOccurance
    prog <- transformProgram name iterate dodump g prog  { progStats = mempty }
    when (dodump && (dump FD.Progress || coreSteps)) $ Stats.printStat ("Total: " ++ name) (progStats prog)
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

transformProgram ::
    String                      -- ^ name of pass
    -> Iterate                     -- ^ wether to iterate
    -> Bool                     -- ^ whether to dump progress
    -> (Program -> IO Program)  -- ^ what to run
    -> Program
    -> IO Program

transformProgram _ (IterateMax n) _ _ prog | n <= 0 = return prog
transformProgram _ (IterateExactly n) _ _ prog | n <= 0 = return prog
transformProgram name iterate dodump f prog = do
    when dodump $ putErrLn $ "-- " ++ name
    when (dodump && dump FD.CorePass) $ printProgram prog
    let istat = progStats prog
    let ferr e = do
        putErrLn $ "\n>>> Exception thrown"
        putErrLn $ "\n>>> Before " ++ name
        printProgram prog
        putErrLn $ "\n>>>"
        putErrLn (show e)
        maybeDie
        return prog
    prog' <- Control.Exception.catch (f prog { progStats = mempty }) ferr
    let estat = progStats prog'
        onerr = do
            putErrLn $ "\n>>> Before " ++ name
            printProgram prog
            Stats.printStat name estat
            putErrLn $ "\n>>> After " ++ name
    when (dodump && dump FD.CoreSteps) $ Stats.printStat name estat
    lintCheckProgram onerr prog'
    if doIterate iterate estat then transformProgram name (iterateStep iterate) dodump f prog' { progStats = istat `mappend` estat } else
        return prog' { progStats = istat `mappend` estat, progPasses = name:progPasses prog' }



-- these are way to complicated and should be simplified

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

onerrNone = return ()
onerrProg prog = putErrLn ">>> Before" >> printProgram prog

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
    let f (tvr,e) = lintCheckE onerr (progDataTable prog) tvr e
    when (hasRepeatUnder fst (programDs prog)) $ do
        onerr
        putErrLn ">>> Repeated top level decls"
        printProgram prog
        putErrLn ">>> program has repeated toplevel definitions"
        maybeDie
    mapM_ f (programDs prog)
    let ids = progExternalNames prog `mappend` fromList (map tvrIdent $ fsts (programDs prog))
        fvs = Set.fromList $ melems (freeVars $ snds $ programDs prog :: IdMap TVr)
        unaccounted = Set.filter (not . (`member` ids) . tvrIdent) fvs
    unless (Set.null unaccounted) $ do
        onerr
        putErrLn ("\n>>> Unaccounted for free variables: " ++ render (pprint $ Set.toList $ unaccounted))
        printProgram prog
        putErrLn (">>> Unaccounted for free variables: " ++ render (pprint $ Set.toList $ unaccounted))
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

printProgram prog@Program {progCombinators = cs, progDataTable = dataTable } = do
    sequence_ $ intersperse (putErrLn "") [ printCheckName'' dataTable v (foldr ELam e as) | (v,as,e) <- cs]
    when (progMainEntry prog /= tvr) $
        putErrLn $ "MainEntry: " ++ pprint (progMainEntry prog)
    when (progEntryPoints prog /= [progMainEntry prog]) $
        putErrLn $ "EntryPoints: " ++ hsep (map pprint (progEntryPoints prog))

printUntypedProgram prog@Program {progCombinators = cs, progDataTable = dataTable } = do
    let pp tvr e = putErrLn (render $ hang 4 (pprint tvr <+> equals <+> pprint e))
    sequence_ $ intersperse (putErrLn "") [ pp v (foldr ELam e as) | (v,as,e) <- cs]
    when (progMainEntry prog /= tvr) $
        putErrLn $ "MainEntry: " ++ pprint (progMainEntry prog)
    when (progEntryPoints prog /= [progMainEntry prog]) $
        putErrLn $ "EntryPoints: " ++ hsep (map pprint (progEntryPoints prog))

printCheckName'' :: DataTable -> TVr -> E -> IO ()
printCheckName'' dataTable tvr e = do
    let (ty,pty) = case inferType dataTable [] e of
            Left err -> (Unknown,vcat $ map text (intersperse "---" $ tail err))
            Right ty -> (ty,pprint ty)
        tmatch = isJust $ match (const Nothing) [] ty (tvrType tvr)
    when (dump FD.EInfo || verbose2) $ putErrLn (show $ tvrInfo tvr)
    putErrLn (render $ hang 4 (pprint tvr <+> text "::" <+> (pprint $ tvrType tvr)))
    when (not tmatch || dump FD.EVerbose) $
        putErrLn (render $ hang 4 (pprint tvr <+> text "::" <+> pty))
    putErrLn (render $ hang 4 (pprint tvr <+> equals <+> pprint e))









