
module Main(main) where

import Control.Exception
import Control.Monad.Identity
import Control.Monad.Writer
import Data.Monoid
import List hiding(group)
import Prelude hiding(putStrLn, putStr,print)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified System

import CanType(getType)
import C.FromGrin
import CharIO
import Class
import C.Prims
import DataConstructors
import Doc.DocLike
import Doc.PPrint
import Doc.Pretty
import E.Annotate(annotate,annotateDs)
import E.Arbitrary()
import E.Diff
import E.E
import E.FromHs
import E.Inline
import E.LambdaLift
import E.LetFloat
import E.Pretty hiding(render)
import E.Rules
import E.Strictness
import E.Subst
import E.Traverse
import E.TypeAnalysis
import E.TypeCheck
import E.WorkerWrapper
import FreeVars
import FrontEnd.FrontEnd
import GenUtil hiding(replicateM,putErrLn,putErr,putErrDie)
import Grin.DeadFunctions
import Grin.FromE
import Grin.Grin
import Grin.Show
import Grin.Whiz
import Ho
import HsSyn
import Info.Types
import Name.Name
import Options
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
import Util.Graph

---------------
-- ∀α∃β . α → β
---------------




bracketHtml action = do
    pn <- System.getProgName
    as <- System.getArgs
    wdump FD.Html $ putStrLn $ "<html><head><title>" ++ (unwords (pn:as)) ++ "</title><meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\"></head><body style=\"background: black; color: lightgrey\"><pre>"
    action `finally` (wdump FD.Html $ putStrLn "</pre></body></html>")

main = runMain $ bracketHtml $ do
    o <- processOptions
    wdump FD.Progress $ do
        name <- System.getProgName
        args <- System.getArgs
        putStrLn (simpleQuote (name:args))
    case o of
        Opt { optShowHo = xs@(_:_) } -> mapM_ dumpHoFile xs
        Opt { optBuildHl = hlName@(_:_) } -> buildHl hlName (optArgs o)
        _ -> processFiles  (optArgs o)

buildHl fname [] = putErrDie "Cannot build hl file without list of input modules"
buildHl fname ms = do
    stats <- Stats.new
    me <- parseFiles [] (map Module ms) processInitialHo (processDecls stats)
    recordHoFile me [fname] HoHeader { hohGeneration = 0, hohDepends = [], hohModDepends = [] }
    return ()

processFiles [] | Nothing <- optMainFunc options = do
    putErrDie "jhc: no input files"
processFiles [] | Just (b,m) <- optMainFunc options = do
    m <- return $ parseName Val m
    Module m <- getModule m
    stats <- Stats.new
    me <- parseFiles [] [Module m] processInitialHo (processDecls stats)
    compileModEnv' stats me
processFiles fs = do
    stats <- Stats.new
    me <- parseFiles  fs [] processInitialHo (processDecls stats)
    compileModEnv' stats me

barendregt e = runIdentity  (renameTraverse' e)

manifestLambdas :: E -> Arity
manifestLambdas e = Arity (f 0 e) where
    f n (ELam _ e) = let n' = n + 1 in n' `seq` f n' e
    f n _ = n

lamann _ nfo = return nfo
letann e nfo = return (Info.insert (manifestLambdas e) nfo)
idann rs ps i nfo = return (props ps i nfo `mappend` rules rs i) where
    props ps i = case tvrName (tvr { tvrIdent = i }) of
        Just n -> case Map.lookup n ps of
            Just ps ->  setProperties ps
            Nothing ->  id
        Nothing -> id
    rules rs i = Info.maybeInsert (getARules rs i) Info.empty

--annotateMethods ch rs ps = (Map.fromList [ (tvrIdent t, Just (EVar t)) | t <- ts ]) where
--    ts = [ let Identity x = idann rs ps (tvrIdent t) (tvrInfo t) in t { tvrInfo = x  } | t <-methodNames ch ]

processInitialHo :: Ho -> IO Ho
processInitialHo ho = do
    putStrLn $ "Initial annotate: " ++ show (Map.keys $ hoModules ho)
    --let imap = annotateMethods (hoClassHierarchy ho) (hoRules ho) (hoProps ho)
    --let f (ds,used) (v,lc) = ((v,lc'):ds,used `mappend` used') where
    --        (lc',used') = runRename used lc
    --    (nds,allUsed) = foldl f ([],Set.empty) (Map.elems $ hoEs ho)
    let Identity ds = annotateDs mempty (idann (hoRules ho) (hoProps ho) ) letann lamann (Map.elems $ hoEs ho)
    wdump FD.Rules $ printRules (hoRules ho)
    return ho { hoEs = Map.fromList [ (runIdentity $ fromId (tvrIdent v),d) |  d@(v,_) <- ds ] }


-- | this is called on parsed, typechecked haskell code to convert it to the internal representation

processDecls ::
    Stats.Stats   -- ^ statistics
    -> Ho     -- ^ Collected ho
    -> Ho     -- ^ preliminary haskell object  data
    -> TiData -- ^ front end output
    -> IO Ho  -- ^ final haskell object file
processDecls stats ho ho' tiData = do
    -- some useful values
    let allHo = ho `mappend` ho'
        decls = concat [ hsModuleDecls  m | (_,m) <- tiDataModules tiData ] ++ Map.elems (tiDataLiftedInstances tiData)

    -- build datatables
    let dataTable = toDataTable (Map.fromList $[ (toName TypeConstructor x,y) | (x,y)<- Map.toList (hoKinds ho')] ) (tiAllAssumptions tiData) decls
    let fullDataTable = (dataTable `mappend` hoDataTable ho)
    wdump FD.Datatable $ putErrLn (render $ showDataTable dataTable)

    -- Convert Haskell decls to E
    let allAssumps = (tiAllAssumptions tiData `mappend` hoAssumps ho)
    ds' <- convertDecls (hoClassHierarchy ho') allAssumps  fullDataTable decls
    let mnames = methodNames (hoClassHierarchy ho')
        ds = ds' ++ [ (runIdentity $ fromId (tvrIdent t),setProperties [prop_PLACEHOLDER,prop_EXPORTED] t, EPrim (primPrim ("Placeholder: " ++ tvrShowName t)) [] (getType t)) | t <- mnames, not $ t `Set.member` cnames]
        cnames = Set.fromList $ fsts $ Map.elems $ hoEs ho

    -- Build rules
    rules <- createInstanceRules (hoClassHierarchy ho' `mappend` hoClassHierarchy initialHo)   (Map.fromList [ (x,(y,z)) | (x,y,z) <- ds] `mappend` hoEs ho)
    let allRules = hoRules ho `mappend` rules `mappend` hoRules ho'
    wdump FD.Rules $ printRules allRules

    -- some more useful values.
    let inscope =  [ tvrIdent n | (n,_) <- Map.elems $ hoEs ho ] ++ [tvrIdent n | (_,n,_) <- ds ] ++ map tvrIdent (methodNames (hoClassHierarchy allHo))
        mangle = mangle' (Just $ Set.fromList $ inscope) fullDataTable
        exports = getExports ho'
        classNames = Set.fromList $ map tvrIdent (methodNames (hoClassHierarchy allHo))
        namesInscope = Set.fromList inscope -- classNames `Set.union` (Set.fromAscList $ Map.keys smap)

    -- initial pass over functions to put them into a normalized form
    let procE (ds,usedIds) (n,v,lc) = do
        lc <- postProcessE stats n inscope usedIds fullDataTable lc
        nfo <- idann  allRules (hoProps ho') (tvrIdent v) (tvrInfo v)
        v <- return $ v { tvrInfo = Info.insert LetBound nfo }
        let used' = collectIds lc
        --let (lc',used') = runRename usedIds lc
        return ((n, shouldBeExported exports v,lc):ds,usedIds `mappend` used')
    (ds,_allIds) <- foldM procE ([],hoUsedIds ho) ds


    -- This is the main function that optimizes the routines before writing them out
    let f (retds,(smap,annmap,idHist')) (rec,ns) = do
        let names = [ n | (n,_,_) <- ns]
        let namesInscope' = Set.fromAscList (Map.keys smap) `Set.union` namesInscope
        when (dump FD.Lambdacube || dump FD.Pass) $ putErrLn ("----\n" ++ show names)
        cds <- annotateDs annmap (idann (hoRules allHo) mempty) letann lamann [ (t,e) | (_,t,e) <- ns]
        --putStrLn "*** After annotate"
        --wdump FD.Lambdacube $ mapM_ (\ (v,lc) -> printCheckName' fullDataTable v lc) cds
        let cm stats e = do
            let sopt = mempty { SS.so_superInline = True, SS.so_exports = inscope, SS.so_boundVars = smap, SS.so_rules = allRules, SS.so_dataTable = fullDataTable }
            let (stat, e') = SS.simplifyE sopt e
            Stats.tickStat stats stat
            return e'
        let mangle = mangle' (Just $ namesInscope' `Set.union` Set.fromList (map (tvrIdent . fst) cds)) fullDataTable
        cds <- flip mapM (zip names cds) $ \ (n,(v,lc)) -> do
            lc <- doopt mangle False stats "SuperSimplify" cm lc
            lc <- mangle (return ()) False ("Barendregt: " ++ show n) (return . barendregt) lc
            lc <- doopt mangle False stats "Float Inward..." (\stats x -> return (floatInward allRules x)) lc
            return (v,lc)
        cds <- E.Strictness.solveDs cds
        cds <- flip mapM (zip names cds) $ \ (n,(v,lc)) -> do
            lc <- doopt mangle False stats "SuperSimplify" cm lc
            lc <- mangle (return ()) False ("Barendregt: " ++ show n) (return . barendregt) lc
            return (v,lc)

        -- cds <- E.Strictness.solveDs cds
        cds <- return (E.CPR.cprAnalyzeDs fullDataTable cds)
        --cds' <- return $ concatMap (uncurry (workWrap fullDataTable)) cds
        wdump FD.Lambdacube $ mapM_ (\ (v,lc) -> printCheckName' fullDataTable v lc) cds
        let (cds',st) = performWorkWrap fullDataTable cds
        Stats.tickStat stats st
        let wws = length cds' - length cds
        wdump FD.Progress $ putErr (replicate wws 'w')

        let graph = (newGraph cds' (\ (b,_) -> tvrIdent b) (\ (b,c) -> bindingFreeVars b c))
            (lb,os) = findLoopBreakers (const 1) nogood graph
            nogood (b,_) = not $ getProperty prop_PLACEHOLDER b || getProperty prop_WRAPPER b
            cds = [ if x `elem` fsts lb then (setProperty prop_NOINLINE x,y) else (x,y) | (x,y) <- os  ]
        cds <- annotateDs annmap (\_ -> return) letann lamann cds

        let mangle = mangle' (Just $ namesInscope' `Set.union` Set.fromList (map (tvrIdent . fst) cds')) fullDataTable
        let dd  (ds,used) (v,lc) = do
                let cm stats e = do
                    let sopt = mempty { SS.so_superInline = True, SS.so_exports = inscope, SS.so_boundVars = Map.fromList [ (tvrIdent v,lc) | (v,lc) <- ds] `Map.union` smap, SS.so_rules = allRules, SS.so_dataTable = fullDataTable }
                    let (stat, e') = SS.simplifyE sopt e
                    Stats.tickStat stats stat
                    return e'
                let (lc', _) = runRename used lc
                lc <- doopt mangle False stats "SuperSimplify" cm lc'
                let (lc', used') = runRename used lc
                return ((v,lc):ds,used' `mappend` used)
        (cds,usedids) <- foldM dd ([],hoUsedIds ho) cds
        cds <- E.Strictness.solveDs cds
        cds <- return (E.CPR.cprAnalyzeDs fullDataTable cds)
        cds <- annotateDs annmap (\_ -> return) letann lamann cds
        --mapM_ (\t -> putStrLn (prettyE (EVar t) <+> show (tvrInfo t))) (fsts cds)
        wdump FD.Lambdacube $ mapM_ (\ (v,lc) -> printCheckName' fullDataTable v lc) cds
        let toName t
                | Just n <- fromId (tvrIdent t) = n
                | otherwise = error $ "toName: " ++ tvrShowName t
        let nvls = [ (toName t,t,e)  | (t,e) <- cds ]
        --let uidMap = Map.fromAscList [  (id,Nothing :: Maybe E) | id <- Set.toAscList $ Set.unions [ collectIds e| (t,e) <- cds]]
        let uidMap = Map.fromAscList [  (id,Nothing :: Maybe E) | id <- Set.toAscList usedids ]
        --let idHist = idHist' `mappend` Histogram.unions [ idHistogram e| (t,e) <- cds]
        --print idHist

        wdump FD.Progress $ putErr (if rec then "*" else ".")
        return (nvls ++ retds, (Map.fromList [ (tvrIdent v,lc) | (_,v,lc) <- nvls] `Map.union` smap, Map.fromList [ (tvrIdent v,(Just (EVar v))) | (_,v,_) <- nvls] `Map.union` annmap , idHist' ))

    -- preparing for optimization
    -- let imap = annotateMethods (hoClassHierarchy allHo) allRules (hoProps allHo)
    let initMap = Map.fromList [ (tvrIdent t, Just (EVar t)) | (t,_) <- (Map.elems (hoEs ho))]
        graph =  (newGraph ds (\ (_,b,_) -> tvrIdent b) (\ (_,b,c) -> bindingFreeVars b c))
        fscc (Left n) = (False,[n])
        fscc (Right ns) = (True,ns)
    (ds,_) <- foldM f ([],(Map.fromList [ (tvrIdent v,e) | (v,e) <- Map.elems (hoEs ho)], initMap, Set.empty)) (map fscc $ scc graph)
    wdump FD.Progress $ putErrLn "!"


    let ds' = reachable (newGraph ds (\ (_,b,_) -> tvrIdent b) (\ (_,b,c) -> bindingFreeVars b c)) [ tvrIdent b | (n,b,_) <- ds, getProperty prop_EXPORTED b]
    --wdump FD.OptimizationStats $ Stats.print "Optimization" stats
    Stats.print "Optimization" stats
    return ho' { hoDataTable = dataTable, hoEs = Map.fromList [ (x,(y,z)) | (x,y,z) <- ds'], hoRules = rules, hoUsedIds = collectIds (ELetRec [ (b,c) | (_,b,c) <- ds'] Unknown) }

-- | take E directly generated from haskell source and bring it into line with
-- expected invarients. this only needs be done once.  it replaces all
-- ambiguous types with the absurd one, gets rid of all newtypes, does a basic
-- renaming pass, and makes sure applications are only to atomic variables.

postProcessE :: Stats.Stats -> Name -> [Id] -> Set.Set Id -> DataTable -> E -> IO E
postProcessE stats n inscope usedIds dataTable lc = do
    let g (TVr { tvrIdent = 0 }) = error "absurded zero"
        g tvr@(TVr { tvrIdent = n, tvrType = k})
            | sortStarLike k =  tAbsurd k
            | otherwise = EVar tvr
    fvs <- return $ foldr Map.delete (freeVars lc)  inscope
    when (Map.size fvs > 0 && dump FD.Progress) $ do
        putDocM putErr $ parens $ text "Absurded vars:" <+> align (hsep $ map pprint (Map.elems fvs))
    let mangle = mangle' (Just $ Set.fromList $ inscope) dataTable
    lc <- mangle (return ()) False ("Absurdize") (return . substMap (Map.map g fvs)) lc
    lc <- mangle (return ()) False "deNewtype" (return . deNewtype dataTable) lc
    --lc <- mangle (return ()) False ("Barendregt: " ++ show n) (return . barendregt) lc
    lc <- doopt mangle False stats "FixupLets..." (\stats x -> atomizeApps usedIds stats x >>= coalesceLets stats)  lc
    return lc

getExports ho =  Set.fromList $ map toId $ concat $  Map.elems (hoExports ho)
shouldBeExported exports tvr
    | tvrIdent tvr `Set.member` exports || getProperty prop_INSTANCE tvr || getProperty prop_SRCLOC_ANNOTATE_FUN tvr  = setProperty prop_EXPORTED tvr
    | otherwise = tvr


collectIds e = execWriter $ annotate mempty (\id nfo -> tell (Set.singleton id) >> return nfo) (\_ -> return) (\_ -> return) e
--idHistogram e = execWriter $ annotate mempty (\id nfo -> tell (Histogram.singleton id) >> return nfo) (\_ -> return) (\_ -> return) e

compileModEnv' stats ho = do

    let dataTable = hoDataTable ho
    let rules = hoRules ho
    wdump FD.Datatable $ putErrLn (render $ showDataTable dataTable)

    if optInteractive options then Interactive.interact ho else do

    --mapM_ putErrLn ([ show x <+> "::" <+> render (ePretty ty) | (x,(TVr _ ty,_)) <- Map.toList $ hoEs ho])
    let mainFunc = parseName Val (maybe "Main.main" snd (optMainFunc options))

    when (dump FD.ClassSummary) $ do
        putStrLn "  ---- class summary ---- "
        printClassSummary (hoClassHierarchy ho)
    when (dump FD.Class) $ do
        putStrLn "  ---- class hierarchy ---- "
        printClassHierarchy (hoClassHierarchy ho)

    es' <- createMethods dataTable (hoClassHierarchy ho) (hoEs ho)
    let initMap = Map.fromList [ (tvrIdent t, Just (EVar t)) | (t,_) <- (Map.elems (hoEs ho)), not $ t `Set.member` tmap]
        tmap = Set.fromList $ [ t | (_,t,_) <- es' ]
    let Identity es'' = annotateDs initMap (idann (hoRules ho) (hoProps ho) ) letann lamann [ (y,z) | (x,y,z) <- es']

    es' <- return [ (x,y,floatInward rules z) | (x,_,_) <- es' | (y,z) <- es'' ]
    wdump FD.Class $ do
        sequence_ [ print x >> printCheckName' dataTable y z |  (x,y,z) <- es']
    let es = Map.fromList [ (x,(y,z)) |  (x,y,z) <- es'] `mappend` hoEs ho
    (_,main,mainv) <- getMainFunction dataTable mainFunc es
    let ds = ((main,mainv):Map.elems es)
    let ds' = reachable (newGraph ds (tvrIdent . fst) (\ (t,e) -> bindingFreeVars t e)) [tvrIdent main]

    let lco = ELetRec ds'  (EVar main)
    wdump FD.Rules $ printRules rules
    let mangle = mangle'  (Just mempty)
    let opt = doopt (mangle dataTable) True stats

    let showTVr t = prettyE (EVar t) <> show (tvrInfo t)

    when (fopts  FO.TypeAnalysis) $ do
        let ELetRec ds _ = lco in do
            putStrLn "Supercombinators"
            ds' <- typeAnalyze ds
            mapM_ (\ (t,e) -> let (_,ts) = fromLam e in putStrLn $  (showTVr t) ++ " \\" ++ concat [ "(" ++ show  (tvrInfo t) ++ ")" | t <- ts, sortStarLike (getType t) ] ) ds'

    lc <- mangle dataTable (return ()) True "Barendregt" (return . barendregt) lco
    wdump FD.Progress $ printEStats lc
    let cm stats e = do
        let sopt = mempty { SS.so_superInline = True, SS.so_rules = rules, SS.so_dataTable = dataTable }
        let (stat, e') = SS.simplifyE sopt e
        Stats.tickStat stats stat
        return e'

    --let imap = annotateMethods (hoClassHierarchy ho) (hoRules ho) (hoProps ho)
    lc <- return $ runIdentity $ annotate mempty (idann rules (hoProps ho) ) letann lamann lc
    lc <- opt "SuperSimplify" cm lc


    --let flLevels = annotateBindings mempty lc
    --print flLevels
    --printCheckName dataTable lc
    lc <- mangle dataTable (return ()) True "Barendregt" (return . barendregt) lc
    lc <- annotate mempty (\_ nfo -> return $ Info.delete (mempty :: ARules) nfo) (\_ -> return) (\_ -> return) lc
    let cm stats e = do
        let sopt = mempty { SS.so_dataTable = dataTable }
        let (stat, e') = SS.simplifyE sopt e
        Stats.tickStat stats stat
        return e'

    lc <- opt "SuperSimplify no Rules" cm lc


    let ds = scCombinators $ eToSC dataTable lc in do
        putStrLn "Supercombinators"
        mapM_ (\ (t,ts,e) -> putStrLn $  (showTVr t) ++ " \\" ++ concat [ "(" ++ show  (tvrInfo t) ++ ")" | t <- ts, sortStarLike (getType t) ]) ds


    wdump FD.LambdacubeBeforeLift $ printCheckName dataTable lc
    finalStats <- Stats.new
    lc <- mangle dataTable (return ()) True "LambdaLift" (lambdaLiftE finalStats dataTable) lc
    lc <- mangle dataTable (return ()) True  "FixupLets..." (\x -> atomizeApps mempty finalStats x >>= coalesceLets finalStats)  lc
    let SC v rs = eToSC dataTable lc

    rs' <- flip mapM rs $ \ (t,ls,e) -> do
        let cm stats e = do
            let sopt = mempty {  SS.so_dataTable = dataTable }
            let (stat, e') = SS.simplifyE sopt e
            Stats.tickStat stats stat
            return e'
        e' <- doopt (mangle' Nothing dataTable) False finalStats "SuperSimplify" cm e
        return (t,ls,e')
    wdump FD.Progress $ Stats.print "PostLifting" finalStats

    lc <- return $ scToE (SC v rs')


    wdump FD.Lambdacube $ printCheckName dataTable lc

    wdump FD.OptimizationStats $ Stats.print "Optimization" stats
    wdump FD.Progress $ printEStats lc

    stats <- Stats.new
    wdump FD.Progress $ putErrLn "Converting to Grin..."
    x <- Grin.FromE.compile dataTable (error "vmap") (eToSC dataTable lc)
    Stats.print "Grin" Stats.theStats
    --wdump FD.Grin $ printGrin x
    x <- return $ normalizeGrin x
    typecheckGrin x
    let opt s  x = do
        stats' <- Stats.new
        x <- deadFunctions stats' [funcMain] x
        x <- Grin.Simplify.simplify stats' x
        when flint $ typecheckGrin x
        t' <- Stats.getTicks stats'
        wdump FD.Progress $ Stats.print s stats'
        Stats.combine stats stats'
        case t' of
            0 -> return x
            _ -> opt s x
    x <- opt "Optimization" x
    wdump FD.OptimizationStats $ Stats.print "Optimization" stats
    x <- return $ normalizeGrin x
    typecheckGrin x
    wdump FD.GrinPreeval $ printGrin x
    wdump FD.Progress $ putErrLn "Points-to analysis..."
    stats <- Stats.new
    x <- Grin.PointsToAnalysis.grinInlineEvalApply stats x
    wdump FD.Progress $ Stats.print "EvalInline" stats
    typecheckGrin x
    wdump FD.GrinPosteval $ printGrin x
    stats <- Stats.new
    x <- return $ normalizeGrin x
    typecheckGrin x
    x <- opt "AE Optimization" x
    wdump FD.OptimizationStats $ Stats.print "AE Optimization" stats
    x <- return $ normalizeGrin x
    typecheckGrin x
    wdump FD.Grin $ printGrin x
    when (optInterpret options) $ do
        wdump FD.Progress $ putErrLn "Interpreting..."
        (v,stats) <- Grin.Interpret.evaluate x
        CharIO.putStrLn $ render $ Grin.Show.prettyVal v
        wdump FD.Stats $  Stats.print "Stats" stats
        return ()

    when (optCompile options) $ do
        let (cg,rls) = compileGrin x
        let fn = optOutName options
        let cf = (fn ++ "_code.c")
        wdump FD.Progress $ putErrLn ("Writing " ++ show cf)
        name <- System.getProgName
        args <- System.getArgs
        let argstring = simpleQuote (name:args)
        writeFile cf $ "char jhc_command[] = \"" ++ argstring ++ "\";\n" ++  cg
        let boehmOpts | fopts FO.Boehm = ["-DUSE_BOEHM_GC", "-lgc"]
                      | otherwise = []
        let profileOpts | fopts FO.Profile = ["-D_JHC_PROFILE"]
                      | otherwise = []
        let comm = shellQuote $ [optCC options, "-std=gnu99", "-foptimize-sibling-calls", "-O", {- "-funit-at-a-time", -} "-g", "-Wall", "-o", fn, cf ] ++ rls ++ optCCargs options  ++ boehmOpts ++ profileOpts
        wdump FD.Progress $ putErrLn ("Running: " ++ comm)
        r <- System.system comm
        when (r /= System.ExitSuccess) $ fail "C code did not compile."
        return ()


mangle ::
    DataTable                -- ^ the datatable used for typechecking
    -> Maybe (Set.Set Id)    -- ^ acceptable free variables
    -> String                -- ^ the name of the pass
    -> Bool                  -- ^ whether to dump progress
    -> Int                   -- ^ maximum number of passes to run. -1 for unlimited
    -> Stats.Stats                 -- ^ the stats to add results to
    -> (Stats.Stats -> E -> IO E)  -- ^ the modification routine
    -> E                     -- ^ the input term
    -> IO E                  -- ^ out it comes
mangle dataTable fv name dumpProgress count stats action e = do
    --when ((dumpProgress && dump FD.Progress) || dump FD.Pass) $ putErrLn $ "-- " ++ name
    let opt 0 e = return e
        opt n e = do
            stats' <- Stats.new
            e' <- mangle' fv dataTable (Stats.print "stats" stats') dumpProgress name (action stats') e
            t <- Stats.getTicks stats'
            case t of
                0 -> return e'
                _ -> do
                    when ((dumpProgress && dump FD.Progress) || dump FD.Pass) $ Stats.print "Optimization" stats'
                    Stats.combine stats stats'
                    opt (n - 1) e'
    opt count e

-- these are way to complicated and should be simplified

doopt mangle dmp stats name func lc = do
    stats' <- Stats.new
    lc <- mangle (Stats.print "stats" stats') dmp name (func stats') lc
    t' <- Stats.getTicks stats'
    case t'  of
        0 -> return lc
        _ -> do
            when ((dmp && dump FD.Progress) || dump FD.Pass) $ Stats.print "Optimization" stats'
            Stats.combine stats stats'
            doopt mangle dmp stats name func lc


mangle' ::
    Maybe (Set.Set Id)  -- ^ Acceptable free variables
    -> DataTable        -- ^ The datatable needed for typechecking
    -> IO ()            -- ^ run on error
    -> Bool             -- ^ Whether to dump progress
    -> String           -- ^ Name of pass
    -> (E -> IO E)      -- ^ Mangling function
    -> E                -- ^ What to mangle
    -> IO E             -- ^ Out it comes
mangle'  fv dataTable erraction b  s action e = do
    when ((b && dump FD.Progress) || dump FD.Pass) $ putErrLn $ "-- " ++ s
    e' <- action e
    if not flint then return e' else do
        let ufreevars e | Just as <- fv = filter ( not . (`Set.member` as) . tvrIdent) (freeVars e)
            ufreevars e = []
        case inferType dataTable [] e' of
            Right _ |  xs@(_:_) <- ufreevars e' -> do
                putErrLn $ "\n>>> internal error: Unaccountable Free Variables\n" ++ render (pprint (xs:: [TVr]))
                putErrLn $ "\n>>>Before" <+> s
                printEStats e
                putDocM CharIO.putErr (ePretty e)
                putErrLn $ "\n>>>After" <+> s
                printEStats e'
                erraction
                --let (_,e'') = E.Diff.diff e e'
                let e''' = findOddFreeVars xs e'
                putDocM CharIO.putErr (ePrettyEx e''')
                putErrLn $ "\n>>> internal error: Unaccountable Free Variables\n" ++ render (pprint (xs:: [TVr]))
                case optKeepGoing options of
                    True -> return e'
                    False -> putErrDie "Unusual free vars in E"
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
                case optKeepGoing options of
                    True -> return e'
                    False -> putErrDie "Type Error in E"
            Right _ -> wdump FD.Stats (printEStats e') >>  return e'


typecheck dataTable e = case inferType dataTable [] e of
    Left ss -> do
        putErrLn (render $ ePretty e)
        putErrLn $ "\n>>> internal error:\n" ++ unlines (intersperse "----" $ tail ss)
        case optKeepGoing options of
            True -> return Unknown
            False -> putErrDie "Type Error in E"
    Right v -> return v


printCheckName dataTable e = do
    putErrLn  ( render $ hang 4 (pprint e <+> text "::") )
    ty <- typecheck dataTable e
    putErrLn  ( render $ indent 4 (pprint ty))

printCheckName' dataTable tvr e = do
    putErrLn (show $ tvrInfo tvr)
    putErrLn  ( render $ hang 4 (pprint tvr <+> equals <+> pprint e <+> text "::") )
    ty <- typecheck dataTable e
    putErrLn  ( render $ indent 4 (pprint ty))




