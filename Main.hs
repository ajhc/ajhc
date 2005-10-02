
module Main(main) where

import Char
import Control.Exception
import Control.Monad.Identity
import Data.Monoid
import List hiding(group)
import Maybe
import Prelude hiding(putStrLn, putStr,print)
import qualified Data.IntMap as IM
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified System

import C.FromGrin
import CharIO
import Class
import DataConstructors
import Doc.DocLike
import Doc.PPrint
import Doc.Pretty
import E.Annotate(annotate)
import E.Arbitrary()
import E.Diff
import E.E
import E.FromHs
import E.LambdaLift
import E.LetFloat
import E.Pretty
import E.Rules
import E.Strictness
import E.Subst
import E.Traverse
import E.TypeCheck
import FreeVars
import FrontEnd.FrontEnd
import GenUtil hiding(replicateM,putErrLn,putErr,putErrDie)
import Grin.DeadFunctions
import Grin.FromE
import Grin.Grin hiding (typecheck)
import Grin.Show
import Grin.Whiz
import Ho
import HsSyn
import Info.Types
import Name
import Options
import qualified E.CPR
import qualified E.SSimplify as SS
import qualified FlagDump as FD
import qualified FlagOpts as FO
import qualified Grin.Interpret
import qualified Grin.PointsToAnalysis
import qualified Grin.Simplify
import qualified Info.Info as Info
import qualified Stats
import Util.Graph

---------------
-- ∀α∃β . α → β
---------------


printCheckName dataTable e = do
    putErrLn  ( render $ hang 4 (pprint e <+> text "::") )
    ty <- typecheck dataTable e
    putErrLn  ( render $ hang 4 (pprint ty))


bracketHtml action = do
    pn <- System.getProgName
    as <- System.getArgs
    wdump FD.Html $ putStrLn $ "<html><head><title>" ++ (unwords (pn:as)) ++ "</title><meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\"></head><body style=\"background: black; color: lightgrey\"><pre>"
    action `finally` (wdump FD.Html $ putStrLn "</pre></body></html>")

main = runMain $ bracketHtml $ do
    o <- processOptions
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
processFiles  fs = do
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

annotateMethods ch rs ps = (Map.fromList [ (tvrIdent t, Just (EVar t)) | t <- ts ]) where
    ts = [ let Identity x = idann rs ps (tvrIdent t) (tvrInfo t) in t { tvrInfo = x  } | t <-methodNames ch ]

processInitialHo :: Ho -> IO Ho
processInitialHo ho = do
    putStrLn $ "Initial annotate: " ++ show (Map.keys $ hoModules ho)
    let imap = annotateMethods (hoClassHierarchy ho) (hoRules ho) (hoProps ho)
    let Identity (ELetRec ds (ESort 0)) = annotate imap (idann (hoRules ho) (hoProps ho) ) letann lamann (ELetRec (Map.elems $ hoEs ho) eStar)
    return ho { hoEs = Map.fromAscList [ (k,d) | k <- Map.keys $ hoEs ho | d <- ds ] }


-- | this is called on parsed, typechecked haskell code to convert it to the internal representation

processDecls ::
    Stats.Stats   -- ^ statistics
    -> Ho     -- ^ Collected ho
    -> Ho     -- ^ preliminary haskell object  data
    -> TiData -- ^ front end output
    -> IO Ho  -- ^ final haskell object file
processDecls stats ho ho' tiData = do
    --mapM_ print [ (EVar t) | (t,_) <- (Map.elems (hoEs ho))]

    let allHo = ho `mappend` ho'

    let isExported n | "Instance@" `isPrefixOf` show n = True
        isExported n = n `Set.member` exports
        exports = Set.fromList $ concat $ Map.elems (hoExports ho')
    let decls = concat [ hsModuleDecls  m | (_,m) <- tiDataModules tiData ] ++ Map.elems (tiDataLiftedInstances tiData)
    let dataTable = toDataTable (Map.fromList $[ (toName TypeConstructor x,y) | (x,y)<- Map.toList (hoKinds ho')] ) (tiAllAssumptions tiData) decls
    let fullDataTable =  (dataTable `mappend` hoDataTable ho)
    let allAssumps = (tiAllAssumptions tiData `mappend` hoAssumps ho)
    ds <- convertDecls (hoClassHierarchy ho') allAssumps  fullDataTable decls
    wdump FD.Progress $ do
        putErrLn $ show (length ds) ++ " declarations converted."
    rules <- createInstanceRules (hoClassHierarchy ho' `mappend` hoClassHierarchy initialHo)   (Map.fromList [ (x,(y,z)) | (x,y,z) <- ds] `mappend` hoEs ho)
    let allRules = hoRules ho `mappend` rules
    wdump FD.Datatable $ putErrLn (render $ showDataTable dataTable)
    let inscope =  [ tvrNum n | (n,_) <- Map.elems $ hoEs ho ] ++ [tvrNum n | (_,n,_) <- ds ] ++ map tvrNum (methodNames (hoClassHierarchy allHo))
    let mangle = mangle' (Just $ Set.fromList $ inscope) fullDataTable
    let doopt' = doopt mangle
    let f (ds,(smap,annmap)) (n,v,lc) = do
        wdump FD.Lambdacube $ putErrLn (show n)
        let g (TVr { tvrIdent = 0 }) = error "absurded zero"
            g tvr@(TVr { tvrIdent = n, tvrType = k})
                | sortStarLike k =  tAbsurd k
                | otherwise = EVar tvr
        nfo <- idann (hoRules ho') (hoProps ho') (tvrIdent v) (tvrInfo v)
        v <- return  v { tvrInfo = nfo }
        fvs <- return $ foldr IM.delete (freeVars lc)  inscope
        when (IM.size fvs > 0) $ do
            putDocM putErr $ parens $ text "Absurded vars:" <+> align (hsep $ map pprint (IM.elems fvs))
        lc <- mangle False ("Annotate") (annotate annmap (idann (hoRules allHo) (hoProps allHo)) letann lamann) lc
        lc <- mangle False ("Absurdize") (return . substMap (IM.map g fvs)) lc
        lc <- mangle False ("Barendregt: " ++ show n) (return . barendregt) lc
        lc <- mangle False "deNewtype" (return . deNewtype fullDataTable) lc
        lc <- doopt' False stats "FixupLets..." (\stats x -> atomizeApps stats x >>= coalesceLets stats)  lc
        lc <- mangle False ("Barendregt: " ++ show n) (return . barendregt) lc
        let cm stats e = do
            let sopt = mempty { SS.so_exports = inscope, SS.so_boundVars = smap, SS.so_rules = allRules, SS.so_dataTable = fullDataTable }
            let (e',stat,occ) = SS.simplify sopt e
            Stats.tickStat stats stat
            return e'
        lc <- doopt' False stats "Float Inward..." (\stats x -> return (floatInward allRules x))  lc
        lc <- doopt' False stats "SuperSimplify" cm lc
        wdump FD.Lambdacube $ printCheckName fullDataTable lc
        wdump FD.Progress $ putErr "."
        nfo <- letann lc (tvrInfo v)
        nfo <- return $ if isExported n then Info.insert Exported nfo else nfo
        v <- return $ v { tvrInfo = Info.insert LetBound nfo }
        return ((n,v,lc):ds, (Map.insert (tvrNum v) lc smap, Map.insert (tvrNum v) (Just (EVar v)) annmap))
    let reached = Set.fromList [ tvrNum b | (_,b,_) <- reachable graph  [ tvrNum b | (n,b,_) <- ds, isExported n]]
        graph =  (newGraph ds (\ (_,b,_) -> tvrNum b) (\ (_,_,c) -> freeVars c))
        (_,dog)  = findLoopBreakers (const 0) graph

    let imap = annotateMethods (hoClassHierarchy allHo) allRules (hoProps allHo)
    let initMap = Map.fromList [ (tvrIdent t, Just (EVar t)) | (t,_) <- (Map.elems (hoEs ho))] `mappend` imap
    (ds,_) <- foldM f ([],(Map.fromList [ (tvrNum v,e) | (v,e) <- Map.elems (hoEs ho)], initMap)) [ x | x@(_,b,_) <- dog, tvrNum b `Set.member` reached ]
    wdump FD.Progress $ putErrLn "!"

    let ds' = reachable (newGraph ds (\ (_,b,_) -> tvrNum b) (\ (_,_,c) -> freeVars c)) [ tvrNum b | (n,b,_) <- ds, isExported n]
    wdump FD.Progress $ putErrLn $ "Functions culled: " ++ show (length ds - length ds')
    wdump FD.OptimizationStats $ Stats.print "Optimization" stats
    return ho' { hoDataTable = dataTable, hoEs = Map.fromList [ (x,(y,z)) | (x,y,z) <- ds'], hoRules = rules }


doopt mangle dmp stats name func lc = do
    stats' <- Stats.new
    lc <- mangle dmp name (func stats') lc
    t' <- Stats.getTicks stats'
    case t'  of
        0 -> return lc
        _ -> do
            when ((dmp && dump FD.Progress) || dump FD.Pass) $ Stats.print "Optimization" stats'
            Stats.combine stats stats'
            doopt mangle dmp stats name func lc

compileModEnv' stats ho = do

    let dataTable = hoDataTable ho
    let rules = hoRules ho
    wdump FD.Datatable $ putErrLn (render $ showDataTable dataTable)

    --mapM_ putErrLn ([ show x <+> "::" <+> render (ePretty ty) | (x,(TVr _ ty,_)) <- Map.toList $ hoEs ho])
    let mainFunc = parseName Val (maybe "Main.main" snd (optMainFunc options))

    when (dump FD.ClassSummary) $ do
        putStrLn "  ---- class summary ---- "
        printClassSummary (hoClassHierarchy ho)
    when (dump FD.Class) $ do
        putStrLn "  ---- class hierarchy ---- "
        printClassHierarchy (hoClassHierarchy ho)

    let initMap = Map.fromList [ (tvrIdent t, Just (EVar t)) | (t,_) <- (Map.elems (hoEs ho))]
    es' <- createMethods dataTable (hoClassHierarchy ho) (hoEs ho)
    let Identity (ELetRec es'' (ESort 0)) = annotate initMap (idann (hoRules ho) (hoProps ho) ) letann lamann (ELetRec [ (y,z) | (x,y,z) <- es']  eStar)

    es' <- return [ (x,y,floatInward rules z) | (x,_,_) <- es' | (y,z) <- es'' ]
    wdump FD.Class $ do
        sequence_ [ putDocM CharIO.putErr (pprint $ ELetRec [(y,z)] Unknown) >> putErrLn "" |  (x,y,z) <- es']
    let es = Map.fromList [ (x,(y,z)) |  (x,y,z) <- es'] `mappend` hoEs ho
    (_,main,mainv) <- getMainFunction mainFunc es
    let ds = ((main,mainv):Map.elems es)
    let ds' = reachable (newGraph ds (tvrNum . fst) (\ (t,e) -> Set.toList $ freeVars e `mappend` freeVars (Info.fetch (tvrInfo t) :: ARules))) [tvrNum main]

    let lco = ELetRec ds'  (EVar main)
    --typecheck dataTable lco
    wdump FD.Rules $ printRules rules
    let mangle = mangle' (Just mempty)
    let opt = doopt (mangle dataTable) True stats

    lc <- mangle dataTable True "Barendregt" (return . barendregt) lco
    wdump FD.Progress $ printEStats lc
    let cm stats e = do
        let sopt = mempty { SS.so_rules = rules, SS.so_dataTable = dataTable }
        let (e',stat,occ) = SS.simplify sopt e
        Stats.tickStat stats stat
        return e'

    --let imap = annotateMethods (hoClassHierarchy ho) (hoRules ho) (hoProps ho)
    lc <- return $ runIdentity $ annotate mempty (idann rules (hoProps ho) ) letann lamann lc
    lc <- opt "SuperSimplify" cm lc

    lc <- mangle dataTable True "Barendregt" (return . barendregt) lc
    (lc,_) <- return $ E.CPR.cprAnalyze mempty lc
    sequence_ [ putStrLn $ (tvrShowName t) <+> show (maybe E.CPR.Top id (Info.lookup (tvrInfo t)) ::  E.CPR.Val) | (t,_,_) <- scCombinators $ eToSC dataTable lc ]
    lc <- if fopts FO.FloatIn then  opt "Float Inward..." (\stats x -> return (floatInward rules  x))  lc  else return lc
    vs <- if fopts FO.Strictness then (collectSolve lc) else return []
    mapM_ putErrLn $  sort [ tshow x <+> "->" <+> tshow y | (x@(E.Strictness.V i),y@Lam {}) <- vs, odd i]
    let cm stats e = do
        let sopt = mempty { SS.so_rules = rules, SS.so_dataTable = dataTable,  SS.so_strictness = Map.fromList [ (i,S n) | (E.Strictness.V i,S n) <- vs] }
        let (e',stat,occ) = SS.simplify sopt e
        Stats.tickStat stats stat
        return e'
    lc <- opt "SuperSimplify" cm lc

    let ELetRec ds _ = lc in mapM_ (\t -> putStrLn (prettyE (EVar t) <+> show (tvrInfo t))) (fsts ds)

    wdump FD.LambdacubeBeforeLift $ printCheckName dataTable lc
    lc <- mangle dataTable True "LambdaLift" (lambdaLiftE stats dataTable) lc
    lc <- mangle dataTable True  "FixupLets..." (\x -> atomizeApps stats x >>= coalesceLets stats)  lc
    wdump FD.Lambdacube $ printCheckName dataTable lc
    wdump FD.OptimizationStats $ Stats.print "Optimization" stats
    wdump FD.Progress $ printEStats lc
    wdump FD.Progress $ putErrLn "Converting to Grin..."
    x <- Grin.FromE.compile dataTable (error "vmap") (eToSC dataTable lc)
    Stats.print "Grin" Stats.theStats
    wdump FD.Grin $ printGrin x
    x <- return $ normalizeGrin x
    typecheckGrin x
    let opt x = do
        wdump FD.Progress $ putErrLn "Optimization Pass..."
        t <- Stats.getTicks stats
        x <- deadFunctions True stats [funcMain] x
        x <- Grin.Simplify.simplify stats x
        when flint $ typecheckGrin x
        t' <- Stats.getTicks stats
        case t == t' of
            False -> opt x
            True -> return x
    x <- opt x
    wdump FD.OptimizationStats $ Stats.print "Optimization" stats
    x <- return $ normalizeGrin x
    typecheckGrin x
    wdump FD.GrinPreeval $ printGrin x
    wdump FD.Progress $ putErrLn "Points-to analysis..."
    x <- Grin.PointsToAnalysis.grinInlineEvalApply x
    typecheckGrin x
    wdump FD.GrinPreeval $ printGrin x
    x <- return $ normalizeGrin x
    typecheckGrin x
    let opt (0::Int) x = return x
        opt n x = do
        wdump FD.Progress $ putErrLn "AE Optimization Pass..."
        t <- Stats.getTicks stats
        x <- deadFunctions False stats [funcMain] x
        x <- Grin.Simplify.simplify stats x
        typecheckGrin x
        t' <- Stats.getTicks stats
        case t == t' of
            False -> opt (n - 1) x
            True -> return x
    x <- opt (-1) x
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
        writeFile cf $ cg -- toUTF8  (prettyC z ++ concatMap (\(i,n) -> "//" ++ 'v':show i ++ " -> " ++ n ++ "\n") (snd us))
        let boehmOpts | fopts FO.Boehm = ["-DUSE_BOEHM_GC", "-lgc"]
                      | otherwise = []
        let comm = shellQuote $ [optCC options, "-std=gnu99", "-foptimize-sibling-calls", "-O", {- "-funit-at-a-time", -} "-g", "-Wall", "-o", fn, cf ] ++ rls ++ optCCargs options  ++ boehmOpts
        wdump FD.Progress $ putErrLn ("Running: " ++ comm)
        r <- System.system comm
        when (r /= System.ExitSuccess) $ fail "C code did not compile."
        return ()




--mangle = mangle' (Just mempty)

mangle' :: Maybe (Set.Set Int) -- ^ Acceptable free variables
    -> DataTable
    -> Bool    -- ^ Whether to dump progress
    -> String      -- ^ Name of pass
    -> (E -> IO E) -- ^ Mangling function
    -> E           -- ^ What to mangle
    -> IO E        -- ^ Out it comes
mangle' fv dataTable b  s action e = do
    when ((b && dump FD.Progress) || dump FD.Pass) $ putErrLn $ "-- " ++ s
    e' <- action e
    if not flint then return e' else do
        let ufreevars e | Just as <- fv = filter ( not . (`Set.member` as) . tvrNum) (freeVars e)
            ufreevars e = []
        case inferType dataTable [] e' of
            Right _ |  xs@(_:_) <- ufreevars e' -> do
                putErrLn $ "\n>>> internal error: Unaccountable Free Variables\n" ++ render (pprint (xs:: [TVr]))
                putErrLn $ "\n>>>Before" <+> s
                printEStats e
                putDocM CharIO.putErr (ePretty e)
                putErrLn $ "\n>>>After" <+> s
                printEStats e'
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
        putErrLn $ "\n>>> internal error:\n" ++ unlines (tail ss)
        case optKeepGoing options of
            True -> return Unknown
            False -> putErrDie "Type Error in E"
    Right v -> return v




