module Grin.Main(compileToGrin) where

import Control.Monad
import Directory
import qualified Data.ByteString.Lazy.UTF8 as LBS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as Map
import qualified System

import Grin.DeadCode
import Grin.Devolve(twiddleGrin,devolveTransform)
import Grin.EvalInline(createEvalApply)
import Grin.FromE
import Grin.Grin
import Grin.Lint
import Grin.NodeAnalyze
import Grin.Optimize
import Grin.SSimplify
import Grin.Show
import Grin.StorageAnalysis
import Options
import Support.Transform
import Util.Gen
import qualified C.FromGrin2 as FG2
import qualified FlagDump as FD
import qualified FlagOpts as FO
import qualified Stats

{-# NOINLINE compileToGrin #-}
compileToGrin prog = do
    stats <- Stats.new
    putProgressLn "Converting to Grin..."
    x <- Grin.FromE.compile prog
    when verbose $ Stats.print "Grin" Stats.theStats
    wdump FD.GrinInitial $ do dumpGrin "initial" x
    x <- transformGrin simplifyParms x
    wdump FD.GrinNormalized $ do dumpGrin "normalized" x
    x <- explicitRecurse x
    lintCheckGrin x
    let pushGrin grin = do
            nf   <- mapMsnd (grinPush undefined) (grinFuncs grin)
            return $ setGrinFunctions nf grin
    putProgressLn "-- Dead Code Analysis"
    x <- deadCode stats (grinEntryPointNames x) x  -- XXX
    x <- transformGrin simplifyParms x
    x <- pushGrin x
    lintCheckGrin x
    x <- transformGrin simplifyParms x
    putProgressLn "-- Speculative Execution Optimization"
    x <- grinSpeculate x
    lintCheckGrin x
    x <- deadCode stats (grinEntryPointNames x) x  -- XXX
    lintCheckGrin x
    x <- transformGrin simplifyParms x
    x <- pushGrin x
    lintCheckGrin x
    x <- transformGrin simplifyParms x
    wdump FD.OptimizationStats $ Stats.print "Optimization" stats
    putProgressLn "-- Node Usage Analysis"
    wdump FD.GrinPreeval $ dumpGrin "preeval" x
    x <- transformGrin nodeAnalyzeParms x
    x <- transformGrin simplifyParms x
    wdump FD.GrinPreeval $ dumpGrin "preeval2" x
    x <- transformGrin nodeAnalyzeParms x
    x <- transformGrin simplifyParms x
    x <- createEvalApply x
    x <- transformGrin simplifyParms x
    lintCheckGrin x
    putProgressLn "-- Grin Devolution"
    wdump FD.GrinFinal $ dumpGrin "predevolve" x
    x <- transformGrin devolveTransform x
    --x <- opt "After Devolve Optimization" x
    x <- transformGrin simplifyParms x
    x <- return $ twiddleGrin x
    x <- storeAnalyze x
    dumpFinalGrin x
    compileGrinToC x



dumpFinalGrin grin = do
    wdump FD.GrinGraph $ do
        let dot = graphGrin grin
        writeFile (outputName ++ "_grin.dot") dot
    wdump FD.GrinFinal $ dumpGrin "final" grin

compileGrinToC grin = do
    let (cg,rls) = FG2.compileGrin grin
        fn = outputName ++ lup "executable_extension"
        cf = case (optOutName options,optMode options) of
            (Just fn,StopC) -> fn
            _ -> (fn ++ "_code.c")
        lup k = maybe "" id $ Map.lookup k (optInis options)
    (argstring,sversion) <- getArgString
    let
        boehmOpts | fopts FO.Boehm = ["-D_JHC_GC=_JHC_GC_BOEHM", "-lgc"]
                  | fopts FO.Jgc   = ["-D_JHC_GC=_JHC_GC_JGC"]
                  | otherwise = []
        profileOpts | fopts FO.Profile || lup "profile" == "true" = ["-D_JHC_PROFILE=1"]
                    | otherwise = []
        comm = shellQuote $ [lup "cc"] ++ words (lup "cflags") ++ ["-o", fn, cf] ++
                            (map ("-l" ++) rls) ++ debug ++ optCCargs options  ++ boehmOpts ++ profileOpts
        debug = if fopts FO.Debug then words (lup "cflags_debug") else words (lup "cflags_nodebug")
        globalvar n c = LBS.fromString $ "char " ++ n ++ "[] = \"" ++ c ++ "\";"
    putProgressLn ("Writing " ++ show cf)
    LBS.writeFile cf $ LBS.intercalate (LBS.fromString "\n") [globalvar "jhc_c_compile" comm, globalvar "jhc_command" argstring,globalvar "jhc_version" sversion,LBS.empty,cg]
    when (optMode options == StopC) $
        exitSuccess
    putProgressLn ("Running: " ++ comm)
    r <- System.system comm
    when (r /= System.ExitSuccess) $ fail "C code did not compile."
    unless (dump FD.C) $ removeFile cf
    return ()

simplifyParms = transformParms {
    transformDumpProgress = verbose,
    transformCategory = "Simplify",
    transformPass = "Grin",
    transformOperation = Grin.SSimplify.simplify,
    transformIterate = IterateDone
    }

nodeAnalyzeParms = transformParms {
    transformDumpProgress = verbose,
    transformCategory = "NodeAnalyze",
    transformPass = "Grin",
    transformOperation = nodealyze
    }  where
        nodealyze grin = do
            stats <- Stats.new
            g <- deadCode stats (grinEntryPointNames grin) grin
            g <- nodeAnalyze g
            return g

