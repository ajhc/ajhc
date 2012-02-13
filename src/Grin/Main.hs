module Grin.Main(compileToGrin) where

import Control.Monad
import Data.List
import Data.Monoid(mappend)
import System.Directory
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.UTF8 as LBS
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified System
import qualified System.FilePath as FP

import C.Prims
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
import Ho.ReadSource
import Options
import PackedString
import RawFiles
import Support.TempDir
import Support.Transform
import Util.Gen
import qualified C.FromGrin2 as FG2
import qualified FlagDump as FD
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
    x <- transformGrin deadCodeParms x
    x <- transformGrin simplifyParms x
    x <- transformGrin pushParms x
    x <- transformGrin simplifyParms x
    putProgressLn "-- Speculative Execution Optimization"
    x <- grinSpeculate x
    lintCheckGrin x
    x <- transformGrin deadCodeParms x
    x <- transformGrin simplifyParms x
    x <- transformGrin pushParms x
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
    putProgressLn "-- Grin Devolution"
    wdump FD.GrinFinal $ dumpGrin "predevolve" x
    x <- transformGrin devolveTransform x
    --x <- opt "After Devolve Optimization" x
    x <- transformGrin simplifyParms x
    x <- return $ twiddleGrin x
 --   x <- return $ normalizeGrin x
--    x <- return $ twiddleGrin x
    x <- storeAnalyze x
    dumpFinalGrin x
    compileGrinToC x

dumpFinalGrin grin = do
    wdump FD.GrinGraph $ do
        let dot = graphGrin grin
        writeFile (outputName ++ "_grin.dot") dot
    wdump FD.GrinFinal $ dumpGrin "final" grin

compileGrinToC grin = do
    let (cg,Requires reqs) = FG2.compileGrin grin
        rls = filter ("-l" `isPrefixOf`) $ map (unpackPS . snd) (Set.toList reqs)
        fn = outputName ++ lup "executable_extension"
        lup k = maybe "" id $ Map.lookup k (optInis options)
    cf <- case (optOutName options,optStop options) of
            (Just fn,StopC) -> return fn
            _ | dump FD.C -> return (fn ++ "_code.c")
              | otherwise -> fileInTempDir ("main_code.c") (\_ -> return ())
    (argstring,sversion) <- getArgString
    (cc,args) <- fetchCompilerFlags
    forM_ [("rts/constants.h",constants_h),
           ("rts/stableptr.c",stableptr_c),
           ("rts/slub.c",slub_c),
           ("rts/profile.c",profile_c),
           ("rts/profile.h",profile_h),
           ("rts/gc.h",gc_h),
           ("rts/rts_support.c",rts_support_c),
           ("rts/rts_support.h",rts_support_h),
           ("rts/cdefs.h",cdefs_h),
           ("sys/queue.h",queue_h),
           ("HsFFI.h",hsffi_h),
           ("sys/wsize.h",wsize_h),
           ("rts/gc_jgc.c",gc_jgc_c),
           ("rts/gc_none.c",gc_none_c),
           ("rts/gc_none.h",gc_none_h),
           ("sys/bitarray.h",bitarray_h)] $ \ (fn,bs) -> do
        fileInTempDir fn $ flip BS.writeFile bs
    let cFiles = ["rts/profile.c","rts/rts_support.c", "rts/gc_none.c"]

    tdir <- getTempDir
    ds <- catch (getDirectoryContents (tdir FP.</> "cbits")) (\_ -> return [])
    let extraCFiles = map (tdir FP.</>) cFiles ++ ["-I" ++ tdir ++ "/cbits", "-I" ++ tdir ] ++ [ tdir FP.</> "cbits" FP.</> fn | fn@(reverse -> 'c':'.':_) <- ds ]
    let comm = shellQuote $ [cc] ++ extraCFiles ++ [cf, "-o", fn] ++ args ++ rls
        globalvar n c = LBS.fromString $ "char " ++ n ++ "[] = \"" ++ c ++ "\";"
    putProgressLn ("Writing " ++ show cf)
    LBS.writeFile cf $ LBS.intercalate (LBS.fromString "\n") [
        globalvar "jhc_c_compile" comm, globalvar "jhc_command" argstring,
        globalvar "jhc_version" sversion,LBS.empty,cg]
    when (optStop options == StopC) $
        exitSuccess
    putProgressLn ("Running: " ++ comm)
    r <- System.system comm
    when (r /= System.ExitSuccess) $ fail "C code did not compile."
    return ()

grinParms = transformParms {
    transformDumpProgress = verbose,
    transformPass = "Grin"
    }

simplifyParms = grinParms {
    transformCategory = "Simplify",
    transformOperation = Grin.SSimplify.simplify,
    transformIterate = IterateDone
    }

nodeAnalyzeParms = grinParms {
    transformCategory = "NodeAnalyze",
    transformOperation = nodealyze
    } where
        nodealyze grin = do
            stats <- Stats.new
            g <- deadCode stats (grinEntryPointNames grin) grin
            g <- nodeAnalyze g
            st <- Stats.readStat stats
            return g { grinStats = grinStats grin `mappend` st }

pushParms = grinParms {
    transformCategory = "Push",
    transformOperation = pushGrin
    } where
        pushGrin grin = do
            nf   <- mapMsnd (grinPush undefined) (grinFuncs grin)
            return $ setGrinFunctions nf grin

deadCodeParms = grinParms {
    transformCategory = "DeadCode",
    transformOperation = op
    } where
        op grin = do
            stats <- Stats.new
            g <- deadCode stats (grinEntryPointNames grin) grin
            st <- Stats.readStat stats
            return g { grinStats = grinStats grin `mappend` st }
