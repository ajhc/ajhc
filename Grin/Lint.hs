module Grin.Lint(
    lintCheckGrin,
    typecheckGrin,
    transformGrin,
    dumpGrin
    ) where

import Options
import Control.Exception
import Grin.Grin
import Grin.Show
import Util.Gen
import Control.Monad
import System.IO
import Support.CanType
import qualified FlagDump as FD
import Support.Transform


lintCheckGrin grin = when flint $ typecheckGrin grin

lintCheckGrin' onerr grin | flint = do
    let errs = [  (err ++ "\n" ++ render (prettyFun a) ) | (a,Left err) <-  [ (a,typecheck (grinTypeEnv grin) c:: Either String Ty)   | a@(_,(_ :-> c)) <-  grinFuncs grin ]]
    if null errs then return () else do
    onerr
    putErrLn ">>> Type Errors"
    mapM_ putErrLn  errs
    unless (null errs || optKeepGoing options) $ fail "There were type errors!"
lintCheckGrin' _ _ = return ()

typecheckGrin grin = do
    let errs = [  (err ++ "\n" ++ render (prettyFun a) ) | (a,Left err) <-  [ (a,typecheck (grinTypeEnv grin) c:: Either String Ty)   | a@(_,(_ :-> c)) <-  grinFuncs grin ]]
    mapM_ putErrLn  errs
    unless (null errs || optKeepGoing options) $ fail "There were type errors!"

dumpGrin pname grin = do
    let fn = optOutName options ++ "_" ++ pname ++ ".grin"
    putErrLn $ "Writing: " ++ fn
    h <- openFile fn  WriteMode
    (argstring,sversion) <- getArgString
    hPutStrLn h $ unlines [ "-- " ++ argstring,"-- " ++ sversion,""]
    hPrintGrin h grin
    hClose h
    wdump FD.Grin $ do
        putErrLn $ "v-- " ++ pname ++ " Grin"
        printGrin grin
        putErrLn $ "^-- " ++ pname ++ " Grin"


transformGrin :: TransformParms Grin -> Grin -> IO Grin

transformGrin TransformParms { transformIterate = IterateMax n } prog | n <= 0 = return prog
transformGrin TransformParms { transformIterate = IterateExactly n } prog | n <= 0 = return prog
transformGrin tp prog = do
    let dodump = transformDumpProgress tp
        name = transformCategory tp ++ pname (transformPass tp) ++ pname (transformName tp)
        scname = transformCategory tp ++ pname (transformPass tp)
        pname "" = ""
        pname xs = '-':xs
        iterate = transformIterate tp
    when dodump $ putErrLn $ "-- " ++ name
    --when (dodump && dump FD.CorePass) $ printGrin prog
    --let istat = progStats prog
    let ferr e = do
        putErrLn $ "\n>>> Exception thrown"
        putErrLn $ "\n>>> Before " ++ name
        dumpGrin ("lint-before-" ++ name) prog
        putErrLn $ "\n>>>"
        putErrLn (show e)
        maybeDie
        return prog
    prog' <- Control.Exception.catch (transformOperation tp prog) ferr
--    let estat = progStats prog'
    let onerr grin' = do
            putErrLn $ "\n>>> Before " ++ name
            dumpGrin ("lint-before-" ++ name) prog
--            Stats.printStat name estat
            putErrLn $ "\n>>> After " ++ name
            dumpGrin ("lint-after-" ++ name) prog
--    if transformSkipNoStats tp && estat == mempty then do
--        when dodump $ putErrLn "program not changed"
--        return prog
--     else do
--    when (dodump && dump FD.CoreSteps && estat /= mempty) $ Stats.printLStat (optStatLevel options) name estat
 --   when collectPassStats $ do
--        Stats.tick Stats.theStats scname
--        Stats.tickStat Stats.theStats (Stats.prependStat scname estat)
    lintCheckGrin' (onerr prog') prog'
    if doIterate iterate False then transformGrin tp { transformIterate = iterateStep iterate } prog' else return prog'
--    if doIterate iterate (estat /= mempty) then transformGrin tp { transformIterate = iterateStep iterate } prog' { progStats = istat `mappend` estat } else
--        return prog' { progStats = istat `mappend` estat, progPasses = name:progPasses prog' }


maybeDie = case optKeepGoing options of
    True -> return ()
    False -> putErrDie "Internal Error"
