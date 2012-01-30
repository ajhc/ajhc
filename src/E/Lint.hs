module E.Lint(
    transformProgram,
    onerrNone,
    lintCheckProgram,
    dumpCore
    ) where

import Control.Exception
import Control.Monad
import Control.Monad.Trans
import Data.List as List
import Data.Maybe
import Support.Compat
import qualified Data.Set as Set

import Data.Monoid
import Doc.DocLike
import Doc.PPrint
import Doc.Pretty
import E.E
import E.Program
import E.Show
import E.Traverse
import E.TypeCheck
import Name.Id
import Options
import Stats
import Support.FreeVars
import Support.TempDir
import Support.Transform
import Util.ContextMonad
import Util.Gen
import Util.SetLike as S
import qualified FlagDump as FD
import qualified IO

-- all transformation routines assume they are being passed a correct program, and only check the output

transformProgram :: MonadIO m => TransformParms Program -> Program -> m Program

transformProgram TransformParms { transformIterate = IterateMax n } prog | n <= 0 = return prog
transformProgram TransformParms { transformIterate = IterateExactly n } prog | n <= 0 = return prog
transformProgram tp prog = liftIO $ do
    let dodump = transformDumpProgress tp
        name = transformCategory tp ++ pname (transformPass tp) ++ pname (transformName tp)
        scname = transformCategory tp ++ pname (transformPass tp)
        pname "" = ""
        pname xs = '-':xs
        iterate = transformIterate tp
    withStackStatus ("transformProgram: " ++ name) $ do
    when dodump $ putErrLn $ "-- " ++ name
    when (dodump && dump FD.CorePass) $ printProgram prog
    wdump FD.ESize $ printESize ("Before "++name) prog
    let istat = progStats prog
    let ferr e = do
        putErrLn $ "\n>>> Exception thrown"
        dumpCoreExtra ("lint-before-" ++ name) prog (show (e::SomeException'))
        putErrLn (show (e::SomeException'))
        maybeDie
        return prog
    prog' <- Control.Exception.catch (transformOperation tp prog { progStats = mempty }) ferr
    let estat = progStats prog'
        onerr = do
            putErrLn $ "\n>>> Before " ++ name
            dumpCore ("lint-before-" ++ name) prog
            Stats.printStat name estat
            putErrLn $ "\n>>> After " ++ name
            let fvs = programFreeVars prog' `mappend` programFreeVars prog
                rtvrs = "FreeVars:\n" ++ (render $ vcat $ map (\tvr -> hang 4 (pprint (tvr :: TVr) <+> text "::" <+> (pprint $ tvrType tvr))) fvs)
            dumpCoreExtra ("lint-after-" ++ name) prog' rtvrs
    if transformSkipNoStats tp && estat == mempty then do
        when dodump $ putErrLn "program not changed"
        return prog
     else do
    when (dodump && dump FD.CoreSteps && (not $ Stats.null estat)) $ Stats.printLStat (optStatLevel options) name estat
    when verbose $ do
        Stats.tick Stats.theStats scname
        Stats.tickStat Stats.theStats (Stats.prependStat scname estat)
    wdump FD.ESize $ printESize ("After  "++name) prog'
    lintCheckProgram onerr prog'
    if doIterate iterate (not $ Stats.null estat) then transformProgram tp { transformIterate = iterateStep iterate } prog' { progStats = istat `mappend` estat } else
        return prog' { progStats = istat `mappend` estat, progPasses = name:progPasses prog' }

maybeDie = case optKeepGoing options of
    True -> return ()
    False -> putErrDie "Internal Error"

onerrNone :: IO ()
onerrNone = return ()

lintCheckE onerr dataTable tvr e | flint = case runContextEither $ inferType dataTable [] e of
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
    let f (tvr@TVr { tvrIdent = n },e) | isNothing $ fromId n = do
            onerr
            putErrLn $ ">>> non-unique name at top level: " ++ pprint tvr
            printProgram prog
            putErrLn $ ">>> non-unique name at top level: " ++ pprint tvr
            maybeDie
        f (tvr,e) = do
            case scopeCheck False mempty e of
                Left s -> do
                    onerr
                    putErrLn $ ">>> scopecheck failed in " ++ pprint tvr ++ " " ++ s
                    printProgram prog
                    putErrLn $ ">>> scopecheck failed in " ++ pprint tvr ++ " " ++ s
                    maybeDie
                Right () -> return ()
            lintCheckE onerr (progDataTable prog) tvr e
    mapM_ f (programDs prog)
    let ids = progExternalNames prog `mappend` fromList (map tvrIdent $ fsts (programDs prog)) `mappend` progSeasoning prog
        fvs = Set.fromList $ values (freeVars $ snds $ programDs prog :: IdMap TVr)
        unaccounted = Set.filter (not . (`member` ids) . tvrIdent) fvs
    unless (Set.null unaccounted) $ do
        onerr
        putErrLn ("\n>>> Unaccounted for free variables: " ++ render (pprint $ Set.toList $ unaccounted))
        printProgram prog
        putErrLn (">>> Unaccounted for free variables: " ++ render (pprint $ Set.toList $ unaccounted))
        maybeDie
lintCheckProgram _ _ = return ()

programFreeVars prog = freeVars (ELetRec (programDs prog) Unknown)

dumpCore pname prog = dumpCoreExtra pname prog ""

dumpCoreExtra pname prog extra = do
    let fn = outputName ++ "_" ++ pname ++ ".jhc_core"
    putErrLn $ "Writing: " ++ fn
    h <- IO.openFile fn IO.WriteMode
    (argstring,sversion) <- getArgString
    IO.hPutStrLn h $ unlines [ "-- " ++ argstring,"-- " ++ sversion,""]
    hPrintProgram h prog
    IO.hPutStrLn h extra
    IO.hClose h
    wdump FD.Core $ do
        putErrLn $ "v-- " ++ pname ++ " Core"
        printProgram prog
        putErrLn $ "^-- " ++ pname ++ " Core"

printESize :: String -> Program -> IO ()
printESize str prog = putErrLn $ str ++ " program e-size: " ++ show (eSize (programE prog))
