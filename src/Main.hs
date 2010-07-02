module Main(main) where

import Control.Exception
import Control.Monad.Identity
import IO(hFlush,stderr)
import Prelude
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.UTF8 as BS

import DataConstructors
import E.Main
import E.Program
import E.Rules
import E.Type
import FrontEnd.Class
import FrontEnd.HsSyn(Module(..))
import Grin.Main(compileToGrin)
import Grin.Show(render)
import Ho.Build
import Ho.Collected
import Ho.Library
import Name.Name
import Options
import Util.Gen
import Util.SetLike as S
import Version.Version(versionString,versionContext,versionSimple)
import qualified FlagDump as FD
import qualified Interactive
import qualified Version.Config as VC

bracketHtml action = do
    (argstring,_) <- getArgString
    wdump FD.Html $ putStrLn $ "<html><head><title>" ++ argstring ++ "</title><meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\"></head><body style=\"background: black; color: lightgrey\"><pre>"
    action `finally` (wdump FD.Html $ putStrLn "</pre></body></html>")

main = bracketHtml $ do
    o <- processOptions
    let darg = progressM $ do
        (argstring,_) <- getArgString
        return (argstring ++ "\n" ++ versionSimple)
    case optMode o of
        BuildHl hl      -> darg >> buildLibrary processInitialHo processDecls hl
        ListLibraries   -> listLibraries
        ShowHo ho       -> dumpHoFile ho
        Version         -> putStrLn versionString
        PrintHscOptions -> putStrLn $ "-I" ++ VC.datadir ++ "/" ++ VC.package ++ "-" ++ VC.shortVersion ++ "/include"
        VersionCtx      -> putStrLn (versionString ++ BS.toString versionContext)
        Preprocess      -> forM_ (optArgs o) $ \fn -> do
            LBS.readFile fn >>= preprocess fn >>= LBS.putStr
        _               -> darg >> processFiles  (optArgs o)


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
    g fs = processCollectedHo . snd =<< parseFiles [] fs processInitialHo processDecls
    fileOrModule f = case reverse f of
        ('s':'h':'.':_)     -> Right f
        ('s':'h':'l':'.':_) -> Right f
        _                   -> Left $ Module f


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
    -- dump final version of various requested things
    wdump FD.Datatable $ putErrLn (render $ showDataTable dataTable)
    wdump FD.DatatableBuiltin $ putErrLn (render $ showDataTable samplePrimitiveDataTable)
    dumpRules (Rules $ fromList [ (combIdent x,combRules x) | x <- combinators, not $ null (combRules x) ])

    -- enter interactive mode
    int <- Interactive.isInteractive
    if int then Interactive.interact cho else do
        prog <- compileWholeProgram prog
        compileToGrin prog

-- | this is called on parsed, typechecked haskell code to convert it to the internal representation

progressM c  = wdump FD.Progress $ (c >>= putErrLn) >> hFlush stderr








