module Grin.Lint(
    lintCheckGrin,
    typecheckGrin,
    dumpGrin
    ) where

import Options
import Grin.Grin
import Grin.Show
import Util.Gen
import Control.Monad
import System.IO
import Support.CanType
import qualified FlagDump as FD
import Support.Transform


lintCheckGrin grin = when flint $ typecheckGrin grin

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


