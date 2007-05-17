module Grin.Lint(
    lintCheckGrin,
    typecheckGrin,
    transformGrin,
    dumpGrin
    ) where

import Control.Exception
import Control.Monad
import Control.Monad.Reader
import Data.Monoid
import System.IO
import qualified Data.Set as Set

import Doc.DocLike
import Grin.Grin
import Grin.Noodle
import Grin.Show
import Options
import Support.CanType
import Support.FreeVars
import Support.Transform
import Util.Gen
import Util.SetLike
import qualified FlagDump as FD


lintCheckGrin grin = when flint $ typecheckGrin grin

lintCheckGrin' onerr grin | flint = do
    let env = TcEnv { envTyEnv = grinTypeEnv grin, envInScope = fromList (fsts $ grinCafs grin) }
    let errs = [  (err ++ "\n" ++ render (prettyFun a) ) | (a,Left err) <-  [ (a,runTc env (tcLam Nothing c))  | a@(_,c) <-  grinFuncs grin ]]
    if null errs then return () else do
    onerr
    putErrLn ">>> Type Errors"
    mapM_ putErrLn  errs
    unless (null errs || optKeepGoing options) $ fail "There were type errors!"
lintCheckGrin' _ _ = return ()

typecheckGrin grin = do
    let env = TcEnv { envTyEnv = grinTypeEnv grin, envInScope = fromList (fsts $ grinCafs grin) }
    let errs = [  (err ++ "\n" ++ render (prettyFun a) ) | (a,Left err) <-  [ (a,runTc env (tcLam Nothing c))  | a@(_,c) <-  grinFuncs grin ]]
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
            dumpGrin ("lint-after-" ++ name) grin'
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


data TcEnv = TcEnv {
    envTyEnv :: TyEnv,
    envInScope :: Set.Set Var
}



newtype Tc a = Tc (ReaderT TcEnv (Either String) a)
    deriving(Monad,MonadReader TcEnv)


runTc :: TcEnv -> Tc a -> Either String a
runTc env (Tc r) = runReaderT r env

same _ t1 t2 | t1 == t2 = return t1
same msg t1 t2 = fail $ "Types not the same:" <+> parens msg <+> parens (tshow t1) <+> parens (tshow t2)

tcLam :: Maybe Ty -> Lam -> Tc Ty
tcLam mty (v :-> e) = f mty where
    f Nothing = ans (tcVal v)
    f (Just ty) = ans $ do
        t <- tcVal v
        same (":->" <+> show mty <+> show (v :-> e)) ty t
    ans r = local (\e -> e { envInScope = freeVars v `mappend` envInScope e }) $ r >> tcExp e

tcExp :: Exp -> Tc Ty
tcExp e = f e where
    f (e :>>= lam) = do
        t1 <- f e
        tcLam (Just t1) lam
    f n@(Prim p as) = do
        let (as',t') = primType p
        as'' <- mapM tcVal as
        if as'' == as' then return t' else
            fail $ "Prim: arguments do not match " ++ show n
    f ap@(App fn [v,a] t) | fn == funcApply = do
        [v',a'] <- mapM tcVal [v,a]
        if v' == TyNode then return t
         else fail $ "App apply arg doesn't match: " ++ show ap
    f ap@(App fn [v] t) | fn == funcApply = do
        v' <- tcVal v
        if v' == TyNode then return t
         else fail $ "App apply arg doesn't match: " ++ show ap
    f ap@(App fn [v] t) | fn == funcEval = do
        v' <- tcVal v
        if v' == tyINode then return t
         else fail $ "App eval arg doesn't match: " ++ show ap
    f a@(App fn as t) = do
        te <- asks envTyEnv
        (as',t') <- findArgsType te fn
        as'' <- mapM tcVal as
        if t' == t then
            if as'' == as' then return t' else
                fail $ "App: arguments do not match: " ++ show (a,as',t')
         else fail $ "App: results do not match: " ++ show (a,t,(as',t'))
    f (Store v) = do
        t <- tcVal v
        return (TyPtr t)
    f Alloc { expValue = v } = do
        t <- tcVal v
        return (TyPtr t)
    f (Return v) = tcVal v
    f (Fetch v) = do
        (TyPtr t) <- tcVal v
        return t
    f (Error _ t) = return t
    f e@(Update w v) = do
        (TyPtr t) <- tcVal w
        t' <- tcVal v
        same (show e) t t'
        return tyUnit
    f (Case _ []) = fail "empty case"
    f (Case v as) = do
        tv <- tcVal v
        es <- mapM (tcLam (Just tv)) as
        foldl1M (same $ "case exp: " ++ show (map head $ sortGroupUnder fst (zip es as)) ) es
    f (Let { expDefs = defs, expBody = body }) = do
        te <- asks envTyEnv
        let nte = extendTyEnv defs te
        local (\e -> e { envTyEnv = extendTyEnv defs (envTyEnv e) }) $ do
            mapM_ (tcLam Nothing) [ b | FuncDef { funcDefBody = b } <- defs ]
            f body

tcVal :: Val -> Tc Ty
tcVal v = f v where
    f (Tag _) = return TyTag
    f e@(Var v t) = do
        s <- asks envInScope
        case v `member` s of
            True -> return t
            False -> fail $ "variable not in scope: " ++ show e
    f (Lit _ t) = return t
    f (NodeV _v as) = do
        mapM_ f as
        return TyNode
    f (Tup xs) = do
        xs <- mapM f xs
        return $ TyTup xs
    f (Const t) = do
        v <- f t
        return (TyPtr v)
    f (Index v offset) = do
        t <- f v
        Ty _ <- f offset
        return t
    f (ValUnknown ty) = return ty
    f (Addr _) = return $ TyPtr (error "typecheck: Addr")
    f (ValPrim _ vs ty) = do mapM_ f vs >> return ty
    f n@(NodeC tg as) = do
        te <- asks envTyEnv
        (as',_) <- findArgsType te tg
        as'' <- mapM f as
        if as'' == as' then return TyNode else
            fail $ "NodeC: arguments do not match " ++ show n ++ show (as'',as')


