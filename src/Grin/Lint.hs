module Grin.Lint(
    lintCheckGrin,
    typecheckGrin,
    transformGrin,
    dumpGrin
    ) where

import Control.Exception
import Control.Monad.Reader
import Data.Monoid
import System.IO
import qualified Data.Set as Set

import Doc.DocLike
import Grin.Grin
import Grin.Show
import Options
import Support.FreeVars
import Support.CanType
import Support.Transform
import Support.Compat
import Util.Gen
import Util.SetLike
import Text.Printf
import qualified FlagDump as FD
import qualified Stats


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

{-# NOINLINE dumpGrin #-}
dumpGrin pname grin = do
    (argstring,sversion) <- getArgString

    let fn ext action = do
            let oname = outputName ++ "_" ++ pname ++ "." ++ ext
            putErrLn $ "Writing: " ++ oname
            h <- openFile oname WriteMode
            action h
            hClose h
    fn "grin" $ \h -> do
        hPutStrLn h $ unlines [ "-- " ++ argstring,"-- " ++ sversion,""]
        hPrintGrin h grin
    wdump FD.GrinDatalog $ fn "datalog" $ \h -> do
        hPutStrLn h $ unlines [ "% " ++ argstring,"% " ++ sversion,""]
        hPrintGrinDL h grin
    wdump FD.Grin $ do
        putErrLn $ "v-- " ++ pname ++ " Grin"
        printGrin grin
        putErrLn $ "^-- " ++ pname ++ " Grin"


class DShow a where
    dshow :: a -> String

instance DShow String where
    dshow s = '\'':f s where
        f ('\'':rs) = "''" ++ f rs
        f (x:xs) = x:f xs
        f [] = "'"

instance DShow Tag where
    dshow s = '\'':f (show s) where
        f ('\'':rs) = "''" ++ f rs
        f (x:xs) = x:f xs
        f [] = "'"

instance DShow Var where
    dshow v = dshow (show v)

instance DShow Ty where
    dshow v = dshow $ show v


instance (DShow a,DShow b) => DShow (Either a b) where
    dshow (Left x) = dshow x
    dshow (Right x) = dshow x

funArg n i = show n ++ "@arg@" ++ show i
funRet n i = show n ++ "@ret@" ++ show i


printFunc h n (l :-> e) = do
    hPrintf h "func(%s,%i).\n" (dshow n) (length l)
    forM_ (zip naturals l) $ \ (i,Var v t) -> do
        hPrintf h "perform(assign,%s,%s).\n" (dshow v) (dshow $ funArg n i)
        hPrintf h "what(%s,funarg).\n" (dshow $ funArg n i)
        hPrintf h "typeof(%s,%s).\n" (dshow $ funArg n i) (dshow t)
        hPrintf h "typeof(%s,%s).\n" (dshow v) (dshow t)
    let rts = getType e
        lts = [ (t,funRet n i) | t <- rts | i <- naturals ]
    mapM_ (hPrintf h "what(%s,funret).\n" . dshow) (snds lts)
    mapM_ (\ (t,n) -> hPrintf h "typeof(%s,%s).\n" (dshow n) (dshow t)) lts
    printDL h n (map (Left . snd) lts) e

hPrintGrinDL :: Handle -> Grin -> IO ()
hPrintGrinDL h grin = do
    let cafs = grinCafs grin
    when (not $ null cafs) $ do
        hPutStrLn h "% cafs"
        mapM_ (\ (x,y) -> hPrintf h "what(%s,'caf').\ntypeof(%s,inode).\n" (dshow x) (dshow x))  cafs
    hPutStrLn h "% functions"
    forM_ (grinFuncs grin) $ \ (n,l :-> e) -> printFunc h n (l :-> e)

bindUnknown h l r = do
    mapM_ (\ (x,t) -> when (tyInteresting t) $ setUnknown h x r) (Set.toList $ freeVars l :: [(Var,Ty)])

setUnknown :: DShow a => Handle -> a -> String -> IO ()
setUnknown h x r = do hPrintf h "unknown(%s,%s).\n" (dshow x) (dshow r)



printDL h n fs e = f fs e where
    f fs (x :>>= l :-> y) = do
        f (map Right l) x
        f fs y
    f bs (Return vs) = do zipWithM_ (assign "assign") bs vs
--    f [Left b] (Store (NodeC n vs)) = hPrintf h "store(%s,%s,%s).\n" (dshow b) (dshow n) (if tagIsWHNF n then "true" else "false")
--    f [Right (Var b _)] (Store (NodeC n vs)) = hPrintf h "store(%s,%s,%s).\n" (dshow b) (dshow n) (if tagIsWHNF n then "true" else "false") >> app n vs
--    f [b] (Store x@Var {}) = do assign "demote" b x
    f [b] (BaseOp Eval [x]) = do assign "eval" b x
    f b (App fn as ty) = do
        forM_ (zip naturals as) $ \ (i,a) -> do
            assign "assign" (Left $ funArg fn i) a
        forM_ (zip naturals b) $ \ (i,a) -> do
            genAssign "assign" a (Left $ funRet fn i)
    f b (Case v ls) = mapM_ (\l -> f b (Return [v] :>>= l)) ls
    f b Let { expDefs = defs, expBody = body } = do
        forM_ defs $ \d -> printFunc h (funcDefName d) (funcDefBody d)
        forM_ defs $ \d -> hPrintf h "subfunc(%s,%s).\n" (dshow $ funcDefName d) (dshow n)
        f b body
    f b Error {} = return ()
    f b Call { expValue = Item fn _, expArgs =  as, expType = ty} = do
        forM_ (zip naturals as) $ \ (i,a) -> do
            assign "assign" (Left $ funArg fn i) a
        forM_ (zip naturals b) $ \ (i,a) -> do
            genAssign "assign" a (Left $ funRet fn i)

    f bs e = do zipWithM_ (assign "assign") bs (map ValUnknown (getType e))
    --app n as | Just (0,fn) <- tagUnfunction n = do
    --    hPrintf h "lazyfunc(%s).\n" (dshow fn)
    --    forM_ (zip naturals as) $ \ (i,a) -> do
    --        assign "assign" (Left $ funArg fn i) a
    --app _ _ = return ()


    assign op b v = genAssign op b (Right v)

    genAssign :: String -> Either String Val -> Either String Val -> IO ()
    genAssign op (Left b) (Left l) = hPrintf h "perform(%s,%s,%s).\n" op (dshow b) (dshow l)
    genAssign op (Right (Var v1 _)) (Left l) = hPrintf h "perform(%s,%s,%s).\n" op (dshow v1) (dshow l)
    genAssign op (Left b) (Right (Var v _)) = hPrintf h "perform(%s,%s,%s).\n" op (dshow b) (dshow v)
    genAssign op (Left b) (Right (Const {})) = hPrintf h "perform(%s,%s,%s).\n" op (dshow b) "const"
    genAssign op (Right (Var v1 _)) (Right (Var v2 _)) = hPrintf h "perform(%s,%s,%s).\n" op (dshow v1) (dshow v2)
    genAssign op (Left b) (Right v) = when (tyInteresting $ getType v) $ setUnknown h b (show (op,v))
    genAssign op (Right b) rv =  bindUnknown h b (take 20 $ show (op,rv))


tyInteresting ty = ty == TyNode || ty == tyINode



transformGrin :: TransformParms Grin -> Grin -> IO Grin

transformGrin TransformParms { transformIterate = IterateMax n } prog | n <= 0 = return prog
transformGrin TransformParms { transformIterate = IterateExactly n } prog | n <= 0 = return prog
transformGrin tp prog = do
    let dodump = transformDumpProgress tp
        name = transformCategory tp ++ pname (transformPass tp) ++ pname (transformName tp)
        _scname = transformCategory tp ++ pname (transformPass tp)
        pname "" = ""
        pname xs = '-':xs
        iterate = transformIterate tp
    when dodump $ putErrLn $ "-- " ++ name
    let ferr e = do
        putErrLn $ "\n>>> Exception thrown"
        putErrLn $ "\n>>> Before " ++ name
        dumpGrin ("lint-before-" ++ name) prog
        putErrLn $ "\n>>>"
        putErrLn (show (e::SomeException'))
        maybeDie
        return prog
    let istat = grinStats prog
    prog' <- Control.Exception.catch (transformOperation tp prog { grinStats = mempty } >>= Control.Exception.evaluate ) ferr
    let estat = grinStats prog'
    let onerr grin' = do
            putErrLn $ "\n>>> Before " ++ name
            dumpGrin ("lint-before-" ++ name) prog
            Stats.printStat name estat
            putErrLn $ "\n>>> After " ++ name
            dumpGrin ("lint-after-" ++ name) grin'
    if transformSkipNoStats tp && Stats.null estat then do
        when dodump $ putErrLn "program not changed"
        return prog
     else do
    when (dodump && not (Stats.null estat)) $ Stats.printStat  name estat
    lintCheckGrin' (onerr prog') prog'
    let tstat = istat `mappend` estat
    if doIterate iterate (not $ Stats.null estat) then transformGrin tp { transformIterate = iterateStep iterate } prog' { grinStats = tstat } else return prog' { grinStats = tstat }
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

tcLam :: Maybe [Ty] -> Lam -> Tc [Ty]
tcLam mty (v :-> e) = f mty where
    f Nothing = ans (mapM tcVal v)
    f (Just ty) = ans $ do
        t <- mapM tcVal v
        same (":->" <+> show mty <+> show (v :-> e)) ty t
    ans r = local (\e -> e { envInScope = freeVars v `mappend` envInScope e }) $ r >> tcExp e

tcExp :: Exp -> Tc [Ty]
tcExp e = f e where
    f (e :>>= lam) = do
        t1 <- f e
        tcLam (Just t1) lam
    f n@(Prim p as t') = do
        mapM_ tcVal as
        return t'
    f ap@(BaseOp (Apply t) vs) = do
        (v':_) <- mapM tcVal vs
        if v' == TyNode then return t
         else fail $ "App apply arg doesn't match: " ++ show ap
    f ap@(BaseOp Eval [v]) = do
        v' <- tcVal v
        if v' == tyINode then return [TyNode]
         else fail $ "App eval arg doesn't match: " ++ show ap
    f a@(App fn as t) = do
        te <- asks envTyEnv
        (as',t') <- findArgsType te fn
        as'' <- mapM tcVal as
        if t' == t then
            if as'' == as' then return t' else
                fail $ "App: arguments do not match: " ++ show (a,as',t')
         else fail $ "App: results do not match: " ++ show (a,t,(as',t'))
    f e@(BaseOp (StoreNode _) vs) = do
        [NodeC {}] <- return vs
        mapM_ tcVal vs
        return (getType e)
    f Alloc { expValue = v, expCount = c, expRegion = r } = do
        t <- tcVal v
        tcVal c
        tcVal r
        return [TyPtr t]
    f (Return v) = mapM tcVal v
    f (BaseOp Promote [v]) = do
        TyINode <- tcVal v
        return [TyNode]
    f (BaseOp Demote [v]) = do
        TyNode <- tcVal v
        return [TyINode]
    f (Error _ t) = return t
    f e@(BaseOp Overwrite [w,v]) = do
        NodeC {} <- return v
        tcVal w
        tcVal v
        return []
    f e@(BaseOp PokeVal [w,v]) = do
        TyPtr t <- tcVal w
        tv <- tcVal v
        when (t /= tv) $
            fail "PokeVal: types don't match"
        return []
    f e@(BaseOp PeekVal [w]) = do
        TyPtr t <- tcVal w
        return [t]
    f (Case _ []) = fail "empty case"
    f (Case v as) = do
        tv <- tcVal v
        es <- mapM (tcLam (Just [tv])) as
        foldl1M (same $ "case exp: " ++ show (map head $ sortGroupUnder fst (zip es as)) ) es
    f (Let { expDefs = defs, expBody = body }) = do
        local (\e -> e { envTyEnv = extendTyEnv defs (envTyEnv e) }) $ do
            mapM_ (tcLam Nothing) [ b | FuncDef { funcDefBody = b } <- defs ]
            f body
    f _ = error "Grin.Lint: unknown value passed to f"

tcVal :: Val -> Tc Ty
tcVal v = f v where
    f e@(Var v t) = do
        s <- asks envInScope
        case v `member` s of
            True -> return t
            False -> fail $ "variable not in scope: " ++ show e
    f (Lit _ t) = return t
    f Unit = return TyUnit
    f (Const t) = do
        v <- f t
        case v of
            TyNode -> return TyINode
            v -> return (TyPtr v)
    f (Index v offset) = do
        t <- f v
        TyPrim _ <- f offset
        return t
    f (ValUnknown ty) = return ty
    f (ValPrim _ vs ty) = do mapM_ f vs >> return ty
    f n@(NodeC tg as) = do
        te <- asks envTyEnv
        (as',_) <- findArgsType te tg
        as'' <- mapM f as
        if as'' == as' then return TyNode else
            fail $ "NodeC: arguments do not match " ++ show n ++ show (as'',as')
    f (Item _ t) = return t


