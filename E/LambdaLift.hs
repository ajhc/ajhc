module E.LambdaLift(lambdaLift)  where

import Control.Monad.Reader
import Control.Monad.Writer
import Data.FunctorM
import Data.IORef
import List hiding(insert)

import Atom
import E.E
import E.FreeVars
import E.Program
import E.Subst
import E.Traverse
import E.TypeCheck
import E.Values
import Fixer.Fixer
import Fixer.Supply
import GenUtil
import Name.Id
import Name.Name
import Stats
import Support.FreeVars
import Util.Graph as G
import Util.SetLike
import Util.UniqueMonad



data S = S {
    funcName :: Name,
    topVars :: IdSet,
    isStrict :: Bool,
    declEnv :: [(TVr,E)]
    }
    {-! derive: update !-}

etaReduce :: E -> (E,Int)
etaReduce e = case f e 0 of
        (ELam {},_) -> (e,0)
        x -> x
    where
        f (ELam t (EAp x (EVar t'))) n | n `seq` True, t == t' && not (tvrIdent t `member` (freeVars x :: IdSet)) = f x (n + 1)
        f e n = (e,n)

calculateLiftees :: Program -> IO IdSet
calculateLiftees prog = do
    fixer <- newFixer
    sup <- newSupply fixer

    let f v env ELetRec { eDefs = ds, eBody = e } = do
            let nenv = fromList [ (tvrIdent t,length (snd (fromLam e))) | (t,e) <- ds ]  `mappend` env
                nenv :: IdMap Int
                g (t,e@ELam {}) = do
                    v <- supplyValue sup (tvrIdent t)
                    let (a,_as) = fromLam e
                    f v nenv a
                g (t,e) = do
                    f (value True) nenv e
            mapM_ g ds
            f v nenv e
        f v env e@ESort {} = return ()
        f v env e@Unknown {} = return ()
        f v env e@EError {} = return ()
        f v env (EVar TVr { tvrIdent = vv }) = do
            nv <- supplyValue sup vv
            assert nv
        f v env e | (EVar TVr { tvrIdent = vv }, as@(_:_)) <- fromAp e, Just n <- mlookup vv env = do
            nv <- supplyValue sup vv
            if length as >= n then v `implies` nv else assert nv
            mapM_ (f (value True) env) as
        f v env e | (a, as@(_:_)) <- fromAp e = do
            mapM_ (f (value True) env) as
            f v env a
        f v env (ELit (LitCons _ as _)) = mapM_ (f (value True) env) as
        f v env ELit {} = return ()
        f v env (EPi TVr { tvrType = a } b) = f (value True) env a >> f (value True) env b
        f v env (EPrim _ as _) = mapM_ (f (value True) env) as
        f v env ec@ECase {} = do
            f v env (eCaseScrutinee ec)
            mapM_ (f v env) (caseBodies ec)
        f v env (ELam _ e) = f (value True) env e
        f _ _ EAp {} = error "this should not happen"
    mapM_ (f (value False) mempty) [ fst (fromLam e) | (_,e) <- programDs prog]

    calcFixpoint "Liftees" fixer
    vs <- supplyReadValues sup
    mapM_ Prelude.print [ (tvr { tvrIdent = id },"Not Lifted") | (id,False) <- vs ]
    return (fromList [ x | (x,False) <- vs])

implies :: Value Bool -> Value Bool -> IO ()
implies x y = addRule $ y `isSuperSetOf` x

assert x = value True `implies` x


lambdaLift ::  Program -> IO Program
lambdaLift prog@Program { progDataTable = dataTable, progCombinators = cs } = do
    noLift <- calculateLiftees prog
    let wp =  fromList [ tvrIdent x | (x,_,_) <- cs ]
    fc <- newIORef []
    statRef <- newIORef mempty
    let z (n,as,v) = do
            let ((v',cs'),stat) = runReader (runStatT $ execUniqT 1 $ runWriterT (f v)) S { funcName = mkFuncName (tvrIdent n), topVars = wp,isStrict = True, declEnv = [] }
            modifyIORef statRef (mappend stat)
            modifyIORef fc (\xs -> (n,as,v'):cs' ++ xs)
        shouldLift t _ | tvrIdent t `member` noLift = False
        shouldLift _ ECase {} = True
        shouldLift _ ELam {} = True
        shouldLift _ _ = False
        f e@(ELetRec ds _)  = do
            let (ds',e') = decomposeLet e
            h ds' e' []
            --local (declEnv_u (ds ++)) $ do
            --    let (ds',e') = decomposeLet e
            --    h ds' e' []
        f e = do
            st <- asks isStrict
            if ((tvrIdent tvr `notMember` noLift && isELam e) || (shouldLift tvr e && not st)) then do
                (e,fvs'') <- pLift e
                doBigLift e fvs'' return
             else g e
        -- This ensures there are no 'orphaned type terms' when something is
        -- lifted out.  The problem occurs when a type is subsituted in some
        -- places and not others, the type as free variable will not be the
        -- same as its substituted instances if the variable is bound by a
        -- lambda, Although the program is still typesafe, it is no longer
        -- easily proven so, so we avoid the whole mess by subtituting known
        -- type variables within lifted expressions. This can not duplicate work
        -- since types are unpointed, but might change space usage slightly.
        g ec@ECase { eCaseScrutinee = (EVar v), eCaseAlts = as, eCaseDefault = d} | sortStarLike (tvrType v) = do
            True <- asks isStrict
            d' <- fmapM f d
            let z (Alt l e) = do
                    e' <- local (declEnv_u ((v,patToLitEE l):)) $ f e
                    return $ Alt l e'
            as' <- mapM z as
            return ec { eCaseAlts = as', eCaseDefault = d'}
        g (ELam t e) = do
            e' <- local (isStrict_s True) (g e)
            return (ELam t e')
        g e = emapE' f e
        pLift e = do
            gs <- asks topVars
            ds <- asks declEnv
            let fvs = freeVars e
                fvs' = filter (not . (`member` gs) . tvrIdent) fvs
                ss = filter (sortStarLike . tvrType) fvs'
                f [] e False = return (e,fvs'')
                f [] e True = pLift e
                f (s:ss) e x
                    | Just v <- lookup s ds = f ss (removeType s v e) True   -- TODO subst
                    | otherwise = f ss e x
                fvs'' = reverse $ topSort $ newGraph fvs' tvrIdent freeVars
            f ss e False
        h (Left (t,e):ds) rest ds' | shouldLift t e = do
            (e,fvs'') <- pLift e
            case fvs'' of
                [] -> doLift t e (h ds rest ds')
                fs -> doBigLift e fs (\e'' -> h ds rest ((t,e''):ds'))
        h (Left (t,e@ELam {}):ds) rest ds' = do
            let (a,as) = fromLam e
            a' <- local (isStrict_s True) (f a)
            h ds rest ((t,foldr ELam a' as):ds')

        h (Left (t,e):ds) rest ds'  = do
            let fvs =  freeVars e :: [Id]
            gs <- asks topVars
            let fvs' = filter (not . (`member` gs) ) fvs
            case fvs' of
                [] -> doLift t e (h ds rest ds')  -- We always lift CAFS to the top level for now. (GC?)
                _ ->  local (isStrict_s False) (f e) >>= \e'' -> h ds rest ((t,e''):ds')
        --h (Left (t,e):ds) e' ds' = local (isStrict_s False) (f e) >>= \e'' -> h ds e' ((t,e''):ds')
        h (Right rs:ds) rest ds' | any (uncurry shouldLift) rs  = do
            gs <- asks topVars
            let fvs =  freeVars (snds rs)--   (Set.fromList (map tvrIdent $ fsts rs) `Set.union` gs)
            let fvs' = filter (not . (`member` (fromList (map tvrIdent $ fsts rs) `mappend` gs) ) . tvrIdent) fvs
                fvs'' = reverse $ topSort $ newGraph fvs' tvrIdent freeVars
            case fvs'' of
                [] -> doLiftR rs (h ds rest ds')  -- We always lift CAFS to the top level for now. (GC?)
                fs -> doBigLiftR rs fs (\rs' -> h ds rest (rs' ++ ds'))
        h (Right rs:ds) e' ds'   = do
            local (isStrict_s False) $ do
                rs' <- flip mapM rs $ \te -> case te of
                    (t,e@ELam {}) -> do
                        let (a,as) = fromLam e
                        a' <- local (isStrict_s True) (f a)
                        return (t,foldr ELam a' as)
                    (t,e) -> do
                        e'' <- f e
                        return (t,e'')
                h ds e' (rs' ++ ds')
        h [] e ds = f e >>= return . eLetRec ds
        doLift t e r = local (topVars_u (insert (tvrIdent t)) ) $ do
            --(e,tn) <- return $ etaReduce e
            let (e',ls) = fromLam e
            mtick (toAtom $ "E.LambdaLift.doLift." ++ typeLift e ++ "." ++ show (length ls))
            --mticks tn (toAtom $ "E.LambdaLift.doLift.etaReduce")
            e'' <- local (isStrict_s True) $ f e'
            tell [(t,ls,e'')]
            r
        doLiftR rs r = local (topVars_u (mappend (fromList (map (tvrIdent . fst) rs)) )) $ do
            flip mapM_ rs $ \ (t,e) -> do
                --(e,tn) <- return $ etaReduce e
                let (e',ls) = fromLam e
                mtick (toAtom $ "E.LambdaLift.doLiftR." ++ typeLift e ++ "." ++ show (length ls))
                --mticks tn (toAtom $ "E.LambdaLift.doLift.etaReduce")
                e'' <- local (isStrict_s True) $ f e'
                tell [(t,ls,e'')]
            r
        newName tt = do
            un <-  newUniq
            n <- asks funcName
            return $ tVr (toId $ mapName (id,(++ ('$':show un))) n) tt
        doBigLift e fs  dr = do
            mtick (toAtom $ "E.LambdaLift.doBigLift." ++ typeLift e ++ "." ++ show (length fs))
            ds <- asks declEnv
            let tt = typeInfer' dataTable ds (foldr ELam e fs)
            tvr <- newName tt
            let (e',ls) = fromLam e
            e'' <- local (isStrict_s True) $ f e'
            tell [(tvr,fs ++ ls,e'')]
            let e'' = foldl EAp (EVar tvr) (map EVar fs)
            dr e''
        doBigLiftR rs fs dr = do
            ds <- asks declEnv
            rst <- flip mapM rs $ \ (t,e) -> do
                case shouldLift t e of
                    True -> do
                        mtick (toAtom $ "E.LambdaLift.doBigLiftR." ++ typeLift e ++ "." ++ show (length fs))
                        let tt = typeInfer' dataTable ds (foldr ELam e fs)
                        tvr <- newName tt
                        let (e',ls) = fromLam e
                        e'' <- local (isStrict_s True) $ f e'
                        --tell [(tvr,fs ++ ls,e'')]
                        let e''' = foldl EAp (EVar tvr) (map EVar fs)
                        return ((t,e'''),[(tvr,fs ++ ls,e'')])
                    False -> do
                        mtick (toAtom $ "E.LambdaLift.skipBigLiftR." ++ show (length fs))
                        return ((t,e),[])
            let (rs',ts) = unzip rst
            tell [ (t,ls,substLet rs' e) | (t,ls,e) <- concat ts]
            dr rs'

        mkFuncName x = case fromId x of
            Just y -> y
            Nothing -> toName Val ("LL@",'f':show x)
    mapM_ z cs
    ncs <- readIORef fc
    nstat <- readIORef statRef
    return $ prog { progCombinators =  ncs, progStats = progStats prog `mappend` nstat }


typeLift ECase {} = "Case"
typeLift ELam {} = "Lambda"
typeLift _ = "Other"

removeType t v e  = subst' t v e
{-
removeType t v e = ans where
    (b,ls) = fromLam e
    ans = foldr f (substLet [(t,v)] e) ls
    f tv@(TVr { tvrType = ty} ) e = ELam nt (subst tv (EVar nt) e) where nt = tv { tvrType = (subst t v ty) }
-}



