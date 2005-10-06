module E.LambdaLift(SC(..), scToE, eToSC, lambdaLift, lambdaLiftE)  where

import Control.Monad.Reader
import Control.Monad.Writer
import Data.FunctorM
import Data.IORef
import List
import qualified Data.Set as Set

import Atom
import DataConstructors
import E.E
import E.FreeVars
import E.Subst
import E.Traverse
import E.TypeCheck
import E.Values
import FreeVars
import GenUtil
import Name
import Stats
import UniqueMonad
import Util.Graph as G


-- super combinators
data SC = SC { scMain :: TVr, scCombinators ::  [(TVr,[TVr],E)] }
    deriving(Eq,Show)

scToE :: SC -> E
scToE (SC v ds) = ELetRec ds' (EVar v) where
    ds' = sortLetDecls [ (t,foldr ELam e as) |  (t,as,e) <- ds]

eToSC :: DataTable -> E -> SC
eToSC _ (ELetRec ds (EVar v)) = SC v ds' where
    ds' = [ (a,b,c) | (a,(c,b)) <- [ (t,fromLam e) | (t,e) <- ds ]]
eToSC dt (ELetRec ds e) = SC tvr ((tvr,as,e'):ds') where
    (e',as) = fromLam e
    tvr = (tVr num (typeInfer dt e))
    --num = -2
    Just num = List.find (`notElem` [ n  | (TVr { tvrIdent = n },_) <- ds ]) [200000,200002 ..]
    ds' = [ (a,b,c) | (a,(c,b)) <- [ (t,fromLam e) | (t,e) <- ds ]]
eToSC dt v = SC tvr [(tvr,as,e')] where
    (e',as) = fromLam v
    tvr = (tVr num (typeInfer dt v))
    num = 200000
-- eToSC (ELetRec ds v) = error $ "eToSC: " ++ show v

-- | pull lets from just in definitions to top level, as they can obscure lambdas.
flattenSC :: SC -> SC
flattenSC (SC v cs) = SC v (concatMap f cs) where
    f (t,[],ELetRec ds e) = fd (t,e):map fd ds
    f (t,as,e) = [(t,as,e)]
    fd (t,e) =  let (c,b) = fromLam e in (t,b,c)

lambdaLiftE stats dt e = fmap scToE (lambdaLift stats dt (eToSC dt e))

data S = S { funcName :: Atom, topVars :: Set.Set Int, isStrict :: Bool, declEnv :: [(TVr,E)] }
    {-! derive: update !-}

etaReduce :: E -> (E,Int)
etaReduce e = case f e 0 of
        (ELam {},_) -> (e,0)
        x -> x
    where
        f (ELam t (EAp x (EVar t'))) n | n `seq` True, t == t' && not (tvrNum t `Set.member` freeVars x) = f x (n + 1)
        f e n = (e,n)

lambdaLift :: Stats -> DataTable -> SC -> IO SC
lambdaLift stats dataTable sc = do
    let SC m cs = sc -- flattenSC sc
    let wp =  Set.fromList [ tvrNum x | (x,_,_) <- cs ]
    fc <- newIORef []
    let z (n,as,v) = do
            let ((v',cs'),stat) = runReader (runStatT $ execUniqT 1 $ runWriterT (f v)) S { funcName = (intToAtom' (tvrNum n)), topVars = wp,isStrict = True, declEnv = [] }
            tickStat stats stat
            modifyIORef fc (\xs -> (n,as,v'):cs' ++ xs)
        f e@(ELetRec ds _)  = do
            local (declEnv_u (ds ++)) $ do
                let (ds',e') = decomposeLet e
                h ds' e' []
        f e = do
            st <- asks isStrict
            if (isELam e || (shouldLift e && not st)) then do
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
        g (ECase (EVar v) b as d) | sortStarLike (tvrType v) = do
            True <- asks isStrict
            d' <- fmapM f d
            let z (Alt l e) = do
                    e' <- local (declEnv_u ((v,patToLitEE l):)) $ f e
                    return $ Alt l e'
            as' <- mapM z as
            return $ ECase (EVar v) b as' d'
        g e = emapE' f e
        pLift e = do
            gs <- asks topVars
            ds <- asks declEnv
            let fvs = freeVars e
                fvs' = filter (not . (`Set.member` gs) . tvrNum) fvs
                ss = filter (sortStarLike . tvrType) fvs'
                f [] e False = return (e,fvs'')
                f [] e True = pLift e
                f (s:ss) e x
                    | Just v <- lookup s ds = f ss (removeType s v e) True   -- TODO subst
                    | otherwise = f ss e x
                fvs'' = reverse $ topSort $ newGraph fvs' tvrNum freeVars
            f ss e False
        h (Left (t,e):ds) rest ds' | shouldLift e = do
            (e,fvs'') <- pLift e
            case fvs'' of
                [] -> doLift t e (h ds rest ds')
                fs -> doBigLift e fs (\e'' -> h ds rest ((t,e''):ds'))

        h (Left (t,e):ds) rest ds'  = do
            let fvs =  freeVars e
            gs <- asks topVars
            let fvs' = filter (not . (`Set.member` gs) ) fvs
            case fvs' of
                [] -> doLift t e (h ds rest ds')  -- We always lift CAFS to the top level for now. (GC?)
                _ ->  local (isStrict_s False) (f e) >>= \e'' -> h ds rest ((t,e''):ds')
        --h (Left (t,e):ds) e' ds' = local (isStrict_s False) (f e) >>= \e'' -> h ds e' ((t,e''):ds')
        h (Right rs:ds) rest ds' | any shouldLift (snds rs)  = do
            gs <- asks topVars
            let fvs =  freeVars (snds rs)--   (Set.fromList (map tvrIdent $ fsts rs) `Set.union` gs)
            let fvs' = filter (not . (`Set.member` (Set.fromList (map tvrIdent $ fsts rs) `Set.union` gs) ) . tvrIdent) fvs
                fvs'' = reverse $ topSort $ newGraph fvs' tvrNum freeVars
            case fvs'' of
                [] -> doLiftR rs (h ds rest ds')  -- We always lift CAFS to the top level for now. (GC?)
                fs -> doBigLiftR rs fs (\rs' -> h ds rest (rs' ++ ds'))
        h (Right rs:ds) e' ds'   = do
            local (isStrict_s False) $ do
                rs' <- flip mapM rs $ \ (t,e) -> do
                    e'' <- f e
                    return (t,e'')
                h ds e' (rs' ++ ds')
        h [] e ds = f e >>= return . eLetRec ds
        doLift t e r = local (topVars_u (Set.insert (tvrNum t)) ) $ do
            --(e,tn) <- return $ etaReduce e
            let (e',ls) = fromLam e
            mtick (toAtom $ "E.LambdaLift.doLift." ++ typeLift e ++ "." ++ show (length ls))
            --mticks tn (toAtom $ "E.LambdaLift.doLift.etaReduce")
            e'' <- local (isStrict_s True) $ f e'
            tell [(t,ls,e'')]
            r
        doLiftR rs r = local (topVars_u (Set.union (Set.fromList (map (tvrNum . fst) rs)) )) $ do
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
            return $ tVr (atomIndex (n `mappend` toAtom ("$" ++ show un))) tt
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
                case shouldLift e of
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

        intToAtom' x = case intToAtom x of
            Just y -> y
            Nothing -> toAtom $ toName Val ("LL@",'f':show x)
    mapM_ z cs
    ncs <- readIORef fc
    return $ SC m ncs

shouldLift EError {} = True
shouldLift ECase {} = True
shouldLift ELam {} = True
shouldLift _ = False

typeLift EError {} = "Error"
typeLift ECase {} = "Case"
typeLift ELam {} = "Lambda"
typeLift _ = "Other"

removeType t v e  = subst' t v e
removeType t v e = ans where
    (b,ls) = fromLam e
    ans = foldr f (substLet [(t,v)] e) ls
    f tv@(TVr { tvrType = ty} ) e = ELam nt (subst tv (EVar nt) e) where nt = tv { tvrType = (subst t v ty) }





--        h ((t,e):ds) rest ds' | shouldLift e = do
--            let fvs =  freeVars e
--            gs <- asks topVars
--            let fvs' = filter (not . (`Set.member` gs) . tvrNum) fvs
--                fvs'' = reverse $ topSort $ newGraph fvs' tvrNum freeVars
--            case fvs'' of
--                [] -> doLift t e (h ds rest ds')
--                fs -> doBigLift e fs (\e'' -> h ds rest ((t,e''):ds'))


--        f e = do
--            st <- asks isStrict
--            if (isELam e || (shouldLift e && not st)) then do
--                let (fvs :: [TVr]) = freeVars e
--                (gs :: Set.Set Int) <- asks topVars
--                let fvs' = filter (not . (`Set.member` gs) . tvrNum) fvs
--                    fvs'' = reverse $ topSort $ newGraph fvs' tvrNum freeVars
--                doBigLift e fvs'' return
--             else emapE' f e

