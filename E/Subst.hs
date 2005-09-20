module E.Subst(subst,subst',eAp, substMap,substMap',noShadow,doSubst,substMap'',litSMapM, app, substLet) where

-- This is tricky.

import CanType
import Control.Monad.Reader
import Data.FunctorM
import Data.Monoid
import E.E
import FreeVars
import E.FreeVars
import GenUtil
import List
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Map as Map
import Stats
import Atom


substLet :: [(TVr,E)] -> E -> E
substLet ds e  = ans where
    (as,nas) = partition (isAtomic . snd) (filter ((/= 0) . tvrNum . fst) ds)
    ans = eLetRec nas (substMap' (Map.fromList [ (n,e) | (TVr { tvrIdent = n },e) <- as]) e)

-- | Basic substitution routine
subst ::
    TVr   -- ^ Variable to substitute
    -> E  -- ^ What to substitute with
    -> E  -- ^ input term
    -> E  -- ^ output term
subst (TVr { tvrIdent = 0 }) _ e = e
--subst (TVr { tvrIdent = i }) w e = doSubst False False (Map.insert i (Just w) $ Map.fromList [ (x,Nothing) | x <- freeVars (getType w) ++ freeVars e ]) e
subst (TVr { tvrIdent = i }) w e = doSubst False False (Map.insert i (Just w) $ Map.fromList [ (x,Nothing) | x <- freeVars w ++ freeVars e ]) e

-- | Identitcal to 'subst' except that it substitutes inside the local types
-- for variables in expressions. This should not be used because it breaks the
-- sharing of types between a binding site of a variable and its uses and can
-- lead to inconsistant terms. However, it is sometimes useful to create
-- transient terms for typechecking.

subst' :: TVr -> E -> E -> E
subst' (TVr { tvrIdent = 0 }) _ e = e
--subst' (TVr { tvrIdent = (i) }) w e = doSubst True False (Map.insert i (Just w) $ Map.fromList [ (x,Nothing) | x <- freeVars (getType w) ++ freeVars e ]) e
subst' (TVr { tvrIdent = (i) }) w e = doSubst True False (Map.insert i (Just w) $ Map.fromList [ (x,Nothing) | x <- freeVars w ++ freeVars e ]) e


substMap :: IM.IntMap E -> E -> E
substMap im e = substMapScope im (IS.unions $ freeVars e: (map freeVars (IM.elems im))) e


litSMapM f (LitCons s es t) = do
    t' <- f t
    es' <- mapM f es
    return $ LitCons s es' t'
litSMapM f l = fmapM f l


substMapScope :: IM.IntMap E -> IS.IntSet -> E -> E
substMapScope im ss e = substMapScope' False im ss e

substMapScope' :: Bool -> IM.IntMap E -> IS.IntSet -> E -> E
substMapScope' allShadow im ss e = doSubst False allShadow (Map.fromAscList [ (x,Just y) |  (x,y) <-  IM.toAscList im] `Map.union` Map.fromAscList [ (x,Nothing) | x <- IS.toAscList ss ]) e

noShadow :: E -> E
noShadow e = doSubst False False (Map.fromList [ (x,Nothing) | x <- freeVars e ]) e

allShadow :: E -> E
allShadow e = doSubst False True (Map.fromList [ (x,Nothing) | x <- freeVars e ]) e

substMap' :: Map.Map Int E -> E -> E
substMap' im e = doSubst False False (Map.fromList [ (x,Map.lookup x im) | x <- (freeVars e ++ freeVars (Map.elems im)) ]) e

-- | doesn't seed with free variables.
substMap'' :: Map.Map Int E -> E -> E
substMap'' im = doSubst False False (Map.map Just im) -- (Map.fromAscList [ (x,Just y) | (x,y) <- Map.toAscList im ]) e

-- Monadic code is so much nicer
doSubst :: Bool -> Bool -> Map.Map Int (Maybe E) -> E -> E
doSubst substInVars allShadow bm e  = f e bm where
    f :: E -> Map.Map Int (Maybe E) -> E
    f eo@(EVar tvr@(TVr { tvrIdent = i, tvrType =  t })) = do
        mp <- ask
        case Map.lookup i mp of
          Just (Just v) -> return v
          _
            | substInVars -> f t >>= \t' -> return $ EVar (tvr { tvrType =  t'})
            | otherwise  -> return  eo
    f (ELam tvr e) = lp ELam tvr e
    f (EPi tvr e) = lp EPi tvr e
    f (EAp a b) = liftM2 EAp (f a) (f b)
    f (EError x e) = liftM (EError x) (f e)
    f (EPrim x es e) = liftM2 (EPrim x) (mapM f es) (f e)
    f (ELetRec dl e) = do
        (as,rs) <- liftM unzip $ mapMntvr (fsts dl)
        local (mconcat rs) $ do
            ds <- mapM f (snds dl)
            e' <- f e
            return $ ELetRec (zip as ds) e'
    f (ELit l) = liftM ELit $ litSMapM f l
    f Unknown = return Unknown
    f e@(ESort {}) = return e
    f ec@(ECase {}) = do
        e' <- f $ eCaseScrutinee ec
        (b',r) <- ntvr [] $ eCaseBind ec
        d <- local r $ fmapM f $ eCaseDefault ec
        let da (Alt (LitCons s vs t) e) = do
                t' <- f t
                (as,rs) <- liftM unzip $ mapMntvr vs
                e' <- local (mconcat rs) $ f e
                return $ Alt (LitCons s as t') e'
            da (Alt l e) = do
                l' <- fmapM f l
                e' <- f e
                return $ Alt l' e'
        alts <- (mapM da $ eCaseAlts ec)
        return  ECase { eCaseScrutinee = e', eCaseDefault = d, eCaseBind = b', eCaseAlts = alts }
    lp lam tvr@(TVr { tvrIdent = n, tvrType = t}) e | n == 0 || (allShadow && n `notElem` freeVars e) = do
        t' <- f t
        e' <- local (Map.insert n Nothing) $ f e
        return $ lam (tvr { tvrIdent =  0, tvrType =  t'}) e'
    lp lam tvr e = do
        (tv,r) <- ntvr [] tvr
        e' <- local r $ f e
        return $ lam tv e'
    mapMntvr ts = f ts [] where
        f [] xs = return $ reverse xs
        f (t:ts) rs = do
            (t',r) <- ntvr vs t
            local r $ f ts ((t',r):rs)
        vs = [ tvrNum x | x <- ts ]

    --mapMntvr [] = return []
    --mapMntvr (t:ts) = do
    --    (t',r) <- ntvr t
    --    ts' <- local r (mapMntvr ts)
    --    return ((t',r):ts')
    --ntvr :: TVr -> Map Int (Maybe E) -> (TVr, Map Int (Maybe E) -> Map Int (Maybe E))
    ntvr xs tvr@(TVr { tvrIdent = 0, tvrType =  t}) = do
        t' <- f t
        let nvr = (tvr { tvrType =  t'})
        return (nvr,id)
    ntvr xs tvr@(TVr {tvrIdent = i, tvrType =  t}) = do
        t' <- f t
        i' <- mnv allShadow xs i
        let nvr = (tvr { tvrIdent =  i', tvrType =  t'})
        case i == i' of
            True -> return (nvr,Map.insert i (Just $ EVar nvr))
            False -> return (nvr,Map.insert i (Just $ EVar nvr) . Map.insert i' Nothing)



mnv allShadow xs i ss
    | allShadow = nv ss
    | i <= 0 || i `Map.member` ss = nv (Map.fromList [ (x,undefined) | x <- xs ] `mappend` ss)
    | otherwise = i


nv ss = v (2 * (Map.size ss + 1)) where
    v n | n `Map.member` ss = v (n + 2)
    v n = n

nv' ss = v (2 * (Map.size ss + 1)) where
    v n | (Just Nothing) <- Map.lookup n ss = v (n + 2)
    v n = n


app (e,[]) = return e
app (e,xs) = app' e xs

app' (ELit (LitCons n xs t)) (a:as)  = do
    mtick (toAtom $ "E.Simplify.typecon-reduce.{" ++ show n ++ "}" )
    app (ELit (LitCons n (xs ++ [a]) (eAp t a)),as)
app' (ELam tvr e) (a:as) = do
    mtick (toAtom "E.Simplify.beta-reduce")
    app (subst tvr a e,as)   -- TODO Fix quadradic substitution
app' (EPi tvr e) (a:as) = do
    mtick (toAtom "E.Simplify.pi-reduce")
    app (subst tvr a e,as)     -- Okay, types are small
app' ec@ECase {} xs = do
    mtick (toAtom "E.Simplify.case-application")
    let f e = app' e xs
    caseBodiesMapM f ec
app' (ELetRec ds e) xs = do
    mtick (toAtom "E.Simplify.let-application")
    e' <- app' e xs
    return $ eLetRec ds e'
app' (EError s t) xs = do
    mtick (toAtom "E.Simplify.error-application")
    return $ EError s (foldl eAp t xs)
app' e as = do
    return $ foldl EAp e as

eAp (EPi (TVr { tvrIdent =  0 }) b) _ = b
eAp (EPi t b) e = subst t e b
--eAp (EPrim n es t@(EPi _ _)) b = EPrim n (es ++ [b]) (eAp t b)  -- only apply if type is pi-like
eAp (ELit (LitCons n es t)) b = (ELit (LitCons n (es ++ [b]) (eAp t b)))
eAp (EError s t) b = EError s (eAp t b)
eAp a b = EAp a b


