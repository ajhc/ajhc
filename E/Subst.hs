module E.Subst(subst,subst',eAp, substMap,substMap',noShadow,doSubst,typeSubst,typeSubst',substMap'',litSMapM ) where

-- This is tricky.

import Control.Monad.Reader
import Data.FunctorM
import Data.Monoid
import List
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Map as Map
import qualified Data.Set as Set

import E.E
import E.FreeVars()
import FreeVars
import GenUtil

eLetRec :: [(TVr,E)] -> E -> E
eLetRec ds e = f (filter ((/= 0) . tvrNum . fst) ds) where
    f [] = e
    f ds = ELetRec ds e


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
litSMapM f (LitInt n t) = do
    t' <- f t
    return $ LitInt n t'
--litSMapM f l = fmapM f l


substMapScope :: IM.IntMap E -> IS.IntSet -> E -> E
substMapScope im ss e = substMapScope' False im ss e

substMapScope' :: Bool -> IM.IntMap E -> IS.IntSet -> E -> E
substMapScope' allShadow im ss e = doSubst False allShadow (Map.fromAscList [ (x,Just y) |  (x,y) <-  IM.toAscList im] `Map.union` Map.fromAscList [ (x,Nothing) | x <- IS.toAscList ss ]) e

noShadow :: E -> E
noShadow e = doSubst False False (Map.fromList [ (x,Nothing) | x <- freeVars e ]) e

allShadow :: E -> E
allShadow e = doSubst False True (Map.fromList [ (x,Nothing) | x <- freeVars e ]) e

substMap' :: Map.Map Id E -> E -> E
substMap' im e = doSubst False False (Map.fromList [ (x,Map.lookup x im) | x <- (freeVars e ++ freeVars (Map.elems im)) ]) e

-- | doesn't seed with free variables.
substMap'' :: Map.Map Id E -> E -> E
substMap'' im = doSubst False False (Map.map Just im) -- (Map.fromAscList [ (x,Just y) | (x,y) <- Map.toAscList im ]) e

-- Monadic code is so much nicer
doSubst :: Bool -> Bool -> Map.Map Id (Maybe E) -> E -> E
doSubst substInVars allShadow bm e  = f e bm where
    f :: E -> Map.Map Id (Maybe E) -> E
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



eAp (EPi (TVr { tvrIdent =  0 }) b) _ = b
eAp (EPi t b) e = subst t e b
--eAp (EPrim n es t@(EPi _ _)) b = EPrim n (es ++ [b]) (eAp t b)  -- only apply if type is pi-like
eAp (ELit (LitCons n es t)) b = (ELit (LitCons n (es ++ [b]) (eAp t b)))
eAp (EError s t) b = EError s (eAp t b)
eAp a b = EAp a b

typeSubst' :: Map.Map Id E -> Map.Map Id E -> E -> E
typeSubst' termSub typeSub e | Map.null termSub && Map.null typeSub = e
typeSubst' termSub typeSub e = typeSubst  (Map.map Just termSub `Map.union` Map.fromAscList [ (x,Map.lookup x termSub) | x <- fvs]) typeSub e  where
    fvs = Set.toAscList (freeVars e `Set.union` fvmap termSub `Set.union` fvmap typeSub)
    fvmap m = Set.unions (map freeVars (Map.elems m))

substType t e e' = typeSubst (freeVars e) (Map.singleton t e) e'
-- Monadic code is so much nicer
typeSubst :: Map.Map Id (Maybe E) -> Map.Map Id E -> E -> E
typeSubst termSubst typeSubst e | Map.null termSubst && Map.null typeSubst = e
typeSubst termSubst typeSubst e  = f e (False,termSubst',typeSubst) where
    termSubst' = termSubst `Map.union` Map.map (const Nothing) typeSubst
    f :: E -> (Bool,Map.Map Id (Maybe E),Map.Map Id E) -> E
    f eo@(EVar tvr@(TVr { tvrIdent = i, tvrType =  t })) = do
        (wh,trm,tp) <- ask
        case (wh,Map.lookup i trm, Map.lookup i tp) of
          (False,(Just (Just v)),_) -> return v
          (True,_,(Just v)) -> return v
          _ -> return eo
    f (ELam tvr e) = lp ELam tvr e
    f (EPi tvr e) = lp EPi tvr e
    f (EAp a b) = liftM2 EAp (f a) (f b)
    f (EError x e) = liftM (EError x) (inType $ f e)
    f (EPrim x es e) = liftM2 (EPrim x) (mapM f es) (inType $ f e)
    f (ELetRec dl e) = do
        (as,rs) <- liftM unzip $ mapMntvr (fsts dl)
        local (mconcat rs) $ do
            ds <- mapM f (snds dl)
            e' <- f e
            return $ ELetRec (zip as ds) e'
    f (ELit l) = liftM ELit $ litSMapM l
    f Unknown = return Unknown
    f e@(ESort {}) = return e
    f ec@(ECase {}) = do
        e' <- f $ eCaseScrutinee ec
        (b',r) <- ntvr [] $ eCaseBind ec
        d <- local r $ fmapM f $ eCaseDefault ec
        let da (Alt (LitCons s vs t) e) = do
                t' <- inType $ f t
                (as,rs) <- liftM unzip $ mapMntvr vs
                e' <- local (mconcat rs) $ f e
                return $ Alt (LitCons s as t') e'
            da (Alt (LitInt n t) e) = do
                t' <- inType (f t)
                e' <- f e
                return $ Alt (LitInt n t') e'
        alts <- (mapM da $ eCaseAlts ec)
        return  ECase { eCaseScrutinee = e', eCaseDefault = d, eCaseBind = b', eCaseAlts = alts }
    lp lam tvr@(TVr { tvrIdent = 0, tvrType = t}) e  = do
        t' <- inType (f t)
        e' <- f e
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
    inType = local (\ (_,trm,typ) -> (True,trm,typ) )
    addMap i (Just e) (b,trm,typ) = (b,Map.insert i (Just e) trm, Map.insert i e typ)
    addMap i Nothing (b,trm,typ) = (b,Map.insert i Nothing trm, typ)
    litSMapM (LitCons s es t) = do
        t' <- inType $ f t
        es' <- mapM f es
        return $ LitCons s es' t'
    litSMapM (LitInt n t) = do
        t' <- inType $ f t
        return $ LitInt n t'
    ntvr xs tvr@(TVr { tvrIdent = 0, tvrType =  t}) = do
        t' <- inType (f t)
        let nvr = (tvr { tvrType =  t'})
        return (nvr,id)
    ntvr xs tvr@(TVr {tvrIdent = i, tvrType =  t}) = do
        t' <- inType (f t)
        (_,map,_) <- ask
        let i' = mnv False xs i map
        let nvr = (tvr { tvrIdent =  i', tvrType =  t'})
        case i == i' of
            True -> return (nvr,addMap i  (Just $ EVar nvr))
            False -> return (nvr,addMap i (Just $ EVar nvr) . addMap i' Nothing)


