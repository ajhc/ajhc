module E.Subst(
    doSubst,
    eAp,
    litSMapM,
    subst,
    subst',
    substMap,
    substMap'',
    typeSubst,
    typeSubst'
    ) where

-- This is tricky.

import Control.Monad.Reader
import Data.FunctorM
import Data.Monoid
import List hiding(union,insert,delete)

import E.E
import E.FreeVars()
import Name.Id
import Support.FreeVars
import GenUtil
import Util.SetLike as S
import Util.HasSize

eLetRec :: [(TVr,E)] -> E -> E
eLetRec ds e = f (filter ((/= 0) . tvrIdent . fst) ds) where
    f [] = e
    f ds = ELetRec ds e


-- | Basic substitution routine
subst ::
    TVr   -- ^ Variable to substitute
    -> E  -- ^ What to substitute with
    -> E  -- ^ input term
    -> E  -- ^ output term
subst (TVr { tvrIdent = 0 }) _ e = e
subst (TVr { tvrIdent = i }) w e = doSubst False False (minsert i (Just w) $ (freeVars w `union` freeVars e))  e

-- | Identitcal to 'subst' except that it substitutes inside the local types
-- for variables in expressions. This should not be used because it breaks the
-- sharing of types between a binding site of a variable and its uses and can
-- lead to inconsistant terms. However, it is sometimes useful to create
-- transient terms for typechecking.

subst' :: TVr -> E -> E -> E
subst' (TVr { tvrIdent = 0 }) _ e = e
subst' (TVr { tvrIdent = (i) }) w e = doSubst True False (minsert i (Just w) $ (freeVars w `union` freeVars e)) e




litSMapM f (LitCons s es t) = do
    t' <- f t
    es' <- mapM f es
    return $ LitCons s es' t'
litSMapM f (LitInt n t) = do
    t' <- f t
    return $ LitInt n t'



substMap :: IdMap E -> E -> E
substMap im e = doSubst False False (fmap ( (`mlookup` im) . tvrIdent) (unions $ (freeVars e :: IdMap TVr):map freeVars (melems im))) e

-- | doesn't seed with free variables.
substMap'' :: IdMap E -> E -> E
substMap'' im = doSubst False False (fmap Just im)

-- Monadic code is so much nicer
doSubst :: Bool -> Bool -> IdMap (Maybe E) -> E -> E
doSubst substInVars allShadow bm e  = f e bm where
    f :: E -> IdMap (Maybe E) -> E
    f eo@(EVar tvr@(TVr { tvrIdent = i, tvrType =  t })) = do
        mp <- ask
        case mlookup i mp of
          Just (Just v) -> return v
          _
            | substInVars -> f t >>= \t' -> return $ EVar (tvr { tvrType =  t'})
            | otherwise  -> return  eo
    f (ELam tvr e) = lp ELam tvr e
    f (EPi tvr e) = lp EPi tvr e
    f (EAp a b) = liftM2 eAp (f a) (f b)
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
        nty <- f (eCaseType ec)
        return  ec { eCaseScrutinee = e', eCaseDefault = d, eCaseBind = b', eCaseAlts = alts, eCaseType = nty }
    lp lam tvr@(TVr { tvrIdent = n, tvrType = t}) e | n == 0 || (allShadow && n `notElem` freeVars e) = do
        t' <- f t
        e' <- local (minsert n Nothing) $ f e
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
        vs = [ tvrIdent x | x <- ts ]

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
            True -> return (nvr,minsert i (Just $ EVar nvr))
            False -> return (nvr,minsert i (Just $ EVar nvr) . minsert i' Nothing)



mnv allShadow xs i ss
    | allShadow = nv ss
    | i <= 0 || i `mmember` ss = nv (fromList [ (x,undefined) | x <- xs ] `mappend` ss)
    | otherwise = i


nv ss = v (2 * (size ss + 1)) where
    v n | n `mmember` ss = v (n + 2)
    v n = n

nv' ss = v (2 * (size ss + 1)) where
    v n | (Just Nothing) <- mlookup n ss = v (n + 2)
    v n = n



eAp (EPi (TVr { tvrIdent =  0 }) b) _ = b
eAp (EPi t b) e = subst t e b
--eAp (EPrim n es t@(EPi _ _)) b = EPrim n (es ++ [b]) (eAp t b)  -- only apply if type is pi-like
eAp (ELit (LitCons n es (EPi t r))) b = ELit (LitCons n (es ++ [b]) (subst t b r))
eAp (EError s t) b = EError s (eAp t b)
eAp a b = EAp a b

typeSubst' :: IdMap E -> IdMap E -> E -> E
typeSubst' termSub typeSub e | isEmpty termSub && isEmpty typeSub = e
--typeSubst' termSub typeSub e = typeSubst  (Map.map Just termSub `Map.union` Map.fromAscList [ (x,Map.lookup x termSub) | x <- fvs]) typeSub e  where
--    fvs = Set.toAscList (freeVars e `Set.union` fvmap termSub `Set.union` fvmap typeSub)
--    fvmap m = Set.unions (map freeVars (Map.elems m))
typeSubst' termSub typeSub e = typeSubst  (fmap Just termSub `union` fmap ((`mlookup` termSub) . tvrIdent) fvs) typeSub e  where
    fvs :: IdMap TVr
    fvs = (freeVars e `union` fvmap termSub `union` fvmap typeSub)
    fvmap m = unions (map freeVars (melems m))

substType t e e' = typeSubst (freeVars e `union` freeVars e') (msingleton t e) e'

-- | substitution routine that can substitute different values at the term and type level.
-- this is useful to enforce the invarient that let-bound variables must not occur at the type level, yet
-- non-atomic values (even typelike ones) cannot appear in argument positions at the term level.

typeSubst ::
    IdMap (Maybe E)  -- ^ substitution to carry out at term level as well as a list of in-scope variables
    -> IdMap E       -- ^ substitution to carry out at type level
    -> (E -> E)           -- ^ the substitution function
typeSubst termSubst typeSubst e | isEmpty termSubst && isEmpty typeSubst = e
typeSubst termSubst typeSubst e  = f e (False,termSubst',typeSubst) where
    termSubst' = termSubst `union` fmap (const Nothing) typeSubst
    f :: E -> (Bool,IdMap (Maybe E),IdMap E) -> E
    f eo@(EVar tvr@(TVr { tvrIdent = i, tvrType =  t })) = do
        (wh,trm,tp) <- ask
        case (wh,mlookup i trm, mlookup i tp) of
          (False,(Just (Just v)),_) -> return v
          (True,_,(Just v)) -> return v
          _ -> return eo
    f (ELam tvr e) = lp ELam tvr e
    f (EPi tvr e) = lp EPi tvr e
    f (EAp a b) = liftM2 eAp (f a) (f b)
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
        nty <- inType (f $ eCaseType ec)
        return  ec { eCaseScrutinee = e', eCaseDefault = d, eCaseBind = b', eCaseAlts = alts, eCaseType = nty }
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
        vs = [ tvrIdent x | x <- ts ]
    inType = local (\ (_,trm,typ) -> (True,trm,typ) )
    addMap i (Just e) (b,trm,typ) = (b,minsert i (Just e) trm, minsert i e typ)
    addMap i Nothing (b,trm,typ) = (b,minsert i Nothing trm, typ)
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


