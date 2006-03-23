module E.Shadow(allShadow) where

import Control.Monad.Reader
import E.E
import E.Subst(litSMapM)
import qualified Data.Map as Map

-- | This is simplified to only work on things that only occur in types and is deterministic, so can be used to compare modulo naming differences.

allShadow :: E -> E
allShadow e  = f e (Map.empty,-2) where
    f :: E -> (Map.Map Int E,Int) -> E
    f eo@(EVar (TVr { tvrIdent =  i})) = do
        (mp,_) <- ask
        case Map.lookup i mp of
          Just v -> return v
          _  -> return  eo
    f (ELam tvr e) = lp ELam tvr e
    f (EPi tvr e) = lp EPi tvr e
    f (EAp a b) = liftM2 EAp (f a) (f b)
    f (EError x e) = liftM (EError x) (f e)
    f (EPrim x es e) = liftM2 (EPrim x) (mapM f es) (f e)
    f Unknown = return Unknown
    f e@(ESort {}) = return e
    f (ELit l) = liftM ELit $ litSMapM f l
    f e = error $ "allShadow: " ++ show e
    lp lam tvr e = do
        (tv,r) <- ntvr tvr
        e' <- local r $ f e
        return $ lam tv e'

    ntvr tvr@(TVr { tvrIdent = i, tvrType =  t }) = do
        t' <- f t
        (_,i') <- ask
        let nvr = (tvr { tvrIdent =  i', tvrType =  t'})
        return (nvr,\ (a,b) -> (Map.insert i (EVar nvr) a,i' - 2))

    {-
    f (ELetRec dl e) = do
        (as,rs) <- liftM unzip $ mapMntvr (fsts dl)
        local (mconcat rs) $ do
            ds <- mapM f (snds dl)
            e' <- f e
            return $ ELetRec (zip as ds) e'
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
        alts <- local r (mapM da $ eCaseAlts ec)
        return  ECase { eCaseScrutinee = e', eCaseDefault = d, eCaseBind = b', eCaseAlts = alts }
    lp lam (TVr n t) e | n == 0 || (n `notElem` freeVars e) = do
        t' <- f t
        e' <- f e
        return $ lam (TVr 0 t') e'
    mapMntvr ts = f ts [] where
        f [] xs = return $ reverse xs
        f (t:ts) rs = do
            (t',r) <- ntvr vs t
            local r $ f ts ((t',r):rs)
        vs = [ tvrNum x | x <- ts ]
    -}



