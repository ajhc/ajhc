module E.Annotate where

import Control.Monad.Reader
import Data.FunctorM
import Data.Monoid
import qualified Data.Map as Map

import E.E
import E.Subst
import GenUtil
import Info.Info as Info
import Info.Types



annotate :: Monad m =>
    (Map.Map Id (Maybe E))
    -> (Id -> m Info)   -- ^ annotate based on Id map
    -> (E -> m Info) -- ^ annotate letbound bindings
    -> (E -> m Info) -- ^ annotate lambdabound bindings
    ->  E            -- ^ term to annotate
    -> m E
annotate imap idann letann lamann e = runReaderT (f e) imap where
    -- f :: Monad m => E -> ReaderT (Map.Map Int (Maybe E)) m E
    f eo@(EVar tvr@(TVr { tvrIdent = i, tvrType =  t })) = do
        mp <- ask
        case Map.lookup i mp of
          Just (Just v) -> return v
          _  -> return eo
    f (ELam tvr e) = lp LambdaBound ELam tvr e
    f (EPi tvr e) = lp PiBound EPi tvr e
    f (EAp a b) = liftM2 EAp (f a) (f b)
    f (EError x e) = liftM (EError x) (f e)
    f (EPrim x es e) = liftM2 (EPrim x) (mapM f es) (f e)
    f (ELetRec dl e) = do
        dl' <- flip mapM dl $ \ (t,e) -> do
            nfo <- lift $ letann e
            return (t,Info.insert LetBound nfo)
        (as,rs) <- liftM unzip $ mapMntvr dl'
        local (mconcat rs) $ do
            ds <- mapM f (snds dl)
            e' <- f e
            return $ ELetRec (zip as ds) e'
    f (ELit l) = liftM ELit $ litSMapM f l
    f Unknown = return Unknown
    f e@(ESort {}) = return e
    f ec@(ECase {}) = do
        e' <- f $ eCaseScrutinee ec
        (b',r) <- ntvr (Info.singleton CaseDefault) [] $ eCaseBind ec
        d <- local r $ fmapM f $ eCaseDefault ec
        let da (Alt (LitCons s vs t) e) = do
                t' <- f t
                (as,rs) <- liftM unzip $ mapMntvr (zip vs (repeat (Info.singleton CasePattern)))
                e' <- local (mconcat rs) $ f e
                return $ Alt (LitCons s as t') e'
            da (Alt l e) = do
                l' <- fmapM f l
                e' <- f e
                return $ Alt l' e'
        alts <- (mapM da $ eCaseAlts ec)
        return  ECase { eCaseScrutinee = e', eCaseDefault = d, eCaseBind = b', eCaseAlts = alts }
    lp bnd lam tvr@(TVr { tvrIdent = n, tvrType = t}) e | n == 0  = do
        t' <- f t
        nfo <- lift $ lamann e
        e' <- local (Map.insert n Nothing) $ f e
        return $ lam (tvr { tvrIdent =  0, tvrType =  t',  tvrInfo = tvrInfo tvr `mappend` Info.insert bnd nfo}) e'
    lp bnd lam tvr e = do
        nfo <- lift $ lamann e
        (tv,r) <- ntvr (Info.insert bnd nfo) [] tvr
        e' <- local r $ f e
        return $ lam tv e'
    mapMntvr ts = f ts [] where
        f [] xs = return $ reverse xs
        f ((t,nfo):ts) rs = do
            (t',r) <- ntvr nfo vs t
            local r $ f ts ((t',r):rs)
        vs = [ tvrNum x | (x,_) <- ts ]
    -- ntvr :: Monad m => Info -> [Int] -> TVr -> ReaderT (Map.Map Int (Maybe E)) m (TVr, (Map.Map Int (Maybe E)) -> (Map.Map Int (Maybe E)))
    ntvr nfo xs tvr@(TVr { tvrIdent = 0, tvrType =  t}) = do
        t' <- f t
        let nvr = (tvr { tvrType =  t', tvrInfo = tvrInfo tvr `mappend` nfo})
        return (nvr,id)
    ntvr nfo xs tvr@(TVr {tvrIdent = i, tvrType =  t}) = do
        t' <- f t
        ss <- ask
        nfo' <- lift $ idann i
        let i' = mnv xs i ss
        let nvr = (tvr { tvrIdent =  i', tvrType =  t', tvrInfo = tvrInfo tvr `mappend` nfo `mappend` nfo'})
        case i == i' of
            True -> return (nvr,Map.insert i (Just $ EVar nvr))
            False -> return (nvr,Map.insert i (Just $ EVar nvr) . Map.insert i' Nothing)

mnv xs i ss
    | i <= 0 || i `Map.member` ss = nv (Map.fromList [ (x,undefined) | x <- xs ] `mappend` ss)
    | otherwise = i


nv ss = v (2 * (Map.size ss + 1)) where
    v n | n `Map.member` ss = v (n + 2)
    v n = n

nv' ss = v (2 * (Map.size ss + 1)) where
    v n | (Just Nothing) <- Map.lookup n ss = v (n + 2)
    v n = n
