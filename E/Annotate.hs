module E.Annotate where

import Control.Monad.Reader
import Data.FunctorM
import Data.Monoid
import qualified Data.Map as Map

import E.E
import E.Rules
import E.Subst
import GenUtil
import Info.Info as Info
import Info.Types

annotateDs :: Monad m =>
    (Map.Map Id (Maybe E))
    -> (Id -> Info -> m Info)   -- ^ annotate based on Id map
    -> (E -> Info -> m Info) -- ^ annotate letbound bindings
    -> (E -> Info -> m Info) -- ^ annotate lambdabound bindings
    -> [(TVr,E)]            -- ^ terms to annotate
    -> m [(TVr,E)]

annotateDs imap idann letann lamann ds = do
    ELetRec ds' Unknown <- annotate imap idann letann lamann (ELetRec ds Unknown)
    return ds'

annotate :: Monad m =>
    (Map.Map Id (Maybe E))
    -> (Id -> Info -> m Info)   -- ^ annotate based on Id map
    -> (E -> Info -> m Info) -- ^ annotate letbound bindings
    -> (E -> Info -> m Info) -- ^ annotate lambdabound bindings
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
            nfo <- lift $ letann e (tvrInfo t)
            return (tvrInfo_u (Info.insert LetBound) t { tvrInfo = nfo })
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
        let caseBind = eCaseBind ec
        caseBind <- procRules caseBind
        (b',r) <- ntvr [] $ caseBind { tvrInfo = Info.insert CaseDefault (tvrInfo caseBind) }
        d <- local r $ fmapM f $ eCaseDefault ec
        let da (Alt (LitCons s vs t) e) = do
                t' <- f t
                (as,rs) <- liftM unzip $ mapMntvr (map (tvrInfo_u (Info.insert CasePattern)) vs)
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
        tvr <- procRules tvr
        nfo <- lift $ lamann e (tvrInfo tvr)
        nfo <- lift $ idann n nfo
        e' <- local (Map.insert n Nothing) $ f e
        return $ lam (tvr { tvrIdent =  0, tvrType =  t', tvrInfo =  Info.insert bnd nfo}) e'
    lp bnd lam tvr e = do
        nfo <- lift $ lamann e (tvrInfo tvr)
        (tv,r) <- ntvr  [] tvr { tvrInfo = nfo }
        e' <- local r $ f e
        return $ lam tv e'
    mapMntvr ts = f ts [] where
        f [] xs = return $ reverse xs
        f (t:ts) rs = do
            (t',r) <- ntvr vs t
            local r $ f ts ((t',r):rs)
        vs = [ tvrNum x | x <- ts ]
    -- ntvr :: Monad m => Info -> [Int] -> TVr -> ReaderT (Map.Map Int (Maybe E)) m (TVr, (Map.Map Int (Maybe E)) -> (Map.Map Int (Maybe E)))
    ntvr xs tvr@(TVr { tvrIdent = 0, tvrType =  t}) = do
        t' <- f t
        tvr <- procRules tvr
        nfo <- lift $ idann 0 (tvrInfo tvr)
        let nvr = (tvr { tvrType =  t', tvrInfo = nfo})
        return (nvr,id)
    ntvr xs tvr@(TVr {tvrIdent = i, tvrType =  t}) = do
        t' <- f t
        ss <- ask
        tvr <- procRules tvr
        nfo' <- lift $ idann i (tvrInfo tvr)
        let i' = mnv xs i ss
        let nvr = (tvr { tvrIdent =  i', tvrType =  t', tvrInfo =  nfo'})
        case i == i' of
            True -> return (nvr,Map.insert i (Just $ EVar nvr))
            False -> return (nvr,Map.insert i (Just $ EVar nvr) . Map.insert i' Nothing)

    procRules tvr = case Info.lookup (tvrInfo tvr) of
        Nothing -> return tvr
        Just r -> do
            r' <- mapABodies f r
            return tvr { tvrInfo = Info.insert r' (tvrInfo tvr) }

mnv xs i ss
    | i <= 0 || i `Map.member` ss = nv (Map.fromList [ (x,undefined) | x <- xs ] `mappend` ss)
    | otherwise = i


nv ss = v (2 * (Map.size ss + 1)) where
    v n | n `Map.member` ss = v (n + 2)
    v n = n

nv' ss = v (2 * (Map.size ss + 1)) where
    v n | (Just Nothing) <- Map.lookup n ss = v (n + 2)
    v n = n
