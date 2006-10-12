module E.Annotate where

import Control.Monad.Reader
import Data.FunctorM
import Data.Monoid

import E.E
import E.Program
import E.Rules
import E.Subst
import GenUtil
import Info.Types
import Name.Id
import qualified Info.Info as Info
import Info.Info(Info)
import Util.SetLike
import Util.HasSize

annotateDs :: Monad m =>
    (IdMap (Maybe E))
    -> (Id -> Info -> m Info)   -- ^ annotate based on Id map
    -> (E -> Info -> m Info) -- ^ annotate letbound bindings
    -> (E -> Info -> m Info) -- ^ annotate lambdabound bindings
    -> [(TVr,E)]            -- ^ terms to annotate
    -> m [(TVr,E)]

annotateDs imap idann letann lamann ds = do
    ELetRec ds' Unknown <- annotate imap idann letann lamann (ELetRec ds Unknown)
    return ds'

annotateProgram :: Monad m =>
    (IdMap (Maybe E))
    -> (Id -> Info -> m Info)   -- ^ annotate based on Id map
    -> (E -> Info -> m Info)    -- ^ annotate letbound bindings
    -> (E -> Info -> m Info)    -- ^ annotate lambdabound bindings
    -> Program                -- ^ terms to annotate
    -> m Program
annotateProgram imap idann letann lamann prog = do
    ds <- annotateDs imap idann letann lamann (programDs prog)
    return $ programSetDs ds prog


annotate :: Monad m =>
    (IdMap (Maybe E))
    -> (Id -> Info -> m Info)   -- ^ annotate based on Id map
    -> (E -> Info -> m Info) -- ^ annotate letbound bindings
    -> (E -> Info -> m Info) -- ^ annotate lambdabound bindings
    ->  E            -- ^ term to annotate
    -> m E
annotate imap idann letann lamann e = runReaderT (f e) imap where
    f eo@(EVar tvr@(TVr { tvrIdent = i, tvrType =  t })) = do
        mp <- ask
        case mlookup i mp of
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
            as <- mapM procRules as
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
        let da (Alt lc@LitCons { litName = s, litArgs = vs, litType = t } e) = do
                t' <- f t
                (as,rs) <- liftM unzip $ mapMntvr (map (tvrInfo_u (Info.insert CasePattern)) vs)
                e' <- local (mconcat rs) $ f e
                return $ Alt lc { litArgs = as, litType = t' } e'
            da (Alt l e) = do
                l' <- fmapM f l
                e' <- f e
                return $ Alt l' e'
        alts <- local r (mapM da $ eCaseAlts ec)
        t' <- f (eCaseType ec)
        return  ECase { eCaseScrutinee = e', eCaseType = t', eCaseDefault = d, eCaseBind = b', eCaseAlts = alts }
    lp bnd lam tvr@(TVr { tvrIdent = n, tvrType = t}) e | n == 0  = do
        t' <- f t
        tvr <- procRules tvr
        nfo <- lift $ lamann e (tvrInfo tvr)
        nfo <- lift $ idann n nfo
        e' <- local (minsert n Nothing) $ f e
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
        vs = [ tvrIdent x | x <- ts ]
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
            True -> return (nvr,minsert i (Just $ EVar nvr))
            False -> return (nvr,minsert i (Just $ EVar nvr) . minsert i' Nothing)
    mrule r = do
        let g tvr = do
            nfo <- lift $ idann (tvrIdent tvr) (tvrInfo tvr)
            return (tvr { tvrInfo = nfo },minsert (tvrIdent tvr) (Just $ EVar tvr))
        bs <- mapM g $ ruleBinds r
        local (mconcat $ snds bs) $ do
            args <- mapM f (ruleArgs r)
            body <- f (ruleBody r)
            return r { ruleBinds = fsts bs, ruleBody = body, ruleArgs = args }
    procRules tvr = case Info.lookup (tvrInfo tvr) of
        Nothing -> return tvr
        Just r -> do
            r' <- mapRules mrule r
            return tvr { tvrInfo = Info.insert r' (tvrInfo tvr) }

mnv xs i ss
    | i <= 0 || i `mmember` ss = nv (fromList [ (x,undefined) | x <- xs ] `mappend` ss)
    | otherwise = i


nv ss = v (2 * (size ss + 1)) where
    v n | n `mmember` ss = v (n + 2)
    v n = n

nv' ss = v (2 * (size ss + 1)) where
    v n | (Just Nothing) <- mlookup n ss = v (n + 2)
    v n = n
