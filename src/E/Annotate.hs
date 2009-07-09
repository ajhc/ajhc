module E.Annotate where

import Control.Monad.Reader
import Data.Monoid
import qualified Data.Traversable as T

import E.E
import E.Program
import E.Rules
import E.Subst
import GenUtil
import Name.Id
import qualified Info.Info as Info
import Info.Info(Info)
import Util.SetLike
import Util.HasSize

annotateCombs :: forall m . Monad m =>
    (IdMap (Maybe E))
    -> (Id -> Info -> m Info)   -- ^ annotate based on Id map
    -> (E -> Info -> m Info)    -- ^ annotate letbound bindings
    -> (E -> Info -> m Info)    -- ^ annotate lambdabound bindings
    -> [Comb]                   -- ^ terms to annotate
    -> m [Comb]

annotateCombs imap idann letann lamann cs = do
    cs <- forM cs $ \comb -> do
        nfo <- letann (combBody comb) (tvrInfo $ combHead comb)
        nt <- annotate imap idann letann lamann (tvrType  $ combHead comb)
        return $ combHead_u (tvrInfo_s nfo . tvrType_s nt) comb
    let nimap = fromList [ (combIdent c, Just . EVar $ combHead c) | c <- cs ] `mappend` imap
        f :: (IdMap (Maybe E)) -> E -> m E
        f ni e = annotate ni idann letann lamann e
    let mrule :: Rule -> m Rule
        mrule r = do
            let g tvr = do
                nfo <- idann (tvrIdent tvr) (tvrInfo tvr)
                let ntvr = tvr { tvrInfo = nfo }
                return (ntvr,minsert (tvrIdent tvr) (Just $ EVar ntvr))
            bs <- mapM g $ ruleBinds r
            let nnimap = (foldr (.) id $ snds bs) nimap :: IdMap (Maybe E)
            args <- mapM (f nnimap) (ruleArgs r)
            body <- (f nnimap) (ruleBody r)
            return r { ruleBinds = fsts bs, ruleBody = body, ruleArgs = args }
    forM cs $ \comb -> do
        rs <- mapM mrule (combRules comb)
        nb <- f nimap (combBody comb)
        return . combRules_s rs . combBody_s nb $ comb

annotateDs :: Monad m =>
    (IdMap (Maybe E))
    -> (Id -> Info -> m Info)  -- ^ annotate based on Id map
    -> (E -> Info -> m Info)   -- ^ annotate letbound bindings
    -> (E -> Info -> m Info)   -- ^ annotate lambdabound bindings
    -> [(TVr,E)]               -- ^ terms to annotate
    -> m [(TVr,E)]

annotateDs imap idann letann lamann ds = do
    ELetRec { eDefs = ds', eBody = Unknown } <- annotate imap idann letann lamann (ELetRec ds Unknown)
    return ds'

annotateProgram :: Monad m =>
    (IdMap (Maybe E))
    -> (Id -> Info -> m Info)   -- ^ annotate based on Id map
    -> (E -> Info -> m Info)    -- ^ annotate letbound bindings
    -> (E -> Info -> m Info)    -- ^ annotate lambdabound bindings
    -> Program                  -- ^ terms to annotate
    -> m Program
annotateProgram imap idann letann lamann prog = do
    ds <- annotateCombs imap idann letann lamann (progCombinators prog)
    return $ programUpdate $ prog { progCombinators = ds }


type AM m = ReaderT (IdMap (Maybe E)) m

annotate :: Monad m =>
    (IdMap (Maybe E))
    -> (Id -> Info -> m Info)   -- ^ annotate based on Id map
    -> (E -> Info -> m Info)    -- ^ annotate letbound bindings
    -> (E -> Info -> m Info)    -- ^ annotate lambdabound bindings
    ->  E                       -- ^ term to annotate
    -> m E
annotate imap idann letann lamann e = runReaderT (f e) imap where
    f eo@(EVar tvr@(TVr { tvrIdent = i, tvrType =  t })) = do
        mp <- ask
        case mlookup i mp of
          Just (Just v) -> return v
          _  -> return eo
    f (ELam tvr e) = lp ELam tvr e
    f (EPi tvr e) = lp EPi tvr e
    f (EAp a b) = liftM2 EAp (f a) (f b)
    f (EError x e) = liftM (EError x) (f e)
    f (EPrim x es e) = liftM2 (EPrim x) (mapM f es) (f e)
    f ELetRec { eDefs = dl, eBody = e } = do
        dl' <- flip mapM dl $ \ (t,e) -> do
            nfo <- lift $ letann e (tvrInfo t)
            return t { tvrInfo = nfo }
        (as,rs) <- liftM unzip $ mapMntvr dl'
        local (foldr (.) id rs) $ do
            ds <- mapM f (snds dl)
            e' <- f e
            return $ ELetRec (zip as ds) e'
    f (ELit l) = liftM ELit $ litSMapM f l
    f Unknown = return Unknown
    f e@(ESort {}) = return e
    f ec@(ECase {}) = do
        e' <- f $ eCaseScrutinee ec
        let caseBind = eCaseBind ec
        (b',r) <- ntvr [] caseBind
        d <- local r $ T.mapM f $ eCaseDefault ec
        let da (Alt lc@LitCons { litName = s, litArgs = vs, litType = t } e) = do
                t' <- f t
                (as,rs) <- liftM unzip $ mapMntvr vs
                e' <- local (foldr (.) id rs) $ f e
                return $ Alt lc { litArgs = as, litType = t' } e'
            da (Alt l e) = do
                l' <- T.mapM f l
                e' <- f e
                return $ Alt l' e'
        alts <- local r (mapM da $ eCaseAlts ec)
        t' <- f (eCaseType ec)
        return $ caseUpdate ECase { eCaseAllFV = error "no eCaseAllFV needed",  eCaseScrutinee = e', eCaseType = t', eCaseDefault = d, eCaseBind = b', eCaseAlts = alts }
    lp lam tvr@(TVr { tvrIdent = n, tvrType = t}) e | n == emptyId  = do
        t' <- f t
        nfo <- lift $ lamann e (tvrInfo tvr)
        nfo <- lift $ idann n nfo
        e' <- local (minsert n Nothing) $ f e
        return $ lam (tvr { tvrIdent = emptyId, tvrType =  t', tvrInfo =  nfo}) e'
    lp lam tvr e = do
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
    ntvr xs tvr@(TVr { tvrIdent = n, tvrType =  t}) | n == emptyId = do
        t' <- f t
        nfo <- lift $ idann emptyId (tvrInfo tvr)
        let nvr = (tvr { tvrType =  t', tvrInfo = nfo})
        return (nvr,id)
    ntvr xs tvr@(TVr {tvrIdent = i, tvrType =  t}) = do
        t' <- f t
        ss <- ask
        nfo' <- lift $ idann i (tvrInfo tvr)
        let i' = mnv xs i ss
        let nvr = (tvr { tvrIdent =  i', tvrType =  t', tvrInfo =  nfo'})
        case i == i' of
            True -> return (nvr,minsert i (Just $ EVar nvr))
            False -> return (nvr,minsert i (Just $ EVar nvr) . minsert i' Nothing)

mnv xs i ss
    | isInvalidId i || i `mmember` ss  = newId (size ss) isOkay
    | otherwise = i
    where isOkay i = (i `mnotMember` ss) && (i `notElem` xs)


