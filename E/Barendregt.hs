module E.Barendregt where

don't use

import E.E
import Control.Monad.State
import qualified IntMap as IM
import qualified IntSet as IS
import FreeVars

barendregt :: E -> E
barendregt = fst . barendregt'

-- ensure all bound variables have unique names.
barendregt' :: E -> (E,IS.IntSet)
barendregt' e = runState (s IM.empty e) (freeVars e) where
    s :: IM.IntMap Int -> E -> State IS.IntSet E
    s im (EVar (TVr (Just i) t)) = case IM.lookup i im of
        Just x -> s im t >>= \t -> return $ EVar (TVr (Just x) t)
        Nothing -> s im t >>= \t -> return $ EVar (TVr (Just i) t)
    s im (ELam tvr e) = lp im ELam tvr e
    s im (EPi tvr e)  = lp im EPi tvr e
    s im (ELetRec dl e) = get >>= \ss -> let 
        s' = s im'
        (ss', dl', im') = foldl f (ss,[], im) dl  
        rn (TVr j t,e) = do
            e' <- s im' e
            t' <- s im' t
            return (TVr j t',e')
        f  (ss,dl,im) (tvr@(TVr (Just i) _),e) |  i `IS.member` ss || i < 0  =  
                (IS.insert v ss,rn (tvr { tvrIdent = Just v },e):dl,IM.insert i v im) where
            v = nv ss
        f  (ss,dl,im) z@((TVr (Just i) _),_) = (IS.insert i ss,rn z:dl,im) 
        f _ _ = error "invalid ELetRec"
        in do
            put ss'
            dl'' <- sequence dl' 
            e' <- s' e
            return $ ELetRec (reverse dl'') e'
    s im (EAp a b) = liftM2 EAp (s im a) (s im b)
    s im (ELit l) = fmap ELit $ sLit im l
    s im (EError x e) = s im e >>= \e -> return (EError x e)
    s im (EPrim x es e) = mapM (s im) es >>= \es -> s im e >>= \e -> return (EPrim x es e)
    s im (ECase e alt) = s im e >>= \e -> sequence [ sPat im p >>= \p -> s im e >>= \e -> return (p,e) | (p,e) <- alt] >>= \as -> return (ECase e as) 
    s _ e = return e
    sLit im (LitCons x es e) = mapM (s im) es >>= \es -> s im e >>= \e -> return (LitCons x es e)
    sLit _ l = return l
    sPat im (PatLit l) = fmap PatLit $ sLit im l
    sPat _ p = return p
    nv ss = v (2 * (IS.size ss + 1)) where 
        v n | n `IS.member` ss = v (n + 2)
        v n = n
    lp im lam (TVr (Just i) t) e  = do 
        ss <- get
        let (v,im') = (if i `IS.member` ss || i < 0 then 
                (nv ss,(IM.insert i v im)) else (i,im))
        modify (IS.insert v)
        t' <- s im' t 
        let ntvr =  (TVr (Just v) t')
        fmap  (lam ntvr) (s im' e)
    lp im lam (TVr Nothing t) e = do
        t' <- s im t
        e' <- s im e
        return $ lam (TVr Nothing t') e'

{-
-- ensure all bound variables have unique names.
barendregt :: E -> E
barendregt e = evalState (s IM.empty e) (freeVSet e) where
    s :: IM.IntMap E -> E -> State IS.IntSet E
    s im (EVar (TVr (Just i) t)) = case IM.lookup i im of
        Just x -> return x
        Nothing -> s im t >>= \t -> return $ EVar (TVr (Just i) t)
    s im (ELam tvr e) = lp im ELam tvr e
    s im (EPi tvr e)  = lp im EPi tvr e
    s im (ELetRec dl e) = get >>= \ss -> let 
        s' = s im'
        (ss', dl', im') = foldl f (ss,[], im) dl  
        f  (ss,dl,im) ((TVr (Just i) t),e) |  i `IS.member` ss  =  
                (IS.insert v ss,(s' e >>= \e' -> return (ntvr,e')):dl,IM.insert i (EVar ntvr) im) where
            v = nv ss
            ntvr =  TVr (Just v) t  -- TODO fix
        f  (ss,dl,im) ((TVr (Just i) t),e) = 
                (IS.insert i ss,(s' e >>= \e' -> return (ntvr, e')):dl,IM.insert i (EVar ntvr) im) where
            ntvr = TVr (Just i) t  -- TODO fix
        f _ _ = error "invalid ELetRec"
        in do
            put ss'
            dl'' <- sequence dl' 
            e' <- s' e
            return $ ELetRec dl'' e'
    s im (EAp a b) = liftM2 EAp (s im a) (s im b)
    --s im ss (ELit (LitCons x es e)) = ELit (LitCons x (map (s im ss) es) (s im ss e))
    s im (ELit l) = fmap ELit $ sLit im l
    s im (EError x e) = s im e >>= \e -> return (EError x e)
    s im (EPrim x es e) = mapM (s im) es >>= \es -> s im e >>= \e -> return (EPrim x es e)
    s im (ECase e alt) = s im e >>= \e -> sequence [ sPat im p >>= \p -> s im e >>= \e -> return (p,e) | (p,e) <- alt] >>= \as -> return (ECase e as) 
    s _ e = return e
    sLit im (LitCons x es e) = mapM (s im) es >>= \es -> s im e >>= \e -> return (LitCons x es e)
    sLit _ l = return l
    sPat im (PatLit l) = fmap PatLit $ sLit im l
    sPat _ p = return p
    nv ss = v (2 * IS.size ss) where 
        v n | n `IS.member` ss = v (n + 2)
        v n = n
    lp im lam (TVr (Just i) t) e  = do 
        t' <- s im t 
        ss <- get
        case i `IS.member` ss of 
            True -> do
                let v = nv (ss `IS.union` freeVSet t')        
                let ntvr =  (TVr (Just v) t')
                modify (IS.insert v)
                fmap  (lam ntvr) (s (IM.insert i (EVar ntvr) im)  e)
            False -> do
                let ntvr =  (TVr (Just i) t')
                modify (IS.insert i)
                fmap  (lam ntvr) (s (IM.insert i (EVar ntvr) im)  e)
    lp im lam (TVr Nothing t) e = do
        t' <- s im t
        e' <- s im e
        return $ lam (TVr Nothing t') e'
        -}
