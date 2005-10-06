module E.Inline(inlineDecompose, basicDecompose, emapE, emapE',emapEG, app, emapE_, bindingFreeVars) where

import Control.Monad.Writer
import Data.FunctorM
import Data.Monoid

import Atom
import E.E
import E.Rules
import E.Subst
import E.Values
import FreeVars
import GenUtil
import Info.Info as Info
import Stats
import Util.Graph
import Util.HasSize



-- To decide whether to inline, we take a few things into account

bindingFreeVars t e = freeVars (tvrType t) `mappend` freeVars e `mappend` freeVars (Info.fetch (tvrInfo t) :: ARules)


baseInlinability e
    | isAtomic e = 5
    | whnfOrBot e = 4
    | otherwise = 0

basicDecompose ::
    Maybe [Int]  -- ^ Just a set of values not to prune or nothing to not prune at all.
    -> Rules     -- ^ Rules for pruning
    -> E             -- ^ body for pruning info
    -> [(TVr,E)]     -- ^ incoming bindings
    -> [Either (TVr,E) [(TVr,E)]]     -- ^ bindings pruned and ordered by inlinability value
basicDecompose prune rules body ds = ans where
    zs = [ ((t,e), tvrNum t, bindingFreeVars t e ) |  (t,e) <- ds ]
    cg zs =  newGraph zs (\ (_,x,_) -> x) ( \ (_,_,x) -> x)
    tg = cg zs
    scc' = scc tg
    scc'' = case prune of
        Nothing -> scc'
        Just s -> scc $ cg $ reachable tg (freeVars body ++ s )
    ans = mapScc f scc''
    f (v,_,_) = v
    mapScc f = map g where
        g (Left x) = Left (f x)
        g (Right xs) = Right (map f xs)


inlineDecompose ::
    Maybe [Int]  -- ^ Just a set of values not to prune or nothing to not prune at all.
    -> E             -- ^ body for pruning info
    -> [(TVr,E)]     -- ^ incoming bindings
    -> [(TVr,E)]     -- ^ bindings pruned and ordered by inlinability value
inlineDecompose prune body ds = ans where
    zs = [ ((t,e), tvrNum t, freeVars e, inlinability e) |  (t,e) <- ds ]
    cg zs =  newGraph zs (\ (_,x,_,_) -> x) ( \ (_,_,x,_) -> x)
    tg = cg zs
    scc' = scc tg
    scc'' = case prune of
        Nothing -> scc'
        Just s -> scc $ cg $ reachable tg (freeVars body ++ s )
    inlinability e = baseInlinability e - size (fst $ fromLam e)
    ans = f scc'' []
    f (Left (v,_,_,_):ds) xs = f ds (v:xs)
    f (Right ms:ds) xs = f (scc' ++ ds) xs where
        scc' = scc (cg [ (a,b,filter (/= i) c,d) | (a,b,c,d) <- ms])
        (_,i,_,_) = minimumUnder (\ (_,_,_,x) -> x) ms
    f [] xs = reverse xs

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


{-
inlineDecompose prune body ds = ans where
    zs = [ ((t,e), tvrNum t, freeVars e, inlinability e) |  (t,e) <- ds ]
    --tg = newGraph zs (\ (_,x,_,_) -> x) ( \ (_,_,x,_) -> x)
    scc = stronglyConnComp [ (x,a,b) | x@(_,a,b,_) <- zs ]
    inlinability e = baseInlinability e - size (fst $ fromLam e)
    ans = f scc []
    f (AcyclicSCC (v,_,_,_):ds) xs = f ds (v:xs)
    f (CyclicSCC ms:ds) xs = f (scc' ++ ds) xs where
        scc' = stronglyConnComp [ (x,a,filter (/= i) b) | x@(_,a,b,_) <- ms ]
        (_,i,_,_) = minimumUnder (\ (_,_,_,x) -> x) ms
    f [] xs = reverse xs

emapE f (EAp aa ab) = do aa <- f aa;ab <- f ab; return $ EAp aa ab
emapE f (ELam aa ab) = do aa <- mapmTvr f aa; ab <- f ab; return $ ELam aa ab
emapE f (EPi aa ab) = do aa <- mapmTvr f aa; ab <- f ab; return $ EPi aa ab
--emapE f (EVar aa) = do aa <- mapmTvr f aa; return $ EVar aa
emapE f (EVar aa) = do return $ EVar aa
emapE f (Unknown) = do return $ Unknown
emapE f (ESort aa) = do return $ ESort aa
emapE f (ELit aa) = do aa <- litSMapM f aa; return $ ELit aa
emapE f (ELetRec aa ab) = do aa <- mapM (\x -> do x <- (do (aa,ab) <- return x; aa <- mapmTvr f aa;ab <- f ab;return (aa,ab)); return x) aa;ab <- f ab; return $ ELetRec aa ab
emapE f (ECase e b as d) = do
    e' <- f e
    b' <- fmapM f b
    as' <- mapmAlt as
    d' <- fmapM f d
    return (ECase e' b' as' d')
--    aa ab) = do aa <- f aa;ab <- mapM (\(x,y) -> do x <- fmapM f x; y <- f y; return (x,y)) ab; return $ ECase aa ab
emapE f (EPrim aa ab ac) = do ab <- mapM f ab;ac <- f ac; return $ EPrim aa ab ac
emapE f (EError aa ab) = do ab <- f ab; return $ EError aa ab


-- do not traverse into types
emapE' f (EAp aa ab) = do aa <- f aa;ab <- f ab; return $ EAp aa ab
emapE' f (ELam aa ab) = do ab <- f ab; return $ ELam aa ab
emapE' f (EPi aa ab) = do aa <- mapmTvr f aa; ab <- f ab; return $ EPi aa ab
--emapE' f (EPi aa ab) = do  ab <- f ab; return $ EPi aa ab
emapE' f (EVar aa) = do return $ EVar aa
emapE' f (Unknown) = do return $ Unknown
emapE' f (ESort aa) = do return $ ESort aa
emapE' f (ELit (LitCons a es e)) = do es <- mapM f es;  return $ ELit (LitCons a es e)
emapE' f (ELit aa) = do aa <- fmapM f aa; return $ ELit aa
emapE' f (ELetRec aa ab) = do aa <- mapM (\x -> do x <- (do (aa,ab) <- return x; ab <- f ab;return (aa,ab)); return x) aa;ab <- f ab; return $ ELetRec aa ab
emapE' f (ECase e b as d) = do
    e' <- f e
    as' <- mapmAlt' as
    d' <- fmapM f d
    return (ECase e' b as' d')
--emapE' f (ECase aa ab) = do aa <- f aa;ab <- mapM (\(x,y) -> do x <- patFmap' f x; y <- f y; return (x,y)) ab; return $ ECase aa ab
emapE' f (EPrim aa ab ac) = do ab <- mapM f ab; return $ EPrim aa ab ac
emapE' f (EError aa ab) =  return $ EError aa ab

mapmTvr f (TVr x e) = f e >>= return . TVr x
mapmAlt f (Alt l e) = do
    e' <- f e
    l' <- litSMapM f l
    return (Alt l' e')
mapmAlt' f (Alt l e) = do
    e' <- f e
    return (Alt l e')


--patFmap' f PatWildCard = return PatWildCard
--patFmap' f (PatLit l) = litFmap' f l >>= return . PatLit
litFmap' f (LitCons a es e) = do es <- mapM f es; return $ (LitCons a es e)
litFmap' _ l = return l

-}

emapE_ :: Monad m => (E -> m a) -> E -> m ()
emapE_ f e = emapEG f' f' e >> return () where
    f' e = f e >> return e
emapE f = emapEG f f
emapE' f = emapEG f return

emapEG f g e = z e where
    z (EAp aa ab) = do aa <- f aa;ab <- f ab; return $ EAp aa ab
    z (ELam aa ab) = do aa <- mapmTvr g aa; ab <- f ab; return $ ELam aa ab
    z (EPi aa ab) = do aa <- mapmTvr f aa; ab <- f ab; return $ EPi aa ab
    z (EVar aa) = do aa <- mapmTvr f aa; return $ EVar aa
    z (Unknown) = do return $ Unknown
    z (ESort aa) = do return $ ESort aa
    z (ELit (LitCons n es t)) = do t' <- g t; es' <- mapM f es; return $ ELit (LitCons n es' t')
    z (ELit aa) = do aa <- fmapM g aa; return $ ELit aa
    z (ELetRec aa ab) = do aa <- mapM (\x -> do x <- (do (aa,ab) <- return x; aa <- mapmTvr g aa;ab <- f ab;return (aa,ab)); return x) aa;ab <- f ab; return $ ELetRec aa ab
    z (ECase e b as d) = do
        e' <- f e
        b' <- fmapM g b
        as' <- mapM mapmAlt as
        d' <- fmapM f d
        return (ECase e' b' as' d')
    --    aa ab) = do aa <- f aa;ab <- mapM (\(x,y) -> do x <- fmapM f x; y <- f y; return (x,y)) ab; return $ ECase aa ab
    z (EPrim aa ab ac) = do ab <- mapM f ab;ac <- f ac; return $ EPrim aa ab ac
    z (EError aa ab) = do ab <- f ab; return $ EError aa ab
    mapmTvr = fmapM
    mapmAlt (Alt (LitCons n xs t) e) = do
        e' <- f e
        xs' <- mapM (fmapM g) xs
        t' <- g t
        return $ Alt (LitCons n xs' t') e'
    mapmAlt (Alt l e) = do
        e' <- f e
        l' <- fmapM g l
        return (Alt l' e')


instance Monoid Int where
    mempty = 0
    mappend = (+)
    mconcat = sum

instance HasSize E where
    size = eSize

eSize :: E -> Int
eSize e = n where
    (_, n) = runWriter (f e)
    f e@ELit {} = tell 1 >> return e
    f e@EPrim {} = tell 1 >> return e
    f e@EError {} = tell 1 >> return e
    f e = tell ( 1) >> emapE' f e

