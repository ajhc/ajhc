module E.LetFloat(
    atomizeApps,
    coalesceLets,
    annotateBindings,
    doCoalesce,
    doLetRec,
    varElim,
    propRec,
    floatInward
  ) where


import Atom
import Control.Monad.Writer
import Data.Monoid
import DDataUtil()
import E.E
import E.Inline
import E.Traverse
import E.Values
import E.Rules
import FreeVars
import GenUtil
import GraphUtil
import List
import qualified GraphUtil as G
import qualified Data.Map as Map
import qualified Data.Set as Set
import SameShape
import Stats
import E.Subst(app)
import Control.Monad.Identity



doLetRec stats [] e = return e
doLetRec stats ds e = do
    return $ ELetRec ds e
    {-
    let fakeDs = (TVr (-1) undefined,e)
    let ds' = reachable (newGraph (fakeDs:ds) (tvrNum . fst) (freeVars . snd)) [-1]
    let ds'' = [ d | d@(t,_) <- ds', tvrNum t /= -1 ]
    liftIO $ ticks stats (length ds - length ds'') (toAtom "E.LetFloat.var-elimination")
    return $ ELetRec ds'' e
    -}

varElim :: Stats -> Int -> IO ()
varElim stats n = do
    ticks stats n (toAtom "E.Simplify.var-elimination")
propRec stats n = do
    ticks stats n (toAtom "E.Simplify.copy-propegate")

atomizeApps :: Stats -> E -> IO E
atomizeApps stats e = traverse travOptions { pruneRecord = varElim stats } f mempty mempty e where
    --f 0 (EPi (TVr Nothing t) b,[])  = do
    --    (t',ds1) <- at t
    --    (b',ds2) <- at b
    --    doLetRec stats
    f 0 (EPrim n xs t,[]) = do
        (xs',dss) <- fmap unzip (mapM at xs)
        doLetRec stats (concat dss) (EPrim n xs' t)
    f 0 (ELit (LitCons n xs t),[]) = do
        (xs',dss) <- fmap unzip (mapM at xs)
        doLetRec stats (concat dss) (ELit (LitCons n xs' t))
    f n (x,xs) | n > 0 ||  all (isAtomic) xs = return $ foldl EAp x xs
    f 0 (x,xs) = do
        (xs',dss) <- fmap unzip (mapM at xs)
        doLetRec stats (concat dss) (foldl EAp x xs')
    at e | not (isAtomic e) = do
        --lift $ putErrLn $ "Atomizing: " ++ render (ePretty e)
        e <- f 0 (e,[])
        lift $ tick stats (toAtom "E.LetFloat.atomizeApps")
        nb@(tvr,_) <- newBinding e
        return (EVar tvr,[nb])
    at e = return (e,[])

doCoalesce :: Stats -> (E,[E]) -> IO (E,[E])
doCoalesce stats (x,xs) = ans where
    ans = do
        (xs',dss) <- fmap unzip (mapM at xs)
        case x of
            ELetRec ds' (ELetRec ds'' x') -> do
                liftIO $ tick stats (toAtom "E.LetFloat.coalesce.fromLet")
                fromLet2 (concat $ ds'':ds':dss) (foldl EAp x' xs')
            ec@ECase { eCaseScrutinee = (ELetRec ds' x') }  -> do
                liftIO $ tick stats (toAtom "E.LetFloat.coalesce.fromCase")
                fromLet2 (concat $ ds':dss) (foldl EAp (ec { eCaseScrutinee = x' } ) xs')
            ELetRec ds' x' | not (null xs) -> do
                liftIO $ tick stats (toAtom "E.LetFloat.coalesce.fromAp")
                fromLet2 (concat $ ds':dss) (foldl EAp x' xs')
            ELetRec ds x' -> do
                fromLet2 (concat $ ds:dss) (foldl EAp x' xs')
            x -> fromLet2 (concat dss) (foldl EAp x xs')
    at (ELetRec ds e) = do
        liftIO $ tick stats (toAtom "E.LetFloat.coalesce.fromArg")
        return (e,ds)
    at e = return (e,[])
    --at' (t,(ELetRec ds e)) = do
    --    liftIO $ tick stats (toAtom "E.LetFloat.coalesce.fromLet2")
    --    return ((t,e),ds)
    at' e = return (e,[])
    fromLet2 ds e = do
        (ds',dss) <- fmap unzip (mapM at' ds)
        let ds'' = (concat $ ds':dss)
        r <- doLetRec stats ds''  e
        return $ fromAp r

fvBind (Left (_,fv)) = fv
fvBind (Right xs) = Set.unions (snds xs)


--    f (ELetRec ds e) xs = g (G.scc $  G.newGraph [ (d, freeVars (tvrType $ fst d) `mappend` freeVars' (fst d) (snd d))  | d <- ds ] (tvrNum . fst . fst) (Set.toList . snd) ) xs where


floatInward ::
    Rules -- ^ rules to augment free variables of definitons
    -> E  -- ^ input term
    -> E  -- ^ output term
floatInward rules e = f e [] where
    --freeVars' v e = freeVars e `mappend` ruleFreeVars rules v
    freeVars' v e = augment (freeVars (tvrType v) `mappend` freeVars e)
    augment fvs = mconcat (fvs:[ ruleFreeVars' rules x | x <- Set.toList fvs ])
    f (ECase e b as d) xs = letRec p' $ ECase (f e pe) b [ Alt l (f e pn) | Alt l e <- as | pn <- ps ] (fmap (flip f pd) d)  where
        (p',_:pe:pd:ps) = sepByDropPoint (mconcat [freeVars l | Alt l _ <- as ]:freeVars e: tvrNum b `Set.delete` freeVars d :[freeVars a | a <- as ]) xs
    f (ELetRec ds e) xs = g (G.scc $  G.newGraph [ (d,freeVars' x y) | d@(x,y) <- ds ] (tvrNum . fst . fst) (Set.toList . snd) ) xs where
        g [] p' = f e p'
        g ((Left ((v,ev),fv)):xs) p = g xs (p0 ++ [Left ((v,ev'),freeVars' v ev')] ++ p') where
            ev' = f ev pv
            (p',[p0,pv,_]) = sepByDropPoint [augment (frest xs), freeVars' v ev, freeVars (tvrType v)] p
        g (Right bs:xs) p =  g xs (p0 ++ [Right [ let ev' = f ev pv in ((v,ev'),freeVars' v ev') | ((v,ev),_) <- bs | pv <- ps ]] ++ p') where
            (p',_:p0:ps) = sepByDropPoint (freeVars (map (tvrType . fst . fst) bs) :augment (frest xs):snds bs) p
        frest xs = mconcat (freeVars e:map fvBind xs)
    f e xs |  not (null ls) = letRec unsafe_binds (foldr ELam (f b safe_binds) ls) where
        (unsafe_binds,safe_binds) =  sepDupableBinds (freeVars $ map tvrType ls) xs
        (b,ls) = fromLam e
    f e (Left ((v',ev),_):xs)
        | (EVar v,as) <- fromAp e, v == v', tvrNum v' `notElem` freeVars as  = f (runIdentity $ app (ev,as) {- foldl EAp ev as -} ) xs
    --    | otherwise = f (EVar v) xs
    --    | otherwise = error $ "floatInward: shouldn't happen:" <+>  tshow (EVar v) <+> tshow (v')
    f e xs = letRec xs e
    letRec [] e = e
    letRec xs e = f (G.scc $ G.newGraph (concatMap fromScc xs) (tvrNum . fst . fst) (Set.toList . snd)) where
        f [] = e
        f (Left (te,_):rs) = eLetRec [te] $ f rs
        f (Right ds:rs) = eLetRec (fsts ds) $ f rs
    --letRec p e = eLetRec  (concatMap (map fst . fromScc) p) e
    --letRec p e = foldr eLetRec e (reverse $ map (map fst . fromScc) p)

type FVarSet = Set.Set Int
type Binds = [Either ((TVr,E),FVarSet) [((TVr,E),FVarSet)]]


sepDupableBinds fvs xs = partition ind xs where
    g = G.reachable (G.newGraph (concatMap fromScc xs) (tvrNum . fst . fst) (Set.toList . snd)) (fvs `mappend` (map (tvrNum . fst . fst) $ concatMap fromScc unsafe_ones))
    uso = map (tvrNum . fst . fst) g
    (_,unsafe_ones) = partition std xs
    std (Left ((_,e),_)) = safeToDup e
    std (Right zs) = all safeToDup (snds $ fsts zs)
    ind x = any ( (`elem` uso) . tvrNum . fst . fst ) (fromScc x)

sameLength [] [] = True
sameLength (_:xs) (_:ys) = sameLength xs ys
sameLength _ _ = False

sepByDropPoint ::
    [FVarSet]    -- ^ list of possible drop points
    -> Binds     -- ^ list of bindings and their free variables
    -> (Binds,[Binds])  -- ^ bindings seperated into those which must be dropped outside of all drop points, and those which can be floated inward into each branch
sepByDropPoint ds [] = ([], [ [] | _ <- ds ])
sepByDropPoint ds fs' | sameShape1 xs ds && sum (length r:map length xs) == length fs' = (r,xs) where
    --fs =  ( G.scc $  G.newGraph (concatMap fromScc fs')  (tvrNum . fst . fst) (Set.toList . snd) )
    (r,xs) = f fs'
    f [] = ([], [ [] | _ <- ds ])
    f (b:bs)
        --  | nu == 0 = f bs -- not (any (`Set.member` allSet) (fvDecls b)) = f bs
        | nu == 1 =   case sepByDropPoint [ if v then d `mappend` fb' else d | (d,v) <- ds'  ] bs of
            (gb,ds'') | sameShape1 ds' ds'' -> (gb, [ if v then b:d else d | d <- ds'' | (_,v) <- ds' ])
        | otherwise = case sepByDropPoint [ d `mappend` fb' | d <- ds  ] bs of
            (gb,ds'') | sameShape1 ds'' ds -> (b:gb,ds'')
      where
        fb' = fvBind b
        fb = Set.toList $ fb'
        ds' = [ (d,any  (`Set.member` d) (fvDecls b)) | d <- ds ]
        nu = length (filter snd ds')
    fvDecls (Left ((t,_),_)) = [tvrNum t]
    fvDecls (Right ts) = [tvrNum t | ((t,_),_) <- ts ]
    comb (a,b) (c,d) = (a ++ c, zipWith (++) b d)

floatOutward :: Map.Map Int Int -> E -> (E,[(TVr,E)])
floatOutward bmap e = (e,[])

-- Beautiful use of lazyness.
annotateBindings :: Map.Map TVr Int -> E -> Map.Map TVr Int
annotateBindings min e = ans where
    ans = min `mappend` execWriter (f 0 e)
    f :: Int -> E -> Writer (Map.Map TVr Int) ()
    f n ec@ECase {} = do
        tell (Map.fromList [ (i,n) | i <- caseBinds ec ])
        emapE_ (f n) ec
    f n (ELetRec ds b) = do
        let ds' = [ (t,freeVars e) | (t,e) <- ds]
            scc = G.scc (G.newGraph ds' (tvrNum . fst) snd)
            g (Left (t,fv)) = tell (Map.singleton t (maximum $ 0:[Map.findWithDefault 0 (tVr v Unknown) ans | v <- fv]))
            g (Right ts) = do
                let ln = maximum [Map.findWithDefault 0 (tVr v Unknown) ans | v <- (snub $ concat (snds ts))  List.\\ [ i | (TVr { tvrIdent = i },_) <- ts ] ]
                tell (Map.fromList [ (t,ln) | (t,_) <- ts])
        mapM_ g scc
        mapM_ (f n) (snds ds)
        f n b
    f n e | (b,ls@(_:_)) <- fromPi e = do   -- not really necessary
        tell (Map.fromList [ (i,n + 1) | i  <- ls ])
        f (n + 1) b
    f n e | (b,ls@(_:_)) <- fromLam e = do
        tell (Map.fromList [ (i,n + 1) | i  <- ls ])
        f (n + 1) b
    f n e = emapE_ (f n) e


coalesceLets :: Stats -> E -> IO E
coalesceLets stats e = traverse travOptions { pruneRecord = varElim stats } f mempty mempty e where
    f n (x,xs) = do
        (x',xs') <- lift $ doCoalesce stats (x,xs)
        return $ foldl EAp x' xs'
    {-
    f n (x,xs) = do
        (xs',dss) <- fmap unzip (mapM at xs)
        case x of
            ECase (ELetRec ds' (ELetRec ds'' x')) as -> do
                lift $ tick stats (toAtom "LetFloat.coalesce.fromLet")
                fromLet2 (concat $ ds'':ds':dss) (foldl EAp x' xs')
            ECase (ELetRec ds' x') as -> do
                lift $ tick stats (toAtom "LetFloat.coalesce.fromCase")
                fromLet2 (concat $ ds':dss) (foldl EAp (ECase x' as) xs')
            ELetRec ds' x' | not (null xs) -> do
                lift $ tick stats (toAtom "LetFloat.coalesce.fromAp")
                fromLet2 (concat $ ds':dss) (foldl EAp x' xs')
            x -> fromLet2 (concat dss) (foldl EAp x xs')
    at (ELetRec ds e) = do
        lift $ tick stats (toAtom "LetFloat.coalesce.fromArg")
        return (e,ds)
    at e = return (e,[])
    at' (t,(ELetRec ds e)) = do
        lift $ tick stats (toAtom "LetFloat.coalesce.fromLet2")
        return ((t,e),ds)
    at' e = return (e,[])
    fromLet2 ds e = do
        (ds',dss) <- fmap unzip (mapM at' ds)
        doLetRec stats (concat $ ds':dss) e

    -}

letFloat :: Stats -> String -> E -> IO (E,[(TVr,E)])
letFloat stats s e = do
    return (e,[])

--notAtomic e | Just _ <- fullyConst e = False
--notAtomic e | sortTypeLike e = False
--notAtomic e = not $ isAtomic e

--notAtomic (ELetRec _ e) = notAtomic e
----notAtomic x | sortTypeLike x  = False
--notAtomic ECase {} = True
--notAtomic EAp {} = True
--notAtomic ELam {} = True
--notAtomic EPrim {} = True
--notAtomic e | Just _ <- fullyConst e = False
--notAtomic (ELit (LitCons n (_:_) _)) = True
--notAtomic EPi {} = True
--notAtomic _ = False

