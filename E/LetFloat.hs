module E.LetFloat(
--    atomizeApps,
    atomizeAp,
    coalesceLets,
    annotateBindings,
    floatInward
  ) where

import Control.Monad.Identity
import Control.Monad.Writer
import Data.Monoid
import List
import qualified Data.Map as Map
import qualified Data.Set as Set

import Atom
import DataConstructors
import Doc.PPrint
import E.E
import E.Inline
import E.Rules
import E.Traverse
import E.Values
import GenUtil
import Name.Name
import Options
import qualified CharIO as C
import qualified Util.Graph as G
import Stats
import Support.CanType
import Support.FreeVars
import Util.UniqueMonad



doLetRec stats [] e = return e
doLetRec stats ds _ | flint && hasRepeatUnder fst ds = error "doLetRec: repeated variables!"
doLetRec stats ds e = return $ ELetRec ds e

varElim :: Stats -> Int -> IO ()
varElim stats n = do
    ticks stats n (toAtom "E.Simplify.var-elimination")

atomizeApps :: Set.Set Id -> Stats -> E -> IO E
atomizeApps usedIds stats e = liftM fst $ traverse travOptions { pruneRecord = varElim stats } f mempty (Map.fromAscList [ (i,NotKnown) | i <- Set.toAscList usedIds ]) e where
    f 0 (ep@(EPi tvr@TVr {tvrIdent = i, tvrType = t} b),[]) | i == 0 || i `notElem` freeVars b = do
        (t',ds1) <- at t
        (b',ds2) <- at b
        liftIO $ C.putStrLn $ "atomizeApps: " ++ pprint ep
        doLetRec stats (ds1 ++ ds2) (EPi tvr { tvrIdent = 0, tvrType = t'} b')
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
    f _ _ = error "LetFloat: odd f"
    at (ELetRec ds e) = do
        (x,xs) <- at e
        return (x,ds ++ xs)
    at e | not (isAtomic e) = do
        liftIO $ C.putStrLn $ "at: " ++ pprint e
        e <- f 0 (e,[])
        lift $ tick stats (toAtom "E.LetFloat.atomizeApps")
        case e of
            ELetRec ds e -> do
                nb@(tvr,_) <- newBinding e
                return (EVar tvr,nb:ds)
            e -> do
                nb@(tvr,_) <- newBinding e
                return (EVar tvr,[nb])

    at e = return (e,[])

atomizeAp :: DataTable -> Stats -> E -> IO E
atomizeAp dataTable stats e = f e  where
    f :: E -> IO E
    f e = do
        (x,ds) <- g e
        ds' <- sequence [  f y >>= return . (,) x | (x,y) <- ds ]
        doLetRec stats ds' x
    g,h :: E -> IO (E,[(TVr,E)])
    g (ELetRec ds e) = do
        e' <- f e
        return (e',ds)
    g (ELam tvr e) = do
        e' <- f e
        return (ELam tvr e',[])
    g (ELit (LitCons n xs t)) = do
        (xs',dss) <- fmap unzip (mapM h xs)
        return (ELit (LitCons n xs' t), concat dss)
    g e@ELit {} = return (e,[])
    g e@EError {} = return (e,[])
    g ep@(EPi tvr@TVr {tvrIdent = i, tvrType = t} b) | i == 0 || i `notElem` freeVars b  = do
        ([t',b'],dss) <- fmap unzip (mapM h [t,b])
        return (EPi tvr { tvrIdent = 0, tvrType = t' } b', concat dss)
    g (EPrim n xs t) = do
        (xs',dss) <- fmap unzip (mapM h xs)
        return (EPrim n xs' t, concat dss)
    g ec@ECase { eCaseScrutinee = e } = do
        ec' <- caseBodiesMapM f ec
        e' <- f e
        return (ec' { eCaseScrutinee = e' },[])
    g e = case fromAp e of
        (EVar x,xs) -> do
            (xs',dss) <- fmap unzip (mapM h xs)
            return (foldl EAp (EVar x) xs', concat dss)
        (x,xs@(_:_)) -> do
            (x',ds) <- g x
            (xs',dss) <- fmap unzip (mapM h xs)
            return (foldl EAp x' xs', concat (ds:dss))
    h e | isAtomic e = return (e,[])
    h (ELetRec ds e) = do
        (e',ds') <- h e
        return (e',ds' ++ ds)
    h e = do
        tick stats (toAtom "E.LetFloat.atomizeAp")
        u <- newUniq
        let n = toName Val ("A@",'v':show u)
            tv = tvr { tvrIdent = toId n, tvrType = infertype dataTable e }
        C.putStrLn $ show n ++ " = " ++ pprint e
        return (EVar tv,[(tv,e)])

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



floatInward ::
    Rules -- ^ rules to augment free variables of definitons
    -> E  -- ^ input term
    -> E  -- ^ output term
floatInward rules e = f e [] where
    augment fvs = fvs
    f ec@ECase { eCaseScrutinee = e, eCaseBind = b, eCaseAlts = as, eCaseDefault =  d } xs = letRec p' $ ec { eCaseScrutinee = (f e pe), eCaseAlts = [ Alt l (f e pn) | Alt l e <- as | pn <- ps ], eCaseDefault = (fmap (flip f pd) d)}  where
        (p',_:pe:pd:ps) = sepByDropPoint (mconcat [freeVars l | Alt l _ <- as ]:freeVars e: tvrNum b `Set.delete` freeVars d :[freeVars a | a <- as ]) xs
    f (ELetRec ds e) xs = g (G.scc $  G.newGraph [ (d,bindingFreeVars x y) | d@(x,y) <- ds ] (tvrNum . fst . fst) (Set.toList . snd) ) xs where
        g [] p' = f e p'
        g ((Left ((v,ev),fv)):xs) p = g xs (p0 ++ [Left ((v,ev'),bindingFreeVars v ev')] ++ p') where
            ev' = f ev pv
            (p',[p0,pv,_]) = sepByDropPoint [augment (frest xs), bindingFreeVars v ev, freeVars (tvrType v)] p
        g (Right bs:xs) p =  g xs (p0 ++ [Right [ let ev' = f ev pv in ((v,ev'),bindingFreeVars v ev') | ((v,ev),_) <- bs | pv <- ps ]] ++ p') where
            (p',_:p0:ps) = sepByDropPoint (freeVars (map (tvrType . fst . fst) bs) :augment (frest xs):snds bs) p
        frest xs = mconcat (freeVars e:map fvBind xs)
    f e xs |  (b,ls@(_:_)) <- fromLam e = letRec xs (foldr ELam (f b []) ls)
    f e (Left ((v',ev),_):xs)
        | (EVar v,as) <- fromAp e, v == v', not (tvrNum v' `Set.member` freeVars as)  = f (runIdentity $ app (ev,as) {- foldl EAp ev as -} ) xs
    f e xs = letRec xs e
    letRec [] e = e
    letRec xs e = f (G.scc $ G.newGraph (concatMap G.fromScc xs) (tvrNum . fst . fst) (Set.toList . snd)) where
        f [] = e
        f (Left (te,_):rs) = eLetRec [te] $ f rs
        f (Right ds:rs) = eLetRec (fsts ds) $ f rs

type FVarSet = Set.Set Int
type Binds = [Either ((TVr,E),FVarSet) [((TVr,E),FVarSet)]]
--type Binds = [(FVarSet,Either (TVr,E) [(TVr,E)])]


{-
sepDupableBinds fvs xs = partition ind xs where
    g = G.reachable (G.newGraph (concatMap G.fromScc xs) (tvrNum . fst . fst) (Set.toList . snd)) (fvs `mappend` (map (tvrNum . fst . fst) $ concatMap G.fromScc unsafe_ones))
    uso = map (tvrNum . fst . fst) g
    (_,unsafe_ones) = partition std xs
    std (Left ((_,e),_)) = safeToDup e
    std (Right zs) = all safeToDup (snds $ fsts zs)
    ind x = any ( (`elem` uso) . tvrNum . fst . fst ) (G.fromScc x)
-}

-- | seperate bindings based on whether they can be floated inward

sepByDropPoint ::
    [FVarSet]           -- ^ list of possible drop points
    -> Binds            -- ^ list of bindings and their free variables
    -> (Binds,[Binds])  -- ^ bindings seperated into those which must be dropped outside of all drop points, and those which can be floated inward into each branch
sepByDropPoint ds [] = ([], [ [] | _ <- ds ])
--sepByDropPoint ds fs' | sameShape1 xs ds && sum (length r:map length xs) <= length fs' = (r,xs) where
sepByDropPoint ds fs' = (r,xs) where
    (r,xs) = f fs'
    f [] = ([], [ [] | _ <- ds ])
    f (b:bs)
        | nu == 0 = f bs
        | nu == 1 =   case sepByDropPoint [ if v then d `mappend` fb' else d | (d,v) <- ds'  ] bs of
            (gb,ds'')  -> (gb, [ if v then b:d else d | d <- ds'' | (_,v) <- ds' ])
            -- (gb,ds'') | sameShape1 ds' ds'' -> (gb, [ if v then b:d else d | d <- ds'' | (_,v) <- ds' ])
        | otherwise = case sepByDropPoint [ d `mappend` fb' | d <- ds  ] bs of
            (gb,ds'')  -> (b:gb,ds'')
            --(gb,ds'') | sameShape1 ds'' ds -> (b:gb,ds'')
      where
        fb' = fvBind b
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
                let ln = maximum $ 0:[Map.findWithDefault 0 (tVr v Unknown) ans | v <- (snub $ concat (snds ts))  List.\\ [ i | (TVr { tvrIdent = i },_) <- ts ] ]
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
coalesceLets stats e = liftM fst $ traverse travOptions { pruneRecord = varElim stats } f mempty mempty e where
    f n (x,xs) = do
        (x',xs') <- lift $ doCoalesce stats (x,xs)
        return $ foldl EAp x' xs'



