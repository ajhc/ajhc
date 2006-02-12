module E.SSimplify(Occurance(..), simplifyE, simplifyDs, SimplifyOpts(..)) where

import Control.Monad.Identity
import Control.Monad.Writer
import Data.FunctorM
import Data.Generics
import Data.Monoid
import List
import Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set

import Atom
import Support.CanType
import DataConstructors
import E.Annotate
import C.Prims
import E.E
import E.Inline
import E.PrimOpt
import E.Rules
import E.Subst
import E.TypeCheck
import E.Values
import Support.FreeVars
import GenUtil
import Info.Types
import Name.Name
import Util.NameMonad
import Options
import qualified E.Strictness as Strict
import qualified FlagOpts as FO
import qualified Info.Info as Info
import qualified Util.Seq as Seq
import Stats hiding(new,print,Stats)
import Util.Graph
import Name.VConsts

data Occurance =
    Unused        -- ^ unused means a var is not used at the term level, but might be at the type level
    | Once        -- ^ Used at most once not inside a lambda or as an argument
    | OnceInLam   -- ^ used once inside a lambda
    | ManyBranch  -- ^ used once in several branches
    | Many        -- ^ used many or an unknown number of times
    | LoopBreaker -- ^ chosen as a loopbreaker, never inline
    deriving(Show,Eq,Ord,Typeable)



combineOccInfo k a b | a == b = a
combineOccInfo k a b =  error $ "Conflicting occurance info: " ++ show (k,a,b)

data StrictInfo = NoStrict | Strict
    deriving(Typeable,Show)

-- | This collects occurance info for variables, deletes dead expressions, and reorders let-bound variables in dependency order.
collectOcc :: SimplifyOpts ->  E -> (E,Set.Set Int,Map.Map TVr Occurance)
collectOcc sopts  e = (e',fvs,occ) where
    topLevels = so_exports sopts
    rules  = so_rules sopts
    dataTable = so_dataTable sopts
    ((e',fvs,_),occ') = runWriter $ f e
    rule_set = ruleAllFreeVars rules
    occ = foldl (Map.unionWithKey combineOccInfo) mempty (Seq.toList occ')
    f e@(EPi (TVr { tvrIdent = 0, tvrType =  a}) b) = return (e,(freeVars [a,b]),(args [a,b]))
    f e@(EPi tvr@(TVr { tvrIdent = n, tvrType =  a}) b) = if n `Set.member` fvs || n `Map.member` ags then return (e,Set.delete n fvs ,Map.delete n ags) else return (EPi (tvr { tvrIdent =  0 } ) b,fvs,ags)  where
        fvs = (freeVars [a,b])
        ags = args [a,b]
    f e@(ELit (LitCons n as t)) = return (e,freeVars (t:as),args as)
    f e@ELit {} = return (e,freeVars e,mempty)
    f e@(EPrim _ as t) = return (e,freeVars (t:as),args as)
    f e@(EError _ t) =  return (e,freeVars t,mempty)
    f e@ELam {} | (b,as) <- fromLam e = do
        (b',fvs,bs) <- f b
        return (foldr ELam b' as,foldr Set.delete  (freeVars (map tvrType as) `mappend` fvs) (map tvrNum as), Map.map inLam $ foldr Map.delete bs (map tvrNum as))
    f e | Just (x,t) <- from_unsafeCoerce e  = do (a,b,c) <- f x ; return (prim_unsafeCoerce a t, b `mappend` freeVars t, c)
    f e | (EVar (TVr { tvrIdent = n, tvrType =  t}),xs) <- fromAp e = do
        return (e,freeVars (t:xs), Map.singleton n Once `andOM` args xs)
    f ec@(ECase e b as d) = do
        (e',fva,sa) <- f e
        (d',fvb,sb) <- case d of
            Nothing -> return (Nothing,mempty,mempty)
            Just e -> do (a,b,c) <- f e; return (Just a,b,c)
        (as',fvas,ass) <- mapAndUnzip3M alt as
        let fvs = mconcat $ [fva,freeVars $ tvrType b, fvb] ++ fvas
        return (ECase e' b as' d', fvs, sa `andOM` orMaps (sb:ass) )
    f (ELetRec ds e) = do
        ds' <- mapM  (censor (const mempty) . listen . f . snd) ds
        (e',fve,se) <- f e
        let gfv (_,fv,i) = fvs ++ Set.toList (mconcat (map (ruleFreeVars' rules) (fvs)))  where
                fvs = Set.toList (Map.keysSet i `Set.union` fv)
            gr = newGraph (zip (fsts ds) ds') (tvrNum . fst) (gfv . fst . snd )
            nn' = reachable gr (Set.toList fve ++ Map.keys se ++  topLevels)
        nn <- sequence [ tell t >> return (x,y) |  (x,(y,t)) <- nn' ]
        let gr' = newGraph nn (tvrNum . fst) (gfv . snd )
            (lb,ds'') = findLoopBreakers (\ (t,(e,_,_)) -> loopFunc t e) (const True) gr'
            cycNodes = Set.fromList $ [ v | (v,_) <- cyclicNodes gr']
            calcStrictInfo t _
                | t `Set.member` cycNodes = setProperty prop_CYCLIC
                | otherwise = id
        let dvars = map (tvrNum . fst) ds
            fvs = foldr Set.delete (mconcat (fve:[ fv `mappend` freeVars t | (TVr { tvrType =  t},(_,fv,_)) <- ds'' ])) dvars
            finalS = Map.union (Map.fromList [(n,LoopBreaker) | (TVr { tvrIdent = n },_) <- lb ]) $   foldl andOM se ([ s | (_,(_,_,s)) <- ds'' ])
        tell $ Seq.singleton (Map.fromList [ (t,Map.findWithDefault Unused n (Map.mapWithKey frules finalS)) | (t@(TVr { tvrIdent = n }),_) <- ds'' ])
        return (eLetRec [ (tvrInfo_u ((calcStrictInfo v e)) v,e) | (v,(e,_,_)) <- ds'' ] e', fvs, finalS  )
        --return (substLet' [ (v,e) | (v,(e,_,_)) <- ds'' ] e', fvs, finalS  )
    f e@(EAp a b)  = case runIdentity $ app (fromAp e) of
            EAp a' b' | a == a' && b == b' -> error $ "SSimplify.collectOcc.f: " ++ show e
            e -> f e
    f e = error $ "SSimplify.collectOcc.f: " ++ show e
    frules k _ | k `Set.member` rule_set = Many
    frules _ x = x
    alt (Alt l e) = do
        (e',b,c) <- f e
        return (Alt l e',foldr Set.delete (freeVars l `mappend` b) (map tvrNum $ litBinds l),foldr Map.delete c (map tvrNum $ litBinds l))
    args as = ans where
        ans = Map.fromList [ (i,Many) | Just (EVar (TVr { tvrIdent = i }),_) <- map (\e -> from_unsafeCoerce e `mplus` Just (e,Unknown)) as]

-- TODO this should use the occurance info
-- loopFunc t _ | getProperty prop_PLACEHOLDER t = -100  -- we must not choose the placeholder as the loopbreaker
loopFunc t e = negate (baseInlinability t e)


mapAndUnzip3M     :: (Monad m) => (a -> m (b,c,d)) -> [a] -> m ([b], [c], [d])
mapAndUnzip3M f xs = sequence (map f xs) >>= return . unzip3

inLam Once = OnceInLam
inLam _ = Many

andOM x y = Map.unionWith andOcc x y
andOcc Unused x = x
andOcc x Unused = x
andOcc _ _ = Many

orMaps ms = Map.map orMany $ foldl (Map.unionWith (++)) mempty (map (Map.map (:[])) ms)

orMany [] = error "empty orMany"
orMany [x] = x
orMany xs = if all (== Once) xs then ManyBranch else Many



data SimplifyOpts = SimpOpts {
    so_noInlining :: Bool,                 -- ^ this inhibits all inlining inside functions which will always be inlined
    so_superInline :: Bool,                -- ^ whether to do superinlining
    so_boundVars :: Map.Map Int E,
    so_rules :: Rules,
    so_dataTable :: DataTable,             -- ^ the data table
    so_exports :: [Int]
    }
    {-! derive: Monoid !-}


data Range = Done E | Susp E Subst
    deriving(Show)
type Subst = Map.Map Int Range

type InScope = Map.Map Int Binding
data Binding = NotAmong [Name] | IsBoundTo Occurance E | NotKnown
    {-! derive: is !-}

data Env = Env {
    envInScope :: Map.Map Int Binding,
    envTypeMap :: Map.Map Int E
    }
    {-! derive: Monoid, update !-}

applySubst :: Subst -> E -> E
applySubst s = substMap'' tm where
    tm = Map.map g s
    g (Done e) = e
    g (Susp e s') = applySubst s' e

dosub sub e = coerceOpt return $ applySubst sub e

simplifyE :: SimplifyOpts -> E -> (Stat,E)
simplifyE sopts e = (stat,e') where
    (stat,[(_,e')]) =  simplifyDs sopts [(tvrSilly,e)]

simplifyDs :: SimplifyOpts -> [(TVr,E)] -> (Stat,[(TVr,E)])
simplifyDs sopts dsIn = (stat,dsOut) where
    collocc dsIn = do
        let ((ELetRec dsIn' _),fvs,occ) = collectOcc sopts (ELetRec dsIn (eTuple (map EVar (fsts dsIn))))
        addNames (map tvrIdent $ Map.keys occ)
        addNames (Set.toList fvs)
        let occ' = Map.mapKeysMonotonic tvrIdent occ
            dsIn'' = runIdentity $ annotateDs mempty (\t nfo -> return $ maybe (Info.delete Many nfo) (flip Info.insert nfo) (Map.lookup t occ')) (\_ -> return) (\_ -> return) dsIn'
        return dsIn''
    initialB = mempty { envInScope =  Map.map (\e -> IsBoundTo Many e) (so_boundVars sopts) }
    initialB' = mempty { envInScope =  Map.map (\e -> NotKnown) (so_boundVars sopts) }
    (dsOut,stat)  = runIdentity $ runStatT (runNameMT doit)
    doit = do
        ds' <- collocc dsIn
        let g (t,e) = do
                e' <- if forceInline t then
                        f e mempty initialB'  -- ^ do not inline into functions which themself will be inlined
                            else f e mempty initialB
                return (t,e')
        mapM g ds'
    go e inb = do
        let (e',_,_) = collectOcc sopts  e
        f e' mempty inb
    go :: E -> Env -> NameMT Int (StatT Identity) E
    f :: E -> Subst -> Env -> NameMT Int (StatT Identity) E
    f e sub inb | (EVar v,xs) <- fromAp e = do
        xs' <- mapM (dosub sub) xs
        case Map.lookup (tvrNum v) sub of
            Just (Done e) -> h e xs' inb   -- e is var or trivial
            Just (Susp e s) -> do
                e' <- f e s inb
                h e' xs' inb
                --app (e',xs')
            Nothing -> h (EVar v) xs' inb
            -- Nothing -> error $ "Var with no subst: " ++ show e ++ "\n" ++  show  sub -- h (EVar v) xs' inb
    f e sub inb | (x,xs) <- fromAp e = do
        xs' <- mapM (dosub sub) xs
        x' <- g x sub inb
        x'' <- coerceOpt return x'
        x <- primOpt' (so_dataTable sopts) x''
        h x xs' inb
        --app (x,xs')
    g (EPrim a es t) sub inb = do
        es' <- mapM (dosub sub) es
        t' <- dosub sub t
        return $ EPrim a es' t'
    g (ELit (LitCons n es t)) sub inb = do
        es' <- mapM (dosub sub) es
        t' <- dosub sub t
        return $ ELit (LitCons n es' t')
    g (ELit (LitInt n t)) sub inb = do
        t' <- dosub sub t
        return $ ELit (LitInt n t')
    g e@(EPi (TVr { tvrIdent = n }) b) sub inb = do
        addNames [n]
        e' <- dosub sub e
        return e'
    g (EError s t) sub inb = do
        t' <- dosub sub t
        return $ EError s t'
    g ec@(ECase e b as d) sub inb = do
        addNames (map tvrNum $ caseBinds ec)
        e' <- f e sub inb
        doCase e' b as d sub inb
    g (ELam v e) sub inb  = do
        addNames [tvrNum v]
        v' <- nname v sub inb
        e' <- f e (Map.insert (tvrNum v) (Done $ EVar v') sub) (envInScope_u (Map.insert (tvrNum v') NotKnown) inb)
        return $ ELam v' e'
    g (ELetRec [] e) sub inb = g e sub inb
    g (ELetRec ds e) sub inb = do
        addNames $ map (tvrNum . fst) ds
        let z (t,EVar t') | t == t' = do    -- look for simple loops and replace them with errors.
                t'' <- nname t sub inb
                mtick $ "E.Simplify.<<loop>>.{" ++ showName (tvrIdent t) ++ "}"
                return (tvrNum t,Many,t'',EError "<<loop>>" (getType t))
            z (t,e) = do
                t' <- nname t sub inb
                case Info.lookup (tvrInfo t) of
                    _ | forceNoinline t -> return (tvrNum t,LoopBreaker,t',e)
                    Just Once -> return (tvrNum t,Once,error $ "Once: " ++ show t,e)
                    Just n -> return (tvrNum t,n,t',e)
                    -- We don't want to inline things we don't have occurance info for because they might lead to an infinite loop. hopefully the next pass will fix it.
                    Nothing -> return (tvrNum t,LoopBreaker,t',e)
                    -- Nothing -> error $ "No Occurance info for " ++ show t
            w ((t,Once,t',e):rs) sub inb ds = do
                mtick $ "E.Simplify.inline.Once.{" ++ showName t ++ "}"
                w rs (Map.insert t (Susp e sub) sub) inb ds
            w ((t,n,t',e):rs) sub inb ds = do
                e' <- f e sub inb
                case isAtomic e' && n /= LoopBreaker of
                    True -> do
                        when (n /= Unused) $ mtick $ "E.Simplify.inline.Atomic.{" ++ showName t ++ "}"
                        w rs (Map.insert t (Done e') sub) (envInScope_u (Map.insert (tvrNum t') (IsBoundTo n e')) inb) ((t',e'):ds)
                    -- False | worthStricting e', Strict <- Info.lookup (tvrInfo t') -> w rs sub
                    False -> w rs sub (if n /= LoopBreaker then (envInScope_u (Map.insert (tvrNum t') (IsBoundTo n e')) inb) else inb) ((t',e'):ds)
            w [] sub inb ds = return (ds,sub,inb)
        s' <- mapM z ds
        let
            sub'' = {- Map.fromList [ (t,Susp e sub'') | (t,Once,_,e) <- s'] `Map.union`-} (Map.fromList [ (t,Done (EVar t'))  | (t,n,t',_) <- s', n /= Once]) `Map.union` sub
        (ds',sub',inb') <- w s' sub'' (envInScope_u (Map.fromList [ (tvrNum t',NotKnown) | (_,n,t',_) <- s', n /= Once] `Map.union`) inb) []
        e' <- f e sub' inb'
        case ds' of
            [(t,e)] | worthStricting e, Just (Strict.S _) <- Info.lookup (tvrInfo t), not (getProperty prop_CYCLIC t) -> do
                mtick "E.Simplify.strictness.let-to-case"
                return $ eStrictLet t e e'
            _ -> do
                let fn ds (ELetRec ds' e) | not (hasRepeatUnder fst (ds ++ ds')) = fn (ds' ++ ds) e
                    fn ds e = f ds (Set.fromList $ fsts ds) [] False where
                        f ((t,ELetRec ds' e):rs) us ds b | all (not . (`Set.member` us)) (fsts ds') = f ((t,e):rs) (Set.fromList (fsts ds') `Set.union` us) (ds':ds) True
                        f (te:rs) us ds b = f rs us ([te]:ds) b
                        f [] _ ds True = fn (concat ds) e
                        f [] _ ds False = (concat ds,e)
                let (ds'',e'') = fn ds' e'
                --when (hasRepeatUnder fst ds'') $ fail "hasRepeats!"
                mticks  (length ds'' - length ds') (toAtom $ "E.Simplify.let-coalesce")
                return $ eLetRec ds'' e''
                {-
                let z (v,ELetRec ds e) = (ds,(v,e))
                    z (v,e) = ([],(v,e))
                    (ds''',ds'') = unzip (map z ds')
                    nds = (concat ds''' ++ ds'')
                --mticks (length (concat ds''')) (toAtom $ "E.Simplify.let-coalesce.{" ++ unwords (sort (map tvrShowName $ map fst (concat ds'''))) ++ "}")

                if hasRepeatUnder fst nds then
                    return $ eLetRec ds' e'
                  else do
                    mticks (length (concat ds''')) (toAtom $ "E.Simplify.let-coalesce")
                    return $ eLetRec nds  e'
                  -}
    g e _ _ = error $ "SSimplify.simplify.g: " ++ show e
    showName t | odd t = tvrShowName (tVr t Unknown)
             | otherwise = "(epheremal)"

    nname tvr@(TVr { tvrIdent = n, tvrType =  t}) sub inb  = do
        t' <- dosub sub t
        let t'' = substMap'' (Map.map (\ (IsBoundTo _ e) -> e) $ Map.filter isIsBoundTo (envInScope inb)) t'  --  (Map.fromAscList [ (t,e) | (t,IsBoundTo _ e) <- Map.toAscList (envInScope inb) ]) t'
        n' <- uniqueName n
        return $ tvr { tvrIdent = n', tvrType =  t'' }
--        case n `Map.member` inb of
--            True -> do
--                n' <- newName
--                return $ TVr n' t'
--            False -> do
--                n' <- uniqueName n
--                return $ TVr n' t'

    -- TODO - case simplification

    doCase (ELetRec ds e) b as d sub inb = do
        mtick "E.Simplify.let-from-case"
        e' <- doCase e b as d sub inb
        return $ substLet' ds e'

    doCase (EVar v) b as d sub inb |  Just (IsBoundTo _ (ELit l)) <- Map.lookup (tvrNum v) (envInScope inb)  = doConstCase l b as d sub inb
    doCase (ELit l) b as d sub inb  = doConstCase l b as d sub inb

    doCase (EVar v) b as d sub inb | Just (IsBoundTo _ e) <- Map.lookup (tvrNum v) (envInScope inb) , isBottom e = do
        mtick "E.Simplify.case-of-bottom'"
        let t = getType (ECase (EVar v) b as d)
        t' <- dosub sub t
        return $ prim_unsafeCoerce (EVar v) t'

    doCase ic@(ECase e b as d) b' as' d' sub inb | length (filter (not . isBottom) (caseBodies ic)) <= 1 || all whnfOrBot (caseBodies ic)  || all whnfOrBot (caseBodies (ECase Unknown b' as' d'))  = do
        mtick (toAtom "E.Simplify.case-of-case")
        let f (Alt l e) = do
                e' <- doCase e b' as' d' sub (envInScope_u (Map.fromList [ (n,NotKnown) | TVr { tvrIdent = n } <- litBinds l ] `Map.union`) inb)
                return (Alt l e')
            --g e >>= return . Alt l
            g x = doCase x b' as' d' sub (envInScope_u (Map.insert (tvrNum b) NotKnown) inb)
        as'' <- mapM f as
        d'' <- fmapM g d
        return (ECase e b as'' d'')      -- we duplicate code so continue for next renaming pass before going further.
    doCase e b as d sub inb | isBottom e = do
        mtick "E.Simplify.case-of-bottom"
        let t = getType (ECase e b as d)
        t' <- dosub sub t
        return $ prim_unsafeCoerce e t'

    doCase e b as@(Alt (LitCons n _ _) _:_) (Just d) sub inb | Just ss <- getSiblings (so_dataTable sopts) n, length ss <= length as = do
        mtick "E.Simplify.case-no-default"
        doCase e b as Nothing sub inb
    doCase e b as (Just d) sub inb | te /= tWorld__, (ELit (LitCons cn _ _)) <- followAliases dt te, Just Constructor { conChildren = Just cs } <- getConstructor cn dt, length as == length cs - 1 || (False && length as < length cs && isAtomic d)  = do
        let ns = [ n | Alt ~(LitCons n _ _) _ <- as ]
            ls = filter (`notElem` ns) cs
            f n = do
                con <- getConstructor n dt
                let g t = do
                        n <- newName
                        return $ tVr n t
                ts <- mapM g (slotTypes (so_dataTable sopts) n te)
                let wtd = ELit $ LitCons n (map EVar ts) te
                return $ Alt (LitCons n ts te) (eLet b wtd d)
        mtick $ "E.Simplify.case-improve-default.{" ++ show (sort ls) ++ "}"
        ls' <- mapM f ls
        doCase e b (as ++ ls') Nothing sub inb
        where
        te = getType e
        dt = (so_dataTable sopts)
    doCase e b [] (Just d) sub inb | not (isLifted e || isUnboxed (getType e)) = do
        mtick "E.Simplify.case-unlifted"
        b' <- nname b sub inb
        d' <- f d (Map.insert (tvrNum b) (Done (EVar b')) sub) (envInScope_u  (Map.insert (tvrNum b') (IsBoundTo Many e)) inb)
        return $ eLet b' e d'
    doCase (EVar v) b [] (Just d) sub inb | Just (NotAmong _) <-  Map.lookup (tvrNum v) (envInScope inb)  = do
        mtick "E.Simplify.case-evaled"
        d' <- f d (Map.insert (tvrNum b) (Done (EVar v)) sub) inb
        return d'
    doCase scrut v [] (Just sc@ECase { eCaseScrutinee = EVar v'} ) sub inb | v == v', not $ tvrNum v `Set.member` freeVars (caseBodies sc)  = do
        mtick "E.Simplify.case-default-case"
        f sc { eCaseScrutinee = scrut } sub inb
    doCase e b as d sub inb = do
        b' <- nname b sub inb
        let dd e' = f e' (Map.insert (tvrNum b) (Done $ EVar b') sub) (envInScope_u (newinb `Map.union`) inb) where
                na = NotAmong [ n | Alt (LitCons n _ _) _ <- as]
                newinb = Map.fromList [ (n,na) | EVar (TVr { tvrIdent = n }) <- [e,EVar b']]
            da (Alt (LitInt n t) ae) = do
                t' <- dosub sub t
                let p' = LitInt n t'
                e' <- f ae sub (mins e (patToLitEE p') inb)
                return $ Alt p' e'
            da (Alt (LitCons n ns t) ae) = do
                t' <- dosub sub t
                ns' <- mapM (\v -> nname v sub inb) ns
                let p' = LitCons n ns' t'
                    nsub = Map.fromList [ (n,Done (EVar t))  | TVr { tvrIdent = n } <- ns | t <- ns' ]
                    ninb = Map.fromList [ (n,NotKnown)  | TVr { tvrIdent = n } <- ns' ]
                e' <- f ae (nsub `Map.union` sub) (envInScope_u (ninb `Map.union`) $ mins e (patToLitEE p') inb)
                return $ Alt p' e'
            mins (EVar v) e = envInScope_u (Map.insert (tvrNum v) (IsBoundTo Many $  e))
            mins _ _ = id

        d' <- fmapM dd d
        as' <- mapM da as
        return $ ECase e b' as' d'

    doConstCase l b as d sub inb = do
        mr <- match l as (b,d)
        case mr of
            Just (bs,e) -> do
                let bs' = [ x | x@(TVr { tvrIdent = n },_) <- bs, n /= 0]
                binds <- mapM (\ (v,e) -> nname v sub inb >>= return . (,,) e v) bs'
                e' <- f e (Map.fromList [ (n,Done $ EVar nt) | (_,TVr { tvrIdent = n },nt) <- binds] `Map.union` sub)   (envInScope_u (Map.fromList [ (n,IsBoundTo Many e) | (e,_,TVr { tvrIdent = n }) <- binds] `Map.union`) inb)
                return $ eLetRec [ (v,e) | (e,_,v) <- binds ] e'
            Nothing -> do
                let t = getType (ECase (ELit l) b as d)
                return $ EError "match falls off bottom" t

    match m@(LitCons c xs _) ((Alt (LitCons c' bs _) e):rs) d | c == c' = do
        mtick (toAtom $ "E.Simplify.known-case." ++ show c )
        return $ Just ((zip bs xs),e)
         | otherwise = match m rs d
    match m@(LitInt a _) ((Alt (LitInt b _) e):rs) d | a == b = do
        mtick (toAtom $ "E.Simplify.known-case." ++ show a)
        return $ Just ([],e)
         | otherwise = match m rs d
    match l [] (b,Just e) = do
        mtick (toAtom "E.Simplify.known-case._")
        return $ Just ([(b,ELit l)],e)
    --match m [] (_,Nothing) = error $ "End of match: " ++ show m
    match m [] (_,Nothing) = do
        mtick (toAtom "E.Simplify.known-case.unmatch")
        return Nothing
    match m as d = error $ "Odd Match: " ++ show ((m,getType m),as,d)


    applyRule v xs inb  = do
        z <- builtinRule v xs
        let lup x = case Map.lookup x (envInScope inb) of
                Just (IsBoundTo _ e) -> Just e
                _ -> Nothing
        case z of
            Nothing | fopts FO.Rules -> applyRules lup (Info.fetch (tvrInfo v)) xs
            x -> return x

    h v xs' inb  | so_superInline sopts, si@(_:_) <- [ (tvr,fromJust body) | EVar tvr <- xs', forceSuperInline tvr, let body = haveBody tvr, isJust body ] = do
        mapM_ (\v -> mtick  (toAtom $ "E.Simplify.inline.superforced.{" ++ tvrShowName v  ++ "}")) (fsts si)
        let siName x = case fromId x of
                Just y ->  [toId (toName Val ("SI@",'f':show y ++ "$" ++ show i)) | i <- [(1::Int)..] ]
                Nothing -> [toId (toName Val ("SI@",'f':show x ++ "$" ++ show i)) | i <- [(1::Int)..] ]
        zs <- flip mapM si $ \ (t,b) -> do
            nn <- newNameFrom (siName (tvrIdent t))
            let t' = unsetProperty prop_SUPERINLINE t { tvrIdent = nn }
            return (t,t',subst t (EVar t') b)
        let xs'' = map (substLet [ (t,EVar t') | (t,t',_) <- zs]) xs'
        e <- app (v,xs'')
        return (eLetRec [ (t',b) | (_,t',b) <- zs] e)
       where
            haveBody tvr = case Map.lookup (tvrIdent tvr) (envInScope inb) of
                (Just (IsBoundTo _ e)) -> Just e
                _ -> Nothing


    h (EVar v) xs' inb | forceNoinline v = do
        z <- applyRule v xs' inb
        case z of
            Just (x,xs) -> didInline inb (x,xs) --h x xs inb
            Nothing -> app (EVar v, xs')

    h (EVar v) xs' inb = do
        z <- applyRule v xs' inb
        case z of
            Just (x,xs) -> didInline inb (x,xs) -- h x xs inb
            Nothing -> case Map.lookup (tvrNum v) (envInScope inb) of
                Just (IsBoundTo LoopBreaker _) -> app (EVar v,xs')
                Just (IsBoundTo Once _) -> error "IsBoundTo: Once"
                Just (IsBoundTo n e) | forceInline v -> do
                    mtick  (toAtom $ "E.Simplify.inline.forced.{" ++ tvrShowName v  ++ "}")
                    didInline inb (e,xs')
                Just (IsBoundTo OnceInLam e) | safeToDup e && someBenefit e xs' -> do
                    mtick  (toAtom $ "E.Simplify.inline.OnceInLam.{" ++ showName (tvrIdent v)  ++ "}")
                    didInline inb (e,xs')
                Just (IsBoundTo ManyBranch e) | multiInline e xs' -> do
                    mtick  (toAtom $ "E.Simplify.inline.ManyBranch.{" ++ showName (tvrIdent v)  ++ "}")
                    didInline inb (e,xs')
                Just (IsBoundTo Many e) | safeToDup e && multiInline e xs' -> do
                    mtick  (toAtom $ "E.Simplify.inline.Many.{" ++ showName (tvrIdent v)  ++ "}")
                    didInline inb (e,xs')
                Just _ -> app (EVar v,xs')
                Nothing  -> app (EVar v,xs')
                -- Nothing | tvrNum v `Set.member` exports -> app (EVar v,xs')
                -- Nothing -> error $ "Var not in scope: " ++ show v
    h e xs' inb = do
        app (e,xs')
    didInline inb z = do
        e <- app z
        go e inb



someBenefit _ _ = True

multiInline e xs = length xs + 2 >= (nsize + if safeToDup b then negate 4 else 0)  where
    (b,as) = fromLam e
    nsize = size b + abs (length as - length xs)
    size e | (x,xs) <- fromAp e = size' x + length xs
    size' (EVar _) = 1
    size' (ELit _) = 1
    size' (EPi _ _) = 1
    size' (ESort _) = 1
    size' (EPrim _ _ _) = 1
    size' (EError _ _) = -1
    size' ec@ECase {} | EVar v <- eCaseScrutinee ec, v `elem` as = sum (map size (caseBodies ec)) - 3
    size' ec@ECase {} = size (eCaseScrutinee ec) + sum (map size (caseBodies ec))
    size' (ELetRec ds e) = size e + sum (map (size . snd) ds)
    size' _ = 2


worthStricting EError {} = True
worthStricting ELit {} = False
worthStricting ELam {} = False
worthStricting x = sortTermLike x


coerceOpt :: MonadStats m =>  (E -> m E) -> E -> m E
coerceOpt fn e = do
    let (n,e',p) = unsafeCoerceOpt e
    n `seq` stat_unsafeCoerce `seq` mticks n stat_unsafeCoerce
    e'' <- fn e'
    return (p e'')

stat_unsafeCoerce = toAtom "E.Simplify.unsafeCoerce"

