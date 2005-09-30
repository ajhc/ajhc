module E.SSimplify(Occurance(..), simplify, SimplifyOpts(..)) where

import Control.Monad.Identity
import Control.Monad.Writer
import Data.FunctorM
import Data.Generics
import Data.Monoid
import List
import qualified Data.Map as Map
import qualified Data.Set as Set

import Atom
import CanType
import DataConstructors
import E.E
import E.PrimOpt
import E.Rules
import E.Subst
import E.Values
import FreeVars
import GenUtil
import Info.Types
import Name
import NameMonad
import Options
import qualified E.Strictness as Strict
import qualified FlagOpts as FO
import qualified Info.Info as Info
import qualified Util.Seq as Seq
import Stats hiding(new,print,Stats)
import Util.Graph
import VConsts

data Occurance =
    Unused        -- ^ unused means a var is not used at the term level, but might be at the type level
    | Once        -- ^ Used at most once not inside a lambda or as an argument
    | OnceInLam   -- ^ used once inside a lambda
    | ManyBranch  -- ^ used once in several branches
    | Many        -- ^ used many or an unknown number of times
    | LoopBreaker -- ^ chosen as a loopbreaker
    deriving(Show,Eq,Ord)



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
        let gfv (_,fv,i) = fvs ++ Set.toList (mconcat (map (ruleFreeVars' rules) (fvs)))  where
                fvs = Set.toList (Set.fromAscList (Map.keys i) `Set.union` fv)
        let gr = newGraph (zip (fsts ds) ds') (tvrNum . fst) (gfv . fst . snd )
        (e',fve,se) <- f e
        let nn' = reachable gr (Set.toList fve ++ Map.keys se ++  topLevels)
        nn <- sequence [ tell t >> return (x,y) |  (x,(y,t)) <- nn' ]
        let gr' = newGraph nn (tvrNum . fst) (gfv . snd )
            (lb,ds'') = findLoopBreakers (\ (_,(e,_,_)) -> loopFunc e) gr'
            cycNodes = Set.fromList $ [ v | (v,_) <- cyclicNodes gr']
            calcStrictInfo t e
                | t `Set.member` cycNodes = NoStrict
                | Just (Strict.S _) <- Map.lookup (tvrNum t) (so_strictness sopts) = Strict
                | otherwise = NoStrict
        let dvars = map (tvrNum . fst) ds
            fvs = foldr Set.delete (mconcat (fve:[ fv `mappend` freeVars t | (TVr { tvrType =  t},(_,fv,_)) <- ds'' ])) dvars
            finalS = Map.union (Map.fromList [(n,LoopBreaker) | (TVr { tvrIdent = n },_) <- lb ]) $   foldl andOM se ([ s | (_,(_,_,s)) <- ds'' ])
        tell $ Seq.singleton (Map.fromList [ (t,Map.findWithDefault Unused n (Map.mapWithKey frules finalS)) | (t@(TVr { tvrIdent = n }),_) <- ds'' ])
        return (eLetRec [ (tvrInfo_u (Info.insert (calcStrictInfo v e)) v,e) | (v,(e,_,_)) <- ds'' ] e', fvs, finalS  )
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
        {-
    f (ELetRec ds e) = do
        ds' <- mapM  (f . snd) ds
        let gfv (_,fv,i) = fvs ++ Set.toList (mconcat (map (ruleFreeVars' rules) (fvs)))  where
                fvs = Set.toList (Set.fromAscList (Map.keys i) `Set.union` fv)
        let gr = newGraph (zip (fsts ds) ds') (tvrNum . fst) (gfv . snd)
        (e',fve,se) <- f e
        let nn = reachable gr (Set.toList fve ++ Map.keys se ++  topLevels)
        let gr' = newGraph nn (tvrNum . fst) (gfv . snd)
            (lb,ds'') = findLoopBreakers (\ (_,(e,_,_)) -> loopFunc e) gr'
        let dvars = map (tvrNum . fst) ds
            fvs = foldr Set.delete (mconcat (fve:[ fv `mappend` freeVars t | (TVr _ t,(_,fv,_)) <- ds'' ])) dvars
            finalS = Map.union (Map.fromList [(n,LoopBreaker) | (TVr n _,_) <- lb ]) $   foldl andOM se ([ s | (_,(_,_,s)) <- ds'' ])
        tell $ Seq.singleton (Map.fromList [ (t,Map.findWithDefault Unused n (Map.mapWithKey frules finalS)) | (t@(TVr n _),_) <- ds'' ])
        return (eLetRec [ (v,e) | (v,(e,_,_)) <- ds'' ] e', fvs, finalS  )
        -}

-- this should use the occurance info
loopFunc EVar {} = 0
loopFunc ELit {} = 1
loopFunc EPi {} = 1
loopFunc EPrim {} = 2
loopFunc EError {} = 2
loopFunc ELam {} = 3
loopFunc _ = 4

mapAndUnzip3M     :: (Monad m) => (a -> m (b,c,d)) -> [a] -> m ([b], [c], [d])
mapAndUnzip3M f xs = sequence (map f xs) >>= return . unzip3

inLam Once = OnceInLam
inLam _ = Many

andOM x y = Map.unionWith andOcc x y
andOcc _ _ = Many

orMaps ms = Map.map orMany $ foldl (Map.unionWith (++)) mempty (map (Map.map (:[])) ms)

orMany [] = error "empty orMany"
orMany [x] = x
orMany xs = if all (== Once) xs then ManyBranch else Many



data SimplifyOpts = SimpOpts {
    so_boundVars :: Map.Map Int E,
    so_properties :: Map.Map Name [Atom],
    so_rules :: Rules,
    so_dataTable :: DataTable,
    so_strictness :: Map.Map Int Strict.SA,
    so_exports :: [Int]
    }
    {-! derive: Monoid !-}


data Range = Done E | Susp E Subst
    deriving(Show)
type Subst = Map.Map Int Range

type InScope = Map.Map Int Binding
data Binding = NotAmong [Name] | IsBoundTo Occurance E | NotKnown

data Env = Env {
    envInScope :: Map.Map Int Binding,
    envTypeMap :: Map.Map Int E
    }
    {-! derive: Monoid, update !-}

applySubst :: Subst -> E -> E
applySubst s = substMap'' (f s) where
    f s = Map.fromAscList [ (x,g y) | (x,y) <- Map.toAscList s ]
    g (Done e) = e
    g (Susp e s') = applySubst s' e

dosub sub e = do
    coerceOpt return $ applySubst sub e
--dosub sub e = coerceOpt (return . applySubst sub) e
--coerceOpt f e = f e

simplify :: SimplifyOpts -> E -> (E,Stat, Map.Map TVr Occurance)
simplify sopts e = (e'',stat,occ) where
    exports = Set.fromList (so_exports sopts)
    --(e',fvs,occ) = collectOcc (Set.toList exports) (so_rules sopts) (so_dataTable sopts)  e
    (e',fvs,occ) = collectOcc sopts  e
    addN = do
        addNames (map tvrNum $ Map.keys occ)
        addNames (Set.toList fvs)
    initialB = mempty { envInScope = Map.fromAscList [ (i,IsBoundTo Many e)  | (i,e) <- Map.toAscList $ so_boundVars sopts] }
    (e'',stat)  = runIdentity $ runStatT (runNameMT (addN >> f e' mempty initialB)) -- (e,mempty)
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
        --x' <- coerceOpt (\x -> g x sub inb) x
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
    --g (EVar v) sub inb = do
    --    case Map.lookup (tvrNum v) sub of
   --         Just (Done e) -> return e
    --        Just (Susp e s) -> do
    --            e' <- f e s inb
    --            return e'
            --Nothing -> return (EVar v)
    --        Nothing -> error $ "vvar with no subst: " ++ show (EVar v) -- h (EVar v) xs' inb
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
        -- let z (t,e) | worthStricting e && Just (S _) <- Map.lookup (tvrNum t) (so_strictness sopts)= do
        let z (t,e) = do
                t' <- nname t sub inb
                case Map.lookup t occ of
                    Just Once -> return (tvrNum t,Once,error $ "Once: " ++ show t,e)
                    Just n -> return (tvrNum t,n,t',e)
                    Nothing -> return (tvrNum t,Many,t',e)
                    -- Nothing -> error $ "No Occurance info for " ++ show t
            w ((t,Once,t',e):rs) sub inb ds = do
                mtick $ "E.Simplify.inline.Once.{" ++ tvrShowName (tVr t Unknown) ++ "}"
                w rs (Map.insert t (Susp e sub) sub) inb ds
            w ((t,n,t',e):rs) sub inb ds = do
                e' <- f e sub inb
                case isAtomic e' && n /= LoopBreaker of
                    True -> do
                        when (n /= Unused) $ mtick $ "E.Simplify.inline.Atomic.{" ++ tvrShowName (tVr t Unknown) ++ "}"
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
            [(t,e)] | worthStricting e, Just Strict <- Info.lookup (tvrInfo t) -> do
                mtick "E.Simplify.let-to-case"
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

    nname tvr@(TVr { tvrIdent = n, tvrType =  t}) sub inb  = do
        t' <- dosub sub t
        let t'' = substMap'' (Map.fromAscList [ (t,e) | (t,IsBoundTo _ e) <- Map.toAscList (envInScope inb) ]) t'
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
        return $ ELetRec ds e'

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
    doCase e b [] (Just d) sub inb | not (isLifted e) = do
        mtick "E.Simplify.case-unlifted"
        b' <- nname b sub inb
        d' <- f d (Map.insert (tvrNum b) (Done (EVar b')) sub) (envInScope_u  (Map.insert (tvrNum b') (IsBoundTo Many e)) inb)
        return $ eLet b' e d'
    doCase (EVar v) b [] (Just d) sub inb | Just (NotAmong _) <-  Map.lookup (tvrNum v) (envInScope inb)  = do
        mtick "E.Simplify.case-evaled"
        d' <- f d (Map.insert (tvrNum b) (Done (EVar v)) sub) inb
        return d'
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
        (bs,e) <- match l as (b,d)
        let bs' = [ x | x@(TVr { tvrIdent = n },_) <- bs, n /= 0]
        binds <- mapM (\ (v,e) -> nname v sub inb >>= return . (,,) e v) bs'
        e' <- f e (Map.fromList [ (n,Done $ EVar nt) | (_,TVr { tvrIdent = n },nt) <- binds] `Map.union` sub)   (envInScope_u (Map.fromList [ (n,IsBoundTo Many e) | (e,_,TVr { tvrIdent = n }) <- binds] `Map.union`) inb)
        return $ eLetRec [ (v,e) | (e,_,v) <- binds ] e'

    match m@(LitCons c xs _) ((Alt (LitCons c' bs _) e):rs) d | c == c' = do
        mtick (toAtom $ "E.Simplify.known-case." ++ show c )
        return ((zip bs xs),e)
         | otherwise = match m rs d
    match m@(LitInt a _) ((Alt (LitInt b _) e):rs) d | a == b = do
        mtick (toAtom $ "E.Simplify.known-case." ++ show a)
        return ([],e)
         | otherwise = match m rs d
    match l [] (b,Just e) = do
        mtick (toAtom "E.Simplify.known-case._")
        return ([(b,ELit l)],e)
    match m [] (_,Nothing) = error $ "End of match: " ++ show m
    match m as d = error $ "Odd Match: " ++ show ((m,getType m),as,d)

    forceInline x | Properties p <- Info.fetch (tvrInfo x) = Set.member prop_INLINE p


    applyRule v xs  = do
        z <- builtinRule v xs
        case z of
            Nothing | fopts FO.Rules -> applyRules (Info.fetch (tvrInfo v)) xs
            x -> return x

    h (EVar v) xs' inb | Properties p <- Info.fetch (tvrInfo v), Set.member prop_NOINLINE p = do
        z <- applyRule v xs'
        case z of
            Just (x,xs) -> h x xs inb
            Nothing -> app (EVar v, xs')

    h (EVar v) xs' inb = do
        z <- applyRule v xs'
        case z of
            Just (x,xs) -> h x xs inb
            Nothing -> case Map.lookup (tvrNum v) (envInScope inb) of
                Just (IsBoundTo LoopBreaker _) -> app (EVar v,xs')
                Just (IsBoundTo Once _) -> error "IsBoundTo: Once"
                Just (IsBoundTo n e) | forceInline v -> do
                    mtick  (toAtom $ "E.Simplify.inline.forced.{" ++ tvrShowName v  ++ "}")
                    didInline inb (e,xs')
                Just (IsBoundTo OnceInLam e) | safeToDup e && someBenefit e xs' -> do
                    mtick  (toAtom $ "E.Simplify.inline.OnceInLam.{" ++ tvrShowName v  ++ "}")
                    didInline inb (e,xs')
                Just (IsBoundTo ManyBranch e) | multiInline e xs' -> do
                    mtick  (toAtom $ "E.Simplify.inline.ManyBranch.{" ++ tvrShowName v  ++ "}")
                    didInline inb (e,xs')
                Just (IsBoundTo Many e) | safeToDup e && multiInline e xs' -> do
                    mtick  (toAtom $ "E.Simplify.inline.Many.{" ++ tvrShowName v  ++ "}")
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
multiInline e xs = length xs + 2 >= nsize   where
    (b,as) = fromLam e
    nsize = size b + abs (length as - length xs)
    size e | (x,xs) <- fromAp e = size' x + length xs
    size' (EVar _) = 1
    size' (ELit _) = 1
    size' (EPi _ _) = 1
    size' (ESort _) = 1
    size' (EPrim _ _ _) = 1
    size' (EError _ _) = 1
    size' _ = 100


worthStricting x = isLifted x && not (isELit x)


coerceOpt :: MonadStats m =>  (E -> m E) -> E -> m E
coerceOpt fn e = do
    let (n,e',p) = unsafeCoerceOpt e
    n `seq` stat_unsafeCoerce `seq` mticks n stat_unsafeCoerce
    e'' <- fn e'
    return (p e'')

stat_unsafeCoerce = toAtom "E.Simplify.unsafeCoerce"

{-
    simp (p@EPrim {},xs) = do
        p' <- primOpt dataTable stats p
        cont (p',xs)
    f (ec@(ECase e _ _ _),[]) | isBottom e = do
        tick stats (toAtom "E.Simplify.case-of-bottom")
        f (prim_unsafeCoerce e (typ ec),[])
    f (ECase e b as (Just (ECase e' b' as' d')),[]) | e == e' = do
        tick stats (toAtom "E.Simplify.case-merging")
        let (nb,mdc)
                | tvrNum b == 0 = (b',id)
                | tvrNum b' == 0 = (b,id)
                | otherwise = (b,ELetRec [(b',EVar b)]) -- error "case-default-case: double bind"
            nas' = filter ( (`notElem` map altHead as) . altHead) as'
        f (ECase e nb (as ++ nas') (fmap mdc d'),[])
    f (oc@(ECase ic@(ECase e b as d) b' as' d'),[]) | length (filter (not . isBottom) (caseBodies ic)) <= 1 || all whnfOrBot (caseBodies ic) || all whnfOrBot (caseBodies oc) = do
        tick stats (toAtom "E.Simplify.case-of-case")
        let f (Alt l e) = Alt l (g e)
            g x = ECase x b' as' d'
        cont (ECase e b (map f as) (fmap g d),[])      -- we duplicate code so continue for next renaming pass before going further.

    f ec@(ECase e b as@(Alt (LitCons n _ _) _:_) (Just d),[]) | Just ss <- getSiblings dataTable n, length ss <= length as = do
        when (length ss < length as) $ fail ("Bad case: " ++ show ec)
        tick stats (toAtom "E.Simplify.case-no-default")
        f (ECase e b as Nothing,[])
    f (ECase e b [] (Just d),[]) | not (isLifted e) = do
        tick stats (toAtom "E.Simplify.case-unlifted")
        f (eLet b e d,[])

    f (ECase e (TVr 0 _) as (Just (ELetRec ds (ECase e' b' as' d'))),[]) | e == e' = do
        tick stats (toAtom "E.Simplify.case-merging")
        let nas' = filter ( (`notElem` map altHead as) . altHead) as'
        f (ELetRec ds  $ ECase e b' (as ++ nas') d',[])

    f (ec@ECase { eCaseScrutinee = el@(ELit l), eCaseAlts = [], eCaseDefault = Just e },[]) | isFullyConst el = do
        tick stats (toAtom "E.Simplify.case-fully-const")
        cont (subst (eCaseBind ec) el e,[])
    f (ec@ECase { eCaseScrutinee = el@(ELit l) },[]) = do
        (x,as) <- match l (eCaseAlts ec) (eCaseDefault ec)
        cont (eLet (eCaseBind ec) el (foldl eAp x as),[])
        --liftM (mapFst $ eLet (eCaseBind ec) el) $
    f (EError s t,xs@(_:_)) = do
        ticks stats (length xs) (toAtom "E.Simplify.error-application")
        f (EError s (foldl eAp t xs),[])
    f (ec@ECase { eCaseScrutinee = (EVar tvr)} ,[]) = do
        e <- lookupBinding tvr
        case e of
            IsBoundTo el@(ELit l) -> liftM (mapFst $ eLet (eCaseBind ec) el) $ match l (eCaseAlts ec) (eCaseDefault ec)
            NotAmong na | ECase e b [] (Just d) <- ec { eCaseAlts =  filtAlts na $ eCaseAlts ec } ->  do
                tick stats (toAtom "E.Simplify.seq-evaled")
                f (eLet b e d,[])
--    f ec@(ECase e b as@(Alt (LitCons n _ _) _:_) (Just d),[]) | Just ss <- getSiblings dataTable n, length ss <= length as = do
 --       when (length ss < length as) $ fail ("Bad case: " ++ show ec)
 --       tick stats (toAtom "E.Simplify.case-no-default")
--        f (ECase e b as Nothing,[])
            _ -> cont (ec,[])
    f (x@(EVar v),xs) = do
        z <- applyRule' stats (so_rules sopts) v xs
        case z of
            Just (x,xs) -> f (x,xs)
            Nothing -> do
                e <- lookupBinding v
                case e of
                    IsBoundTo exp | forceInline v -> do
                        tick stats (toAtom $ "E.Simplify.inline.forced.{" ++ tvrShowName v  ++ "}")
                        cont (exp,xs)
                    IsBoundTo (EVar v') -> do
                        tick stats (toAtom "E.Simplify.inline.copy-propagate")
                        f (EVar v',xs)
                    IsBoundTo (ELit l) -> do
                        tick stats (toAtom "E.Simplify.inline.constant-folding")
                        cont (ELit l,xs)
                    IsBoundTo x@(EError s t) -> do
                        tick stats (toAtom "E.Simplify.inline.error-folding")
                        ticks stats (length xs) (toAtom "E.Simplify.error-application")
                        f (EError s (foldl eAp t xs),[])
                    IsBoundTo exp
                        | shouldInline exp xs -> do
                            let name = tvrShowName v
                                name' = if  ("Instance@." `isPrefixOf` name) then "Instance@" else name
                            tick stats (toAtom $ "E.Simplify.inline.value.{" ++ name'  ++ "}")
                            cont (exp,xs)
                        | otherwise -> cont (x,xs)
                    _ -> cont (x,xs)
    f (x,xs) = cont (x,xs)
    cont (x,xs) = do
        x <- g' x
        xs <- mapM g' xs
        liftIO $ doCoalesce stats (x,xs)
    isGood (LitCons _ (_:_) _) = False
    isGood _ = True
    --match :: Lit E -> [(Pat E,E)] -> IO (E,[E])
    match (LitCons c xs _) ((Alt (LitCons c' bs _) e):_) _ | c == c' = do
        tick stats (toAtom $ "E.Simplify.known-case." ++ show c )
        cont (ELetRec (zip bs xs) e,[])
    match l ((Alt l' e):_) _ | litMatch l l' = do
        tick stats (toAtom $ "E.Simplify.known-case." ++ show l')
        f (e,[])
    --match l ((PatWildCard,e):_) = do
    --    tick stats (toAtom "E.Simplify.known-case._")
    --    f (e,[ELit l])
    match m (_:xs) d = match m xs d
    match l [] (Just e) = do
        tick stats (toAtom "E.Simplify.known-case._")
        f (e,[])
    match m [] Nothing = error $ "End of match: " ++ show m


    g' (EPrim p xs t) = do
        xs' <- mapM g' xs
        return $ EPrim p xs' t
    g' (ELit (LitCons p xs t)) = do
        xs' <- mapM g' xs
        return $ ELit (LitCons p xs' t)
    g' x = do
        (x',[]) <- g (x,[])
        return x'
    g (ELam (TVr n t) e,[]) | n /= 0,  n `notElem` freeVars e = do
        tick stats (toAtom "E.Simplify.blank-lam")
        return (ELam (TVr 0 t) e,[])
    g (EPi (TVr n t) e,[]) | n /= 0,  n `notElem` freeVars e = do
        tick stats (toAtom "E.Simplify.blank-pi")
        return (EPi (TVr 0 t) e,[])
--    g (EPi (TVr (Just i) _) (EAp a (EVar (TVr (Just i') _))),[]) | i == i' && not (i `elem` freeVars a) = do
--        tick stats (toAtom "E.Simplify.eta-reduce-pi")
--        g (a,[])
--    g (ELam (TVr (Just i) _) (EAp a (EVar (TVr (Just i') _))),[]) | i == i' && not (i `elem` freeVars a) = do
--        tick stats (toAtom "E.Simplify.eta-reduce-lam")
--        g (a,[])

    g (x@(EVar v),xs@[]) = do
        e <- lookupBinding v
        case e of
            IsBoundTo (EVar v') -> do
                tick stats (toAtom "E.Simplify.inline.copy-propagate")
                g (EVar v',xs)
            IsBoundTo e | Just _ <- fullyConst e -> do
                tick stats (toAtom $ "E.Simplify.inline.constant-folding")
                return (e,xs)
            IsBoundTo e | Just (EVar _,_) <- from_unsafeCoerce e -> do
                tick stats (toAtom "E.Simplify.inline.arg-unsafeCoerce")
                return (e,xs)
            IsBoundTo (ELit l) | isGood l -> do
                tick stats (toAtom "E.Simplify.inline.constant-folding2")
                return (ELit l,xs)
            --IsBoundTo x@(EError {}) -> do
            --    tick stats (toAtom "E.Simplify.error-folding")
            --    return (x,xs)
            --Just z | sortTypeLike z -> do
            --    tick stats (toAtom "E.Simplify.constant-folding")
            --    f (z,xs)
            _ -> return (x,xs)
    g (x,[]) = return (x,[])
    forceInline x | Just n <- tvrName x, Just xs <- Map.lookup n funcProps  = toAtom "INLINE" `elem` xs
    forceInline _ = False

filtAlts ns (Alt (LitCons n _ _) _:as) | n `elem` ns  = filtAlts ns as
filtAlts ns (a:as) = a:filtAlts ns as
filtAlts ns [] = []

litMatch (LitInt a _) (LitInt b _) = a == b
--litMatch (LitFrac a _) (LitFrac b _) = a == b
litMatch LitCons {} LitCons {} = False -- taken care of above
litMatch x y = error $ "litMatch: " ++ show (x,y)
-}




                {-
                --Just (IsBoundTo n e) | isAtomic e -> do
                --    mtick (toAtom "E.Simplify.inline.copy-propegate")
                --    h  e xs' inb
                Just (IsBoundTo n e) |  length xs <= length xs' -> case x of
                        ELit {} -> do
                            mtick (toAtom "E.Simplify.inline.const")
                            app (e,xs')
                        EPi {} -> do
                            mtick (toAtom "E.Simplify.inline.const")
                            app (e,xs')
                        EError {} -> do
                            mtick (toAtom "E.Simplify.inline.error")
                            app (e,xs')
                        EPrim {} | length xs > 0 -> do
                            mtick (toAtom "E.Simplify.inline.prim")
                            app (e,xs')
                        EVar {} -> do
                            mtick (toAtom "E.Simplify.inline.simple")
                            app (e,xs')
                        _ -> app (EVar v,xs')
                    where (x,xs) = fromLam e

                -}
