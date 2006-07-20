module E.SSimplify(
    Occurance(..),
    simplifyE,
    collectOccurance',
    programPruneOccurance,
    programSSimplify,
    programSSimplifyPStat,
    SimplifyOpts(..)
    ) where

import Control.Monad.Identity
import Control.Monad.Writer
import Control.Monad.Reader
import Data.FunctorM
import Data.Generics
import Data.Monoid
import List hiding(delete,union)
import Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set

import Atom
import C.Prims
import DataConstructors
import Doc.PPrint
import E.Annotate
import E.E
import E.Program
import E.Eta
import E.Inline
import E.PrimOpt
import E.Rules
import E.Subst
import E.TypeCheck
import E.Values
import GenUtil
import Info.Types
import Name.Name
import Name.VConsts
import Options
import qualified E.Demand as Demand
import qualified FlagOpts as FO
import qualified FlagDump as FD
import qualified Info.Info as Info
import qualified Util.Seq as Seq
import Stats hiding(new,print,Stats)
import Support.CanType
import Support.FreeVars
import Util.Graph
import Util.HasSize
import Util.NameMonad
import Name.Id
import Util.ReaderWriter
import Util.SetLike as S

type Bind = (TVr,E)

data Occurance =
    Unused        -- ^ unused means a var is not used at the term level, but might be at the type level
    | Once        -- ^ Used at most once not inside a lambda or as an argument
    | OnceInLam   -- ^ used once inside a lambda
    | ManyBranch  -- ^ used once in several branches
    | Many        -- ^ used many or an unknown number of times
    | LoopBreaker -- ^ chosen as a loopbreaker, never inline
    deriving(Show,Eq,Ord,Typeable)


programPruneOccurance :: Program -> Program
programPruneOccurance prog =
    let dsIn = programDs (runIdentity $ programMapBodies (return . subst (tVr (-1) Unknown) Unknown) prog)
        (dsIn',(OMap fvs,uids)) = runReaderWriter (unOM $ collectDs dsIn $ if progClosed prog then mempty else fromList $ map (flip (,) Many) (map (tvrIdent . fst) dsIn)) (fromList $ map tvrIdent $ progEntryPoints prog)
    in (programSetDs dsIn' prog) { progFreeIds = idMapToIdSet fvs, progUsedIds = uids }


newtype OM a = OM (ReaderWriter IdSet (OMap,IdSet) a)
    deriving(Monad,Functor,MonadWriter (OMap,IdSet),MonadReader IdSet)

unOM (OM a) = a

newtype OMap = OMap (IdMap Occurance)
   deriving(HasSize,SetLike,BuildSet (Id,Occurance),MapLike Id Occurance,Show,IsEmpty,Eq,Ord)

instance Monoid OMap where
    mempty = OMap mempty
    mappend (OMap a) (OMap b) = OMap (andOM a b)


maybeLetRec [] e = e
maybeLetRec ds e = ELetRec ds e

-- | occurance analysis

grump :: OM a -> OM (a,OMap)
grump m = fmap ( \ (x, (y,z)) -> (x,y) ) $ censor (\ (_,y) -> (mempty,y)) (listen m)

collectOccurance' :: E -> (E,IdMap Occurance)
collectOccurance' e = (fe,omap) where
    (fe,(OMap omap,_)) = runReaderWriter (unOM $ collectOccurance e) mempty

collectOccurance :: E -> OM E -- ^ (annotated expression, free variables mapped to their occurance info)
collectOccurance e = f e  where
    f e@ESort {} = return e
    f e@Unknown {} = return e
    f (EPi tvr@TVr { tvrIdent = 0, tvrType =  a} b) = arg $ do
        a <- f a
        b <- f b
        return (EPi tvr { tvrType = a } b)
    f (EPi tvr@(TVr { tvrIdent = n, tvrType =  a}) b) = arg $ do
        a <- f a
        (b,tfvs) <- grump (f b)
        case mlookup n tfvs of
            Nothing -> tell (tfvs,mempty) >>  return (EPi tvr { tvrIdent =  0, tvrType = a } b)
            Just occ -> tell (mdelete n tfvs,singleton n) >> return (EPi (annb' tvr { tvrType = a }) b)
    f (ELit (LitCons n as t)) = arg $ do
        t <- f t
        as <- mapM f as
        return (ELit (LitCons n as t))
    f (ELit (LitInt i t)) = do
        t <- arg (f t)
        return $ ELit (LitInt i t)
    f (EPrim p as t) = arg $ do
        t <- f t
        as <- mapM f as
        return (EPrim p as t)
    f (EError err t) = do
        t <- arg (f t)
        return $ EError err t
    f e | (b,as@(_:_)) <- fromLam e = do
        (b',bvs) <- grump (f b)
        (as',asfv) <- grump (arg $ mapM ftvr as)
        let avs = bvs `andOM` asfv
            as'' = map (annbind' avs) as'
        tell $ (inLam $ foldr mdelete avs (map tvrIdent as),fromList $ map tvrIdent as)
        return (foldr ELam b' as'')
    f e | Just (x,t) <- from_unsafeCoerce e  = do x <- f x ; t <- (arg (f t)); return (prim_unsafeCoerce x t)
    f (EVar tvr@TVr { tvrIdent = n, tvrType =  t}) = do
        tell $ (msingleton n Once,mempty)
        t <- arg (f t)
        return $ EVar tvr { tvrType = t }
    f e | (x,xs@(_:_)) <- fromAp e = do
        x <- f x
        xs <- arg (mapM f xs)
        return (foldl EAp x xs)
    f ec@ECase { eCaseScrutinee = e, eCaseBind = b, eCaseAlts = as, eCaseDefault = d} = do
        scrut' <- f e
        (d',fvb) <- grump (fmapM f d)
        (as',fvas) <- mapAndUnzipM (grump . alt) as
        let fidm = orMaps (fvb:fvas)
        ct <- arg $ f (eCaseType ec)
        b <- arg (ftvr b)
        tell $ (mdelete (tvrIdent b) fidm,singleton (tvrIdent b))
        return ec { eCaseScrutinee = scrut', eCaseAlts = as', eCaseBind = annbind' fidm b, eCaseType = ct, eCaseDefault = d'}
    f (ELetRec ds e) = do
        (e',fve) <- grump (f e)
        ds''' <- collectDs ds fve
        return (maybeLetRec ds''' e')
    f e = error $ "SSimplify.collectOcc.f: " ++ show e
    alt (Alt l e) = do
        (e',fvs) <- grump (f e)
        l <- arg (mapLitBindsM ftvr l)
        l <- arg (fmapM f l)
        let fvs' = foldr mdelete fvs (map tvrIdent $ litBinds l)
            l' = mapLitBinds (annbind' fvs) l
        tell (fvs',fromList $ map tvrIdent (litBinds l'))
        return (Alt l' e')
    arg m = do
        let mm (OMap mp,y) = (OMap $ fmap (const Many) mp,y)
        censor mm m
    ftvr tvr = do
        tt <- f (tvrType tvr)
        return tvr { tvrType = tt }

-- delete any occurance info for non-let-bound vars to be safe
annb' tvr = tvrInfo_u (Info.delete Many) tvr
annbind' idm tvr = case mlookup (tvrIdent tvr) idm of
    Nothing -> annb' tvr { tvrIdent = 0 }
    Just _ -> annb' tvr

-- add ocucrance info
annbind idm tvr = case mlookup (tvrIdent tvr) idm of
    Nothing -> annb Unused tvr { tvrIdent = 0 }
    Just x -> annb x tvr
annb x tvr = tvrInfo_u (Info.insert x) tvr

mapLitBinds f (LitCons n es t) = LitCons n (map f es) t
mapLitBinds f (LitInt e t) = LitInt e t
mapLitBindsM f (LitCons n es t) = do
    es <- mapM f es
    return (LitCons n es t)
mapLitBindsM f (LitInt e t) = return $  LitInt e t

collectBinding :: Bind -> OM (Bind,OMap)
collectBinding (t,e) = do
    e' <- collectOccurance e
    let rvars = freeVars (Info.fetch (tvrInfo t) :: ARules) :: IdMap TVr
        romap = OMap $ fmap (const Many) rvars
    return ((t,e'),romap)

unOMap (OMap x) = x

collectDs :: [Bind] -> OMap -> OM [Bind]
collectDs ds (OMap fve) = do
    ds' <- mapM (grump . collectBinding) ds
    exp <- ask
    let graph = newGraph ds' (\ (((t,_),_),_) -> tvrIdent t) (\ ((_,rv),fv) -> mkeys (fv `mappend` rv))
        rds = reachable graph (mkeys fve ++ [ tvrIdent t | (t,_) <- ds,  (tvrIdent t `member` exp)])
        -- ignore rules when calculating loopbreakers
        -- we must not simplify the expanded body of a rule without recalculating occurance info.
        graph' = newGraph rds (\ (((t,_),_),_) -> tvrIdent t) (\ (_,fv) -> mkeys fv)
        (lb,lbds) =  findLoopBreakers (\ (((t,e),_),_) -> loopFunc t e) (const True) graph'
        ds'' = map ( \ ((t,rv),rv') -> (t,rv `mappend` rv') ) lbds
        fids = foldl andOM mempty (fve:map unOMap (snds ds''))
        ffids = fromList [ (tvrIdent t,lup t) | ((t,_),_) <- ds'' ]
        cycNodes = (fromList $ [ tvrIdent v | (((v,_),_),_) <- cyclicNodes graph'] :: IdSet)
        calcStrictInfo :: TVr -> TVr
        calcStrictInfo t
            | tvrIdent t `member` cycNodes = setProperty prop_CYCLIC t
            | otherwise = t
        lup t = case tvrIdent t `elem` [ tvrIdent t | (((t,_),_),_) <- lb] of
            True -> LoopBreaker
            False -> case  (tvrIdent t `member` exp) of
                True -> Many
                False | Just r <- mlookup (tvrIdent t) fids -> r
        ds''' = [ (calcStrictInfo $ annbind ffids t ,e) | ((t,e),_) <- ds'']
        froo (t,e) = ((t {tvrType = t' },e),fvs) where
            (t',fvs) = collectOccurance' (tvrType t)
        (ds'''',nfids) = unzip $ map froo ds'''
        nfid' = fmap (const Many) (mconcat nfids)
    tell $ ((OMap $ nfid' `andOM` fids) S.\\ ffids,fromList (map (tvrIdent . fst) ds''''))
    return (ds'''')

-- TODO this should use the occurance info
-- loopFunc t _ | getProperty prop_PLACEHOLDER t = -100  -- we must not choose the placeholder as the loopbreaker
loopFunc t e = negate (baseInlinability t e)


inLam (OMap om) = OMap (fmap il om) where
    il Once = OnceInLam
    il _ = Many

andOM x y = munionWith andOcc x y
andOcc Unused x = x
andOcc x Unused = x
andOcc _ _ = Many

orMaps ms = OMap $ fmap orMany $ foldl (munionWith (++)) mempty (map (fmap (:[])) (map unOMap ms)) where
    unOMap (OMap m) = m

orMany [] = error "empty orMany"
orMany [x] = x
orMany xs = if all (== Once) xs then ManyBranch else Many



data SimplifyOpts = SimpOpts {
    so_noInlining :: Bool,                 -- ^ this inhibits all inlining inside functions which will always be inlined
    so_finalPhase :: Bool,                 -- ^ no rules and don't inhibit inlining
    so_boundVars :: IdMap (TVr,E),         -- ^ bound variables
    so_dataTable :: DataTable              -- ^ the data table
    }
    {-! derive: Monoid !-}


data Range = Done OutE | Susp InE Subst OutE -- cached result
    deriving(Show,Eq,Ord)
type Subst = IdMap Range

type InScope = IdMap Binding

data Forced = ForceInline | ForceNoinline | NotForced
    deriving(Eq,Ord)

data Binding =
    NotAmong [Name]
    | IsBoundTo {
        bindingOccurance :: Occurance,
        bindingE :: OutE,
        bindingCheap :: Bool,
        inlineForced :: Forced,
        bindingAtomic :: Bool
        }
    | NotKnown
    deriving(Ord,Eq)
    {-! derive: is !-}

isBoundTo o e = IsBoundTo {
    bindingOccurance = o,
    bindingE = e,
    bindingCheap = isCheap e,
    inlineForced = if o == LoopBreaker then ForceNoinline else NotForced,
    bindingAtomic = atomic
    } where
    atomic = isAtomic e


instance Monoid Forced where
    mempty = NotForced
    mappend NotForced x = x
    mappend x NotForced = x
    mappend _ ForceNoinline = ForceNoinline
    mappend ForceNoinline _ = ForceNoinline
    mappend ForceInline ForceInline = ForceInline

fixInline finalPhase v bt@IsBoundTo {} = bt { inlineForced = inlineForced bt `mappend` calcForced finalPhase v }  where

calcForced finalPhase v =
    case (forceNoinline v,finalPhase,forceInline v) of
        (True,_,_) -> ForceNoinline
        (False,True,_) -> NotForced
        (False,False,True) -> ForceInline
        (False,False,False) -> NotForced


data Env = Env {
    envCachedSubst :: IdMap (Maybe E),
    envSubst :: Subst,
    envInScope :: IdMap Binding
    }
    {-! derive: Monoid, update !-}

susp:: E -> Subst -> Range
susp e sub =  Susp e sub undefined -- (substMap'' (fmap mkSubst sub) e)

insertSuspSubst :: TVr -> InE -> Env -> Env
insertSuspSubst t e env = insertSuspSubst' (tvrIdent t) e env

insertSuspSubst' :: Id -> InE -> Env -> Env
insertSuspSubst' 0 _e env = env
insertSuspSubst' t e env = cacheSubst env { envSubst = minsert t (susp e (envSubst env)) (envSubst env) }

insertDoneSubst :: TVr -> OutE -> Env -> Env
insertDoneSubst t e env = insertDoneSubst' (tvrIdent t) e env

insertDoneSubst' :: Id -> OutE -> Env -> Env
insertDoneSubst' 0 _e env = env
insertDoneSubst' t e env = cacheSubst env { envSubst = minsert t (Done e) (envSubst env) }


insertInScope :: Id -> Binding -> Env -> Env
insertInScope 0 _b env = env
insertInScope t b env = cacheSubst env { envInScope = minsert t b (envInScope env) }

substLookup :: Id -> Env -> Maybe Range
substLookup id env = mlookup id (envSubst env)

substAddList ls env = cacheSubst env { envSubst = fromList ls `union` envSubst env }

--applySubst :: Subst -> IdMap a -> E -> E
--applySubst s nn = applySubst' s where
--    nn' = fmap (const Nothing) s `mappend` fmap (const Nothing) nn
--    applySubst' s = substMap'' (tm `mappend` nn') where
--        tm = fmap g s
--        g (Done e) = Just e
--        g (Susp e s') = Just $ applySubst' s' e

{-
applySubst :: Subst -> IdMap a -> E -> E
applySubst s nn = applySubst' s where
    nn' = fmap (const Nothing) s `mappend` fmap (const Nothing) nn
    applySubst' s = substMap'' (tm `mappend` nn') where
        tm = fmap g s
        g (Done e) = Just e
        g (Susp _ _ e)  = Just e
        --g (Susp e s')  = Just $ applySubst' s' e
-}

applySubst :: Subst -> IdMap a -> IdMap (Maybe OutE)
applySubst s nn = applySubst' s where
    nn' = fmap (const Nothing) s `mappend` fmap (const Nothing) nn
    applySubst' s = (tm `mappend` nn') where
        tm = fmap g s
        g (Done e) = Just e
        g (Susp e s' _) = Just $ substMap'' (applySubst' s') e

--mkSubst :: Range -> Maybe E
--mkSubst (Done e) = Just e
--mkSubst (Susp _ _ e) = Just e
--mkSubst (Susp e s' e) = Just $ substMap'' (fmap mkSubst s') e

--cacheSubst env = env { envCachedSubst = fmap mkSubst (envSubst env) }
cacheSubst env = env { envCachedSubst = applySubst (envSubst env) (envInScope env) }

dosub inb e = coerceOpt return $ substMap'' (envCachedSubst inb) e

--dosub inb e = coerceOpt return $ applySubst (envSubst inb) (envInScope inb) e

simplifyE :: SimplifyOpts -> InE -> (Stat,OutE)
simplifyE sopts e = (stat,e') where
    Identity ([(_,e')],stat) =  runStatT $ simplifyDs program sopts [(tvrSilly,e)]

programSSimplify :: SimplifyOpts -> Program -> Program
programSSimplify sopts prog = let
    Identity (dsIn,stats) = runStatT $ simplifyDs prog sopts (programDs prog)
    in (programSetDs dsIn prog) { progStats = progStats prog `mappend` stats }

programSSimplifyPStat :: SimplifyOpts -> Program -> IO Program
programSSimplifyPStat sopts prog = do
    setPrintStats True
    dsIn <- simplifyDs prog sopts (programDs prog)
    return (programSetDs dsIn prog)


type InE = E
type OutE = E
type InTVr = TVr
type OutTVr = TVr

type SM m = IdNameT m

simplifyDs :: forall m . MonadStats m => Program -> SimplifyOpts -> [(TVr,E)] -> m [(TVr,E)]
simplifyDs prog sopts dsIn = ans where
    finalPhase = so_finalPhase sopts
    ans = do
        (dsOut,_) <- (runIdNameT doit)
        return dsOut
    exportedSet = fromList $ map tvrIdent (progEntryPoints prog) :: IdSet
    getType e = infertype (so_dataTable sopts) e
    initialB = let
            bb (t,(_,e)) | isFullyConst e = [(t,Done e)]
            bb _ = []
        in cacheSubst mempty { envSubst = fromList $ concatMap bb  (massocs $ so_boundVars sopts),  envInScope =  fmap (\ (t,e) -> fixInline finalPhase t $ isBoundTo Many e) (so_boundVars sopts) }
    doit = do
        addNamesIdSet (progUsedIds prog)
        addBoundNamesIdSet (progFreeIds prog)
        addBoundNamesIdMap (so_boundVars sopts)
        doDs dsIn initialB
    go :: E -> Env -> SM m E
    go e inb = do
        let (e',_) = collectOccurance' e
        f e' (cacheSubst inb { envSubst = mempty })
    f :: InE -> Env -> SM m OutE
    f e inb | (ELam t b,(x:xs)) <- fromAp e = do
        xs' <- mapM (dosub inb) xs
        b' <- f b (insertSuspSubst t x inb) -- minsert (tvrIdent t) (Susp x sub) sub) inb
        mtick (toAtom "E.Simplify.f-beta-reduce")
        h b' xs' inb
    f e inb | (EPi t b,(x:xs)) <- fromAp e = do
        xs' <- mapM (dosub inb) xs
        b' <- f b (insertSuspSubst t x inb) -- (minsert (tvrIdent t) (Susp x sub) sub) inb
        mtick (toAtom "E.Simplify.f-pi-reduce")
        h b' xs' inb
    f e inb | (EVar v,xs) <- fromAp e = do
        xs' <- mapM (dosub inb) xs
        case substLookup (tvrIdent v) inb of
            Just (Done e) -> h e xs' inb   -- e is var or trivial
            Just (Susp e s _) -> do
                e' <- f e (cacheSubst inb { envSubst = s })
                h e' xs' inb
            Nothing -> h (EVar v) xs' inb
    f e inb | (x,xs) <- fromAp e = do
        eed <- etaExpandDef (so_dataTable sopts) tvr { tvrIdent = 0 } e
        case eed of
            Just (_,e) -> f e inb -- go e inb
            Nothing -> do
                xs' <- mapM (dosub inb) xs
                x' <- g x inb
                x'' <- coerceOpt return x'
                x <- primOpt' (so_dataTable sopts) x''
                h x xs' inb
    g (EPrim a es t) inb = do
        es' <- mapM (dosub inb) es
        t' <- dosub inb t
        return $ EPrim a es' t'
    g (ELit (LitCons n es t)) inb = do
        es' <- mapM (dosub inb) es
        t' <- dosub inb t
        return $ ELit (LitCons n es' t')
    g (ELit (LitInt n t)) inb = do
        t' <- dosub inb t
        return $ ELit (LitInt n t')
    g e@(EPi (TVr { tvrIdent = n }) b) inb = do
        addNames [n]
        e' <- dosub inb e
        return e'
    g (EError s t) inb = do
        t' <- dosub inb t
        return $ EError s t'
    g ec@ECase { eCaseScrutinee = e, eCaseBind = b, eCaseAlts = as, eCaseDefault = d} inb = do
        addNames (map tvrIdent $ caseBinds ec)
        e' <- f e inb
        doCase e' (eCaseType ec) b as d inb
    g (ELam v e) inb  = do
        addNames [tvrIdent v]
        v' <- nname v inb
        e' <- f e (insertDoneSubst v (EVar v') . insertInScope (tvrIdent v') NotKnown $ inb) --        minsert (tvrIdent v) (Done $ EVar v') sub)
        return $ ELam v' e'
    g (ELetRec ds@(_:_) e) inb = do
        (ds',inb') <- doDs ds inb
        e' <- f e inb'
        case ds' of
            [(t,e)] | worthStricting e, Just (Demand.S _) <- Info.lookup (tvrInfo t), not (getProperty prop_CYCLIC t) -> do
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
                when (flint && hasRepeatUnder fst ds'') $ fail "hasRepeats!"
                mticks  (length ds'' - length ds') (toAtom $ "E.Simplify.let-coalesce")
                return $ eLetRec ds'' e''
    g e _ = error $ "SSimplify.simplify.g: " ++ show e ++ "\n" ++ pprint e
    showName t | odd t || dump FD.EVerbose = tvrShowName (tVr t Unknown)
             | otherwise = "(epheremal)"

    nname tvr@(TVr { tvrIdent = n, tvrType =  t}) inb  = do
        t' <- dosub inb t
        let t'' = substMap'' (fmap (\ IsBoundTo { bindingE = e } -> Just e) $ mfilter isIsBoundTo (envInScope inb)) t'
        n' <- if n == 0 then return 0 else uniqueName n
        return $ tvr { tvrIdent = n', tvrType =  t'' }
    -- TODO - case simplification
    doCase :: OutE -> InE -> InTVr -> [Alt InE] -> (Maybe InE) -> Env -> SM m OutE
    doCase (ELetRec ds e) t b as d inb = do
        mtick "E.Simplify.let-from-case"
        e' <- doCase e t b as d inb
        return $ substLet' ds e'

    doCase (EVar v) t b as d inb |  Just IsBoundTo { bindingE = ELit l } <- mlookup (tvrIdent v) (envInScope inb)  = doConstCase l t  b as d inb
    doCase (ELit l) t b as d inb  = doConstCase l t b as d inb

    doCase (EVar v) t b as d inb | Just IsBoundTo { bindingE = e } <- mlookup (tvrIdent v) (envInScope inb) , isBottom e = do
        mtick "E.Simplify.case-of-bottom'"
        t' <- dosub inb t
        return $ prim_unsafeCoerce (EVar v) t'
    doCase e t b as d inb | isBottom e = do
        mtick "E.Simplify.case-of-bottom"
        t' <- dosub inb t
        return $ prim_unsafeCoerce e t'

    doCase ic@ECase { eCaseScrutinee = e, eCaseBind =  b, eCaseAlts =  as, eCaseDefault =  d } t b' as' d' inb | length (filter (not . isBottom) (caseBodies ic)) <= 1 || all whnfOrBot (caseBodies ic)  || all whnfOrBot (caseBodies emptyCase { eCaseAlts = as', eCaseDefault = d'} )  = do
        mtick (toAtom "E.Simplify.case-of-case")
        let f (Alt l e) = do
                e' <- doCase e t b' as' d' (cacheSubst $ envInScope_u (fromList [ (n,NotKnown) | TVr { tvrIdent = n } <- litBinds l ] `union`) inb)
                return (Alt l e')
            --g e >>= return . Alt l
            g x = doCase x t b' as' d' (insertInScope (tvrIdent b) NotKnown inb)
        as'' <- mapM f as
        d'' <- fmapM g d
        t' <- dosub inb t
        return ECase { eCaseScrutinee = e, eCaseType = t', eCaseBind = b, eCaseAlts = as'', eCaseDefault = d''} -- XXX     -- we duplicate code so continue for next renaming pass before going further.
    doCase e t b as@(Alt (LitCons n _ _) _:_) (Just d) inb | Just ss <- getSiblings (so_dataTable sopts) n, length ss <= length as = do
        mtick "E.Simplify.case-no-default"
        doCase e t b as Nothing inb
    doCase e t b as (Just d) inb | te /= tWorld__, (ELit (LitCons cn _ _)) <- followAliases dt te, Just Constructor { conChildren = Just cs } <- getConstructor cn dt, length as == length cs - 1 || (False && length as < length cs && isAtomic d)  = do
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
        ec <- dosub inb emptyCase { eCaseScrutinee = e, eCaseType = t, eCaseBind = b, eCaseAlts = as ++ ls' }
        return ec { eCaseScrutinee = e }
        --doCase e t b (as ++ ls') Nothing inb
        where
        te = getType b
        dt = (so_dataTable sopts)
    doCase e _ b [] (Just d) inb | not (isLifted e || isUnboxed (getType e)) = do
        mtick "E.Simplify.case-unlifted"
        b' <- nname b inb
        d' <- f d (insertDoneSubst b (EVar b') (insertInScope (tvrIdent b') (fixInline finalPhase b' $ isBoundTo Many e) inb))
        return $ eLet b' e d'
    -- atomic unboxed values may be substituted or discarded without replicating work or affecting program semantics.
    doCase e _ b [] (Just d) inb | isUnboxed (getType e), isAtomic e = do
        mtick "E.Simplify.case-atomic-unboxed"
        f d (insertDoneSubst b e inb) -- minsert (tvrIdent b) (Done e) sub) inb
        --f d (minsert (tvrIdent b) (Susp e sub) sub) inb
    doCase e _ TVr { tvrIdent = 0 } [] (Just d) inb | isOmittable inb e = do
        mtick "E.Simplify.case-omittable"
        f d inb
    doCase (EVar v) _ b [] (Just d) inb | Just (NotAmong _) <-  mlookup (tvrIdent v) (envInScope inb)  = do
        mtick "E.Simplify.case-evaled"
        d' <- f d (insertDoneSubst b (EVar v) inb) -- minsert (tvrIdent b) (Done (EVar v)) sub) inb
        return d'
    doCase e _ b [] (Just (EVar v')) inb | b == v' = do
        mtick "E.Simplify.case-trailing"
        return e
    doCase scrut _ v [] (Just sc@ECase { eCaseScrutinee = EVar v'} ) inb | v == v', tvrIdent v `notMember` (freeVars (caseBodies sc) :: IdSet)  = do
        mtick "E.Simplify.case-default-case"
        doCase scrut (eCaseType sc) (eCaseBind sc) (eCaseAlts sc) (eCaseDefault sc) inb
    --    f sc { eCaseScrutinee = scrut } sub inb
    doCase e t b as d inb = do
        b' <- nname b inb
        (ids,b') <- case (e,tvrIdent b') of
            (EVar v,0) -> do
                nn <- newName
                b' <- return b' { tvrIdent = nn }
                return $ (insertInScope (tvrIdent v) (isBoundTo Many (EVar b')),b')
            (EVar v,_) -> return $ (insertDoneSubst b (EVar b') . insertInScope (tvrIdent v) (isBoundTo Many (EVar b')),b')
            _ -> return $ (insertDoneSubst b (EVar b'),b')
        let dd e' = f e' ( ids $ envInScope_u (newinb `union`) inb) where
                na = NotAmong [ n | Alt (LitCons n _ _) _ <- as]
                newinb = fromList [ (n,na) | EVar (TVr { tvrIdent = n }) <- [EVar b']]
            da (Alt (LitInt n t) ae) = do
                t' <- dosub inb t
                let p' = LitInt n t'
                e' <- f ae (ids $ mins e (patToLitEE p') inb)
                return $ Alt p' e'
            da (Alt (LitCons n ns t) ae) = do
                t' <- dosub inb t
                ns' <- mapM (\v -> nname v inb) ns
                let p' = LitCons n ns' t'
                    nsub =  [ (n,Done (EVar t))  | TVr { tvrIdent = n } <- ns | t <- ns' ]
                    ninb = fromList [ (n,NotKnown)  | TVr { tvrIdent = n } <- ns' ]
                e' <- f ae (ids $ substAddList nsub (envInScope_u (ninb `union`) $ mins e (patToLitEE p') inb))
                return $ Alt p' e'
            --mins (EVar v) e = envInScope_u (minsert (tvrIdent v) (isBoundTo Many e))
            mins _ e | 0 `notMember` (freeVars e :: IdSet) = insertInScope (tvrIdent b') (isBoundTo Many e)
            mins _ _ = id
            --mins _ _ = id

        d' <- fmapM dd d
        as' <- mapM da as
        t' <- dosub inb t
        return ECase { eCaseScrutinee = e, eCaseType = t', eCaseBind =  b', eCaseAlts = as', eCaseDefault = d'}

    isOmittable _ ELit {} = True
    isOmittable _ EPi {} = True
    isOmittable _ ELam {} = True
    isOmittable _ (EPrim (APrim p _) _ _) = primIsConstant p
    isOmittable inb (EVar v) = case mlookup (tvrIdent v) (envInScope inb) of
        Just IsBoundTo { bindingE = e } | not (isEVar e) -> isOmittable inb e
        Just (NotAmong _) -> True
        _ -> False
    isOmittable _ _ = False

    doConstCase :: {- Out -} Lit E E -> InE -> InTVr -> [Alt E] -> Maybe InE -> Env -> SM m OutE
    doConstCase l t b as d inb = do
        t' <- dosub inb t
        mr <- match l as (b,d)
        case mr of
            Just (bs,e) -> do
                let bs' = [ x | x@(TVr { tvrIdent = n },_) <- bs, n /= 0]
                binds <- mapM (\ (v,e) -> nname v inb >>= return . (,,) e v) bs'
                e' <- f e (substAddList [ (n,Done $ EVar nt) | (_,TVr { tvrIdent = n },nt) <- binds] $ envInScope_u (fromList [ (n,isBoundTo Many e) | (e,_,TVr { tvrIdent = n }) <- binds] `union`) inb)
                return $ eLetRec [ (v,e) | (e,_,v) <- binds ] e'
            Nothing -> do
                return $ EError ("match falls off bottom: " ++ pprint l) t'

    match m@(LitCons c xs _) ((Alt (LitCons c' bs _) e):rs) d@(b,_) | c == c' = do
        mtick (toAtom $ "E.Simplify.known-case." ++ show c )
        return $ Just ((b,ELit m):(zip bs xs),e)
         | otherwise = match m rs d
    match m@(LitInt x _) ((Alt (LitInt y _) e):rs) d@(b,_) | x == y = do
        mtick (toAtom $ "E.Simplify.known-case." ++ show x)
        return $ Just ([(b,ELit m)],e)
         | otherwise = match m rs d
    match l [] (b,Just e) = do
        mtick (toAtom "E.Simplify.known-case._")
        return $ Just ([(b,ELit l)],e)
    --match m [] (_,Nothing) = error $ "End of match: " ++ show m
    match m [] (_,Nothing) = do
        mtick (toAtom "E.Simplify.known-case.unmatch")
        return Nothing
    match m as d = error $ "Odd Match: " ++ show ((m,getType m),as,d)


    applyRule :: OutTVr -> [OutE] -> Env -> SM m (Maybe (OutE,[OutE]))
    applyRule v xs inb  = do
        z <- builtinRule v xs
        let lup x = case mlookup x (envInScope inb) of
                Just IsBoundTo { bindingE = e } -> Just e
                _ -> Nothing
        case z of
            Nothing | fopts FO.Rules -> applyRules lup (Info.fetch (tvrInfo v)) xs
            x -> return x
    h :: OutE -> [OutE] -> Env -> SM m OutE
    h (EVar v) xs' inb = do
        z <- applyRule v xs' inb
        case z of
            (Just (x,xs)) -> didInline inb x xs  -- h x xs inb
            _ -> case mlookup (tvrIdent v) (envInScope inb) of
                Just IsBoundTo { inlineForced = ForceNoinline } -> appVar v xs'
                Just IsBoundTo { bindingOccurance = Once } -> error "IsBoundTo: Once"
                Just IsBoundTo { bindingE = e, bindingAtomic = True }  -> do
                    mtick  (toAtom $ "E.Simplify.inline.atomic/{" ++ tvrShowName v  ++ "}")
                    didInline inb e xs'
                Just IsBoundTo { bindingE = e, inlineForced = ForceInline } | someBenefit v e xs' -> do
                    mtick  (toAtom $ "E.Simplify.inline.Forced/{" ++ tvrShowName v  ++ "}")
                    didInline inb e xs'
                Just IsBoundTo { bindingE = e } | forceInline v, someBenefit v e xs' -> do
                    mtick  (toAtom $ "E.Simplify.inline.forced/{" ++ tvrShowName v  ++ "}")
                    didInline inb e xs'
                Just IsBoundTo { bindingOccurance = OnceInLam, bindingE = e, bindingCheap = True } | someBenefit v e xs' -> do
                    mtick  (toAtom $ "E.Simplify.inline.OnceInLam/{" ++ showName (tvrIdent v)  ++ "}")
                    didInline inb e xs'
                Just IsBoundTo { bindingOccurance = ManyBranch, bindingE = e } | multiInline v e xs' -> do
                    mtick  (toAtom $ "E.Simplify.inline.ManyBranch/{" ++ showName (tvrIdent v)  ++ "}")
                    didInline inb  e xs'
                Just IsBoundTo { bindingOccurance = Many, bindingE = e, bindingCheap = True } | multiInline v e xs' -> do
                    mtick  (toAtom $ "E.Simplify.inline.Many/{" ++ showName (tvrIdent v)  ++ "}")
                    didInline inb  e xs'
                Just _ -> appVar v xs'
                Nothing  -> appVar v xs'
                -- Nothing | tvrIdent v `Set.member` exports -> app (EVar v,xs')
                -- Nothing -> error $ "Var not in scope: " ++ show v
    h e xs' inb = do app (e,xs')
    didInline :: Env -> OutE -> [OutE] -> SM m OutE
    didInline inb z zs = do
        e <- app (z,zs)
        return e
        --go e inb
    appVar v xs = do
        me <- etaExpandAp (so_dataTable sopts) v xs
        case me of
            Just e -> return e
            Nothing -> app (EVar v,xs)

    app (e,[]) = return e
    app (e,xs) = app' e xs

    app' (ELit (LitCons n xs t@EPi {})) (a:as)  = do
        mtick (toAtom $ "E.Simplify.typecon-reduce.{" ++ show n ++ "}" )
        app (ELit (LitCons n (xs ++ [a]) (eAp t a)),as)
    app' ec@ECase {} xs = do
        mticks (length xs) (toAtom "E.Simplify.case-application")
        let f e = app' e xs
        ec' <- caseBodiesMapM f ec
        let t = foldl eAp (eCaseType ec') xs
        return ec' { eCaseType = t }
    app' (ELetRec ds e) xs = do
        mticks (length xs) (toAtom "E.Simplify.let-application")
        e' <- app' e xs
        return $ eLetRec ds e'
    app' (EError s t) xs = do
        mticks (length xs) (toAtom "E.Simplify.error-application")
        return $ EError s (foldl eAp t xs)
    app' e as = do
        return $ foldl EAp e as
    doDs ds inb = do
        addNames $ map (tvrIdent . fst) ds
        let z :: (InTVr,InE) -> SM m (Id,Occurance,OutTVr,InE)
            z (t,EVar t') | t == t' = do    -- look for simple loops and replace them with errors.
                t'' <- nname t inb
                mtick $ "E.Simplify.<<loop>>.{" ++ showName (tvrIdent t) ++ "}"
                return (tvrIdent t,Many,t'',EError "<<loop>>" (getType t))
            z (t,e) = do
                t' <- nname t inb
                case Info.lookup (tvrInfo t) of
                    _ | forceNoinline t -> return (tvrIdent t,LoopBreaker,t',e)
                    Just Once -> return (tvrIdent t,Once,error $ "Once: " ++ show t,e)
                    Just n -> return (tvrIdent t,n,t',e)
                    -- We don't want to inline things we don't have occurance info for because they might lead to an infinite loop. hopefully the next pass will fix it.
                    Nothing -> return (tvrIdent t,LoopBreaker,t',e)
                    -- Nothing -> error $ "No Occurance info for " ++ show t
            w :: [(Id,Occurance,OutTVr,InE)] -> Env -> [(OutTVr,OutE)] -> SM m ([(OutTVr,OutE)],Env)
            w ((t,Once,t',e):rs) inb ds = do
                mtick $ "E.Simplify.inline.Once/{" ++ showName t ++ "}"
                w rs inb ds -- (minsert t (Susp e sub) sub) inb ds
            w ((t,n,t',e):rs) inb ds = do

                let inb' = case isForced of
                        ForceInline -> (cacheSubst $ envInScope_u (fmap nogrowth) inb)
                        _ -> inb
                    isForced = calcForced finalPhase t'
                    nogrowth IsBoundTo { bindingAtomic = False } = NotKnown
                    nogrowth x = x
                e' <- f e inb'
                let ibt = fixInline finalPhase t' $ isBoundTo n e'
                case (bindingAtomic ibt,inlineForced ibt) of
                    (True,f) | f /= ForceNoinline -> do
                        --when (n /= Unused) $ mtick $ "E.Simplify.inline.Atomic.{" ++ showName t ++ "}"
                        w rs (insertDoneSubst' t e' . insertInScope (tvrIdent t') ibt $ inb) ((t',e'):ds)
                    _ -> w rs (insertInScope (tvrIdent t') ibt inb)  ((t',e'):ds)
--                w rs (cacheSubst $ envInScope_u (minsert (tvrIdent t') (isBoundTo n e')) inb) ((t',e'):ds)
                --w rs (if n /= LoopBreaker then (cacheSubst $ envInScope_u (minsert (tvrIdent t') (isBoundTo n e')) inb) else inb) ((t',e'):ds)
                --case isAtomic e' && n /= LoopBreaker && t `notMember` exportedSet  of
                    --True -> do
                    --    when (n /= Unused) $ mtick $ "E.Simplify.inline.Atomic.{" ++ showName t ++ "}"
                    --    w rs (insertDoneSubst' t e' . envInScope_u (minsert (tvrIdent t') (isBoundTo n e')) $ inb) ds -- ((t',e'):ds) -- (minsert t (Done e') sub) (envInScope_u (minsert (tvrIdent t') (isBoundTo n e')) inb) ((t',e'):ds)
                    --False -> w rs (if n /= LoopBreaker then (cacheSubst $ envInScope_u (minsert (tvrIdent t') (isBoundTo n e')) inb) else inb) ((t',e'):ds)
                --    _ -> w rs (cacheSubst $ envInScope_u (minsert (tvrIdent t') (isBoundTo n e')) inb)  ((t',e'):ds)
            w [] inb ds = return (ds,inb)
        s' <- mapM z ds
        let sub'' = fromList [ (t,susp e sub'') | (t,Once,_,e) <- s'] `union` fromList [ (t,Done (EVar t'))  | (t,n,t',_) <- s', n /= Once] `union` envSubst inb
        (ds',inb') <- w s'  (cacheSubst (envSubst_s sub'' $ envInScope_u (fromList [ (tvrIdent t',NotKnown) | (_,n,t',_) <- s', n /= Once] `union`) inb)) []
        ds' <- sequence [ etaExpandDef' (so_dataTable sopts) t e | (t,e) <- ds']
        return (ds',inb')



someBenefit _ e _ | isAtomic e = True
someBenefit _ ELit {} _ = True
someBenefit _ EPi {} _ = True
someBenefit _ EPrim {} _ = True
someBenefit v (ELetRec ds e) xs | someBenefit v e xs = True
someBenefit _v ECase {} (_:_) = True
someBenefit _ e xs | f e xs = True where
    f (ELam _ e) (x:xs) = f e xs
    f ELam {} [] = False
    f _ _ = True
someBenefit v e xs = False

multiInline _ e xs | isSmall (f e xs) = True  where -- should be noSizeIncrease
    f e [] = e
    f (ELam _ e) (_:xs) = f e xs
    f e xs = foldl EAp e xs
multiInline v e xs | not (someBenefit v e xs) = False
multiInline _ e xs = length xs + 2 >= (nsize + if safeToDup b then negate 4 else 0)  where
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

