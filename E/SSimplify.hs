module E.SSimplify(
    Occurance(..),
    simplifyE,
    collectOccurance',
    programPruneOccurance,
    programSSimplify,
    programSSimplifyPStat,
    SimplifyOpts(..)
    ) where

import Util.RWS
import Control.Monad.Identity
import Control.Monad.Writer
import Control.Monad.Reader
import Data.FunctorM
import Data.Typeable
import Data.Monoid
import List hiding(delete,union,insert)
import Debug.Trace
import Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set

import Atom
import C.Prims
import DataConstructors
import Doc.PPrint
import E.Annotate
import E.E
import E.Eta
import E.Inline
import E.PrimOpt
import E.Program
import E.Rules
import E.Subst
import E.Traverse(runRename)
import E.TypeCheck
import E.Values
import GenUtil
import Info.Types
import Name.Id
import Name.Name
import Name.VConsts
import Number
import Options
import Stats hiding(new,print,Stats,singleton)
import Support.CanType
import Support.FreeVars
import Util.Graph
import Util.HasSize
import Util.NameMonad
import Util.ReaderWriter
import Util.SetLike as S
import qualified E.Demand as Demand
import qualified FlagDump as FD
import qualified FlagOpts as FO
import qualified Info.Info as Info
import qualified Util.Seq as Seq

type Bind = (TVr,E)

data Occurance =
    Unused        -- ^ unused means a var is not used at the term level, but might be at the type level
    | Once        -- ^ Used at most once not inside a lambda or as an argument
    | OnceInLam   -- ^ used once inside a lambda
    | ManyBranch  -- ^ used once in several branches
    | Many        -- ^ used many or an unknown number of times
    | LoopBreaker -- ^ chosen as a loopbreaker, never inline
    deriving(Show,Eq,Ord)

data UseInfo = UseInfo {
    useOccurance :: !Occurance,   -- ^ occurance Info
    minimumArgs  :: !Int          -- ^ minimum number of args that are ever passed to this function (if used)
    }
    deriving(Show,Eq,Ord,Typeable)

noUseInfo = UseInfo { useOccurance = Many, minimumArgs = 0 }
notUsedInfo = UseInfo { useOccurance = Unused, minimumArgs = maxBound }

programPruneOccurance :: Program -> Program
programPruneOccurance prog =
    let dsIn = programDs (runIdentity $ programMapBodies (return . subst (tVr (-1) Unknown) Unknown) prog)
        (dsIn',(OMap fvs,uids)) = runReaderWriter (unOM $ collectDs dsIn mempty) (fromList $ map tvrIdent $ progEntryPoints prog)
    in (programSetDs dsIn' prog) { progFreeIds = idMapToIdSet fvs, progUsedIds = uids }


newtype OM a = OM (ReaderWriter IdSet (OMap,IdSet) a)
    deriving(Monad,Functor,MonadWriter (OMap,IdSet),MonadReader IdSet)

unOM (OM a) = a

newtype OMap = OMap (IdMap UseInfo)
   deriving(HasSize,SetLike,BuildSet (Id,UseInfo),MapLike Id UseInfo,Show,IsEmpty,Eq,Ord)

instance Monoid OMap where
    mempty = OMap mempty
    mappend (OMap a) (OMap b) = OMap (andOM a b)


maybeLetRec [] e = e
maybeLetRec ds e = ELetRec ds e

-- | occurance analysis

grump :: OM a -> OM (a,OMap)
grump m = fmap ( \ (x, (y,z)) -> (x,y) ) $ censor (\ (_,y) -> (mempty,y)) (listen m)

collectOccurance' :: E -> (E,IdMap UseInfo)
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
    f (ELit lc@LitCons { litArgs = as, litType = t }) = arg $ do
        t <- f t
        as <- mapM f as
        return (ELit lc { litArgs = as, litType = t })
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
        case all (getProperty prop_ONESHOT) as of
            True ->  tell $ (foldr mdelete avs (map tvrIdent as),fromList $ map tvrIdent as)
            False -> tell $ (inLam $ foldr mdelete avs (map tvrIdent as),fromList $ map tvrIdent as)
        return (foldr ELam b' as'')
    f e | Just (x,t) <- from_unsafeCoerce e  = do x <- f x ; t <- (arg (f t)); return (prim_unsafeCoerce x t)
    f (EVar tvr@TVr { tvrIdent = n, tvrType =  t}) = do
        tell $ (msingleton n UseInfo { useOccurance = Once, minimumArgs = 0 },mempty)
        t <- arg (f t)
        return $ EVar tvr { tvrType = t }
    f e | (EVar tvr@TVr { tvrIdent = n, tvrType = t},xs@(_:_)) <- fromAp e = do
        tell $ (msingleton n UseInfo { useOccurance = Once, minimumArgs = length xs },mempty)
        t <- arg (f t)
        xs <- arg (mapM f xs)
        return (foldl EAp (EVar tvr { tvrType = t}) xs)
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
        return $ caseUpdate ec { eCaseScrutinee = scrut', eCaseAlts = as', eCaseBind = annbind' fidm b, eCaseType = ct, eCaseDefault = d'}
    f ELetRec { eDefs = ds, eBody = e } = do
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
        let mm (OMap mp,y) = (OMap $ fmap (const noUseInfo) mp,y)
        censor mm m
    ftvr tvr = do
        tt <- f (tvrType tvr)
        return tvr { tvrType = tt }

-- delete any occurance info for non-let-bound vars to be safe
annb' tvr = tvrInfo_u (Info.delete noUseInfo) tvr
annbind' idm tvr = case mlookup (tvrIdent tvr) idm of
    Nothing -> annb' tvr { tvrIdent = 0 }
    Just _ -> annb' tvr

-- add ocucrance info
annbind idm tvr = case mlookup (tvrIdent tvr) idm of
    Nothing -> annb notUsedInfo tvr { tvrIdent = 0 }
    Just x -> annb x tvr
annb x tvr = tvrInfo_u (Info.insert x) tvr

mapLitBinds f lc@LitCons { litArgs = es } = lc { litArgs = map f es }
mapLitBinds f (LitInt e t) = LitInt e t
mapLitBindsM f lc@LitCons { litArgs = es } = do
    es <- mapM f es
    return lc { litArgs = es }
mapLitBindsM f (LitInt e t) = return $  LitInt e t

collectBinding :: Bind -> OM (Bind,OMap)
collectBinding (t,e) = do
    e' <- collectOccurance e
    let rvars = freeVars (Info.fetch (tvrInfo t) :: ARules) :: IdSet
        romap = OMap (idSetToIdMap (const noUseInfo) rvars)
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
            True -> noUseInfo { useOccurance = LoopBreaker }
            False -> case  (tvrIdent t `member` exp) of
                True -> noUseInfo
                False | Just r <- mlookup (tvrIdent t) fids -> r
        ds''' = [ (calcStrictInfo $ annbind ffids t ,e) | ((t,e),_) <- ds'']
        froo (t,e) = ((t {tvrType = t' },e),fvs) where
            (t',fvs) = collectOccurance' (tvrType t)
        (ds'''',nfids) = unzip $ map froo ds'''
        nfid' = fmap (const noUseInfo) (mconcat nfids)
    tell $ ((OMap $ nfid' `andOM` fids) S.\\ ffids,fromList (map (tvrIdent . fst) ds''''))
    return (ds'''')

-- TODO this should use the occurance info
-- loopFunc t _ | getProperty prop_PLACEHOLDER t = -100  -- we must not choose the placeholder as the loopbreaker
loopFunc t e = negate (baseInlinability t e)


inLam (OMap om) = OMap (fmap il om) where
    il ui@UseInfo { useOccurance = Once } = ui { useOccurance = OnceInLam }
    il ui = ui { useOccurance = Many }

--andOM :: IdMap UseInfo -> IdMap UseInfo -> IdMap UseInfo
andOM x y = munionWith andOcc x y
andOcc UseInfo { useOccurance = Unused } x = x
andOcc x UseInfo { useOccurance = Unused } = x
andOcc x y = UseInfo { useOccurance = Many, minimumArgs = min (minimumArgs x) (minimumArgs y) }

orMaps ms = OMap $ fmap orMany $ foldl (munionWith (++)) mempty (map (fmap (:[])) (map unOMap ms)) where
    unOMap (OMap m) = m

orMany [] = error "empty orMany"
orMany xs = f (filter ((/= Unused) . useOccurance) xs) where
    f [] = notUsedInfo
    f [x] = x
    f xs = if all good (map useOccurance xs) then ui ManyBranch else ui Many where
        good Once = True
        good ManyBranch = True
        good _ = False
        ui x = UseInfo { minimumArgs =  minimum (map minimumArgs xs), useOccurance = x }



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
    bindingOccurance = useOccurance o,
    bindingE = e,
    bindingCheap = isCheap e,
    inlineForced = if useOccurance o == LoopBreaker then ForceNoinline else NotForced,
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
    let props = getProperties v in
        case (forceNoinline props,finalPhase,forceInline props) of
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
susp e sub =  Susp e sub Unknown -- (substMap'' (fmap mkSubst sub) e)

insertSuspSubst :: TVr -> InE -> Env -> Env
insertSuspSubst t e env = insertSuspSubst' (tvrIdent t) e env

insertSuspSubst' :: Id -> InE -> Env -> Env
insertSuspSubst' 0 _e env = env
insertSuspSubst' t e env = cacheSubst env { envSubst = minsert t (susp e (envSubst env)) (envSubst env) }

insertRange 0 e env = env
insertRange t e env = cacheSubst env { envSubst = minsert t e (envSubst env) }

insertDoneSubst :: TVr -> OutE -> Env -> Env
insertDoneSubst t e env = insertDoneSubst' (tvrIdent t) e env

insertDoneSubst' :: Id -> OutE -> Env -> Env
insertDoneSubst' 0 _e env = env
insertDoneSubst' t e env = cacheSubst env { envSubst = minsert t (Done e) (envSubst env) }


insertInScope :: Id -> Binding -> Env -> Env
insertInScope 0 _b env = env
insertInScope t b env = cacheSubst env { envInScope = minsert t b (envInScope env) }

substLookup :: Id -> SM (Maybe Range)
substLookup id = SM $ ask >>= return . mlookup id . envSubst

substAddList ls env = cacheSubst env { envSubst = fromList ls `union` envSubst env }


applySubst :: Subst -> IdMap a -> IdMap (Maybe OutE)
applySubst s nn = applySubst' s where
    nn' = fmap (const Nothing) s `mappend` fmap (const Nothing) nn
    applySubst' s = (tm `mappend` nn') where
        tm = fmap g s
        g (Done e) = Just e
        g (Susp e s' _) = Just $ substMap'' (applySubst' s') e

evalRange :: Range -> SM OutE
evalRange (Done e) = return e
evalRange (Susp e s _) = localEnv (envSubst_s s)  $ dosub e

cacheSubst env = env { envCachedSubst = applySubst (envSubst env) (envInScope env) }

dosub :: InE -> SM OutE
dosub e = ask >>= \inb ->  coerceOpt return (substMap'' (envCachedSubst inb) e)

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

data Cont =
    ApplyTo {
        contArg  :: Range,
        contNext :: Cont
        }
    | LazyContext TVr  -- the RHS of a let statement
    | StartContext
    | ArgContext
    | Coerce Range Cont
    | Scrutinee {
        contExamined :: Bool  -- ^ whether the result is actually examined, or just bound to a variable
        }
    deriving(Show)

isApplyTo ApplyTo {} = True
isApplyTo _ = False

simplifyDs :: forall m . MonadStats m => Program -> SimplifyOpts -> [(TVr,E)] -> m [(TVr,E)]
simplifyDs prog sopts dsIn = ans where
    finalPhase = so_finalPhase sopts
    ans = do
        let ((dsOut,_),stats) = runSM initialB doit
        mtickStat stats
        return dsOut
    exportedSet = fromList $ map tvrIdent (progEntryPoints prog) :: IdSet
    getType e = infertype (so_dataTable sopts) e
    initialB = let
            bb (t,(_,e)) | isFullyConst e = [(t,Done e)]
            bb _ = []
        in cacheSubst mempty { envSubst = fromList $ concatMap bb  (massocs $ so_boundVars sopts),  envInScope =  fmap (\ (t,e) -> fixInline finalPhase t $ isBoundTo noUseInfo e) (so_boundVars sopts) }
    doit = do
        smAddNamesIdSet (progUsedIds prog)
        smAddBoundNamesIdSet (progFreeIds prog)
        smAddBoundNamesIdMap (so_boundVars sopts)
        doDs dsIn
    go :: E -> SM E
    go e = do
        let (e',_) = collectOccurance' e
        localEnv (envSubst_s mempty) $ f StartContext e'
    --mtick a = Stats.mtick $ trace (show a) a
    makeRange b = do
        sub <- asks envSubst
        return $ susp b sub
    f :: Cont -> InE -> SM OutE
    --f cont e | trace (take 20 (show cont) ++ " - " ++ take 40 (show e)) False = undefined
    f ArgContext e = dosub e
    f c (EAp a b) = do
        b' <- makeRange b
        f ApplyTo { contArg = b', contNext = c } a
    f (ApplyTo rng cont) (ELam t b) = do
        addBoundNames [tvrIdent t]
        mtick (toAtom "E.Simplify.f-beta-reduce")
        localEnv (insertRange (tvrIdent t) rng) $ f cont b
    f (ApplyTo rng cont) (EPi t b) = do
        addBoundNames [tvrIdent t]
        mtick (toAtom "E.Simplify.f-pi-reduce")
        localEnv (insertRange (tvrIdent t) rng) $ f cont b
    f cont (EVar v) = do
        z <- substLookup (tvrIdent v)
        case z of
            Just (Done e) -> done cont e
            Just (Susp e s _) -> localEnv (envSubst_s s)  $ f cont e
            Nothing -> done cont (EVar v)
    f (Coerce t cont) (EError s _) = evalRange t >>= \t' -> done cont (EError s t')
    f (Coerce t cont) (ELit (LitInt n _)) = evalRange t >>= \t' -> done cont (ELit (LitInt n t'))
    f cont v | Just (e,t) <- from_unsafeCoerce v =
        makeRange t >>= \t' -> f (g t' cont) e where g t' (Coerce _ cont) = Coerce t' cont
    f cont ep@EPrim {} = do
        ep' <- primOpt' (so_dataTable sopts) ep
        ep'' <- dosub ep'
        done cont ep''
    f cont e@ELit {} = dosub e >>= done cont
    f cont (ELam v e)  = do
        addNames [tvrIdent v]
        v' <- nname v
        e' <- localEnv (insertDoneSubst v (EVar v') . insertInScope (tvrIdent v') NotKnown) $ f StartContext e
        done cont $ ELam v' e'
    f cont e@(EPi (TVr { tvrIdent = n }) _) = do
        addNames [n]
        e' <- dosub e
        done cont e'
    f cont (EError s t) = (EError s `fmap` dosub t) >>= done cont
    f cont ec@ECase { eCaseScrutinee = e, eCaseBind = b, eCaseAlts = as, eCaseDefault = d} = do
        addNames (map tvrIdent $ caseBinds ec)
        e' <- f (Scrutinee (not $ null as)) e
        ec' <- doCase e' (eCaseType ec) b as d
        done cont ec'
    f cont ELetRec { eDefs = ds@(_:_), eBody =  e } = do
        (ds',inb') <- doDs ds
        e' <- localEnv (const inb') $ f cont e
        res <- case ds' of
            [(t,e)] | worthStricting e, Just (Demand.S _) <- Info.lookup (tvrInfo t), not (getProperty prop_CYCLIC t) -> do
                mtick "E.Simplify.strictness.let-to-case"
                return $ eStrictLet t e e'
            [(t,ec@ECase { eCaseScrutinee = sc@(EPrim (APrim p _) _ _), eCaseAlts = [], eCaseDefault = Just def })] | primEagerSafe p && not (getProperty prop_CYCLIC t) -> do
                mtick "E.Simplify.strictness.cheap-eagerness.def"
                return $ caseUpdate ec { eCaseDefault = Just $ ELetRec [(t,def)] e', eCaseType = getType e' }
            [(t,ec@ECase { eCaseScrutinee = sc@(EPrim (APrim p _) _ _), eCaseAlts = [Alt c def], eCaseDefault = Nothing })] | primEagerSafe p && not (getProperty prop_CYCLIC t) -> do
                mtick "E.Simplify.strictness.cheap-eagerness.con"
                return $ caseUpdate ec { eCaseAlts = [Alt c (ELetRec [(t,def)] e')], eCaseType = getType e' }
            _ -> do
                let fn ds (ELetRec { eDefs = ds', eBody = e}) | not (hasRepeatUnder fst (ds ++ ds')) = fn (ds' ++ ds) e
                    fn ds e = f ds (Set.fromList $ fsts ds) [] False where
                        f ((t,ELetRec { eDefs = ds', eBody = e}):rs) us ds b | all (not . (`Set.member` us)) (fsts ds') = f ((t,e):rs) (Set.fromList (fsts ds') `Set.union` us) (ds':ds) True
                        f (te:rs) us ds b = f rs us ([te]:ds) b
                        f [] _ ds True = fn (concat ds) e
                        f [] _ ds False = (concat ds,e)
                let (ds'',e'') = fn ds' e'
                when (flint && hasRepeatUnder fst ds'') $ fail "hasRepeats!"
                mticks  (length ds'' - length ds') (toAtom $ "E.Simplify.let-coalesce")
                return $ eLetRec ds'' e''
        done StartContext res
    f cont e = dosub e >>= done cont
--    f cont e | isApplyTo cont = els
--             | otherwise = tryEta
--            where
--            els = do
--                x' <- dosub e
--                done cont x'
--            tryEta = do
--                eed <- etaExpandDef (so_dataTable sopts) 0 tvr { tvrIdent = 0 } e
--                case eed of
--                    Just (_,e) -> f cont e
--                    Nothing -> els

    g :: InE -> SM OutE
    --g e | trace ("g: " ++ take 20 (show e)) False = undefined
    g e = error $ "SSimplify.simplify.g: " ++ show e ++ "\n" ++ pprint e
    showName t | odd t || dump FD.EVerbose = tvrShowName (tVr t Unknown)
             | otherwise = "(epheremal)"

    nname tvr@(TVr { tvrIdent = n, tvrType =  t})  = do
        t' <- dosub t
        inb <- ask
        let t'' = substMap'' (fmap (\ IsBoundTo { bindingE = e } -> Just e) $ mfilter isIsBoundTo (envInScope inb)) t'
        n' <- if n == 0 then return 0 else uniqueName n
        return $ tvr { tvrIdent = n', tvrType =  t'' }
    -- TODO - case simplification
    doCase :: OutE -> InE -> InTVr -> [Alt InE] -> (Maybe InE) ->  SM OutE
    doCase ELetRec { eDefs = ds, eBody = e} t b as d = do
        mtick "E.Simplify.let-from-case"
        e' <- doCase e t b as d
        return $ substLet' ds e'

    --doCase (EVar v) t b as d inb |  Just IsBoundTo { bindingE = ELit l } <- mlookup (tvrIdent v) (envInScope inb)  = doConstCase l t  b as d inb
    doCase (ELit l) t b as d  = doConstCase l t b as d

    --doCase (EVar v) t b as d inb | Just IsBoundTo { bindingE = e } <- mlookup (tvrIdent v) (envInScope inb) , isBottom e = do
    --    mtick "E.Simplify.case-of-bottom'"
    --    t' <- dosub inb t
    --    return $ prim_unsafeCoerce (EVar v) t'
    doCase e t b as d | isBottom e = do
        mtick "E.Simplify.case-of-bottom"
        t' <- dosub t
        return $ prim_unsafeCoerce e t'

    doCase ic@ECase { eCaseScrutinee = e, eCaseBind =  b, eCaseAlts =  as, eCaseDefault =  d } t b' as' d' | length (filter (not . isBottom) (caseBodies ic)) <= 1 || all whnfOrBot (caseBodies ic)  || all whnfOrBot (caseBodies emptyCase { eCaseAlts = as', eCaseDefault = d'} )  = do
        mtick (toAtom "E.Simplify.case-of-case")
        let f (Alt l e) = do
                e' <- localEnv (envInScope_u (fromList [ (n,NotKnown) | TVr { tvrIdent = n } <- litBinds l ] `union`)) $ doCase e t b' as' d'
                return (Alt l e')
            --g e >>= return . Alt l
            g x = localEnv (insertInScope (tvrIdent b) NotKnown) $ doCase x t b' as' d'
        as'' <- mapM f as
        d'' <- fmapM g d
        t' <- dosub t
        return $ caseUpdate ECase { eCaseScrutinee = e, eCaseType = t', eCaseBind = b, eCaseAlts = as'', eCaseDefault = d''} -- XXX     -- we duplicate code so continue for next renaming pass before going further.
    doCase e t b as@(Alt LitCons { litName = n } _:_) (Just d) | Just ss <- getSiblings (so_dataTable sopts) n, length ss <= length as = do
        mtick "E.Simplify.case-no-default"
        doCase e t b as Nothing
    doCase e t b as (Just d) | te /= tWorld__, (ELit LitCons { litName = cn }) <- followAliases dt te, Just Constructor { conChildren = Just cs } <- getConstructor cn dt, length as == length cs - 1 || (False && length as < length cs && isAtomic d)  = do
        let ns = [ n | Alt ~LitCons { litName = n } _ <- as ]
            ls = filter (`notElem` ns) cs
            ff n = do
                con <- getConstructor n dt
                let g t = do
                        n <- newName
                        return $ tVr n t
                ts <- mapM g (slotTypes (so_dataTable sopts) n te)
                let wtd = ELit $ updateLit (so_dataTable sopts) litCons { litName = n, litArgs = map EVar ts, litType = te }
                return $ Alt (updateLit (so_dataTable sopts) litCons { litName = n, litArgs = ts, litType = te }) (eLet b wtd d)
        mtick $ "E.Simplify.case-improve-default.{" ++ show (sort ls) ++ "}"
        ls' <- mapM ff ls
        --ec <- dosub $ caseUpdate emptyCase { eCaseScrutinee = e, eCaseType = t, eCaseBind = b, eCaseAlts = as ++ ls' }
        --localEnv (envSubst_s mempty) $ f StartContext (caseUpdate ec { eCaseScrutinee = e })
        doCase e t b (as ++ ls') Nothing
        where
        te = getType b
        dt = (so_dataTable sopts)
    doCase e _ b [] (Just d) | not (isLifted e || isUnboxed (getType e)) = do
        mtick "E.Simplify.case-unlifted"
        b' <- nname b
        d' <- localEnv (insertDoneSubst b (EVar b') . (insertInScope (tvrIdent b') (fixInline finalPhase b' $ isBoundTo noUseInfo e))) $ f StartContext d
        return $ eLet b' e d'
    doCase e@ELam {} _ b [] (Just d)  = do
        mtick "E.Simplify.case-lambda"
        b' <- nname b
        d' <- localEnv (insertDoneSubst b (EVar b') . (insertInScope (tvrIdent b') (fixInline finalPhase b' $ isBoundTo noUseInfo e))) $ f StartContext d
        return $ eLet b' e d'
    -- atomic unboxed values may be substituted or discarded without replicating work or affecting program semantics.
    doCase e _ b [] (Just d) | isUnboxed (getType e), isAtomic e = do
        mtick "E.Simplify.case-atomic-unboxed"
        localEnv (insertDoneSubst b e) $ f StartContext d
    --doCase e _ TVr { tvrIdent = 0 } [] (Just d) inb | isOmittable inb e = do
    --    mtick "E.Simplify.case-omittable"
    --    f d inb
    --doCase (EVar v) _ b [] (Just d) inb | Just (NotAmong _) <-  mlookup (tvrIdent v) (envInScope inb)  = do
    --    mtick "E.Simplify.case-evaled"
    --    d' <- f d (insertDoneSubst b (EVar v) inb) -- minsert (tvrIdent b) (Done (EVar v)) sub) inb
    --    return d'
    -- TODO valid only in strict context
    doCase e _ b [] (Just (EVar v')) | b == v' = do
        mtick "E.Simplify.case-trailing"
        return e
    doCase scrut _ v [] (Just sc@ECase { eCaseScrutinee = EVar v'} ) | v == v', tvrIdent v `notMember` (freeVars (caseBodies sc) :: IdSet)  = do
        mtick "E.Simplify.case-default-case"
        doCase scrut (eCaseType sc) (eCaseBind sc) (eCaseAlts sc) (eCaseDefault sc)
    doCase e t b as d = do
        b' <- nname b
        (ids,b') <- case (e,tvrIdent b') of
            (EVar v,0) -> do
                nn <- newName
                b' <- return b' { tvrIdent = nn }
                return $ (insertInScope (tvrIdent v) (isBoundTo noUseInfo (EVar b')),b')
            (EVar v,_) -> return $ (insertDoneSubst b (EVar b') . insertInScope (tvrIdent v) (isBoundTo noUseInfo (EVar b')),b')
            _ -> return $ (insertDoneSubst b (EVar b'),b')
        inb <- ask
        let dd e' = localEnv (const $ ids $ envInScope_u (newinb `union`) inb) $ f StartContext e' where
                na = NotAmong [ n | Alt LitCons { litName = n } _ <- as]
                newinb = fromList [ (n,na) | EVar (TVr { tvrIdent = n }) <- [EVar b']]
            da (Alt (LitInt n t) ae) = do
                t' <- dosub t
                let p' = LitInt n t'
                e' <- localEnv (ids . mins e (patToLitEE p')) $ f StartContext ae
                return $ Alt p' e'
            da (Alt lc@LitCons { litName = n, litArgs = ns, litType = t } ae) = do
                t' <- dosub t
                ns' <- mapM nname ns
                let p' = lc { litArgs = ns', litType = t' }
                    nsub =  [ (n,Done (EVar t))  | TVr { tvrIdent = n } <- ns | t <- ns' ]
                    ninb = fromList [ (n,NotKnown)  | TVr { tvrIdent = n } <- ns' ]
                e' <- localEnv (const $ ids $ substAddList nsub (envInScope_u (ninb `union`) $ mins e (patToLitEE p') inb)) $ f StartContext ae
                return $ Alt p' e'
            --mins (EVar v) e = envInScope_u (minsert (tvrIdent v) (isBoundTo Many e))
            mins _ e | 0 `notMember` (freeVars e :: IdSet) = insertInScope (tvrIdent b') (isBoundTo noUseInfo e)
            mins _ _ = id
            --mins _ _ = id

        d' <- fmapM dd d
        as' <- mapM da as
        t' <- dosub t
        return $ caseUpdate ECase { eCaseScrutinee = e, eCaseType = t', eCaseBind =  b', eCaseAlts = as', eCaseDefault = d'}

    isOmittable _ ELit {} = True
    isOmittable _ EPi {} = True
    isOmittable _ ELam {} = True
    isOmittable _ (EPrim (APrim p _) _ _) = primIsConstant p
    isOmittable inb (EVar v) = case mlookup (tvrIdent v) (envInScope inb) of
        Just IsBoundTo { bindingE = e } | not (isEVar e) -> isOmittable inb e
        Just (NotAmong _) -> True
        _ -> False
    isOmittable _ _ = False

    doConstCase :: {- Out -} Lit E E -> InE -> InTVr -> [Alt E] -> Maybe InE -> SM OutE
    doConstCase l t b as d = do
        t' <- dosub t
        mr <- match l as (b,d)
        inb <- ask
        case mr of
            Just (bs,e) -> do
                let bs' = [ x | x@(TVr { tvrIdent = n },_) <- bs, n /= 0]
                binds <- mapM (\ (v,e) -> nname v >>= return . (,,) e v) bs'
                e' <- localEnv (const $ substAddList [ (n,Done $ EVar nt) | (_,TVr { tvrIdent = n },nt) <- binds] $ envInScope_u (fromList [ (n,isBoundTo noUseInfo e) | (e,_,TVr { tvrIdent = n }) <- binds] `union`) inb) $ f StartContext e
                return $ eLetRec [ (v,e) | (e,_,v) <- binds ] e'
            Nothing -> do
                return $ EError ("match falls off bottom: " ++ pprint l) t'

    match m@LitCons { litName = c, litArgs = xs } ((Alt LitCons { litName = c', litArgs = bs } e):rs) d@(b,_) | c == c' = do
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


    applyRule :: OutTVr -> [OutE] -> SM (Maybe (OutE,[OutE]))
    applyRule v xs  = do
        inb <- ask
        z <- builtinRule v xs
        let lup x = case mlookup x (envInScope inb) of
                Just IsBoundTo { bindingE = e } -> Just e
                _ -> Nothing
        case z of
            Nothing | fopts FO.Rules -> applyRules lup (Info.fetch (tvrInfo v)) xs
            x -> return x
    done cont e = z cont [] where
        z (ApplyTo r cont') rs = evalRange r >>= \a -> z cont' (a:rs)
        z (Coerce t cont) rs = do
            t' <- evalRange t
            z <- h e (reverse rs)
            done cont (prim_unsafeCoerce z t')
        z _ rs = h e (reverse rs)
    h :: OutE -> [OutE] -> SM OutE
    h (EVar v) xs' = do
        inb <- ask
        z <- applyRule v xs'
        let txs = map tx xs' where
                tx (ELit l) = knowLit l
                tx EPi {} = KnowSomething
                tx (EVar v) = case mlookup (tvrIdent v) (envInScope inb) of
                    Just (NotAmong xs) -> KnowNotOneOf xs
                    Just IsBoundTo { bindingE = ELit l } -> knowLit l
                    Just IsBoundTo {} -> KnowSomething
                    _ -> KnowNothing
                tx _ = KnowNothing
                knowLit LitCons { litName = c } = KnowIsCon c
                knowLit (LitInt n _) = KnowIsNum n
        case z of
            (Just (x,xs)) -> didInline x xs  -- h x xs inb
            _ -> case mlookup (tvrIdent v) (envInScope inb) of
                Just IsBoundTo { inlineForced = ForceNoinline } -> appVar v xs'
                Just IsBoundTo { bindingOccurance = Once } -> error "IsBoundTo: Once"
                Just IsBoundTo { bindingE = e, bindingAtomic = True }  -> do
                    mtick  (toAtom $ "E.Simplify.inline.atomic/{" ++ tvrShowName v  ++ "}")
                    didInline e xs'
                Just IsBoundTo { bindingE = e, inlineForced = ForceInline } | someBenefit v e txs -> do
                    mtick  (toAtom $ "E.Simplify.inline.Forced/{" ++ tvrShowName v  ++ "}")
                    didInline e xs'
                Just IsBoundTo { bindingOccurance = OnceInLam, bindingE = e, bindingCheap = True } | someBenefit v e txs -> do
                    mtick  (toAtom $ "E.Simplify.inline.OnceInLam/{" ++ showName (tvrIdent v)  ++ "}")
                    didInline e xs'
                Just IsBoundTo { bindingOccurance = ManyBranch, bindingE = e } | multiInline v e txs -> do
                    mtick  (toAtom $ "E.Simplify.inline.ManyBranch/{" ++ showName (tvrIdent v)  ++ "}")
                    didInline e xs'
                Just IsBoundTo { bindingOccurance = Many, bindingE = e, bindingCheap = True } | multiInline v e txs -> do
                    mtick  (toAtom $ "E.Simplify.inline.Many/{" ++ showName (tvrIdent v)  ++ "}")
                    didInline e xs'
                Just _ -> appVar v xs'
                Nothing  -> appVar v xs'
                -- Nothing | tvrIdent v `Set.member` exports -> app (EVar v,xs')
                -- Nothing -> error $ "Var not in scope: " ++ show v
    h e xs' = do app (e,xs')
    didInline ::OutE -> [OutE] -> SM OutE
    didInline z zs = do
        used <- smUsedNames
        let (ne,nn) = runRename used (foldl EAp z zs)
        smAddNamesIdSet nn
        return ne
        --e <- app (z,zs)
        --return e
        --go e inb
    appVar v xs = do
        me <- etaExpandAp (so_dataTable sopts) v xs
        case me of
            Just e -> return e
            Nothing -> app (EVar v,xs)

    app (e,[]) = return e
    app (e,xs) = app' e xs

    app' (ELit lc@LitCons { litName = n, litArgs = xs, litType = EPi ta tt }) (a:as)  = do
        mtick (toAtom $ "E.Simplify.typecon-reduce.{" ++ show n ++ "}" )
        app' (ELit lc { litArgs = xs ++ [a], litType = subst ta a tt }) as
    app' (ELit LitCons { litName = n, litArgs = es, litAliasFor = Just af }) bs@(_:_) = do
        mtick (toAtom $ "E.Simplify.newtype-reduce.{" ++ show n ++ "}" )
        app' (foldl eAp af (es ++ bs)) []
--    app' (ELit (LitCons n xs t@EPi {})) (a:as)  = do
--        mtick (toAtom $ "E.Simplify.typecon-reduce.{" ++ show n ++ "}" )
--        app' (ELit (LitCons n (xs ++ [a]) (eAp t a))) as
    app' ec@ECase {} xs = do
        mticks (length xs) (toAtom "E.Simplify.case-application")
        let f e = app' e xs
        ec' <- caseBodiesMapM f ec
        let t = foldl eAp (eCaseType ec') xs
        return $ caseUpdate ec' { eCaseType = t }
    app' ELetRec { eDefs = ds, eBody = e } xs = do
        mticks (length xs) (toAtom "E.Simplify.let-application")
        e' <- app' e xs
        return $ eLetRec ds e'
    app' (EError s t) xs = do
        mticks (length xs) (toAtom "E.Simplify.error-application")
        return $ EError s (foldl eAp t xs)
    app' e as = do
        return $ foldl EAp e as
    doDs ds = do
        addNames $ map (tvrIdent . fst) ds
        let z :: (InTVr,InE) -> SM (Id,UseInfo,OutTVr,InE)
            z (t,EVar t') | t == t' = do    -- look for simple loops and replace them with errors.
                t'' <- nname t
                mtick $ "E.Simplify.<<loop>>.{" ++ showName (tvrIdent t) ++ "}"
                return (tvrIdent t,noUseInfo,t'',EError "<<loop>>" (getType t))
            z (t,e) = do
                t' <- nname t
                case Info.lookup (tvrInfo t) of
                    _ | forceNoinline t -> return (tvrIdent t,noUseInfo { useOccurance = LoopBreaker },t',e)
                    Just ui@UseInfo { useOccurance = Once } -> return (tvrIdent t,ui,error $ "Once: " ++ show t,e)
                    Just n -> return (tvrIdent t,n,t',e)
                    -- We don't want to inline things we don't have occurance info for because they might lead to an infinite loop. hopefully the next pass will fix it.
                    Nothing -> return (tvrIdent t,noUseInfo { useOccurance = LoopBreaker },t',e)
                    -- Nothing -> error $ "No Occurance info for " ++ show t
            w :: [(Id,UseInfo,OutTVr,InE)] -> [(OutTVr,OutE)] -> SM ([(OutTVr,OutE)],Env)
            w ((t,UseInfo { useOccurance = Once },t',e):rs) ds = do
                mtick $ "E.Simplify.inline.Once/{" ++ showName t ++ "}"
                w rs ds -- (minsert t (Susp e sub) sub) inb ds
            w ((t,n,t',e):rs) ds = do
                inb <- ask
                let inb' = case isForced of
                        ForceInline -> (cacheSubst $ envInScope_u (fmap nogrowth) inb)
                        _ -> inb
                    isForced = calcForced finalPhase t'
                    nogrowth IsBoundTo { bindingAtomic = False } = NotKnown
                    nogrowth x = x
                e' <- localEnv (const inb') $ f (LazyContext t') e
                let ibt = fixInline finalPhase t' $ isBoundTo n e'
                case (bindingAtomic ibt,inlineForced ibt) of
                    (True,f) | f /= ForceNoinline -> do
                        --when (n /= Unused) $ mtick $ "E.Simplify.inline.Atomic.{" ++ showName t ++ "}"
                        localEnv (insertDoneSubst' t e' . insertInScope (tvrIdent t') ibt) $ w rs  ((t',e'):ds)
                    _ -> localEnv (insertInScope (tvrIdent t') ibt) $ w rs ((t',e'):ds)
            w [] ds = ask >>= \inb -> return (ds,inb)
        s' <- mapM z ds
        inb <- ask
        let sub'' = fromList [ (t,susp e sub'') | (t, UseInfo { useOccurance = Once },_,e) <- s'] `union` fromList [ (t,Done (EVar t'))  | (t,n,t',_) <- s', useOccurance n /= Once] `union` envSubst inb
        (ds',inb') <- localEnv (envSubst_s sub'' . envInScope_u (fromList [ (tvrIdent t',NotKnown) | (_,n,t',_) <- s', useOccurance n /= Once] `union`)) $ w s' []
        let minArgs t = case Info.lookup (tvrInfo t) of
                Just (UseInfo { minimumArgs = min }) -> min
                Nothing -> 0

        ds' <- sequence [ etaExpandDef' (so_dataTable sopts) (minArgs t) t e | (t,e) <- ds']
        return (ds',inb')


data KnowSomething = KnowNothing | KnowNotOneOf [Name] | KnowIsCon Name | KnowIsNum Number | KnowSomething
    deriving(Eq)


someBenefit _ e _ | isAtomic e = True
someBenefit _ ELit {} _ = True
someBenefit _ EPi {} _ = True
someBenefit _ EPrim {} _ = True
someBenefit v ELetRec { eDefs = ds, eBody = e } xs | someBenefit v e xs = True
--someBenefit _v ECase {} (_:_) = True
someBenefit _ e xs | f e xs = True where
    f (ELam _ e) (_:xs) = f e xs
    f ELam {} [] = any (/= KnowNothing) xs
    f _ _ = not (null xs)
someBenefit v e xs = any (/= KnowNothing) xs

exprSize ::
    Int            -- ^ maximum size before bailing out
    -> E           -- ^ expression
    -> Int         -- ^ discount for case of something known
    -> [(Id,KnowSomething)]        -- ^ things that are known
    -> Maybe Int
exprSize max e discount known = f max e >>= \n -> return (max - n) where
    f n _ | n <= 0 = fail "exprSize: expression too big"
    f n EVar {} = return $! n - 1
    f n (EAp x@(EVar v) y) | Just _ <- lookup (tvrIdent v) known = do
        v <- f (n + discount) x
        f v x
    f n (EAp x y) = do
        v <- f n x
        f v x
    f n (ELam t x) = f (n - 1) x
    f n EPi {} = return $! n - 1
    f n ELit {} = return $! n - 1
    f n ESort {} = return $! n - 1
    f n EPrim {} = return $! n - 1
    f n EError {} = return $! n - 1
    f n ec@ECase { eCaseScrutinee = EVar tv } | Just l <- lookup (tvrIdent tv) known = do
        n <- f (n + discount) (EVar tv)
        let g n []  | Just d <- eCaseDefault ec = f n d
                    | otherwise  = return n
            g n (Alt LitCons { litName = c' } e:rs) | KnowIsCon c <- l = if c == c' then f n e else g n rs
            g n (Alt (LitInt c' _) e:rs) | KnowIsNum c <- l = if c == c' then f n e else g n rs
            g n (Alt LitCons { litName = c } e:rs) | KnowNotOneOf na <- l = if c `elem` na then g n rs else f n e >>= \n' -> g n' rs
            g n (Alt _ e:rs) = f n e >>= \n' -> g n' rs
        g n (eCaseAlts ec)
    f n ec@ECase {} = do
        n <- f n (eCaseScrutinee ec)
        foldM f n (caseBodies ec)
    f n ELetRec {eDefs = ds, eBody = e } = do
        n <- foldM f n (snds ds)
        f n e


noSizeIncrease e xs = f e xs where
    currentSize = 1 + length xs
    f (ELam t e) (x:xs) = f e xs
    f ELam {} [] = False -- ^ abort if we will create a lambda
    f e [] = isJust $ exprSize currentSize  e 3 []
    f e xs = isJust $ exprSize (currentSize - length xs) e 3 []


--multiInline _ e xs | isSmall (f e xs) = True  where -- should be noSizeIncrease
--    f e [] = e
--    f (ELam _ e) (_:xs) = f e xs
--    f e xs = foldl EAp e xs
--
--

scrutineeDiscount = 4
extraArgDiscount = 1
knowSomethingDiscount = 2

multiInline _ e xs | noSizeIncrease e xs = True
multiInline v e xs | not (someBenefit v e xs) = False
multiInline v e xs = f e xs [] where
    currentSize = 1 + length xs
    f (ELam t e) (KnowNothing:xs) rs = f e xs rs
    f (ELam t e) (x:xs) rs = f e xs ((tvrIdent t,x):rs)
    f e xs rs = isJust $ exprSize (knowSomethingDiscount*(length rs) + discount + currentSize + (if null xs then 0 else extraArgDiscount)) e scrutineeDiscount rs where
           discount = if safeToDup e then 4 else 0



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


-----------------------
-- simplification Monad
-----------------------

data SmState = SmState {
    idsUsed :: !IdSet,
    idsBound :: !IdSet
    }

smState = SmState { idsUsed = mempty, idsBound = mempty }

newtype SM a = SM (RWS Env Stats.Stat SmState a)
    deriving(Monad,Functor,MonadReader Env)

localEnv f (SM action) = SM $ local (cacheSubst . f) action


runSM :: Env -> SM a -> (a,Stat)
runSM env (SM x) = (r,s) where
    (r,_,s) = runRWS x (cacheSubst env) smState

instance MonadStats SM where
   mticks' n k = SM $ tell (Stats.singleStat n k) >> return ()

modifyIds fn = SM $ modify f where
    f s@SmState { idsUsed = used, idsBound = bound } = case fn (used,bound) of (used',bound') -> s { idsUsed = used', idsBound = bound' }
getIds = SM $ liftM f get where
    f s@SmState { idsUsed = used, idsBound = bound } = (used,bound)
putIds x = SM $ modify (f x) where
    f (used,bound) = \s -> s { idsUsed = used, idsBound = bound }

instance NameMonad Id SM where
    addNames ns = do
        modifyIds (\ (used,bound) -> (fromList ns `union` used, bound) )
    addBoundNames ns = do
        let nset = fromList ns
        modifyIds (\ (used,bound) -> (nset `union` used, nset `union` bound) )
    uniqueName n = do
        (used,bound) <- getIds
        if n `member` bound then newName else putIds (insert n used,insert n bound) >> return n
    newNameFrom vs = do
        (used,bound) <- getIds
        let f (x:xs)
                | x `member` used = f xs
                | otherwise = x
            f [] = error "newNameFrom: finite list!"
            nn = f vs
        putIds (insert nn used, insert nn bound)
        return nn
    newName  = do
        (used,bound) <- getIds
        let genNames i = [st, st + 2 ..]  where
                st = abs i + 2 + abs i `mod` 2
        newNameFrom  (genNames (size used + size bound))


smUsedNames = SM $ gets idsUsed
smBoundNames = SM $ gets idsBound



smAddNamesIdSet nset = do modifyIds (\ (used,bound) -> (nset `union` used, bound) )
smAddBoundNamesIdSet nset = do modifyIds (\ (used,bound) -> (nset `union` used, nset `union` bound) )

smAddBoundNamesIdMap nmap = do
    modifyIds (\ (used,bound) -> (nset `union` used, nset `union` bound) ) where
        nset = idMapToIdSet nmap


