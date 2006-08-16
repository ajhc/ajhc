
module E.Traverse(
    basicDecompose,
    Binding(..),
    emapE_,
    emapE,
    emapE',
    eSize,
    lookupBinding,
    newBinding,
    newVarName,
    renameE,
    renameTraverse,
    renameTraverse',
    runRename,
    traverse,
    travOptions,
    TravOptions(..),
    TravM
    ) where

import Control.Monad.Reader
import Control.Monad.Writer
import Data.FunctorM
import Data.Monoid

import DataConstructors()
import Util.HasSize
import E.E
import E.Rules
import E.TypeCheck
import E.Values
import Support.FreeVars
import GenUtil
import Name.Name
import Util.NameMonad
import Util.Graph
import Name.Id
import Util.SetLike as S

-- Generic traversal routines rock.

newtype MInt = MInt Int

instance Monoid MInt where
    mempty = MInt 0
    mappend (MInt a) (MInt b) = a `seq` b `seq` MInt (a + b)


renameTraverse e = (e',c) where
    (e',MInt c) = runWriter $ liftM fst $ traverse travOptions { pruneUnreachable = Nothing } (\_ (x,xs) -> lift (tell (MInt 1)) >> (return $ foldl EAp x xs)) mempty mempty  e
renameTraverse' e = e' where
    e' = liftM fst $ traverse travOptions { pruneUnreachable = Nothing } (\_ (x,xs) -> (return $ foldl EAp x xs)) mempty mempty  e

runRename :: IdSet -> E -> (E,IdSet)
runRename set e = renameE set mempty e
--runRename set e = runIdentity $ traverse travOptions { pruneUnreachable = Nothing } (\_ (x,xs) -> (return $ foldl EAp x xs)) mempty (idSetToIdMap (const NotKnown) set)  e

data  TravOptions m = TravOptions {
    pruneUnreachable :: Maybe [Int],
    pruneRecord :: Int -> m (),
    propagateRecord :: Int -> m (),
    letToCaseRecord :: Int -> m (),
    trav_rules :: Rules,
    _hiddenTricky :: m ()    -- ^ This ensures types are not ambiguous if we don't fill in the monadic routines
    }

travOptions :: Monad m => TravOptions m
travOptions = TravOptions {
    pruneUnreachable = Just mempty,
    pruneRecord = \_ -> return (),
    propagateRecord = \_ -> return (),
    letToCaseRecord = \_ -> return (),
    trav_rules = mempty,
    _hiddenTricky = return ()
    }
{-
travOptionsI :: TravOptions Identity
travOptionsI = TravOptions {
    pruneUnreachable = Just mempty,
    pruneRecord = \_ -> Identity ()
    }
-}
type Subst = IdMap E  -- Map apply anywhere. so should range only over atoms
type InScope = IdMap Binding
data Binding = NotAmong [Name] | IsBoundTo E | NotKnown
    deriving(Eq,Ord)

--newtype TravM m a = TravM (StateT (Map.Map Int Binding) m a)
--    deriving(Monad,MonadTrans,Functor,MonadIO)
newtype TravM m a = TravM (ReaderT (IdMap Binding) (IdNameT m) a)
    deriving(Monad,Functor,MonadIO)

instance MonadTrans TravM where
    lift a = TravM $ lift $ lift a

fromTravM (TravM x) = x

{-# INLINE newBinding #-}
newBinding :: Monad m => E -> TravM m (TVr,E)
newBinding e = do
    v <- newVarName
    return (tVr ( v) (typeInfer mempty e),e)

{-# INLINE lookupBinding #-}
lookupBinding :: Monad m => TVr -> TravM m Binding
lookupBinding (TVr { tvrIdent = n }) = TravM $ do
    x <- ask
    return (maybe NotKnown id $  mlookup n x)

{-# INLINE newVarName #-}
newVarName :: Monad m => TravM m Int
newVarName = TravM $ do
    newName
    --m <- get
    --let nv = newVar m
    --put (Map.insert nv NotKnown m)
    --return nv

{-

{-# INLINE newVar #-}
newVar ss = newVar' ss (2 * Map.size ss + 2)
newVar' ss n | n <= 0 || n `Map.member` ss  = v $ (2 * Map.size ss + 2) + (n + (n `mod` 2))  where
    v n | n `Map.member` ss = v (n + 2)
    v n = n
newVar' _ n = n

-}

traverse :: (MonadFix m,Monad m) => TravOptions m -> (Int -> (E,[E]) -> TravM m E) -> Subst -> (IdMap Binding) -> E -> m (E,IdSet)
traverse (tOpt :: TravOptions m) func subst smap e = runIdNameT' $ initNames >> runReaderT (f e) (smap,subst,0::Int)  where
    initNames = do
        addBoundNamesIdSet $ freeVars e
        addBoundNamesIdMap subst
        addBoundNamesIdMap smap
    f :: E -> ReaderT (IdMap Binding, Subst, Int) (IdNameT m) E
    f' e = do
        local (\ (a,b,c) -> (a,b,c + 1)) $  f e
    f  e | (x,xs) <- fromAp e = do
        xs' <- mapM l xs
        x' <- g x
        (m,p,lvl) <- ask
        (z) <- lift $ runReaderT (fromTravM $ func lvl (x',xs')) m
        return z
    g  e@(EVar (TVr { tvrIdent = n, tvrType =  t})) = do
        (_,im,lvl) <- ask
        case mlookup n im of
            Just n'@(EVar t) | tvrIdent t == n -> return $ n'
            Just n' -> do
                lift $ lift $  propagateRecord tOpt 1
                return $ n'
            Nothing -> return e
    g  (ELit (LitCons n xs t)) = do
        xs' <- mapM l xs
        t' <- f' t
        return $ ELit (LitCons n xs' t')
    g (ELit l) = return $ ELit l
    g (EError x t) = do
        t' <- f' t
        return $ EError x t'
    g (EPrim n es t) = do
        es' <- mapM l es
        t' <- f' t
        return $ EPrim n es' t'
    g ec@ECase { eCaseScrutinee = e, eCaseBind = b, eCaseAlts = as, eCaseDefault = d} = do
        e' <- f e
        t' <- f' (eCaseType ec)
        addNames $ map tvrIdent (caseBinds ec)
        (ob,b') <- ntvr f' b
        localSubst [(ob,EVar b')] $ do
            as' <- mapM (da [ v  | EVar v <- [e',EVar b']])   as
            d' <- localVars [ (tvrIdent v,NotAmong [ n | Alt (LitCons n _ _) _ <- as]) | EVar v <- [e',EVar b'] ] $ fmapM f d
            return $ ec { eCaseScrutinee = e', eCaseType = t', eCaseBind = b', eCaseAlts = as', eCaseDefault = d' }
    g (ELam tvr e) = lp f' ELam tvr e
    g (EPi tvr e) = lp f EPi tvr e
    g (ELetRec ds e) = do
            addNames $ map ( tvrIdent . fst ) ds
            z (basicDecompose  (pruneUnreachable tOpt) (trav_rules tOpt) e ds) e  where
        z [] e = f e
--        z (Left (tvr,x):rs) e | worthStricting x, Just (Demand.S _) <- Info.lookup (tvrInfo tvr)   = do
--            (n,tvrn) <- ntvr f' tvr
--            x' <- f x
--            nr <- localSubst [(n,EVar tvrn)]   (z rs e)
--            lift $ lift $  letToCaseRecord tOpt 1
--            return $ eStrictLet tvrn x' nr
        z (Left (tvr,x):rs) e = do
            (n,tvrn) <- ntvr f' tvr
            x' <- f x
            nr <- localVars [(tvrIdent tvrn, IsBoundTo x')] $ localSubst [(n,EVar tvrn)]   (z rs e)
            return $ eLetCoalesce [(tvrn,x')] nr
        z (Right ds:rs) e = do
            ds' <- mapM (ntvr f' . fst) ds
            let (fz,gz)  = unzip [ ((n',NotKnown),(n,EVar tvr)) |  (n,tvr@(TVr  { tvrIdent =  n' })) <- ds']
            localVars fz $ localSubst gz $ do
                ds''' <- sequence [ f x >>= return . (,) tvr | (_,tvr) <- ds' | (_,x) <- ds ]
                nr <- (z rs e)
                return $ eLetCoalesce ds''' nr
                --h ds'' (z rs e) []
    {-
    g (ELetRec ds e) = do
        ds' <- mapM ( ntvr f' . fst) ds
        let ds'' = inlineDecompose (pruneUnreachable tOpt) e ds
        lift $ lift $  pruneRecord tOpt (length ds - length ds'')
        let (fz,gz)  = unzip [ ((n',NotKnown),(n,EVar tvr)) |  (n,tvr@(TVr  n' _)) <- ds']
        localVars fz $ localSubst gz $ do
            h ds'' e []
    -}
    g x@(ESort {}) = return x
    g Unknown = return Unknown
    g e = error $ "g: " ++ show e
    eLetCoalesce ds (ELetRec ds' e) = ELetRec (ds ++ ds') e
    eLetCoalesce ds e = ELetRec ds e
    l x@EAp {} = f x
    l x = g x
    da vs (Alt p@(LitCons n xs t) l) = do
        t' <- f' t
        xs' <-  mapM (ntvr f') xs
        localVars [ (tvrIdent v, IsBoundTo (ELit $ LitCons n (map (EVar . snd) xs') t')) |  v <- vs ] $ do
            localSubst [ (x,EVar y) | (x,y) <- xs'] $ do
                l' <- f l
                return (Alt (LitCons n (snds xs') t') l')
    da vs (Alt p@LitInt {} l) = do
        p' <- fmapM f' p
        localVars [ (tvrIdent v, IsBoundTo (patToLitEE p')) |  v <- vs ] $ do
            l' <- f l
            return (Alt p' l')
    --lp elam (TVr Nothing t) e = do
    --    t' <- f' t
    --    e' <- f e
    --    return $ elam (TVr Nothing t') e'
    lp fg elam tv e = do
        (n,tvr@(TVr { tvrIdent = n' })) <- ntvr fg tv
        e' <- localVars [(n',NotKnown)] $ localSubst [(n,EVar tvr)]   (f e)
        return $ elam tvr e'
    lb n me n' ne (m,im,lvl) = (minsert n me m,if n' /= 0 then minsert n' ne im else im ,lvl)
    localVars ex x = do
        let ex' = fromList [ (a,b) |  (a,IsBoundTo b) <- ex, isAtomic b ] :: IdMap E
            z (EVar (TVr { tvrIdent = n })) | Just v <- mlookup n ex' = v
            z e = e
        r <- local (\ (a,b,c) ->  (fromList ex `mappend` a, fmap z  b ,c)) x
        return r
    localSubst (ex :: [(Int,E)]) x = do
        r <- local (\ (a,b,c) ->  (a, fromList ex `mappend` b ,c)) x
        return r
    ntvr fg tvr@(TVr { tvrIdent = 0, tvrType = t}) = do
        t' <- fg t
        --let tvr = (TVr 0 t')
        return (0,tvr { tvrType = t'})
    ntvr fg ttvr@(TVr { tvrIdent = n, tvrType = t}) = do
        n' <- if n > 0 then uniqueName  n else newName
        t' <- fg t
        let tvr = ttvr { tvrIdent = n', tvrType = t' }
        return (n:: Int,tvr:: TVr)
    h [] e ds = do
        e' <- e
        return $ eLetCoalesce ds e'
    h (((TVr { tvrIdent = n }),x):dds) e ds = do
        (_,tm,_) <- ask
        let (Just (EVar nt)) = mlookup n tm
        x' <- f x
        localVars [(tvrIdent nt, IsBoundTo x')] $ h dds e ((nt,x'):ds)
        --case isAtomic x' of
        --    False -> do
                --modify (\ (a,b,c) -> (Map.insert (tvrIdent nt) (IsBoundTo x') a,b,c))
        --        localVars [(tvrIdent nt, IsBoundTo x')] $ h dds e ((nt,x'):ds)
        --    True -> do
        --        localSubst [(n,x')] $ h dds e ((nt,x'):ds)
    {-
    h (Left ((TVr (Just n) _),x):dds) e ds = do
        (_,tm,_) <- get
        let (Just nt) = Map.lookup n tm
        x' <- f x
        modify (\ (a,b,c) -> (Map.insert (tvrIdent nt) (Just x') a,b,c))
        h dds e ((nt,x'):ds)
    h (Right ds:dds) e rs = do
        let l ((TVr (Just n) _),x) = do
                (_,tm,_) <- get
                let Just nt = Map.lookup n tm
                x' <- f x
                return (nt,x')
        ds' <- mapM l ds
        h dds e (ds' ++ rs)
    da vs _ (Alt p@(LitCons n xs t) l) = do
        localVars [ (tvrIdent v, IsBoundTo (ELit $ patToLitEE p)) |  v <- vs ] $ do
            t' <- f' t
            xs' <-  mapM (ntvr f') xs
            localSubst [ (x,EVar y) | (x,y) <- xs'] $ do
                l' <- f l
                return (Alt (LitCons n (snds xs') t') l')
    da vs _ (Alt p l) = do
        localVars [ (tvrIdent v, IsBoundTo (ELit $ patToLitEE p)) |  v <- vs ] $ do
            p' <- fmapM f' p
            l' <- f l
            return (Alt p' l')
    -}


worthStricting x = isLifted x && not (isELit x)

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
    z ec@ECase {} = do
        e' <- f $ eCaseScrutinee ec
        b' <- fmapM g (eCaseBind ec)
        as' <- mapM mapmAlt (eCaseAlts ec)
        d' <- fmapM f (eCaseDefault ec)
        t' <- g (eCaseType ec)
        return ECase { eCaseScrutinee =e', eCaseBind = b', eCaseAlts = as', eCaseDefault = d', eCaseType = t'}
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

basicDecompose ::
    Maybe [Int]  -- ^ Just a set of values not to prune or nothing to not prune at all.
    -> Rules     -- ^ Rules for pruning
    -> E             -- ^ body for pruning info
    -> [(TVr,E)]     -- ^ incoming bindings
    -> [Either (TVr,E) [(TVr,E)]]     -- ^ bindings pruned and ordered by inlinability value
basicDecompose prune rules body ds = ans where
    zs = [ ((t,e), tvrIdent t, idSetToList $ bindingFreeVars t e ) |  (t,e) <- ds ]
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

renameE :: IdSet -> IdMap E -> E -> (E,IdSet)
renameE initSet initMap e = runReader (runIdNameT' $ addBoundNamesIdMap initMap >> addBoundNamesIdSet initSet >> f e) initMap  where
    f,f' :: E -> IdNameT (Reader (IdMap E)) E
    f' e = f e
    f  (EAp a b) = return EAp `ap` f a `ap` f b
    f  (ELit (LitCons n xs t)) = do
        xs' <- mapM f xs
        t' <- f' t
        return $ ELit (LitCons n xs' t')
    f (ELit (LitInt n t)) = do
        t' <- f' t
        return (ELit (LitInt n t'))
    f (EError x t) = return (EError x) `ap` f' t
    f (EPrim n es t) = do
        es' <- mapM f es
        t' <- f' t
        return $ EPrim n es' t'
    f (ELam tvr e) = lp f' ELam tvr e
    f (EPi tvr e) = lp f EPi tvr e
    f  e@(EVar TVr { tvrIdent = n }) = do
        im <- lift ask
        case mlookup n im of
            Just n' -> do return n'
            Nothing -> return e
    f x@(ESort {}) = return x
    f Unknown = return Unknown
    f ec@ECase { eCaseScrutinee = e, eCaseBind = b, eCaseAlts = as, eCaseDefault = d } = do
        e' <- f e
        t' <- f' (eCaseType ec)
        addNames $ map tvrIdent (caseBinds ec)
        (ob,b') <- ntvr f' b
        localSubst ob $ do
            as' <- mapM da as
            d' <- fmapM f d
            return $ ec { eCaseScrutinee = e', eCaseType = t', eCaseBind = b', eCaseAlts = as', eCaseDefault = d' }
    f (ELetRec ds e) = do
        addNames (map (tvrIdent . fst) ds)
        ds' <- mapM ( ntvr f' . fst) ds
        localSubst (mconcat $ fsts ds') $ do
            es <- mapM f (snds ds)
            e' <- f e
            return (ELetRec (zip (snds ds') es) e')
    --f e = error $ "renameE.f: " ++ show e
    da :: Alt E -> IdNameT (Reader (IdMap E)) (Alt E)
    da (Alt (LitCons n xs t) l) = do
        t' <- f' t
        xs' <-  mapM (ntvr f') xs
        localSubst (mconcat [ x | (x,_) <- xs']) $ do
            l' <- f l
            return (Alt (LitCons n (snds xs') t') l')
    da (Alt (LitInt n t) l) = do
        t' <- f' t
        l' <- f l
        return (Alt (LitInt n t') l')
    localSubst :: (IdMap E) -> IdNameT (Reader (IdMap E)) a  -> IdNameT (Reader (IdMap E)) a
    localSubst ex action = do local (ex `mappend`) action
    ntvr fg tv@TVr { tvrIdent = 0, tvrType = t} = do
        t' <- fg t
        return (mempty,tv { tvrType = t'})
    ntvr fg tv@(TVr { tvrIdent = n, tvrType = t}) = do
        n' <- if n > 0 then uniqueName  n else newName
        t' <- fg t
        let tv' = tv { tvrIdent = n', tvrType = t' }
        return (msingleton n (EVar tv'),tv')
    lp fg elam tv e = do
        (n,tv') <- ntvr fg tv
        e' <- localSubst n (f e)
        return $ elam tv' e'


