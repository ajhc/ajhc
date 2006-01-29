
module E.Traverse(TravM, newVarName, lookupBinding, newBinding, traverse, renameTraverse, renameTraverse', runRename, TravOptions(..), Binding(..), travOptions, emapE, emapE') where

import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.Writer
import Data.FunctorM
import Data.Monoid
import qualified Data.Map as Map
import qualified Data.Set as Set

import DataConstructors()
import E.E
import E.Inline
import E.Rules
import E.Strictness as Strict
import E.TypeCheck
import E.Values
import Support.FreeVars
import GenUtil
import Name.Name
import Util.NameMonad

-- Generic traversal routines rock.

newtype MInt = MInt Int

instance Monoid MInt where
    mempty = MInt 0
    mappend (MInt a) (MInt b) = a `seq` b `seq` MInt (a + b)


renameTraverse e = (e',c) where
    (e',MInt c) = runWriter $ liftM fst $ traverse travOptions { pruneUnreachable = Nothing } (\_ (x,xs) -> lift (tell (MInt 1)) >> (return $ foldl EAp x xs)) mempty mempty  e
renameTraverse' e = e' where
    e' = liftM fst $ traverse travOptions { pruneUnreachable = Nothing } (\_ (x,xs) -> (return $ foldl EAp x xs)) mempty mempty  e

runRename :: Set.Set Int -> E -> (E,Set.Set Int)
runRename set e = runIdentity $ traverse travOptions { pruneUnreachable = Nothing } (\_ (x,xs) -> (return $ foldl EAp x xs)) mempty (Map.fromAscList [ (v,NotKnown) | v <- Set.toAscList set])  e

data  TravOptions m = TravOptions {
    pruneUnreachable :: Maybe [Int],
    pruneRecord :: Int -> m (),
    propagateRecord :: Int -> m (),
    letToCaseRecord :: Int -> m (),
    trav_rules :: Rules,
    trav_strictness :: Map.Map Int Strict.SA,
    _hiddenTricky :: m ()    -- ^ This ensures types are not ambiguous if we don't fill in the monadic routines
    }

travOptions :: Monad m => TravOptions m
travOptions = TravOptions {
    pruneUnreachable = Just mempty,
    pruneRecord = \_ -> return (),
    propagateRecord = \_ -> return (),
    letToCaseRecord = \_ -> return (),
    trav_rules = mempty,
    trav_strictness = mempty,
    _hiddenTricky = return ()
    }
{-
travOptionsI :: TravOptions Identity
travOptionsI = TravOptions {
    pruneUnreachable = Just mempty,
    pruneRecord = \_ -> Identity ()
    }
-}
type Subst = Map.Map Int E  -- Map apply anywhere. so should range only over atoms
type InScope = Map.Map Int Binding
data Binding = NotAmong [Name] | IsBoundTo E | NotKnown

--newtype TravM m a = TravM (StateT (Map.Map Int Binding) m a)
--    deriving(Monad,MonadTrans,Functor,MonadIO)
newtype TravM m a = TravM (ReaderT (Map.Map Int Binding) (NameMT Int m) a)
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
    return (maybe NotKnown id $  Map.lookup n x)

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

traverse :: (MonadFix m,Monad m) => TravOptions m -> (Int -> (E,[E]) -> TravM m E) -> Subst -> (Map.Map Int Binding) -> E -> m (E,Set.Set Int)
traverse (tOpt :: TravOptions m) func subst smap e = runNameMT' $ initNames >> runReaderT (f e) (smap,subst,0::Int)  where
    initNames = do
        addBoundNames $ freeVars e
        addBoundNames (Map.keys subst)
        addBoundNames (Map.keys smap)
    f :: E -> ReaderT (Map.Map Int Binding, Subst, Int) (NameMT Int m) E
    f' e = do
        local (\ (a,b,c) -> (a,b,c + 1)) $  f e
        --(y,z,n) <- get
        --put (y,z,n + 1)
        --put (y,z,n)
        --return x
    f  e | (x,xs) <- fromAp e = do
        xs' <- mapM l xs
        x' <- g x
        (m,p,lvl) <- ask
        (z) <- lift $ runReaderT (fromTravM $ func lvl (x',xs')) m
        --put (m,p,lvl)
        return z
    g  e@(EVar (TVr { tvrIdent = n, tvrType =  t})) = do
        (_,im,lvl) <- ask
        case Map.lookup n im of
            Just n'@(EVar t) | tvrNum t == n -> return $ n'
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
    g ec@(ECase e b as d) = do
        e' <- f e
        addNames $ map tvrIdent (caseBinds ec)
        (ob,b') <- ntvr f' b
        localSubst [(ob,EVar b')] $ do
            as' <- mapM (da [ v  | EVar v <- [e',EVar b']])   as
            d' <- localVars [ (tvrNum v,NotAmong [ n | Alt (LitCons n _ _) _ <- as]) | EVar v <- [e',EVar b'] ] $ fmapM f d
            return $ ECase e' b' as' d'
    g (ELam tvr e) = lp f' ELam tvr e
    g (EPi tvr e) = lp f EPi tvr e
    g (ELetRec ds e) = do
            addNames $ map ( tvrIdent . fst ) ds
            z (basicDecompose  (pruneUnreachable tOpt) (trav_rules tOpt) e ds) e  where
        z [] e = f e
        z (Left (tvr,x):rs) e | worthStricting x, Just (S _) <- Map.lookup (tvrNum tvr) (trav_strictness tOpt)  = do
            (n,tvrn) <- ntvr f' tvr
            x' <- f x
            nr <- localSubst [(n,EVar tvrn)]   (z rs e)
            lift $ lift $  letToCaseRecord tOpt 1
            return $ eStrictLet tvrn x' nr
        z (Left (tvr,x):rs) e = do
            (n,tvrn) <- ntvr f' tvr
            x' <- f x
            nr <- localVars [(tvrNum tvrn, IsBoundTo x')] $ localSubst [(n,EVar tvrn)]   (z rs e)
            return $ eLetCoalesce [(tvrn,x')] nr
        z (Right ds:rs) e = do
            ds' <- mapM (ntvr f' . fst) ds
            --let ds'' = inlineDecompose (pruneUnreachable tOpt) e ds
            --lift $ lift $  pruneRecord tOpt (length ds - length ds'')
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
    g e = error $ "g: " ++ show e
    eLetCoalesce ds (ELetRec ds' e) = ELetRec (ds ++ ds') e
    eLetCoalesce ds e = ELetRec ds e
    l x@EAp {} = f x
    l x = g x
    da vs (Alt p@(LitCons n xs t) l) = do
        t' <- f' t
        xs' <-  mapM (ntvr f') xs
        localVars [ (tvrNum v, IsBoundTo (ELit $ LitCons n (map (EVar . snd) xs') t')) |  v <- vs ] $ do
            localSubst [ (x,EVar y) | (x,y) <- xs'] $ do
                l' <- f l
                return (Alt (LitCons n (snds xs') t') l')
    da vs (Alt p@LitInt {} l) = do
        p' <- fmapM f' p
        localVars [ (tvrNum v, IsBoundTo (patToLitEE p')) |  v <- vs ] $ do
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
    lb n me n' ne (m,im,lvl) = (Map.insert n me m,if n' /= 0 then Map.insert n' ne im else im ,lvl)
    localVars ex x = do
        let ex' = Map.fromList [ (a,b) |  (a,IsBoundTo b) <- ex, isAtomic b ]
            z (EVar (TVr { tvrIdent = n })) | Just v <- Map.lookup n ex' = v
            z e = e
        r <- local (\ (a,b,c) ->  (Map.fromList ex `mappend` a, fmap z  b ,c)) x
        return r
    localSubst (ex :: [(Int,E)]) x = do
        r <- local (\ (a,b,c) ->  (a, Map.fromList ex `mappend` b ,c)) x
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
        let (Just (EVar nt)) = Map.lookup n tm
        x' <- f x
        localVars [(tvrNum nt, IsBoundTo x')] $ h dds e ((nt,x'):ds)
        --case isAtomic x' of
        --    False -> do
                --modify (\ (a,b,c) -> (Map.insert (tvrNum nt) (IsBoundTo x') a,b,c))
        --        localVars [(tvrNum nt, IsBoundTo x')] $ h dds e ((nt,x'):ds)
        --    True -> do
        --        localSubst [(n,x')] $ h dds e ((nt,x'):ds)
    {-
    h (Left ((TVr (Just n) _),x):dds) e ds = do
        (_,tm,_) <- get
        let (Just nt) = Map.lookup n tm
        x' <- f x
        modify (\ (a,b,c) -> (Map.insert (tvrNum nt) (Just x') a,b,c))
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
        localVars [ (tvrNum v, IsBoundTo (ELit $ patToLitEE p)) |  v <- vs ] $ do
            t' <- f' t
            xs' <-  mapM (ntvr f') xs
            localSubst [ (x,EVar y) | (x,y) <- xs'] $ do
                l' <- f l
                return (Alt (LitCons n (snds xs') t') l')
    da vs _ (Alt p l) = do
        localVars [ (tvrNum v, IsBoundTo (ELit $ patToLitEE p)) |  v <- vs ] $ do
            p' <- fmapM f' p
            l' <- f l
            return (Alt p' l')
    -}


worthStricting x = isLifted x && not (isELit x)

