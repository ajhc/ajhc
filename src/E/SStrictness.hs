module E.SStrictness(
    analyzeProgram
    ) where


import Control.Monad
import Data.List
import Data.FunctorM
import Control.Monad.RWS
import qualified Data.Map as Map
import Data.Typeable

import Doc.PPrint
import E.Program
import Util.SetLike
import Name.Id
import Util.BooleanSolver
import E.E
import Info.Info as Info
import GenUtil

-- our 2 point lattice
-- True == strict
-- False == not strict

type SL = Bool


x `islte` y = x `implies` y
x `isgte` y = y `implies` x


data TAnot l = TAnot l (TTyp l)
    deriving (Eq,Typeable)

data TTyp l = (TAnot l) `TFun` (TAnot l) | TAtomic | TCPR [TAnot l]
    deriving (Eq,Typeable)

type Typ = TAnot (CV (CA Var))

instance Functor TAnot where
    fmap f (TAnot l t) = TAnot (f l) (fmap f t)

instance Functor TTyp where
    fmap _ TAtomic = TAtomic
    fmap f (x `TFun` y) = fmap f x `TFun` fmap f y
    fmap f (TCPR xs) = TCPR $ map (fmap f) xs

instance FunctorM TAnot where
    fmapM f (TAnot l t) = do l <- f l; t <- fmapM f t; return $ TAnot l t

instance FunctorM TTyp where
    fmapM _ TAtomic = return TAtomic
    fmapM f (x `TFun` y) = do x <- fmapM f x; y <- fmapM f y; return $ x `TFun` y
    fmapM f (TCPR xs) = do xs <- mapM (fmapM f) xs; return $ TCPR xs

instance Show l => Show (TAnot l) where
    showsPrec d (TAnot l typ) = showParen (d > 10) $ showsPrec 11 typ . showString "^" . showsPrec 11 l

instance Show l => Show (TTyp l) where
    showsPrec d (t1 `TFun` t2) = showParen (d > 9) $ showsPrec 10 t1 . showString " -> " . showsPrec 10 t2
    showsPrec _ TAtomic = showString "@"
    showsPrec d (TCPR ts) = showParen True $ foldr (.) id (intersperse (showString ",") (map shows ts))


newtype Var = V Int
    deriving(Eq,Ord,Typeable)

instance Show Var where
    showsPrec _ (V x) = ('v':) . shows x

type Constraints = C (CA Var)

type Environment = Map.Map Id Typ

newtype IM a = IM (RWST Environment Constraints Int IO a)
    deriving(MonadState Int,MonadReader Environment,MonadWriter Constraints,Monad,Functor,MonadIO)

newVar :: IM Var
newVar = do
    v <- get
    put (v + 1)
    return (V v)

newtype ShowString = ShowString String

instance Show ShowString where
    showsPrec _ (ShowString s) = showString s

fn (CJust v) = ShowString (show v)
fn CTrue = ShowString "S"
fn CFalse = ShowString "L"

strict,lazy :: CV (CA Var)
strict = CTrue
lazy = CFalse

data Variance = Nowhere | Positive | Negative | Both
    deriving(Eq,Ord,Show)

instance Monoid Variance where
    mempty = Nowhere
    mappend x y | x == y = x
    mappend Positive Negative = Both
    mappend Negative Positive = Both
    mappend Nowhere x = x
    mappend x Nowhere = x

flipVariance Positive = Negative
flipVariance Negative = Positive
flipVariance x = x

collect :: Typ -> [(Var,Variance)]
collect t = execWriter $ f Positive t where
    f p (TAnot (CJust v) t) = tell [(fromCA v,p)] >> g p t
    f p (TAnot _ t) = g p t
    g p TAtomic = return ()
    g p (x `TFun` y) = f (flipVariance p) x >> f p y


{-# NOINLINE analyzeProgram #-}
analyzeProgram prog = do
    flip mapM_ (programDs prog) $ \ (t,e) -> case (runIM (infer e)) of
        Left err -> putStrLn $ "strictness error :" ++ pprint t ++ "\n" ++ err
        Right (c,(ty,_)) -> do
            putStrLn $ "strictnes " ++ pprint t
            print c
            let cc (TAnot l TAtomic) = strict `islte` l
                cc (TAnot _ (_ `TFun` b)) = cc b
            print (fmap fn ty)
            putStrLn "solving:"
            --(cc,cvs) <- groundConstraints $ c -- `mappend` cc ty
            processConstraints True c
--            rs <- flip mapM cvs $ \cv -> do
--                res <- readValue cv
--                let rr = case res of
--                        ResultJust True -> CTrue
--                        ResultJust False -> CFalse
--                        ResultBounded a _ _ -> CJust (fromCA a)
--                return (fromCA cv, rr )
--            let mp :: Map.Map Var (CV Var)
--                mp = Map.fromList rs
--                zz (CJust x) | Just y <- Map.lookup x (Map.fromList rs) = y
--                zz (CJust y) = CJust y
--                zz CTrue = CTrue
--                zz CFalse = CFalse
--                ty' = fmap zz ty
--            print (fmap fn ty)
--            let varmap = (Map.fromListWith mappend $ collect ty')
--            print varmap
--            flip mapM_ cvs $ \cv -> do
--                res <- readValue cv
--                print (fromCA cv,fmap fromCA res)
--            --print (fmap (zz . CJust . fromCA) cc)

    return ()


runIM :: MonadIO m => IM a -> m (Constraints,a)
runIM (IM s) = do
    (a,_,c) <- liftIO $ runRWST s mempty 1
    return (c,a)

atom = TAnot lazy TAtomic

mkVar :: IM (CV (CA Var))
mkVar = do
    v <- newVar
    ca <- mkCA v
    return (CJust ca)

infer :: E -> IM (Typ,E)
infer e@(ELit l) = do
    return (TAnot strict TAtomic,e)
    --return (atom,e)
infer e@EPi {} = do
    return (TAnot strict TAtomic,e)
    --return (atom,e)
infer (EVar tvr) = do
    env <- ask
    case mlookup (tvrIdent tvr) env `mplus` Info.lookup (tvrInfo tvr) of
        Nothing -> do
            -- guess a pessimistic type if we know nothing about a variable
            t <- guessType (tvrType tvr)
            return (t,EVar tvr)
        Just t -> return (t,EVar tvr)
infer (EPrim p xs t) = do
    ts <- mapM infer xs
    v <- mkVar
    mapM_ (\ (TAnot t _) -> tell (v `islte` t)) (map fst ts)
    return (TAnot v TAtomic,EPrim p (map snd ts) t)
infer (EError s t) = do
    v <- mkVar
    return (TAnot v TAtomic,EError s t)
infer (ELam x@TVr {tvrType = t1} m) = do
    s1 <- freshAnot t1
    (s2,e) <- local (minsert (tvrIdent x) s1) $
        infer m
    v <- mkVar
    return (TAnot strict $ s1 `TFun` s2,ELam x e)
infer ec@ECase {} = do
    nv <- mkVar
    (TAnot t _,e') <- infer (eCaseScrutinee ec)
    tell (nv `implies` t)
    ((ty:tys) ,ec) <- caseBodiesMapM' infer ec
    (TAnot res rt) <- foldM freshGLB ty tys
    tell (nv `implies` res)
    return (TAnot nv rt,ec { eCaseScrutinee = e' })
infer (EAp a b) = do
    (TAnot k (s1 `TFun` (TAnot rst s2)),a) <- infer a
    (s1'@(TAnot zz _),b) <- infer b
    s1 `subsA` s1'
    res <- mkVar
    -- the function is strict if we are strict
    tell (res `implies` k)
    tell (res `implies` rst)
    return (TAnot res s2,EAp a b)
--infer (ELetRec ds e) = do


infer e = fail $ "infer: unsupported\n" ++ show e

caseBodiesMapM' :: Monad m => (E -> m (t,E)) -> E -> m ([t],E)
caseBodiesMapM' f ec@ECase { eCaseAlts = as, eCaseDefault = d } = do
    let g (Alt l e) = do (t,e) <- f e ; return (t,Alt l e)
    as' <- mapM g as
    d' <- fmapM f d
    let ts = fsts as' ++ maybe [] ((:[]) . fst) d'
    return $ (ts,ec { eCaseAlts = snds as', eCaseDefault = fmap snd d' })
caseBodiesMapM' _ _ = error "caseBodiesMapM'"

-- | pessimistic guess of type for variables we know nothing about.
-- warning! newtypes of infinite functions are wonky. need to figure out solution.
guessType (EPi TVr {tvrType = t1 } t2) = do
    TAnot _ t1 <- guessType t1
    t2 <- guessType t2
    v <- mkVar
    return (TAnot v $ TAnot lazy t1 `TFun` t2)
guessType _ = do
    v <- mkVar
    return (TAnot v TAtomic)

freshAnot (EPi TVr {tvrType = t1 } t2) = do
    t1 <- freshAnot t1
    t2 <- freshAnot t2
    v <- mkVar
    return (TAnot v $  t1 `TFun` t2)
freshAnot _ = do
    v <- mkVar
    return (TAnot v TAtomic)

freshGLB (TAnot k1 TAtomic) (TAnot k2 TAtomic) = do
    v <- mkVar
    tell (v `islte` k1)
    tell (v `islte` k2)
    return (TAnot v TAtomic)


freshGLB (TAnot k1 (TFun a1 b1)) (TAnot k2 (TFun a2 b2)) = do
    v <- mkVar
    tell (v `islte` k1)
    tell (v `islte` k2)
    a <- freshLUB a1 a2
    b <- freshGLB a2 b2
    return (TAnot v (TFun a b))

freshLUB (TAnot k1 TAtomic) (TAnot k2 TAtomic) = do
    v <- mkVar
    tell (v `isgte` k1)
    tell (v `isgte` k2)
    return (TAnot v TAtomic)

freshLUB (TAnot k1 (TFun a1 b1)) (TAnot k2 (TFun a2 b2)) = do
    v <- mkVar
    tell (v `isgte` k1)
    tell (v `isgte` k2)
    a <- freshLUB a1 a2
    b <- freshGLB a2 b2
    return (TAnot v (TFun a b))

subs (x1 `TFun` y2) (x3 `TFun` y4) = do
    x3 `subsA` x1
    y2 `subsA` y4
subs TAtomic TAtomic = return ()

subsA (TAnot a t1) (TAnot b t2) = do
    tell (a `islte` b)
    t1 `subs` t2



