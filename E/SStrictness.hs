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
import Util.UnionSolve
import E.E
import Info.Info as Info
import GenUtil

-- simple 2 point lattice for the moment
data SL = L | S
    deriving (Eq,Typeable,Show)

instance Fixable SL where
    isTop s = s == S
    isBottom l = l == L
    join L L = L
    join _ _ = S
    meet S S = S
    meet _ _ = L
    eq = (==)
    lte S L = False
    lte _ _ = True



data TAnot l = TAnot l (TTyp l)
    deriving (Eq,Typeable)

data TTyp l = (TAnot l) `TFun` (TAnot l) | TAtomic | TCPR [TAnot l]
    deriving (Eq,Typeable)

type Typ = TAnot (Either Var SL)

instance Functor TAnot where
    fmap f (TAnot l t) = TAnot (f l) (fmap f t)

instance Functor TTyp where
    fmap _ TAtomic = TAtomic
    fmap f (x `TFun` y) = fmap f x `TFun` fmap f y
    fmap f (TCPR xs) = TCPR $ map (fmap f) xs

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

type Constraints = C SL Var

type Environment = Map.Map Id Typ

newtype IM t a = IM (RWST Environment Constraints Int t a)
    deriving(MonadState Int,MonadReader Environment,MonadWriter Constraints,Monad,Functor)

newVar :: Monad m => IM m Var
newVar = do
    v <- get
    put (v + 1)
    return (V v)

newtype ShowString = ShowString String

instance Show ShowString where
    showsPrec _ (ShowString s) = showString s

fn (Left v) = ShowString (show v)
fn (Right v) = ShowString (show v)

analyzeProgram prog = do
    flip mapM_ (programDs prog) $ \ (t,e) -> case (runIM (infer e)) of
        Left err -> putStrLn $ "strictness error :" ++ pprint t ++ "\n" ++ err
        Right (c,(ty,_)) -> do
            putStrLn $ "strictnes " ++ pprint t
            print c
            let cc (TAnot l TAtomic) = Right S `islte` l
                cc (TAnot _ (_ `TFun` b)) = cc b
            print (fmap fn ty)
            putStrLn "solving:"
            (mp,rs) <- solve c
            let fn' (Right v) = ShowString (show v)
                fn' (Left x)
                    | Just x <- Map.lookup x mp, Just (ResultJust _ x) <- Map.lookup x rs = ShowString (show x)
                    | Just x <- Map.lookup x mp = ShowString (show x)
                    | otherwise = ShowString (show x)
            print (fmap fn' ty)
            mapM_ print (Map.elems rs)
            putStrLn "solving:"
            (mp,rs) <- solve $ c `mappend` cc ty
            let fn' (Right v) = ShowString (show v)
                fn' (Left x)
                    | Just x <- Map.lookup x mp, Just (ResultJust _ x) <- Map.lookup x rs = ShowString (show x)
                    | Just x <- Map.lookup x mp = ShowString (show x)
                    | otherwise = ShowString (show x)
            print (fmap fn' ty)
            mapM_ print (Map.elems rs)

    return ()


runIM :: Monad m => IM m a -> m (Constraints,a)
runIM (IM s) = do
    (a,_,c) <- runRWST s mempty 1
    return (c,a)

atom = TAnot (Right L) TAtomic

infer :: Monad m => E -> IM m (TAnot (Either Var SL),E)
infer e@(ELit l) = do
    v <- fmap Left newVar
    return (TAnot v TAtomic,e)
infer e@EPi {} = do
    return (atom,e)
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
    v <- fmap Left newVar
    mapM_ (\ (TAnot t _) -> tell (v `islte` t)) (map fst ts)
    return (TAnot v TAtomic,EPrim p (map snd ts) t)
infer (EError s t) = do
    return (atom,EError s t)
infer (ELam x@TVr {tvrType = t1} m) = do
    s1 <- freshAnot t1
    (s2,e) <- local (minsert (tvrIdent x) s1) $
        infer m
    return (TAnot (Right L) $ s1 `TFun` s2,ELam x e)
infer ec@ECase {} = do
    (TAnot t _,e') <- infer (eCaseScrutinee ec)
    ((ty:tys) ,ec) <- caseBodiesMapM' infer ec
    rt@(TAnot res _) <- foldM freshGLB ty tys
    tell (t `isgte` res)
    return (rt,ec { eCaseScrutinee = e' })
infer (EAp a b) = do
    (TAnot k (s1 `TFun` s2@(TAnot res _)),a) <- infer a
    (s1',b) <- infer b
    s1' `subsA` s1
    res <- fmap Left newVar
    -- the function is strict if we are strict
    tell (k `isgte` res)
    return (s2,EAp a b)
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
    v <- fmap Left newVar
    return (TAnot v $ TAnot (Right L) t1 `TFun` t2)
guessType _ = do
    v <- newVar
    return (TAnot (Left v) TAtomic)

freshAnot (EPi TVr {tvrType = t1 } t2) = do
    t1 <- freshAnot t1
    t2 <- freshAnot t2
    v <- fmap Left newVar
    return (TAnot v $  t1 `TFun` t2)
freshAnot _ = do
    v <- newVar
    return (TAnot (Left v) TAtomic)

freshGLB (TAnot k1 TAtomic) (TAnot k2 TAtomic) = do
    v <- fmap Left newVar
    tell (v `islte` k1)
    tell (v `islte` k2)
    return (TAnot v TAtomic)


freshGLB (TAnot k1 (TFun a1 b1)) (TAnot k2 (TFun a2 b2)) = do
    v <- fmap Left newVar
    tell (v `islte` k1)
    tell (v `islte` k2)
    a <- freshLUB a1 a2
    b <- freshGLB a2 b2
    return (TAnot v (TFun a b))

freshLUB (TAnot k1 TAtomic) (TAnot k2 TAtomic) = do
    v <- fmap Left newVar
    tell (v `isgte` k1)
    tell (v `isgte` k2)
    return (TAnot v TAtomic)

freshLUB (TAnot k1 (TFun a1 b1)) (TAnot k2 (TFun a2 b2)) = do
    v <- fmap Left newVar
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



