module Util.UnionSolve(
    C(),
    solve,
    Fixable(..),
    Topped(..),
    Result(..),
    islte,isgte,equals,
    varIsInteresting
    ) where

import Data.List(intersperse)
import Data.Monoid
import qualified Data.Sequence as S
import qualified Data.Foldable as S
import qualified Data.Set as Set
import qualified Data.Map as Map
import Util.UnionFind as UF

-- simple constraint solver based on ideas from 'Once upon a polymorphic type' paper.


class Fixable a where
    -- determine if we are at the top or bottom of the lattice, we can
    -- solidify bounds if we know we are at an endpoint.
    isBottom :: a -> Bool
    isTop :: a -> Bool

    join :: a -> a -> a
    meet :: a -> a -> a

    eq :: a -> a -> Bool
    lte :: a -> a -> Bool

    showFixable :: a -> String

    showFixable x | isBottom x = "B"
                  | isTop x = "T"
                  | otherwise = "*"
    eq x y = lte x y && lte y x
    isBottom _ = False
    isTop _ = False


-- arguments are the lattice and the variable type
-- we make the fields strict because many empty values will be
-- mappended together when used in a writer monad.
data C l v = C !(S.Seq (CL l v)) !(Set.Set v)

instance Ord v => Monoid (C l v) where
    mempty = C mempty mempty
    mappend (C a b) (C c d) = C (a `mappend` c) (b `mappend` d)

data CL l v = (Either v l) `Clte` (Either v l) | (Either v l) `Cset` (Either v l)
    deriving(Eq,Ord)

instance (Show e,Show l) => Show (C l e) where
    showsPrec _ (C xs _) = showString "" . foldr (.) id (intersperse (showString "\n") (map shows (S.toList xs))) . showString "\n"

seither (Left x) = shows x
seither (Right x) = shows x

instance (Show e,Show l) => Show (CL l e) where
    showsPrec _ (x `Clte` y) = seither x . showString " <= " . seither y
    showsPrec _ (x `Cset` l) = seither x . showString " := " . seither l

-- basic constraints
islte,isgte,equals :: Ord v => Either v l -> Either v l -> C l v
islte  x y = C (S.singleton (x `Clte` y)) mempty
isgte  x y = islte y x
equals x y = C (S.singleton (x `Cset` y)) mempty

varIsInteresting :: v -> C l v
varIsInteresting v = C mempty (Set.singleton v)

-- a variable is either set to a value or bounded by other values
data R l a = R l |  Ri (Maybe l) (Set.Set (RS l a))  (Maybe l) (Set.Set (RS l a))
    deriving(Show)

type RS l a =  Element (R l a) a

data Result l a = ResultJust a l
    | ResultBounded {
        resultRep :: a,
        resultLB :: Maybe l,
        resultUB :: Maybe l,
        resultLBV ::[a],
        resultUBV ::[a]
    }

instance (Show l, Show a) => Show (Result l a) where
    showsPrec _ x = (showResult x ++)

showResult (ResultJust a l) = show a ++ " = " ++ show l
showResult rb@ResultBounded {} = sb (resultLB rb) (resultLBV rb) ++ " <= " ++ show (resultRep rb) ++ " <= " ++ sb (resultUB rb) (resultUBV rb)  where
    sb Nothing n | null n = "_"
    sb (Just x) n | null n = show x
    sb Nothing n = show n
    sb (Just x) n = show x ++ show n


collectVars (Cset x y:xs) = x:y:collectVars xs
collectVars (Clte x y:xs) = x:y:collectVars xs
collectVars [] = []

--
-- (C l v) represents a constraint (or set of constraints) that confine the
-- variables 'v' to within specific values of 'l'
--


{-# NOINLINE solve #-}
solve :: (Fixable l, Show l, Show v, Ord v)
    => (String -> IO ())
    -> C l v
    -> IO (Map.Map v v,Map.Map v (Result l v))
solve putLog (C csp _vset) = do
    let vars = Set.fromList [ x | Left x <- collectVars cs]
        cs = S.toList csp
    ufs <- flip mapM (Set.toList vars) $ \a -> do
        uf <- UF.new (Ri Nothing mempty Nothing mempty) a
        return (a,uf)
    let prule (Left x `Clte` Left y) = ans where
            Just xe = Map.lookup x umap
            Just ye = Map.lookup y umap
            ans = do
                xe <- UF.find xe
                ye <- UF.find ye
                xe `lessThenOrEqual` ye
        prule (Right x `Clte` Left y) = ans where
            Just ye = Map.lookup y umap
            ans = do
                ye <- UF.find ye
                x `lessThen` ye
        prule (Left x `Clte` Right y) = ans where
            Just xe = Map.lookup x umap
            ans = do
                xe <- UF.find xe
                y `greaterThen` xe
        prule (Right v `Cset` Left x) = prule (Left x `Cset` Right v)
        prule (Left x `Cset` Right v) = ans where
            Just xe = Map.lookup x umap
            ans = do
                xe <- UF.find xe
                xe `setValue` v
        prule (Left x `Cset` Left y) = ans where
            Just xe = Map.lookup x umap
            Just ye = Map.lookup y umap
            ans = do
                xe <- UF.find xe
                ye <- UF.find ye
                xe `lessThenOrEqual` ye
                xe <- UF.find xe
                ye <- UF.find ye
                ye `lessThenOrEqual` xe
        -- handle constant cases, just check if valid, and perhaps report error
        prule (Right x `Cset` Right y)
            | x `eq` y = return ()
            | otherwise = fail $ "equality of two different values" ++ show (x,y)
        prule (Right x `Clte` Right y)
            | x `lte` y = return ()
            | otherwise = fail $ "invalid constraint: " ++ show x ++ " <= " ++ show y
        setValue xe v = do
            putLog $ "Setting value of " ++ show (fromElement xe) ++ " to " ++ show v
            xw <- getW xe
            case xw of
                R c | c `eq` v -> return ()
                    | otherwise -> fail $ "UnionSolve: equality constraints don't match " ++ show (c,v)
                Ri ml lb mu ub | testBoundLT ml v && testBoundGT mu v -> do
                    mapM_ (v `greaterThen`) (Set.toList lb)
                    mapM_ (v `lessThen`)    (Set.toList ub)
                    updateW (const (R v)) xe
                _ -> error "Util.UnionSolve: invalid Ri"
        testBoundLT Nothing _ = True
        testBoundLT (Just x) y = x `lte` y
        testBoundGT Nothing _ = True
        testBoundGT (Just x) y = y `lte` x
        v `greaterThen` xe = do
            putLog $ "make sure " ++ show (fromElement xe) ++ " is less than " ++ show v
            xw <- UF.getW xe
            case xw of
                R c | c `lte` v -> return ()
                    | otherwise -> fail $ "UnionSolve: greaterThen " ++ show (v,c)
                Ri _ _ (Just n) _ | n `lte` v -> return ()
                Ri ml lb mu ub | testBoundLT ml v -> do
                    doUpdate (Ri ml lb (mmeet (Just v) mu) ub) xe
                    mapM_ (greaterThen v) (Set.toList lb)
                               | otherwise -> fail $ "UnionSolve: testBoundLT " ++ show (ml,v)
        v `lessThen` xe = do
            putLog $ "make sure " ++ show (fromElement xe) ++ " is greater than " ++ show v
            xw <- getW xe
            case xw of
                R c | v `lte` c -> do return ()
                    | otherwise -> fail $ "UnionSolve: lessThen " ++ show (v,c)
                Ri (Just n) _ _ _ |  v `lte` n -> do return ()
                Ri ml lb mu ub | testBoundGT mu v -> do
                    doUpdate (Ri (mjoin (Just v) ml) lb mu ub) xe
                    mapM_ (lessThen v) (Set.toList ub)
                               | otherwise -> fail $ "UnionSolve: testBoundGT " ++ show (mu,v)
        --checkRS :: R l a -> RS l a -> IO ()
        checkRS (Ri (Just l) _ (Just u) _) xe | l `eq` u = do
            putLog $ "Boxed in value of " ++ show (fromElement xe) ++ " being set to " ++ show l
            setValue xe l
        checkRS (Ri (Just l) _ (Just u) _) xe | u `lte` l = fail "checkRS: you crossed the streams"
        checkRS (Ri (Just l) _ _ _) xe  | isTop l = do
            putLog $ "Going up:   " ++ show (fromElement xe)
            setValue xe l
        checkRS (Ri  _ _ (Just u) _) xe | isBottom u = do
            putLog $ "Going down: " ++ show (fromElement xe)
            setValue xe u
        checkRS r xe = return ()
        xe `lessThenOrEqual` ye | xe == ye = return ()
        xe `lessThenOrEqual` ye = do
            xw <- UF.getW xe
            case xw of
                R v -> (v `lessThen` ye)
                Ri xml xlb xmu xub -> do
                    xlb <- finds xlb
                    if ye `Set.member` xub then return () else do
                    xub <- finds xub
                    if ye `Set.member` xlb then equal xe ye  else do
                    yw <- UF.getW ye
                    case yw of
                        R v -> (v `greaterThen` xe)
                        Ri yml ylb ymu yub -> do
                            xlb <- finds xlb
                            if xe `Set.member` ylb then return () else do
                            xub <- finds xub
                            if xe `Set.member` yub then equal xe ye  else do
                            updateW (const (Ri xml xlb (mmeet ymu xmu) (Set.delete xe $ Set.insert ye xub))) xe
                            updateW (const (Ri (mjoin yml xml) (Set.delete ye $ Set.insert xe ylb) ymu yub)) ye
                            w <- getW xe
                            checkRS w xe
                            w <- getW ye
                            checkRS w ye
        doUpdate r xe = do
            updateW (const r) xe
            checkRS r xe
        equal xe ye | xe == ye = return ()
        equal xe ye = do
            xw <- getW xe
            yw <- getW ye
            union const xe ye
            xe <- find xe
            case (xw,yw) of
                (Ri xml xlb xmu xub,Ri yml ylb ymu yub) -> do
                    let nml = xml `mjoin` yml
                        nmu = xmu `mmeet` ymu
                    nlb <- finds (xlb `mappend` ylb)
                    nub <- finds (yub `mappend` xub)
                    doUpdate (Ri nml (Set.delete xe nlb) nmu (Set.delete xe nub)) xe
                _ -> error "Util.UnionSolve: equality, can't happen."
        mjoin Nothing b = b
        mjoin x Nothing = x
        mjoin (Just x) (Just y) = Just (join x y)
        mmeet Nothing b = b
        mmeet x Nothing = x
        mmeet (Just x) (Just y) = Just (meet x y)
        finds set = fmap Set.fromList $ mapM UF.find (Set.toList set)
        umap = Map.fromList ufs
    mapM_ prule cs
    rs <- flip mapM ufs $ \ (a,e) -> do
        e <- find e
        w <- getW e
        rr <- case w of
            R v -> return (ResultJust (fromElement e) v)
            Ri ml lb mu ub -> do
                ub <- fmap (map fromElement . Set.toList) $ finds ub
                lb <- fmap (map fromElement . Set.toList) $ finds lb
                return (ResultBounded { resultRep = fromElement e, resultUB = mu, resultLB = ml, resultLBV = lb, resultUBV = ub })
        let aa = fromElement e
        return ((a,aa),(aa,rr))
    let (ma,mb) = unzip rs
    return (Map.fromList ma,Map.fromList mb)





-------------------
-- useful instances
-------------------

instance Ord n => Fixable (Set.Set n)  where
    isBottom = Set.null
    join a b = Set.union a b
    meet a b = Set.intersection a b
    lte a b = Set.isSubsetOf a b
    eq = (==)


instance Fixable Bool where
    isBottom x = not x
    isTop x = x
    join a b = a || b
    meet a b = a && b
    eq = (==)
    lte = (<=)

-- bottom is zero and the join is the maximum of integer values, as in this is the lattice of maximum, not the additive one.
instance Fixable Int where
    join a b = max a b
    meet a b = min a b
    lte = (<=)
    eq = (==)

instance (Fixable a,Fixable b) => Fixable (a,b) where
    isBottom (a,b) = isBottom a && isBottom b
    isTop (a,b) = isTop a && isTop b
    join (x,y) (x',y') = (join x x', join y y')
    meet (x,y) (x',y') = (meet x x', meet y y')
    lte (x,y) (x',y') = (lte x x' && lte y y')


-- the maybe instance creates a new bottom of nothing. note that (Just bottom) is a distinct point.
instance Fixable a => Fixable (Maybe a) where
    isBottom Nothing = True
    isBottom _ = False
    isTop Nothing = False
    isTop (Just x) = isTop x
    join Nothing b = b
    join a Nothing = a
    join (Just a) (Just b) = Just (join a b)
    meet Nothing b = Nothing
    meet a Nothing = Nothing
    meet (Just a) (Just b) = Just (meet a b)
    lte Nothing _ = True
    lte _ Nothing = False
    lte (Just x) (Just y) = x `lte` y

-- the topped instance creates a new top of everything.
-- this is the opposite of the 'Maybe' instance
data Topped a = Top | Only a
    deriving(Eq,Ord,Show)

-- the maybe instance creates a new bottom of nothing. note that (Just bottom) is a distinct point.
instance Fixable a => Fixable (Topped a) where
    isBottom (Only x) = isBottom x
    isBottom Top = False
    isTop Top = True
    isTop _ = False
    meet Top b = b
    meet a Top = a
    meet (Only a) (Only b) = Only (meet a b)
    join Top b = Top
    join a Top = Top
    join (Only a) (Only b) = Only (join a b)
    eq Top Top = True
    eq (Only x) (Only y) = eq x y
    eq _ _ = False
    lte _ Top = True
    lte Top _ = False
    lte (Only x) (Only y) = x `lte` y





