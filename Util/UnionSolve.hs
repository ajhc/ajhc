module Util.UnionSolve(
    C(),
    solve,
    Fixable(..),
    Topped(..),
    Result(..),
    islte,isgte,equals,is
    ) where

import Control.Monad(when)
import Data.List(intersperse)
import Data.Monoid
import qualified Data.Set as Set
import qualified Data.Map as Map
import Util.UnionFind as UF

-- simple constraint solver based on ideas from 'Once upon a polymorphic type' paper.


class Fixable a where
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
newtype C l v = C ([CL l v] -> [CL l v])

instance Monoid (C l v) where
    mempty = C id
    mappend (C a) (C b) = C (a . b)

data CL l v = (Either v l) `Clte` (Either v l) | (Either v l) `Cset` (Either v l)
    deriving(Eq,Ord)


instance (Show e,Show l) => Show (C l e) where
    showsPrec _ (C xs) = showString "" . foldr (.) id (intersperse (showString "\n") (map shows (xs []))) . showString "\n"

seither (Left x) = shows x
seither (Right x) = shows x

instance (Show e,Show l) => Show (CL l e) where
    showsPrec _ (x `Clte` y) = seither x . showString " <= " . seither y
    showsPrec _ (x `Cset` l) = seither x . showString " := " . seither l

-- basic constraints
islte,isgte,equals :: Either v l -> Either v l -> C l v

islte x y = C ((x `Clte` y):)
isgte x y = islte y x
--equals x y = (x `islte` y) `mappend` (y `islte` x)

equals x y = is x y

is :: Either v l -> Either v l -> C l v
is x y = C ((x `Cset` y):)


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

data Direction = Lower | Upper



--
-- (C l v) represents a constraint (or set of constraints) that confine the
-- variables 'v' to within specific values of 'l'
--


{-# NOINLINE solve #-}
solve :: (Fixable l, Show l, Show v, Ord v) =>  C l v -> IO (Map.Map v v,Map.Map v (Result l v))
solve (C csp) = do
    let vars = Set.fromList [ x | Left x <- collectVars cs]
        cs = csp []
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
                xe `equal` ye
        -- handle constant cases, just check if valid, and perhaps report error
        prule (Right x `Cset` Right y)
            | x `eq` y = return ()
            | otherwise = fail $ "equality of two different values" ++ show (x,y)
        prule (Right x `Clte` Right y)
            | x `lte` y = return ()
            | otherwise = fail $ "invalid constraint: " ++ show x ++ " <= " ++ show y
        setValue xe v = do
            putStrLn $ "Setting value of " ++ show (fromElement xe) ++ " to " ++ show v
            xw <- getW xe
            case xw of
                R c | c `eq` v -> return ()
                    | otherwise -> fail $ "UnionSolve: equality constraints don't match " ++ show (c,v)
                Ri ml lb mu ub | testBoundLT ml v && testBoundGT mu v -> do
                    updateW (const (R v)) xe
                    case (isBottom v,isTop v) of
                        (True,_) -> do
                            mapM_ (unifyTo Lower xe) (Set.toList lb)
                        (_,True) -> do
                            mapM_ (unifyTo Upper xe) (Set.toList ub)
                        _ -> do
                            when (Just v `nem` mu) $ mapM_ (\lb -> v `greaterThen` lb) (Set.toList lb)
                            when (Just v `nem` ml) $ mapM_ (\ub -> v `lessThen` ub) (Set.toList ub)
        nem Nothing Nothing = False
        nem (Just x) (Just y) = not (x `eq` y)
        nem _ _ = True
        getBounds Lower (Ri _ lb _ _) = lb
        getBounds Upper (Ri _ _ _ ub) = ub
        getBounds _ _ = Set.empty
        unifyTo d xe ye = do
            xe <- find xe
            ye <- find ye
            putStrLn $ "unifying to " ++ show (xe,ye)
            when (xe /= ye) $ do
                yw <- UF.getW ye
                UF.union const xe ye
                mapM_ (unifyTo d xe) (Set.toList (getBounds d yw))
        testBoundLT Nothing _ = True
        testBoundLT (Just x) y = x `lte` y
        testBoundGT Nothing _ = True
        testBoundGT (Just x) y = y `lte` x
        v `greaterThen` xe = do
            putStrLn $ "make sure " ++ show (fromElement xe) ++ " is less than " ++ show v
            xw <- UF.getW xe
            case xw of
                R c | c `lte` v -> return ()
                    | otherwise -> fail $ "UnionSolve: greaterThen " ++ show (v,c)
                Ri _ _ (Just n) _ | n `lte` v -> return ()
                Ri ml lb mu ub | testBoundLT ml v -> do
                    checkRS ((Ri ml lb (mjoin (Just v) mu) ub)) xe
                    mapM_ (greaterThen v) (Set.toList lb)
                               | otherwise -> fail $ "UnionSolve: testBoundLT " ++ show (ml,v)
        v `lessThen` xe = do
            putStrLn $ "make sure " ++ show (fromElement xe) ++ " is greater than " ++ show v
            xw <- getW xe
            case xw of
                R c | v `lte` c -> return ()
                    | otherwise -> fail $ "UnionSolve: lessThen " ++ show (v,c)
                Ri (Just n) _ _ _ | v `lte` n -> return ()
                Ri ml lb mu ub | testBoundGT mu v -> do
                    checkRS ((Ri (mmeet (Just v) ml) lb mu ub)) xe
                    mapM_ (lessThen v) (Set.toList ub)
                               | otherwise -> fail $ "UnionSolve: testBoundGT " ++ show (mu,v)
        --checkRS :: R l a -> RS l a -> IO ()
        checkRS (Ri (Just l) _ (Just u) _) xe | l `eq` u = setValue xe l
        checkRS (Ri (Just l) _ (Just u) _) xe | u `lte` l = error "you crossed the streams"
        checkRS (Ri (Just l) _ _ _) xe  | isTop l = setValue xe l
        checkRS (Ri  _ _ (Just u) _) xe | isBottom u = setValue xe u
        checkRS r xe = updateW (const r) xe
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
                            updateW (const (Ri xml xlb (mjoin ymu xmu) (Set.delete xe $ Set.insert ye xub))) xe
                            updateW (const (Ri (mmeet yml xml) (Set.delete ye $ Set.insert xe ylb) ymu yub)) ye
        equal xe ye | xe == ye = return ()
        equal xe ye = do
            xw <- getW xe
            yw <- getW ye
            union const xe ye
            xe <- find xe
            case (xw,yw) of
                (Ri xml xlb xmu xub,Ri yml ylb ymu yub) -> do
                    nlb <- finds (xlb `mappend` ylb)
                    nub <- finds (yub `mappend` xub)
                    updateW (\_ -> Ri (xml `mmeet` yml) (Set.delete xe nlb) (xmu `mjoin` ymu) (Set.delete xe nub)) xe
                (Ri xml xlb xmu xub,R c) -> do xe `setValue` c
                (R c,Ri xml xlb xmu xub) -> do xe `setValue` c
                (R c1,R c2) | c1 `eq` c2 -> return ()
                _ -> fail $ "error: " ++ show (xw,yw)
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
    --mapM rr cs





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
    meet (Only a) (Only b) = Only (join a b)
    join Top b = Top
    join a Top = Top
    join (Only a) (Only b) = Only (meet a b)
    lte _ Top = True
    lte Top _ = False
    lte (Only x) (Only y) = x `lte` y





