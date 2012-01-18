module Util.UnionSolve(
    C(),
    solve,
    Fixable(..),
    Topped(..),
    Result(..),
    cAnnotate,
    islte,isgte,equals,
    (@<=),(@>=),(@=),(@<=@),(@>=@),(@=@)
    ) where

import Data.List(intersperse)
import Data.Monoid
import qualified Data.Foldable as S
import qualified Data.Map as Map
import qualified Data.Sequence as S
import qualified Data.Set as Set

import Util.UnionFind as UF

-- simple constraint solver based on ideas from 'Once upon a polymorphic type' paper.

class Fixable a where
    -- determine if we are at the top or bottom of the lattice, we can
    -- solidify bounds if we know we are at an endpoint.
    isBottom :: a -> Bool
    isTop :: a -> Bool
    -- lattice operators
    join :: a -> a -> a
    meet :: a -> a -> a
    eq :: a -> a -> Bool
    lte :: a -> a -> Bool
    -- used for debugging
    showFixable :: a -> String
    -- default methods
    showFixable x | isBottom x = "B"
                  | isTop x = "T"
                  | otherwise = "*"
    eq x y = lte x y && lte y x
    isBottom _ = False
    isTop _ = False

-- arguments are the lattice and the variable type
-- mappended together when used in a writer monad.
-- (C l v) represents a constraint (or set of constraints) that confine the
-- variables 'v' to within specific values of 'l'

newtype C l v = C (S.Seq (CL l v))
    deriving(Monoid)

data Op = OpLte | OpEq | OpGte

instance Show Op where
    show OpEq  = " = "
    show OpGte = " >= "
    show OpLte = " <= "

data CL l v = CV v Op v | CL v Op l | CLAnnotate String (CL l v)

cAnnotate :: String -> C l v -> C l v
cAnnotate s (C seq) = C (fmap (CLAnnotate s) seq)

instance (Show e,Show l) => Show (C l e) where
    showsPrec _ (C xs) = showString "" . foldr (.) id (intersperse (showString "\n") (map shows (S.toList xs))) . showString "\n"

instance (Show e,Show l) => Show (CL l e) where
    showsPrec _ x = case x of
        CV v1 op v2 -> shows v1 . shows op . shows v2
        CL v1 op v2 -> shows v1 . shows op . shows v2

bool t f b = if b then t else f

-- operator constraits, the @ is on the side that takes a variable.
v @<= l = cL v OpLte l
v @>= l = cL v OpGte l
v @=  l = cL v OpEq l
v @<=@ l = cV v OpLte l
v @>=@ l = cV v OpGte l
v @=@  l = cV v OpEq l

cL x y z = C (S.singleton (CL x y z))
cV x y z = C (S.singleton (CV x y z))

-- basic constraints
islte,isgte,equals :: (Fixable l,Ord v) => Either v l -> Either v l -> C l v
islte (Left v1) (Left v2)   = C (S.singleton (CV v1 OpLte v2))
islte (Left v1) (Right v2)  = C (S.singleton (CL v1 OpLte v2))
islte (Right v1) (Left v2)  = C (S.singleton (CL v2 OpGte v1))
islte (Right l1) (Right l2) = bool mempty (error $ "invalid constraint: " ++ showFixable l1 ++ " <= " ++ showFixable l2) (l1 `lte` l2)

isgte (Left v1) (Left v2)   = C (S.singleton (CV v1 OpGte v2))
isgte (Left v1) (Right v2)  = C (S.singleton (CL v1 OpGte v2))
isgte (Right v1) (Left v2)  = C (S.singleton (CL v2 OpLte v1))
isgte (Right l1) (Right l2) = bool mempty (error $ "invalid constraint: " ++ showFixable l1 ++ " >= " ++ showFixable l2) (l2 `lte` l1)

equals (Left v1) (Left v2)   = C (S.singleton (CV v1 OpEq v2))
equals (Left v1) (Right v2)  = C (S.singleton (CL v1 OpEq v2))
equals (Right v1) (Left v2)  = C (S.singleton (CL v2 OpEq v1))
equals (Right l1) (Right l2) = bool mempty (error $ "invalid constraint: " ++ showFixable l1 ++ " = " ++ showFixable l2) (l1 `eq` l2)

-- a variable is either set to a value or bounded by other values
data R l a = R l |  Ri (Maybe l) (Set.Set (RS l a))  (Maybe l) (Set.Set (RS l a))
    deriving(Show)
type RS l a = Element (R l a) a

-- replace variables with UnionFind elements
prepareConstraints :: Ord v => C l v -> IO ([CL l (RS l v)], Map.Map v (RS l v))
prepareConstraints (C cseq) = f Map.empty (S.toList cseq) id [] where
    f m (c:cs) ar rs = do
        let h x mp = case Map.lookup x mp of
                Just v -> return (v,mp)
                Nothing -> do
                    v <- UF.new (Ri Nothing mempty Nothing mempty) x
                    return (v, Map.insert x v mp)
        case c of
            CL x op l -> do
                (x',m') <- h x m
                f m' cs id (ar (CL x' op l):rs)
            CV x op y -> do
                (x',m') <- h x m
                (y',m'') <- h y m'
                f m'' cs id (ar (CV x' op y'):rs)
            CLAnnotate s c -> f m (c:cs) (ar . CLAnnotate s) rs
    f m [] _ rs = return (rs,m)

check op x y = case op of
    OpEq -> x `eq` y
    OpLte -> x `lte` y
    OpGte -> y `lte` x

{-# NOINLINE solve #-}
solve :: (Fixable l, Show l, Show v, Ord v)
    => (String -> IO ())
    -> C l v
    -> IO (Map.Map v v,Map.Map v (Result l v))
solve putLog csp = do
    (pcs,varMap) <- prepareConstraints csp
    let prule (CLAnnotate s cr) =  putLog s >> prule cr
        prule (CV x OpGte y) = prule (CV y OpLte x)
        prule (CV xe OpEq ye) = do
            prule (CV xe OpGte ye)
            prule (CV xe OpLte ye)
        prule (CV xe OpLte ye) = do
            xe <- UF.find xe
            ye <- UF.find ye
            xe `lessThanOrEqual` ye
        prule (CL v op l) = do
            ve <- UF.find v
            doOp "" ve op l
            --case op of
            --    OpGte -> l `lessThan` ve
            --    OpLte -> l `greaterThan` ve
            --    OpEq -> ve `setValue` l
        doOp lvl ve op l = do
            let doOp' ve op l = doOp ('-':lvl) ve op l
            ve <- UF.find ve
            putLog $ lvl ++ "Constraining: " ++ show (fromElement ve) ++ show op ++ show l
            vw <- getW ve
            case vw of
                R c | check op c l -> return ()
                    | otherwise -> fail $ "UnionSolve: constraint doesn't match (" ++ show c ++ show op ++ show l ++ ") when setting " ++ show (fromElement ve)
                ri -> case (op,ri) of
                    (OpEq,Ri ml lb mu ub) | testBoundLT ml l && testBoundGT mu l -> do
                        updateW (const (R l)) ve
                        mapM_ (\v -> doOp' v OpLte l) (Set.toList lb)
                        mapM_ (\v -> doOp' v OpGte l) (Set.toList ub)
                    (OpEq,_) | otherwise -> fail $ "UnionSolve: setValue " ++ show (fromElement ve,vw,l)
                    (OpLte,Ri _ _ (Just n) _) | n `lte` l -> return ()
                    (OpLte,Ri (Just n) _ _ _) | n `eq` l -> doOp' ve OpEq l
                    (OpGte,Ri (Just n) _ _ _) | l `lte` n -> return ()
                    (OpGte,Ri _ _ (Just n) _) | n `eq` l -> doOp' ve OpEq l
                    (OpLte,Ri (Just n) _ _ _) | l `lte` n -> fail $ "UnionSolve: lower than lower bound  " ++ show (fromElement ve,vw,l,n)
                    (OpGte,Ri _ _ (Just n) _) | n `lte` l -> fail $ "UnionSolve: higher than higher bound  " ++ show (fromElement ve,vw,l,n)
                    (OpLte,Ri ml lb mu ub) | testBoundLT ml l -> do
                        doUpdate (Ri ml lb (mmeet (Just l) mu) ub) ve
                        mapM_ (\v -> doOp' v OpGte l) (Set.toList lb)
                    (OpGte,Ri ml lb mu ub) | testBoundGT mu l -> do
                        doUpdate (Ri (mjoin (Just l) ml) lb mu ub) ve
                        mapM_ (\v -> doOp' v OpLte l) (Set.toList ub)
                    _ -> fail $ "UnionSolve: bad " ++  show (fromElement ve,vw,op,l)
                        --mapM_ (lessThan v) (Set.toList ub)
                   --     doUpdate (Ri ml lb (mmeet (Just l) mu) ub) xe
                  --      mapM_ (\v -> CL v OpGte) (Set.toList lb)
         --               mapM_ (greaterThan v) (Set.toList lb)
          --                     | otherwise -> fail $ "UnionSolve: testBoundLT " ++ show (ml,v)
        setValue xe v = doOp "*" xe OpEq v
        greaterThan v xe = doOp "*" xe OpLte v
        lessThan v xe = doOp "*" xe OpGte v
--        setValue xe v = do
--            putLog $ "Setting value of " ++ show (fromElement xe) ++ " to " ++ show v
--            xw <- getW xe
--            case xw of
--                R c | c `eq` v -> return ()
--                    | otherwise -> fail $ "UnionSolve: equality constraints don't match " ++ show (c,v)  ++ " when setting " ++ show (fromElement xe)
--                Ri ml lb mu ub | testBoundLT ml v && testBoundGT mu v -> do
--                    mapM_ (v `greaterThan`) (Set.toList lb)
--                    mapM_ (v `lessThan`)    (Set.toList ub)
--                    updateW (const (R v)) xe
--                _ -> fail $ "UnionSolve: setValue " ++ show (fromElement xe,xw,v)
        testBoundLT Nothing _ = True
        testBoundLT (Just x) y = x `lte` y
        testBoundGT Nothing _ = True
        testBoundGT (Just x) y = y `lte` x
--        v `greaterThan` xe = do
--            putLog $ "make sure " ++ show (fromElement xe) ++ " is less than " ++ show v
--            xw <- UF.getW xe
--            case xw of
--                R c | c `lte` v -> return ()
--                    | otherwise -> fail $ "UnionSolve: greaterThan " ++ show (v,c)
--                Ri _ _ (Just n) _ | n `lte` v -> return ()
--                Ri ml lb mu ub | testBoundLT ml v -> do
--                    doUpdate (Ri ml lb (mmeet (Just v) mu) ub) xe
--                    mapM_ (greaterThan v) (Set.toList lb)
--                               | otherwise -> fail $ "UnionSolve: testBoundLT " ++ show (ml,v)
--        v `lessThan` xe = do
--            putLog $ "make sure " ++ show (fromElement xe) ++ " is greater than " ++ show v
--            xw <- getW xe
--            case xw of
--                R c | v `lte` c -> do return ()
--                    | otherwise -> fail $ "UnionSolve: lessThan " ++ show (v,c)
--                Ri (Just n) _ _ _ |  v `lte` n -> do return ()
--                Ri ml lb mu ub | testBoundGT mu v -> do
--                    doUpdate (Ri (mjoin (Just v) ml) lb mu ub) xe
--                    mapM_ (lessThan v) (Set.toList ub)
--                               | otherwise -> fail $ "UnionSolve: testBoundGT " ++ show (mu,v)
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
        xe `lessThanOrEqual` ye | xe == ye = return ()
        xe `lessThanOrEqual` ye = do
            putLog $ "+Constraining: " ++ show (fromElement xe) ++ show OpLte ++ show (fromElement ye)
            xw <- UF.getW xe
            case xw of
                R v -> (v `lessThan` ye)
                Ri xml xlb xmu xub -> do
                    xub <- finds xub
                    if ye `Set.member` xub then return () else do
                    xlb <- finds xlb
                    if ye `Set.member` xlb then equal xe ye  else do
                    yw <- UF.getW ye
                    case yw of
                        R v -> (v `greaterThan` xe)
                        Ri yml ylb ymu yub -> do
                            ylb <- finds ylb
                            if xe `Set.member` ylb then return () else do
                            yub <- finds yub
                            if xe `Set.member` yub then equal xe ye  else do
                            let newxu = mmeet ymu xmu
                            updateW (const (Ri xml xlb newxu (Set.delete xe $ Set.insert ye xub))) xe
                            case newxu of
                                Just v -> mapM_ (v `greaterThan`) (Set.toList xlb)
                                _ -> return ()
                            let newyl = mjoin yml xml
                            updateW (const (Ri newyl (Set.delete ye $ Set.insert xe ylb) ymu yub)) ye
                            case newyl of
                                Just v -> mapM_ (v `lessThan`) (Set.toList yub)
                                _ -> return ()
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
    mapM_ prule pcs
    rs <- flip mapM (Map.toList varMap) $ \ (a,e) -> do
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

-----------------------------------------------------------
-- The data type the results of the analysis are placed in.
-----------------------------------------------------------
data Result l a =
    ResultJust {
        resultRep :: a,
        resultValue :: l
    } |
    ResultBounded {
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

-------------------------------
-- useful instances for Fixable
-------------------------------

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

-- join is the maximum of integer values, as in this is the lattice of maximum, not the additive one.
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
    eq (x,y) (x',y') = (eq x x' && eq y y')

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
data Topped a = Only a | Top
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
