module E.Strictness where

import Boolean.Algebra
import Prelude hiding((&&),(||),not,and,or,any,all)
import E.E
import Data.Monoid
import MonoidUtil()
import GenUtil
import C.Prims
import Control.Monad.Writer
import FindFixpoint
import Control.Monad.Identity
import E.Values
import E.Subst

import qualified Data.Map as Map

newtype Var = V Int
    deriving(Eq,Ord)

instance Show Var where
    show (V x) = tvrShowName (tvr { tvrIdent = x })

-- I believe this is a lattice, but I don't think it is bounded, and I could
-- be wrong.

data SA =
    S Int     -- Strict, argument is number of args guarenteed to be passed to it
    | L       -- Lazy. We don't know whether it will be evaluated
    | A       -- Absent. definitly not evaluated
    | U [SA]  -- Unary Constructor.
    | O Var Int Int -- depends on some other value, called with first int number of arguments and is the second ints argument number.
    | SOr SA SA     -- A or B
    | SAnd SA SA    -- A and B
    | Lam [SA]      -- Lambda Function
    | If Var Int SA SA  -- if
        deriving(Ord,Eq,Show)

type SAMap = Map.Map Var SA



type CResult = [(Var,SA)]

collectSolve :: E -> IO CResult
collectSolve e = ans where
    cr = collect mempty (tVr (-1) Unknown,e)
    ans = E.Strictness.solve [ c  | c@(x,_) <- cr, x /= (V $ -1) ]


solve :: CResult -> IO CResult
solve vs = ans where
    mp = Map.fromList [ ((x, case y of Lam _ -> True ; _ -> False) ,i) | (x,y) <- vs | i <- [0..] ]
    wts = [ sol y | (_,y) <- vs]
    ans = do
        bs <- FindFixpoint.solve (Just "Strictness") L wts
        return [ (x,b) |  (x,_) <- vs | b <- bs ]
    getVal' x
        | Just i <- Map.lookup x mp = getVal i
        | otherwise = return L

    sol L = return L
    sol A = return A
    sol (S n) = return (S n)
    sol (If v n a b) = do
        x <- getVal' (v,False)
        case x of
            S n' | n' >= n -> return a
            _ -> return b
    sol (O v a i) = do
        x <- getVal' (v,True)
        case x of
            Lam as | length as <= a && i < length as -> sol (as !! i)
            _ -> return L
    sol (SAnd a b) = do
        a' <- sol a
        b' <- sol b
        return (sand a' b')
    sol (SOr a b) = do
        a' <- sol a
        b' <- sol b
        return (sor a' b')
    sol (Lam xs) = do
        xs' <- mapM sol xs
        return $ Lam xs'



collect :: SAMap -> (TVr,E) -> CResult
collect env e = ans where
    ans = execWriter (g e)
    f :: E -> Writer CResult SAMap
    g :: (TVr,E) -> Writer CResult SAMap
    --f (EVar (TVr i _)) = (Map.single i (S 0),[])
    f (EPrim _ as _) = return (andSA (Map.empty:(map (arg (S 0)) as)))
    f (ELit (LitCons _ as _)) = return $ andSA (mempty:(map (arg L) as))
    f (EPi (TVr { tvrType = a }) b) = return $ arg L a `andsa` arg L b
    f (ELit (LitInt {})) = return mempty
    f e | (EVar (TVr { tvrIdent = n } ),as) <- fromAp e = return $ andSA  ((Map.singleton (V n) (S (length as))):[ arg (O (V n) (length as) i) a | a <- as | i <- [0..] ])
    f ec@(ECase e b as d) = do
        fe <- f e
        fb <- mapM f (caseBodies ec)
        cb <- finS (caseBinds ec) (orSA fb)
        return $ fe `andsa` cb
    f (EError {}) = return mempty  -- TODO
    f e@(ELam {}) | (b,as) <- fromLam e = f b >>= fin as
    f (ELetRec ds e) = do
        ds' <- mapM g ds
        fe <- f e
        fin (fsts ds)  $ andSA (fe:ds')
    f e@(EAp a b)  = case runIdentity $ app (fromAp e) of
            EAp a' b' | a == a' && b == b' -> error $ "Strictness.f: " ++ show e
            e -> f e
    f e = error $ "Strictness: " ++ show e
    fin ts sm = do
        tell [ (V i,Map.findWithDefault A (V i) sm) | (TVr { tvrIdent = i }) <- ts]
        return $ Map.fromAscList [ (V i,L) | (V i,v) <- Map.toAscList sm, i `notElem` map tvrNum ts]
    finS ts sm = do
        return $ Map.fromAscList [ (V i,v) | (V i,v) <- Map.toAscList sm, i `notElem` map tvrNum ts]
    g (t,e)  = ans where
        (b,as) = fromLam e
        ans = do
            samap <- f b
            when (not $ null as) $ tell [(V (tvrNum t),Lam [ Map.findWithDefault A (V t) samap |  TVr { tvrIdent = t } <- as])]
            return $ Map.fromAscList [ (V i,saIf (V $ tvrNum t) (length as) v L) | (V i,v) <- Map.toAscList samap, i `notElem` map tvrNum as]
    arg sa (EVar (TVr { tvrIdent = i })) = Map.singleton (V i) sa
    arg sa (ELit _) = mempty
    arg sa (EPi _ _) = mempty
    arg sa (EPrim (APrim (PrimPrim "unsafeCoerce") _) [x] _) = arg sa x
    arg sa e = error $ "Strictness.arg: " ++ show (sa,e)


saIf _ _ a b | a == b = a
saIf x y a b = If x y a b

andSA,orSA :: [SAMap] -> SAMap
andSA = foldl andsa mempty
orSA = foldl1 orsa
andsa,orsa :: SAMap -> SAMap -> SAMap
andsa = squiddle (&&)
orsa = squiddle (||)

squiddle f ma mb = Map.unionWith f ma' mb' where
    nk = snub (Map.keys ma ++ Map.keys mb)
    tm = Map.fromList [ (k,A) | k <- nk]
    ma' = ma `Map.union` tm
    mb' = mb `Map.union` tm

instance SemiBooleanAlgebra (SAMap,[(Var,SA)]) where
    (x,y) && (x',y') = (Map.unionWith (&&) x x',y ++ y')
    (x,y) || (x',y') = (Map.unionWith (||) x x',y ++ y')

instance SemiBooleanAlgebra SA where
    a && b = sand a b
    a || b = sor a b

-- Can this be simplified? is this a lattice?

sand a b = (if x == SAnd a b then sand' b a else x ) where
    x = sand' a b
    sand' a b | a == b = a
    sand' (S a) (S b) = S (max a b)
    sand' (S a) _ = S a
    sand' _ (S a) = S a
    sand' A x = x
    sand' L L = L
    sand' a b = SAnd a b
--sand' (a `SAnd` b) c = sand a (sand b c)
--sand (U sa) (U sb) = U (zipWith sand sa sb)
--sand (U a) _ = U a
--sand _ (U a) = U a



sor a b = if x == SOr a b then sor' b a else x where
    x = sor' a b
    sor' a b | a == b = a
    sor' (S a) (S b) = S (min a b)
    sor' A (S _) = L
    sor' L (S _) = L
    sor' L _ = L
    sor' A A = A
    sor' a b = SOr a b
    --sor' (a `SOr` b) c = sor' a (sor' b c)



