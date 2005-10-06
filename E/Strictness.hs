module E.Strictness(SA(..), solveDs) where

import Control.Monad.Identity
import Control.Monad.Writer
import Data.Monoid
import Data.Typeable
import Monad
import Prelude hiding((&&),(||),not,and,or,any,all)
import qualified Data.Map as Map

import Boolean.Algebra
import Binary
import C.Prims
import E.Annotate
import E.E
import E.Inline
import FindFixpoint
import GenUtil
import Info.Info as Info
import MonoidUtil()


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
--    | U [SA]  -- Unary Constructor.
    | O TVr Int Int -- depends on some other value, called with first int number of arguments and is the second ints argument number.
    | SOr SA SA     -- A or B
    | SAnd SA SA    -- A and B
    | Lam [SA]      -- Lambda Function
    | If TVr Int SA SA  -- if
        deriving(Ord,Eq,Show,Typeable)
        {-! derive: GhcBinary !-}

type SAMap = Map.Map TVr SA



type CResult = [(TVr,SA)]

collectSolve :: E -> IO CResult
collectSolve e = ans where
    cr = collect (tvrSilly,e)
    ans = E.Strictness.solve [ c  | c@(x,_) <- cr, x /= tvrSilly ]

solveDs :: [(TVr,E)] -> IO [(TVr,E)]
solveDs ds = do
    let idclear _ nfo = return $ Info.delete L nfo
        ds' = runIdentity (annotateDs mempty idclear (\_ -> return) (\_ -> return) ds)
        vs = concatMap collect ds'
    cr <- E.Strictness.solve [ c | c@(x,_) <- vs, x /= tvrSilly ]
    let idm = Map.fromList $ [ (tvrIdent x,y) | (x,y) <- cr]
    --mapM_ (\ (tvr,n) -> print (tvrShowName tvr,n)) cr
    let idann id nfo = case Map.lookup id idm of
            Just x -> return $ Info.insert x nfo
            Nothing -> return nfo -- error $ "Could not find :" ++ tvrShowName tvr { tvrIdent = id }
    return $ runIdentity (annotateDs mempty idann (\_ -> return) (\_ -> return) ds')




solve :: CResult -> IO CResult
solve vs = ans where
    mp = Map.fromList [ ((x, case y of Lam _ -> True ; _ -> False) ,i) | (x,y) <- vs | i <- [0..] ]
    wts = [ sol y | (_,y) <- vs]
    ans = do
        bs <- FindFixpoint.solve Nothing L wts
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



collect ::  (TVr,E) -> CResult
collect e = ans where
    ans = execWriter (g e)
    f :: E -> Writer CResult SAMap
    g :: (TVr,E) -> Writer CResult SAMap
    --f (EVar (TVr i _)) = (Map.single i (S 0),[])
    f (EPrim _ as _) = return (andSA (Map.empty:(map (arg (S 0)) as)))
    f (ELit (LitCons _ as _)) = return $ andSA (mempty:(map (arg L) as))
    f (EPi (TVr { tvrType = a }) b) = return $ arg L a `andsa` arg L b
    f (ELit (LitInt {})) = return mempty
    f e | (EVar tvr,as) <- fromAp e = return $ andSA  ((Map.singleton tvr (S (length as))):[ arg (saO tvr (length as) i) a | a <- as | i <- [0..] ])
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
        tell [ (tvr,Map.findWithDefault A tvr sm) | tvr <- ts]
        return $ Map.fromAscList [ (i,L) | (i,v) <- Map.toAscList sm, i `notElem` ts]
    finS ts sm = do
        return $ Map.fromAscList [ (i,v) | (i,v) <- Map.toAscList sm, i `notElem` ts]
    g (t,e)  = ans where
        (b,as) = fromLam e
        ans = do
            samap <- f b
            unless (null as) $ tell [(t,Lam [ Map.findWithDefault A tvr samap |  tvr <- as])]
            return $ Map.fromAscList [ (i,saIf t (length as) v L) | (i,v) <- Map.toAscList samap, i `notElem`  as]
    arg sa (EVar tvr) = Map.singleton tvr sa
    arg sa (ELit _) = mempty
    arg sa (EPi _ _) = mempty
    arg sa (EPrim (APrim (PrimPrim "unsafeCoerce") _) [x] _) = arg sa x
    arg sa e = error $ "Strictness.arg: " ++ show (sa,e)


saIf _ _ a b | a == b = a
saIf t n a b | Just s <- Info.lookup (tvrInfo t) = case s of
    S n' | n' >= n -> a
    _ -> b
saIf x y a b = If x y a b

saO v a i | Just s <- Info.lookup (tvrInfo v) = case s of
    Lam as | length as <= a && i < length as -> as !! i
    _ -> L
saO x y z = O x y z


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

instance SemiBooleanAlgebra (SAMap,[(TVr,SA)]) where
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



