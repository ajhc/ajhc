
-- a fast, straightforward points to analysis
-- meant to determine nodes that are always in whnf
-- and find out evals or applys that always
-- apply to a known value

module Grin.NodeAnalyze(nodeAnalyze) where

import Control.Monad.RWS hiding(join)
import Control.Monad.Identity hiding(join)
import Data.Monoid
import System
import qualified Data.Map as Map
import qualified Data.Set as Set

import Support.FreeVars
import Support.Tuple
import Support.CanType
import Atom
import IO
import Grin.Grin hiding(V)
import Grin.Simplify
import Grin.Noodle
import Util.UnionSolve
import Util.Gen


atomUnknown = toAtom "(unknown)"

data NodeType = WHNF | LazyWHNF | Lazy
    deriving(Eq,Ord,Show)


data N = N !NodeType (Topped (Set.Set Atom))
    deriving(Eq)

instance Show N where
    show (N nt ts) = show nt ++ "-" ++ f ts  where
        f Top = "[?]"
        f (Only x) = show (Set.toList x)

instance Fixable NodeType where
    isBottom x = x == WHNF
    isTop x = x == Lazy
    join x y = max x y
    meet x y = min x y
    eq = (==)
    lte x y = x <= y


instance Fixable N where
    isBottom (N a b) = isBottom a && isBottom b
    isTop (N a b) = isTop a && isTop b
    join  (N x y) (N x' y') = N (join x x') (join y y')
    meet  (N x y) (N x' y') = N (meet x x') (meet y y')
    lte   (N x y) (N x' y') = lte x x' && lte y y'


data V = V Va Ty | VIgnore
    deriving(Eq,Ord)

data Va =
    Vr !Var
    | Fa !Atom !Int
    | Fr !Atom !Int
    deriving(Eq,Ord)

vr v t = V (Vr v) t
fa n i t = V (Fa n i) t
fr n i t = V (Fr n i) t

class NodeLike a where
    isGood :: a -> Bool

instance NodeLike Ty where
    isGood TyNode = True
    isGood (TyPtr TyNode) = True
    isGood _ = False

instance NodeLike Val where
    isGood v = isGood (getType v)

instance NodeLike V where
    isGood (V _ t) = isGood t
    isGood _ = False

instance NodeLike (Either V b) where
    isGood (Left n) = isGood n
    isGood _ = True

instance Show V where
    showsPrec _ (V (Vr v) ty) = shows (Var v ty)
    showsPrec _ (V (Fa a i) _) = shows (a,i)
    showsPrec _ (V (Fr a i) _) = shows (i,a)
    showsPrec _ VIgnore = showString "IGN"

newtype M a = M (RWS TyEnv (C N V) Int a)
    deriving(Monad,Functor,MonadWriter (C N V))

runM :: Grin -> M a -> C N V
runM grin (M w) = case runRWS w (grinTypeEnv grin) 1 of
    (_,_,w) -> w

forMn_ xs = forM_ (zip xs [0 :: Int .. ])

{-# NOINLINE nodeAnalyze #-}
nodeAnalyze :: Grin -> IO Grin
nodeAnalyze grin' = do
    let cs = runM grin $ do
            mapM_ doFunc (grinFuncs grin)
            mapM_ docaf (grinCafs grin)
        grin = renameUniqueGrin grin'
        docaf (v,tt) | True = tell $ Right top `equals` Left (V (Vr v) (TyPtr TyNode))
                     | otherwise = return ()
    putStrLn "----------------------------"
    print cs
    putStrLn "----------------------------"
    putStrLn "-- NodeAnayze"
    --(rm,res) <- solve (const (return ())) cs
    (rm,res) <- solve putStrLn cs
    let cmap = Map.map (runIdentity . flip Map.lookup res) rm
    putStrLn "----------------------------"
    mapM_ (\ (x,y) -> putStrLn $ show x ++ " -> " ++ show y) (Map.toList rm)
    putStrLn "----------------------------"
    mapM_ print (Map.elems res)
    putStrLn "----------------------------"
    hFlush stdout
    --exitWith ExitSuccess
    nfs <- mapM (fixupFunc cmap) (grinFuncs grin)
    return $ setGrinFunctions nfs grin


data Todo = Todo Bool [V] | TodoNothing

doFunc :: (Atom,Lam) -> M ()
doFunc (name,arg :-> body) = ans where
    -- restrict values of TyNode type to be in WHNF
    dVar v TyNode = do
        tell $ Left v `islte` Right (N WHNF Top)
    dVar _ _ = return ()
    -- set concrete values for vars based on their type only
    -- should only be used in patterns
    zVar v TyNode = tell $ Left (vr v TyNode) `equals` Right (N WHNF Top)
    zVar v t = tell $ Left (vr v t) `equals` Right top
    ans = do
        let rts = fromTuple $ getType body
        forMn_ rts $ \ (t,i) -> dVar (fr name i t) t
        forMn_ (fromTuple arg) $ \ (~(Var v vt),i) -> do
            dVar (vr v vt) vt
            tell $ Left (fa name i vt) `equals` Left (vr v vt)
        fn (Todo True [ fr name i t | i <- naturals | t <- rts ]) body
    fn ret body = f body where
        f (x :>>= Var v vt :-> rest) = do
            dVar (vr v vt) vt
            gn (Todo True [vr v vt]) x
            f rest
        f (x :>>= Tup vs :-> rest) = do
            vs' <- forM vs $ \ (Var v vt) -> do
                dVar (vr v vt) vt
                return $ vr v vt
            gn (if all (== VIgnore) vs' then TodoNothing else Todo True vs') x
            f rest
        f (x :>>= v :-> rest) = do
            forM_ (Set.toList $ freeVars v) $ \ (v,vt) -> zVar v vt
            gn TodoNothing x
            f rest
        f body = gn ret body
    isfn _ x y | not (isGood x) = mempty
    isfn (Todo True  _) x y = Left x `equals` y
    isfn (Todo False _) x y = Left x `isgte` y
    isfn TodoNothing x y =  mempty
    equals x y | isGood x && isGood y = Util.UnionSolve.equals x y
               | otherwise = mempty
    isgte x y | isGood x && isGood y = Util.UnionSolve.isgte x y
              | otherwise = mempty
    islte x y | isGood x && isGood y = Util.UnionSolve.islte x y
              | otherwise = mempty
    gn ret head = f head where
        fl ret (v :-> body) = do
            forM_ (Set.toList $ freeVars v) $ \ (v,vt) -> zVar v vt
            fn ret body
        dunno ty = do
            dres [Right (if TyNode == t then N WHNF Top else top) | t <- fromTuple ty ]
        dres res = do
            case ret of
                Todo b vs -> forM_ (zip vs res) $ \ (v,r) -> tell (isfn ret v r)
                _ -> return ()
        f (_ :>>= _) = error $ "Grin.NodeAnalyze: :>>="
        f (Case v as)
            | Todo _ n <- ret = mapM_ (fl (Todo False n)) as
            | TodoNothing <- ret = mapM_ (fl TodoNothing) as
        f (App { expFunction = fn, expArgs = [x] }) | fn == funcEval = do
            dres [Right (N WHNF Top)]
        f (App { expFunction = fn, expArgs = [x,y], expType = ty }) | fn == funcApply = do
            convertVal x
            convertVal y
            dunno ty
        f (App { expFunction = fn, expArgs = vs, expType = ty }) = do
            vs' <- mapM convertVal vs
            forMn_ (zip vs vs') $ \ ((tv,v),i) -> when (isGood tv) $ do
                tell $ v `islte` Left (fa fn i (getType tv))
            dres [Left $ fr fn i t | i <- [ 0 .. ] | t <- fromTuple ty ]
        f (Call { expValue = Item fn _, expArgs = vs, expType = ty }) = do
            vs' <- mapM convertVal vs
            forMn_ (zip vs vs') $ \ ((tv,v),i) -> when (isGood tv) $ do
                tell $ v `islte` Left (fa fn i (getType tv))
            dres [Left $ fr fn i t | i <- [ 0 .. ] | t <- fromTuple ty ]
        f (Return x) = do
            ww' <- mapM convertVal (fromTuple x)
            dres ww'
        f (Store w) | TyNode == getType w = do
            ww <- convertVal w
            dres [ww]
        f (Store w) = do
            ww <- convertVal w
            dunno (TyPtr (getType w))
        f (Fetch w) | TyPtr TyNode == getType w = do
            ww <- convertVal w
            dres [ww]
        f (Fetch w) | TyPtr (TyPtr TyNode) == getType w = do
            dres [Right top]
        f Error {} = dres []
        f Prim { expArgs = as } = mapM_ convertVal as
        f Alloc { expValue = v } | getType v == TyNode = do
            v' <- convertVal v
            dres [v']
        f NewRegion { expLam = _ :-> body } = fn ret body
        f (Update (Var vname ty) v) | ty == TyPtr TyNode  = do
            v' <- convertVal v
            tell $ Left (vr vname ty) `isgte` v'
            dres []
        f (Update (Var vname ty) v) | ty == TyPtr (TyPtr TyNode)  = do
            v' <- convertVal v
            dres []
        f Let { expDefs = ds, expBody = e } = do
            mapM_ doFunc (map (\x -> (funcDefName x, funcDefBody x)) ds)
            fn ret e
        f exp = error $ "NodeAnalyze.f: " ++ show exp
--        f _ = dres []


    convertVal (Const _) = return $ Right (N WHNF Top)
    convertVal (NodeC t vs) = case tagUnfunction t of
        Nothing -> return $ Right (N WHNF (Only $ Set.singleton t))
        Just (n,fn) -> do
            vs' <- mapM convertVal vs
            forMn_ (zip vs vs') $ \ ((vt,v),i) -> do
                tell $ v `islte` Left (fa fn i (getType vt))
            forM_ [0 .. n - 1 ] $ \i -> do
               tell $ Right top `islte` Left (fa fn (length vs + i) (TyPtr TyNode))
            return $ Right (N (if n == 0 then Lazy else WHNF) (Only $ Set.singleton t))
    convertVal (Var v t) = return $ Left (vr v t)
    convertVal v | isGood v = return $ Right (N Lazy Top)
    convertVal Lit {} = return $ Left VIgnore
    convertVal Tag {} = return $ Left VIgnore
    convertVal ValPrim {} = return $ Left VIgnore
    convertVal Index {} = return $ Left VIgnore
    convertVal Item {} = return $ Left VIgnore
    convertVal ValUnknown {} = return $ Left VIgnore
    convertVal v = error $ "convertVal " ++ show v

bottom = N WHNF (Only (Set.empty))
top = N Lazy Top

tyWNode = TyNode
tySNode = TyPtr TyNode


fixupFunc cmap (name,l :-> body) = fmap (\b -> (name, l :-> b)) (f body) where
    lupVar (Var v t) =  case Map.lookup (vr v t) cmap of
        _ | v < v0 -> fail "nocafyet"
        Just (ResultJust _ lb) -> return lb
        Just ResultBounded { resultLB = Just lb } -> return lb
        _ -> fail "lupVar"
    lupVar _ = fail "lupVar"
    f a@App { expFunction = fn, expArgs = [arg] } | fn == funcEval, Just n <- lupVar arg = case n of
        N WHNF _ -> do
                putStrLn $ "NA-EVAL-WHNF-" ++ show fn
                return (Fetch arg)
        _ -> return a
--    f a@App { expFunction = fn, expArgs = [afunc,what] } | fn == funcApply, Just n <- lupVar afunc = case n of
--        N WHNF set | [x] <- Set.toList set, Just (1,fn) <- tagUnfunction x -> do
--            putStrLn "NA-APPLY-KNOWN"
--            return (a { expFunction = fn, expArgs =
--        _ -> return a
    f e = mapExpExp f e

