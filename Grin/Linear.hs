module Grin.Linear(grinLinear,W(One,Omega)) where

import Control.Monad.Identity
import Data.IORef
import Data.Monoid
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.State

import Atom
import Grin.Grin
import Util.UnionFind

data W = One | Omega | LessThan (Set.Set E)
    deriving(Ord,Eq,Show)
type E = Element W Var

instance Monoid W where
    mappend Omega Omega = Omega
    mappend One One = One
    mappend (LessThan xs) (LessThan ys) = LessThan (Set.union xs ys)
    mappend x LessThan {} = x
    mappend LessThan {} x = x
    mappend x y = error $ "mappend: " ++ show (x,y)
    mempty = LessThan Set.empty

grinLinear :: Grin -> IO [(Var,W)]
grinLinear  grin@(Grin { grinTypeEnv = typeEnv, grinFunctions = grinFunctions, grinCafs = cafs }) = do
    fm <- flip mapM grinFunctions $ \ (a,Tup xs :-> _) ->  do
        xs' <- flip mapM xs $ \ (Var v _) -> new (mempty :: W) v
        return $ Map.fromList [ ((a,x),y) | x <- [0::Int ..] | y <- xs']
    storeVars <- newIORef []
    mapM_ (go (Map.unions fm) storeVars) grinFunctions
    svs <- readIORef storeVars
    mapM_ (updateW (\x -> case x of LessThan {} -> One ; _ -> x)) svs
    svs <- mapM (\x -> do w <- getW x; return (fromElement x,w))  svs
    return svs

go (fm:: Map.Map (Atom,Int) E) storeVars (fn,Tup vs :-> fb) = f fb (Map.fromList [ (v,(0::Int, runIdentity $ Map.lookup (fn,z) fm)) | ~(Var v _) <- vs | z <- [ 0 ..]]) where
    f (e@Store {} :>>= (Var v (TyPtr TyNode)) :-> fb) mp = do
        mp' <- g e mp
        ee <- new mempty v
        modifyIORef storeVars (ee:)
        mp' <- f fb (Map.insert v (0,ee) mp')
        return mp'
    f (e :>>= _ :-> fb) mp = do
        mp' <- g e mp
        f fb mp'
    f e mp = g e mp
    g (Case _ ls) mp = do
        ms <- sequence [ f e mp |  _ :-> e <- ls ]
        let z (x,y) (x',y') = (max x x',y)
        return (Map.unionsWith z ms)
    g e mp = execStateT (h e) mp
    h (App a [_,b]) | a == funcApply = omegaize b
    h (App a [Var v _]) | a == funcEval = eval v
    h (App a vs) = fuse a vs
    h Store { expValue = NodeC a vs } | tagIsSuspFunction a =  fuse (tagFlipFunction a) vs
    h Update { expValue = NodeC a vs } | tagIsSuspFunction a =  fuse (tagFlipFunction a) vs
    h Return { expValue = NodeC a vs } | tagIsSuspFunction a =  fuse (tagFlipFunction a) vs
    h Store { expValue = NodeC a vs } = mapM_ omegaize vs
    h Update { expValue = NodeC a vs } = mapM_ omegaize vs
    h Return { expValue = NodeC a vs } = mapM_ omegaize vs
    h Prim {} = return ()
    h Error {} = return ()
    h Cast {} = return ()   -- casts argument are never node pointers
    h Return { } = return ()
    h Store { } = return ()
    h e = fail ("Grin.Linear.h: " ++ show e)
    fuse a vs = mapM_ farg $ zip (zip (repeat a) [0..]) vs
    omegaize Const {} = return ()
    omegaize Lit {} = return ()
    omegaize (Var v _) = do
        mp <- get
        case Map.lookup v mp of
            Nothing -> return ()
            Just (_,v) -> toOmega v
    omegaize x = fail $ "omegaize: " ++ show x
    farg (_,Const {}) = return ()
    farg z@(an,Var v _) = do
        eval v
        ea <- Map.lookup an fm
        mp <- get
        case Map.lookup v mp of
            Just (_,ev) -> ea `isLessThan` ev
            Nothing -> return ()
    eval v = do
        mp <- get
        case Map.lookup v mp of
            Just (0,e) -> modify (Map.insert v (1,e))
            Just (1,e) -> toOmega e
            Nothing -> return ()




e1 `isLessThan` e2 = do
    w <- getW e2
    case w of
        Omega -> return ()
        _ -> do
            w <- getW e1
            case w of
                Omega -> toOmega e2
                LessThan xs -> updateW (const $ LessThan $ Set.insert e2 xs) e1

toOmega e = do
    w <- getW e
    case w of
        Omega -> return ()
        LessThan ss -> do
            updateW (const Omega) e
            mapM_ toOmega (Set.toList ss)

unify e1 e2 = do
    w1 <- getW e1
    w2 <- getW e2
    union mappend e1 e2
    let f Omega (LessThan ss) = mapM_ toOmega (Set.toList ss)
        f _ _ = return ()
    f w1 w2
    f w2 w1

