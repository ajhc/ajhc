module Grin.Linear(grinLinear,W(..)) where

import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Set as Set

import Fixer.Fixer
import Fixer.Supply
import GenUtil
import Grin.Grin
import Support.FreeVars
import Util.SetLike

data W = Zero | One | Omega
    deriving(Ord,Eq,Show)

instance Fixable W where
    bottom = Zero
    isBottom Zero = True
    isBottom _ = False
    lub a b = max a b
    minus a b | a > b = a
    minus _ _ = bottom


{-# NOINLINE grinLinear #-}
grinLinear :: Grin -> IO [(Var,W)]
grinLinear  grin@(Grin { grinTypeEnv = typeEnv, grinCafs = cafs }) = do
    fixer <- newFixer
    argSupply <- newSupply fixer
    varSupply <- newSupply fixer
    mapM_ (go argSupply varSupply) (grinFuncs grin)
    calcFixpoint "linear nodes" fixer
    as <- supplyReadValues argSupply
    mapM_ print $ sortGroupUnderFG fst (snd . snd)  [ (n,(a,v)) | ((n,a),v) <- as ]
    supplyReadValues varSupply

go argSupply varSupply (fn,~(Tup vs) :-> fb) = ans where
    ans = do
        ms <- flip mapM [ (v,z) | ~(Var v _) <- vs | z <- [ 0::Int ..]] $ \ (v,z) -> do
            vv <- supplyValue argSupply (fn,z)
            return (v,(0::Int,vv))
        f fb (Map.fromList ms)
    f (e@Store {} :>>= (Var v (TyPtr TyNode)) :-> fb) mp = do
        mp' <- g e mp
        ee <- supplyValue varSupply v
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
    g Let { expDefs = defs, expBody = body } mp = do
        mp' <- execStateT (mapM_ omegaize (map (\v -> Var v undefined) $ Set.toList $ freeVars defs)) mp
        f body mp'
    g e mp = execStateT (h e) mp
    h (App a [_,b] _) | a == funcApply = omegaize b
    h (App a [Var v _] _) | a == funcEval = eval v
    h (Fetch (Var v _)) = eval v -- XXX can this be weakened?
    h (Fetch (Index (Var v _) _)) = eval v -- XXX can this be weakened?
    h (App a vs _) = fuse a vs
    -- TODO if result of a P1_ partial ap is used once, then the function arguments should be fuse'd rather than omegaized
    h Store { expValue = NodeC a vs } | tagIsSuspFunction a =  fuse (tagFlipFunction a) vs
    h Alloc { expValue = NodeC a vs } | tagIsSuspFunction a =  fuse (tagFlipFunction a) vs
    h Update { expValue = NodeC a vs } | tagIsSuspFunction a =  fuse (tagFlipFunction a) vs
    h Return { expValue = NodeC a vs } | tagIsSuspFunction a =  fuse (tagFlipFunction a) vs
    h Store { expValue = NodeC a vs } = mapM_ omegaize vs
    h Alloc { expValue = NodeC a vs } = mapM_ omegaize vs
    h Update { expValue = NodeC a vs } = mapM_ omegaize vs
    h Return { expValue = NodeC a vs } = mapM_ omegaize vs
    h Prim {} = return ()
    h Error {} = return ()
    h Return { } = return ()
    h Store {} = return ()
    h Alloc {} = return ()
    h Update {} = return ()

    h e = fail ("Grin.Linear.h: " ++ show e)
    fuse a vs = mapM_ farg $ zip (zip (repeat a) [0..]) vs
    omegaize Const {} = return ()
    omegaize Lit {} = return ()
    omegaize ValUnknown {} = return ()
    omegaize ValPrim {} = return ()
    omegaize (Var v _) = do
        mp <- get
        case mlookup v mp of
            Nothing -> return ()
            Just (_,v) -> toOmega v
    omegaize x = fail $ "omegaize: " ++ show x
    farg (_,Const {}) = return ()
    farg (_,Lit {}) = return ()
    farg (_,ValPrim {}) = return ()
    farg z@(an,Var v _) = do
        eval v
        ea <-  supplyValue argSupply an
        mp <- get
        case mlookup v mp of
            Just (_,ev) -> addRule $ ev `isSuperSetOf` ea
            Nothing -> return ()
    farg x = fail ("Grin.Linear.farg: " ++ show x)
    eval v = do
        mp <- get
        case mlookup v mp of
            Just (0,e) -> do
                addRule $ e `isSuperSetOf` value One
                modify (Map.insert v (1,e))
            Just (1,e) -> toOmega e
            Nothing -> return ()


toOmega e = addRule $ e `isSuperSetOf` value Omega



