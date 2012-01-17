module Grin.Devolve(twiddleGrin,devolveTransform) where

import Control.Monad.Identity
import Control.Monad.RWS
import Data.Functor
import Data.IORef
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set

import Grin.Grin
import Grin.Noodle
import Options (verbose,fopts)
import Support.FreeVars
import Support.Transform
import Util.Gen
import Util.SetLike
import qualified FlagOpts as FO

{-# NOINLINE devolveTransform #-}
devolveTransform = transformParms {
    transformDumpProgress = verbose,
    transformCategory = "Devolve",
    transformPass = "Grin",
    transformOperation = devolveGrin
    }

-- devolve grin into a form in which it can be readily converted into C code
-- This lifts any local functions which are ever called in a non-tail-calllike form
-- to the top level.

devolveGrin :: Grin -> IO Grin
devolveGrin grin = do
    col <- newIORef []
    let g (n,l :-> r) = f r >>= \r -> return (n,l :-> r)
        f lt@Let { expDefs = defs, expBody = body, .. } = do
            let iterZ :: Bool -> Map.Map Tag (Set.Set Val) -> [FuncDef] -> Map.Map Tag (Set.Set Val)
                iterZ b pmap (fd@FuncDef { funcDefName = name, funcDefBody = as :-> r }:fs) = iterZ (b || xs' /= xs) (Map.insert name xs pmap) fs where
                    xs = Set.unions $ xs':catMaybes [ Map.lookup t pmap | t <- Set.toList $ freeVars fd]
                    xs' = maybe Set.empty id (Map.lookup name pmap)
                iterZ True pmap [] = iterZ False pmap defs
                iterZ False pmap [] = pmap

                nndefs = [ fd | fd <- defs, funcDefName fd `Set.member` expNonNormal ]
                pmap = iterZ False (fromList [ (funcDefName fd, fromList [ Var x y | (x,y) <- Set.toList $ freeVars (funcDefBody fd), x > v0]) | fd <- nndefs ]) nndefs

                (nmaps,rmaps) = splitEither (map z defs)
                z fd@FuncDef { funcDefName = name, funcDefBody = as :-> r }
                    | name `Set.member` expNonNormal = Left ((name,(as ++ xs) :-> pr),xs)
                    | otherwise = Right fd { funcDefBody = as :-> pr }
                  where xs = maybe [] Set.toList $ Map.lookup name pmap
                        pr = runIdentity $ proc r
                proc (App a as t) | Just xs <- Map.lookup a pmap = return (App a (as ++ Set.toList xs) t)
                proc e = mapExpExp proc e
            --mapM_ print (Map.toList pmap)
            nmaps <- mapM (g . fst) nmaps
            modifyIORef col (++ nmaps)
            updateLetProps <$> mapExpExp f lt { expDefs = rmaps, expBody = runIdentity $ proc body }
        f e = mapExpExp f e
    nf <- mapM g (grinFuncs grin)
    lf <- readIORef col
    let ntenv = extendTyEnv [ createFuncDef False x y | (x,y) <- lf ] (grinTypeEnv grin)
    return $  setGrinFunctions (lf ++ nf) grin { grinPhase = PostDevolve, grinTypeEnv = ntenv }
    --if null lf then return ng else devolveGrin ng
    --if null lf then return ng else devolveGrin ng

-- twiddle does some final clean up before translation to C
-- it replaces unused arguments with 'v0' and adds GC notations

data Env = Env {
    envMap   :: Map.Map Var Var,
    envRoots :: Set.Set Val,
    envVar   :: Var
    }

newtype R a = R (RWS Env (Set.Set Var) () a)
    deriving(Monad,Functor,MonadReader Env,MonadWriter (Set.Set Var))

runR (R x) = fst $ evalRWS x Env { envRoots = mempty, envMap = mempty, envVar = v1 } ()

class Twiddle a where
    twiddle :: a -> R a
    twiddle a = return a

instance Twiddle Exp where
    twiddle = twiddleExp

instance Twiddle Val where
    twiddle = twiddleVal

instance Twiddle a => Twiddle [a] where
    twiddle xs = mapM twiddle xs

twiddleExp e = f e where
--    f (BaseOp Promote vs :>>= rest) = f (Return vs :>>= rest)
--    f (BaseOp Demote vs :>>= rest) = f (Return vs :>>= rest)
    f (x :>>= lam) | fopts FO.Jgc && isAllocing x = do
        roots <- asks envRoots
        let nroots = Set.fromList [ Var v t | (v,t) <- Set.toList (freeVars (if isUsing x then ([] :-> x :>>= lam) else lam)), isNode t, v > v0] Set.\\ roots
        local (\e -> e { envRoots = envRoots e `Set.union` nroots}) $ do
            ne <- return (:>>=) `ap` twiddle x `ap` twiddle lam
            return $ gcRoots (Set.toList nroots) ne
    f (x :>>= lam) = return (:>>=) `ap` twiddle x `ap` twiddle lam
    f l@Let {} = do
        ds <- twiddle (expDefs l)
        b <- twiddle (expBody l)
        return . updateLetProps $ l { expDefs = ds, expBody = b }
    f (Case v as) = return Case `ap` twiddle v `ap` twiddle as
    f x | fopts FO.Jgc && isUsing x && isAllocing x = do
        roots <- asks envRoots
        let nroots = Set.fromList [ Var v t | (v,t) <- Set.toList (freeVars x), isNode t, v > v0] Set.\\ roots
        local (\e -> e { envRoots = envRoots e `Set.union` nroots}) $ do
            ne <- mapExpVal twiddleVal x
            return $ gcRoots (Set.toList nroots) ne
    f n = do e <- mapExpVal twiddleVal n ; mapExpExp twiddle e

    isUsing (BaseOp StoreNode {} _) = True
    isUsing Alloc {} = True
    isUsing _ = False

    isAllocing (BaseOp StoreNode {} _) = True
    isAllocing (BaseOp Eval {} _) = True
    isAllocing (Return [Var {}]) = False
    isAllocing (Return [NodeC {}]) = True
    isAllocing App {} = True
    isAllocing Call {} = True
    isAllocing Let {} = True
    isAllocing (Case _ as) = any isAllocing [ b | _ :-> b <- as]
    isAllocing Alloc {} = True
    isAllocing (e :>>= _ :-> y) = isAllocing e || isAllocing y
    isAllocing _ = False

    gcRoots [] x = x
    gcRoots xs e = GcRoots xs e

    isNode TyNode = True
    isNode TyINode = True
    isNode (TyPtr TyNode) = True
    isNode (TyPtr TyINode) = True
    isNode _ = False

instance Twiddle Lam where
    twiddle (vs :-> y) = do
        let fvs = freeVars vs
        (y,uv) <- censor (Set.filter (`notElem` fvs)) $ listen (twiddle y)
        let fvp' = Map.fromList $ concatMap (\v -> if v `Set.member` uv then [] else [(v,v0)]) fvs
        vs <- censor (const mempty) . local (\e -> e { envMap = fvp' }) $ twiddle vs
        return (vs :-> y)
--    twiddle (vs :-> y) = do
--        cv <- asks envVar
--        let fvp = Map.fromList $ zip fvs [cv ..]
--            fvs = freeVars vs
--        local (\e -> e { envVar = head $ drop (length fvs) [cv .. ], envMap = fvp `Map.union` envMap e }) $ do
--        (y,uv) <- censor (Set.filter (`notElem` take (length fvs) [cv .. ])) $ listen (twiddle y)
--        let fvp' = fmap (\v -> if v `Set.member` uv then v else v0) fvp
--        vs <- censor (const mempty) . local (\e -> e { envMap = fvp' }) $ twiddle vs
--        return (vs :-> y)

twiddleGrin grin = grinFunctions_s fs' grin where
    fs' = runR . twiddle  $ grinFunctions grin

instance Twiddle FuncDef where
    twiddle = funcDefBody_uM twiddle

twiddleVal x = f x where
    f var@(Var v ty) = do
        em <- asks envMap
        case Map.lookup v em of
            Just n -> tell (Set.singleton n) >> return (Var n ty)
            Nothing -> tell (Set.singleton v) >> return var
    f x = mapValVal f x
