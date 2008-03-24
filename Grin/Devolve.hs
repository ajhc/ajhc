module Grin.Devolve(twiddleGrin,devolveTransform,devolveGrin) where

import Control.Monad.Identity
import Control.Monad.RWS
import Data.IORef
import qualified Data.Map as Map
import qualified Data.Set as Set

import Util.Gen
import Support.Transform
import Grin.Grin
import Grin.Noodle
import Support.FreeVars

devolveTransform = transformParms {
    transformDumpProgress = True,
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
        f lt@Let { expDefs = defs, expBody = body } = do
            let nonTail = expNonNormal lt
                (nmaps,rmaps) = splitEither (map z defs)
                z fd@FuncDef { funcDefName = name, funcDefBody = as :-> r }
                    | name `Set.member` nonTail = Left ((name,(as ++ xs) :-> proc r),xs)
                    | otherwise = Right fd { funcDefBody = as :-> proc r }
                  where xs = [ Var v t |  (v,t) <- Set.toList $ freeVars (as :-> r), v > v0]
                pmap = Map.fromList [ (n,xs) | ((n,_),xs) <- nmaps]
                proc b = runIdentity (proc' b)
                proc' (App a as t) | Just xs <- Map.lookup a pmap = return (App a (as ++ xs) t)
                proc' e = mapExpExp proc' e
            mapM_ print (Map.toList pmap)
            nmaps <- mapM (g . fst) nmaps
            modifyIORef col (++ nmaps)
            mapExpExp f $  updateLetProps lt { expDefs = rmaps, expBody = proc body }
        f e = mapExpExp f e
    nf <- mapM g (grinFuncs grin)
    lf <- readIORef col
    let ntenv = extendTyEnv [ createFuncDef False x y | (x,y) <- lf ] (grinTypeEnv grin)
    return $ setGrinFunctions (lf ++ nf) grin { grinPhase = PostDevolve, grinTypeEnv = ntenv }


data Env = Env {
    envMap :: Map.Map Var Var,
    envVar :: Var
    }

newtype R a = R (RWS Env (Set.Set Var) () a)
    deriving(Monad,Functor,MonadReader Env,MonadWriter (Set.Set Var))

runR (R x) = fst $ evalRWS x Env { envMap = mempty, envVar = v1 } ()


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
    f (x :>>= lam) = return (:>>=) `ap` twiddle x `ap` twiddle lam
    f l@Let {} = do
        ds <- twiddle (expDefs l)
        b <- twiddle (expBody l)
        return . updateLetProps $ l { expDefs = ds, expBody = b }
    f (Case v as) = return Case `ap` twiddle v `ap` twiddle as
    f n = do e <- mapExpVal twiddleVal n ; mapExpExp twiddle e

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






