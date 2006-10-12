module Grin.Devolve(devolveGrin) where

import Control.Monad.Identity
import Control.Monad.Writer
import Data.IORef
import qualified Data.Map as Map
import qualified Data.Set as Set

import Util.Gen
import Grin.Grin
import Grin.Noodle
import Support.FreeVars


devolveGrin :: Grin -> IO Grin
devolveGrin grin = do
    col <- newIORef []
    let g (n,l :-> r) = f r >>= \r -> return (n,l :-> r)
        f lt@Let { expDefs = defs, expBody = body } = do
            let nonTail = snd $ mconcatMap collectFuncs (body : map (lamExp . funcDefBody) defs)
                (nmaps,rmaps) = splitEither (map z defs)
                z fd@FuncDef { funcDefName = name, funcDefBody = Tup as :-> r }
                    | name `Set.member` nonTail = Left ((name,Tup (as ++ xs) :-> proc r),xs)
                    | otherwise = Right fd { funcDefBody = Tup as :-> proc r }
                  where xs = [ Var v t |  (v,t) <- Set.toList $ freeVars (Tup as :-> r)]
                pmap = Map.fromList [ (n,xs) | ((n,_),xs) <- nmaps]
                proc b = runIdentity (proc' b)
                proc' (App a as t) | Just xs <- Map.lookup a pmap = return (App a (as ++ xs) t)
                proc' e = mapExpExp proc' e
            mapM_ print (Map.toList pmap)
            modifyIORef col (++ fsts nmaps)
            return $  updateLetProps lt { expDefs = rmaps, expBody = proc body }
        f e = mapExpExp f e
    nf <- mapM g (grinFuncs grin)
    lf <- readIORef col
    let ntenv = extendTyEnv [ createFuncDef False x y | (x,y) <- lf ] (grinTypeEnv grin)
    return $ setGrinFunctions (lf ++ nf) grin { grinPhase = PostDevolve, grinTypeEnv = ntenv }

