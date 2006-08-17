module Grin.Devolve(devolveGrin) where

import Control.Monad.Identity
import Control.Monad.Writer
import Data.IORef
import qualified Data.Map as Map
import qualified Data.Set as Set

import GenUtil
import Grin.Grin
import Support.FreeVars


devolveGrin :: Grin -> IO Grin
devolveGrin grin = do
    col <- newIORef []
    let g (n,l :-> r) = f r >>= \r -> return (n,l :-> r)
        f lt@Let { expDefs = defs, expBody = body } = do
            let nonTail = concatMap (execWriter . cfunc) (body : map (lamExp . funcDefBody) defs)
                (nmaps,rmaps) = splitEither (map z defs)
                z fd@FuncDef { funcDefName = name, funcDefBody = Tup as :-> r }
                    | name `elem` nonTail = Left ((name,Tup (as ++ xs) :-> proc r),xs)
                    | otherwise = Right fd { funcDefBody = Tup as :-> proc r }
                  where xs = [ Var v t |  (v,t) <- Set.toList $ freeVars (Tup as :-> r)]
                pmap = Map.fromList [ (n,xs) | ((n,_),xs) <- nmaps]
                proc b = runIdentity (proc' b)
                proc' (App a as t) | Just xs <- Map.lookup a pmap = return (App a (as ++ xs) t)
                proc' e = mapExpExp proc' e
            mapM_ print (Map.toList pmap)
            modifyIORef col (++ fsts nmaps)
            return $ if null rmaps then proc body else lt { expDefs = rmaps, expBody = proc body }
        f e = mapExpExp f e
        clfunc (l :-> r) = cfunc r
        cfunc (e :>>= y) = do
            xs <- cfunc e
            tell xs
            clfunc y
        cfunc (App a _ _) = return [a]
        cfunc (Case _ as) = do
            rs <- mapM clfunc as
            return (concat rs)
        cfunc Let { expDefs = defs, expBody = body } = do
            b <- cfunc body
            rs <- mapM (clfunc . funcDefBody) defs
            return $ concat (b:rs)
        cfunc Fetch {} = return []
        cfunc Error {} = return []
        cfunc Prim {} = return []
        cfunc Return {} = return []
        cfunc Store {} = return []
        cfunc Update {} = return []
        cfunc NewRegion { expLam = l } = clfunc l
        cfunc Alloc {} = return []
        cfunc MkCont { expCont = l1, expLam = l2 } = do
            a <- clfunc l1
            b <- clfunc l2
            return (a ++ b)

    nf <- mapM g (grinFuncs grin)
    lf <- readIORef col
    return $ setGrinFunctions (lf ++ nf) grin

