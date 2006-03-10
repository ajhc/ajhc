module E.Eta(
    etaExpandAp,
    etaReduce
    ) where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Monad.Identity
import Data.Monoid

import Support.FreeVars
import Util.NameMonad
import E.E
import E.Subst
import E.Annotate
import E.Program
import E.Traverse(emapE')
import GenUtil hiding(replicateM_)
import DataConstructors
import Support.CanType
import Stats
import qualified Info.Info as Info
import Info.Types(Arity(..))

manifestLambdas :: E -> Arity
manifestLambdas e = Arity (f 0 e) where
    f n (ELam _ e) = let n' = n + 1 in n' `seq` f n' e
    f n _ = n

lamann _ nfo = return nfo
letann e nfo = return (Info.insert (manifestLambdas e) nfo)

{-# NOINLINE etaExpand #-}
etaExpand :: Program -> IO Program
etaExpand prog = do
    prog <- annotateProgram mempty (const return) letann lamann prog
    let (p,stat) = runStatM $ programMapBodies (etaExpandE $ progDataTable prog) prog
    Stats.printStat "EtaExpansion" stat
    return p

{-# NOINLINE etaExpandDs #-}
etaExpandDs :: MonadStats m => DataTable -> [(TVr,E)] -> m [(TVr,E)]
etaExpandDs dataTable ds = do
    let Identity ds' = annotateDs mempty (const return) letann lamann ds
    sequence [ do e <- etaExpandE dataTable e; return (t,e) | (t,e) <- ds' ]

fromPi' :: DataTable ->  E -> (E,[TVr])
fromPi' dataTable e = f [] (followAliases dataTable e) where
    f as (EPi v e) = f (v:as) (followAliases dataTable e)
    f as e  =  (e,reverse as)

etaExpandE :: MonadStats m => DataTable -> E -> m E
etaExpandE dataTable e = f e where
    f (ELetRec ds e) = do
        ds' <- sequence [ do e' <- ee e; e'' <- f e'; return (t,e'') | (t,e) <- ds]
        e <- ee e
        e' <- f e
        return $ ELetRec ds' e'
    f ec@ECase {} = do
        ec' <- caseBodiesMapM ee ec
        emapE' f ec'
    f e = emapE' f e
    ee e@EVar {} = return e
    ee e = ee' e
    ee' (ELam t e) = do
        e' <- ee' e
        return (ELam t e')
    ee' e | (EVar t,as) <- fromAp e , Just (Arity n) <- Info.lookup (tvrInfo t), n > length as = do
        let (_,ts) = fromPi' dataTable (getType e)
            ets = (take (n - length as) ts)
        replicateM_ (length ets) $ mtick ("EtaExpand.{" ++ tvrShowName t)
        let tvrs = f mempty [ (tvrIdent t,t { tvrIdent = n }) |  n <- [2,4 :: Int ..], not $ n `Set.member` freeVars (e,ets) | t <- ets ]
            f map ((n,t):rs) = t { tvrType = substMap map (tvrType t)} : f (Map.insert n (EVar t) map) rs
            f _ [] = []
        return (foldr ELam (foldl EAp e (map EVar tvrs)) tvrs)
    ee' e = return e

-- | eta reduce as much as possible
etaReduce :: E -> E
etaReduce e = f e where
        f (ELam t (EAp x (EVar t'))) | t == t' && not (tvrNum t `Set.member` freeVars x) = f x
        f e = e

-- | only reduce if all lambdas can be discarded. otherwise leave them in place
etaReduce' :: E -> (E,Int)
etaReduce' e = case f e 0 of
        (ELam {},_) -> (e,0)
        x -> x
    where
        f (ELam t (EAp x (EVar t'))) n | n `seq` True, t == t' && not (tvrNum t `Set.member` freeVars x) = f x (n + 1)
        f e n = (e,n)


etaExpandAp :: MonadStats m => DataTable -> TVr -> [E] -> m (Maybe E)
etaExpandAp _ _ [] = return Nothing  -- so simple renames don't get eta-expanded
etaExpandAp dataTable t as | Just (Arity n) <- Info.lookup (tvrInfo t), n > length as = do
    let e = foldl EAp (EVar t) as
    let (_,ts) = fromPi' dataTable (getType e)
        ets = (take (n - length as) ts)
    mticks (length ets) ("EtaExpand.{" ++ tvrShowName t)
    let tvrs = f mempty [ (tvrIdent t,t { tvrIdent = n }) |  n <- [2,4 :: Int ..], not $ n `Set.member` freeVars (e,ets) | t <- ets ]
        f map ((n,t):rs) = t { tvrType = substMap map (tvrType t)} : f (Map.insert n (EVar t) map) rs
        f _ [] = []
    return (Just $ foldr ELam (foldl EAp e (map EVar tvrs)) tvrs)
etaExpandAp _ t as = return Nothing


