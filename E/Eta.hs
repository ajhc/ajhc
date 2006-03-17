module E.Eta(
    ArityType(ATop,ABottom),
    etaExpandAp,
    annotateArity,
    etaExpandDef,
    etaExpandDef',
    getArityInfo,
    etaReduce
    ) where

import Control.Monad.Identity
import Data.Monoid
import Data.Typeable
import Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set

import Support.FreeVars
import Util.NameMonad
import E.E
import E.Subst
import E.Annotate
import E.Program
import E.Traverse(emapE')
import E.Values
import GenUtil hiding(replicateM_)
import DataConstructors
import Support.CanType
import Stats
import qualified Info.Info as Info
import Info.Types(Arity(..))


data ArityType = AFun Bool ArityType | ABottom | ATop
    deriving(Eq,Ord,Show,Typeable)

arity at = f at 0 where
    f (AFun _ a) n = f a $! (1 + n)
    f x n | n `seq` x `seq` True = (x,n)

getArityInfo tvr
    | Just at <- Info.lookup (tvrInfo tvr) = arity at
    | otherwise = (ATop,0)

isOneShot x = False

arityType :: E -> ArityType
arityType e = f e where
    f EError {} = ABottom
    f (ELam x e) = AFun (isOneShot x) (f e)
    f (EAp a b) = case f a of
        AFun _ xs | isCheap b -> xs
        _ -> ATop
    f ec@ECase { eCaseScrutinee = scrut } = case foldr1 andArityType (map f $ caseBodies ec) of
        xs@(AFun True _) -> xs
        xs | isCheap scrut -> xs
        _ -> ATop
    f (ELetRec ds e) = case f e of
        xs@(AFun True _) -> xs
        xs | all isCheap (snds ds) -> xs
        _ -> ATop
    f (EVar tvr) | Just at <- Info.lookup (tvrInfo tvr) = at
    f _ = ATop


andArityType ABottom	    at2		  = at2
andArityType ATop	    at2		  = ATop
andArityType (AFun t1 at1)  (AFun t2 at2) = AFun (t1 && t2) (andArityType at1 at2)
andArityType at1	    at2		  = andArityType at2 at1

lamann _ nfo = return nfo

annotateArity e nfo = annotateArity' (arityType e) nfo

annotateArity' at nfo = Info.insert (Arity n) $ Info.insert at nfo where
    (_,n) = arity at


{-
{-# NOINLINE etaExpand #-}
etaExpand :: Program -> IO Program
etaExpand prog = do
    prog <- annotateProgram mempty (const return) annotateArity lamann prog
    let (p,stat) = runStatM $ programMapBodies (etaExpandE $ progDataTable prog) prog
    Stats.printStat "EtaExpansion" stat
    return p

{-# NOINLINE etaExpandDs #-}
etaExpandDs :: MonadStats m => DataTable -> [(TVr,E)] -> m [(TVr,E)]
etaExpandDs dataTable ds = do
    let Identity ds' = annotateDs mempty (const return) annotateArity lamann ds
    sequence [ do e <- etaExpandE dataTable e; return (t,e) | (t,e) <- ds' ]

-}

expandPis :: DataTable -> E -> E
expandPis dataTable e = f (followAliases dataTable e) where
    f (EPi v r) = EPi v (f (followAliases dataTable r))
    f e = e


fromPi' :: DataTable ->  E -> (E,[TVr])
fromPi' dataTable e = f [] (followAliases dataTable e) where
    f as (EPi v e) = f (v:as) (followAliases dataTable e)
    f as e  =  (e,reverse as)

{-
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
        let (_,ts) = expandPis dataTable (getType e)
            ets = (take (n - length as) ts)
        replicateM_ (length ets) $ mtick ("EtaExpand.{" ++ tvrShowName t)
        let tvrs = f mempty [ (tvrIdent t,t { tvrIdent = n }) |  n <- [2,4 :: Int ..], not $ n `Set.member` freeVars (e,ets) | t <- ets ]
            f map ((n,t):rs) = t { tvrType = substMap map (tvrType t)} : f (Map.insert n (EVar t) map) rs
            f _ [] = []
        return (foldr ELam (foldl EAp e (map EVar tvrs)) tvrs)
    ee' e = return e
-}

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


etaExpandDef' dataTable t e = etaExpandDef dataTable t e >>= \x -> case x of
    Nothing -> return (t,e)
    Just x -> return x

etaExpandDef :: MonadStats m => DataTable -> TVr -> E -> m (Maybe (TVr,E))
etaExpandDef _ _ e | isAtomic e = return Nothing -- will be inlined
etaExpandDef dataTable t e  = ans where
    fvs = freeVars (e,tvrType t)
    at = arityType e
    nameSupply = [ n |  n <- [2,4 :: Int ..], not $ n `Set.member` fvs  ]
    ans = do
        (ne,flag) <- f at e (expandPis dataTable $ tvrType t) nameSupply
        if flag then return (Just (tvrInfo_u (annotateArity' at) t,ne)) else return Nothing
    f (AFun _ a) (ELam tvr e) ty ns | (EPi _ rt) <- followAliases dataTable ty = do
        (ne,flag) <- f a e rt ns
        return (ELam tvr ne,flag)
    f (AFun _ a) e (EPi tt rt) (n:ns) = do
        mtick ("EtaExpand.def.{" ++ tvrShowName t)
        let nv = tt { tvrIdent = n }
            eb = EAp e (EVar nv)
        (ne,_) <- f a eb rt ns
        return (ELam nv ne,True)
    f _ e _ _ = do
        return (e,False)

etaExpandAp :: MonadStats m => DataTable -> TVr -> [E] -> m (Maybe E)
etaExpandAp _ _ [] = return Nothing  -- so simple renames don't get eta-expanded
etaExpandAp dataTable t as | Just at <- Info.lookup (tvrInfo t),let n = snd (arity at), n > length as = do
    let e = foldl EAp (EVar t) as
    let (_,ts) = fromPi' dataTable (getType e)
        ets = (take (n - length as) ts)
    mticks (length ets) ("EtaExpand.{" ++ tvrShowName t)
    let tvrs = f mempty [ (tvrIdent t,t { tvrIdent = n }) |  n <- [2,4 :: Int ..], not $ n `Set.member` freeVars (e,ets) | t <- ets ]
        f map ((n,t):rs) = t { tvrType = substMap map (tvrType t)} : f (Map.insert n (EVar t) map) rs
        f _ [] = []
    return (Just $ foldr ELam (foldl EAp e (map EVar tvrs)) tvrs)
etaExpandAp _ t as = return Nothing


