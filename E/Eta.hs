module E.Eta(
    ArityType(ATop,ABottom),
    etaExpandAp,
    annotateArity,
    deleteArity,
    etaExpandDef,
    etaExpandDef',
    etaExpandProgram,
    getArityInfo,
    etaAnnotateProgram,
    etaReduce
    ) where

import Control.Monad.Identity
import Control.Monad.State
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
import E.Inline
import E.Traverse(emapE')
import E.Values
import GenUtil hiding(replicateM_)
import DataConstructors
import Support.CanType
import Stats
import qualified Info.Info as Info
import Info.Types


data ArityType = AFun Bool ArityType | ABottom | ATop
    deriving(Eq,Ord,Typeable)

instance Show ArityType where
    showsPrec _ ATop = ("ArT" ++)
    showsPrec _ ABottom = ("ArB" ++)
    showsPrec _ (AFun False r) = ('\\':) . shows r
    showsPrec _ (AFun True r) = ("\\o" ++) . shows r

arity at = f at 0 where
    f (AFun _ a) n = f a $! (1 + n)
    f x n | n `seq` x `seq` True = (x,n)

getArityInfo tvr
    | Just at <- Info.lookup (tvrInfo tvr) = arity at
    | otherwise = (ATop,0)

isOneShot x = getProperty prop_ONESHOT x

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

annotateArity' at nfo = Info.insert (Arity n (b == ABottom)) $ Info.insert at nfo where
    (b,n) = arity at

-- delety any arity information
deleteArity nfo = Info.delete  (undefined :: Arity) $ Info.delete (undefined :: Arity) nfo


expandPis :: DataTable -> E -> E
expandPis dataTable e = f (followAliases dataTable e) where
    f (EPi v r) = EPi v (f (followAliases dataTable r))
    f e = e


fromPi' :: DataTable ->  E -> (E,[TVr])
fromPi' dataTable e = f [] (followAliases dataTable e) where
    f as (EPi v e) = f (v:as) (followAliases dataTable e)
    f as e  =  (e,reverse as)



-- this annotates, but only expands top-level definitions
etaExpandProgram :: MonadStats m => Program -> m Program
etaExpandProgram prog = programMapDs f (etaAnnotateProgram prog) where
    f (t,e) = etaExpandDef' (progDataTable prog) t e

-- this annotates a program with its arity information, iterating until a fixpoint is reached.
etaAnnotateProgram :: Program -> Program
etaAnnotateProgram prog = runIdentity $ programMapRecGroups mempty pass iletann pass f prog where
    pass _ = return
    iletann e nfo = return $ annotateArity e nfo
    letann e nfo = case Info.lookup nfo of
        Nothing -> put True >> return (annotateArity e nfo)
        Just at -> do
            let at' = arityType e
            when (at /= at') (put True)
            return $ annotateArity' at' nfo
    f (rec,ts) = do
        let (ts',fs) = runState (annotateDs mempty pass letann pass ts) False
        if fs then f (rec,ts') else return ts'




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
    Nothing -> return (tvrInfo_u (annotateArity e) t,e)
    Just x -> return x

-- | eta expand a definition
etaExpandDef :: MonadStats m => DataTable -> TVr -> E -> m (Maybe (TVr,E))
etaExpandDef _ _ e | isAtomic e = return Nothing -- will be inlined
etaExpandDef dataTable t e  = ans where
    fvs = freeVars (e,tvrType t)
    at = arityType e
    nameSupply = [ n |  n <- [2,4 :: Int ..], not $ n `Set.member` fvs  ]
    ans = do
        -- note that we can't use the type in the tvr, because it will not have the right free typevars.
        (ne,flag) <- f at e (expandPis dataTable $ infertype dataTable e) nameSupply
        if flag then return (Just (tvrInfo_u (annotateArity' at) t,ne)) else return Nothing
    f (AFun _ a) (ELam tvr e) (EPi tvr' rt) ns = do
        (ne,flag) <- f a e (subst tvr' (EVar tvr) rt) ns
        return (ELam tvr ne,flag)
    f (AFun _ a) e (EPi tt rt) (n:ns) = do
        if tvrIdent t == 0
         then mtick ("EtaExpand.random")
          else mtick ("EtaExpand.def.{" ++ tvrShowName t)
        let nv = tt { tvrIdent = n }
            eb = EAp e (EVar nv)
        (ne,_) <- f a eb (subst tt (EVar nv) rt) ns
        return (ELam nv ne,True)
    f _ e _ _ = do
        return (e,False)

-- | eta expand a use of a value
etaExpandAp :: MonadStats m => DataTable -> TVr -> [E] -> m (Maybe E)
etaExpandAp _ _ [] = return Nothing  -- so simple renames don't get eta-expanded
etaExpandAp dataTable t as | Just (Arity n err) <- Info.lookup (tvrInfo t) = case () of
    () | n > length as -> do
            let e = foldl EAp (EVar t) as
            let (_,ts) = fromPi' dataTable (infertype dataTable e)
                ets = (take (n - length as) ts)
            mticks (length ets) ("EtaExpand.use.{" ++ tvrShowName t)
            let tvrs = f mempty [ (tvrIdent t,t { tvrIdent = n }) |  n <- [2,4 :: Int ..], not $ n `Set.member` freeVars (e,ets) | t <- ets ]
                f map ((n,t):rs) = t { tvrType = substMap map (tvrType t)} : f (Map.insert n (EVar t) map) rs
                f _ [] = []
            return (Just $ foldr ELam (foldl EAp e (map EVar tvrs)) tvrs)
       | err && length as > n -> do
            let ot = infertype dataTable (foldl EAp (EVar t) as)
            mticks (length as - n) ("EtaExpand.bottoming.{" ++ tvrShowName t)
            return $ Just (prim_unsafeCoerce ot (foldl EAp (EVar t) (take n as)))  -- we can drop any extra arguments applied to something that bottoms out.
       | otherwise -> return Nothing

etaExpandAp _ t as = return Nothing


