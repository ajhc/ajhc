module E.WorkerWrapper where

import Data.Monoid

import DataConstructors
import E.E
import E.CPR
import E.Strictness
import E.Values
import CanType
import Info.Info as Info
import Info.Types
import Name
import E.TypeCheck


topLike Top = True
topLike (Fun v) = topLike v
topLike _ = False

wrapable (Fun x) (ELam _ e) = f x e where
    f (Fun x) (ELam _ e) = f x e
    f (Tup _) _ = True
    f _ _ = False
wrapable _ _ = False

workWrap :: DataTable -> TVr -> E -> [(TVr,E)]
workWrap dataTable tvr e | wrapable cpr e = ans where
    cpr = maybe Top id (Info.lookup (tvrInfo tvr))
    sa = maybe L id (Info.lookup (tvrInfo tvr))
    ans = [(setProperty prop_WRAPPER tvr,wrapper),(setProperty prop_WORKER tvr',worker)]
    tvr' = TVr { tvrIdent = workerName (tvrIdent tvr), tvrInfo = mempty, tvrType = wt }
    workerName x = case fromId x of
        Just y -> toId (toName Val ("W@",'f':show y))
        Nothing -> toId (toName Val ("W@",'f':show x))
    wt = typeInfer dataTable  worker
    worker = foldr ELam body' args where
        body' = eCase body [cb] Unknown
        cb = Alt (LitCons cname vars bodyTyp) (if isSingleton then EVar sv else (ELit $ unboxedTuple (map EVar vars)))
    wrapper = foldr ELam ne args where
        ne | isSingleton = eStrictLet sv (foldl EAp (EVar tvr') (map EVar args))  (ELit $ LitCons cname [EVar sv] bodyTyp)
           | otherwise = eCase (foldl EAp (EVar tvr') (map EVar args)) [ca] Unknown
        ca = Alt (unboxedTuple vars) (ELit $ LitCons cname (map EVar vars) bodyTyp)
    vars@(~[sv]) = [  tVr i t | t <- slotTypes dataTable cname bodyTyp | i <- [2,4..] ]
    isSingleton = case vars of
        [v] -> getType (getType v) == eHash
        _ -> False
    bodyTyp = typeInfer dataTable body
    (cname,args,body) = f cpr e []
    f (Fun x) (ELam a e) as = f x e (a:as)
    f (Tup n) e as = (n,reverse as,e)
    f x y z = error $ show (x,y,z)
workWrap _dataTable tvr e = [(tvr,e)]


