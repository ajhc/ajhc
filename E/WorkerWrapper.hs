module E.WorkerWrapper(workWrap,performWorkWrap) where

import Control.Monad.Identity
import Data.Monoid
import Monad

import Atom
import CanType
import DataConstructors
import E.CPR
import E.E
import E.Inline
import E.Strictness
import E.TypeCheck
import E.Values
import Info.Info as Info
import Info.Types
import Name
import qualified Stats


topLike Top = True
topLike (Fun v) = topLike v
topLike _ = False

wrapable (Fun x) (ELam _ e) = f x e where
    f (Fun x) (ELam _ e) = f x e
    f (Tup _) _ = True
    f (Tag [_]) _ = True
    f _ _ = False
wrapable _ _ = False


{-
wrappable :: Monad m =>
    DataTable   -- ^ data table
    -> TVr      -- ^ function name we want to workwrap
    -> E        -- ^ function body
    -> m (E,[TVr])  -- ^ (Body,Args)
wrappable dataTable tvr e@ELam {} = ans where
    cpr = maybe Top id (Info.lookup (tvrInfo tvr))
    Lam sa = maybe (Lam (repeat L)) id (Info.lookup (tvrInfo tvr))
    ans = f e sa cpr
    f (ELam t e) (s:ss) (Fun x) =
    -}


wrappable _ _ _ = fail "Only lambdas are wrappable"

workWrap dataTable tvr e = case workWrap' dataTable tvr e of
    Nothing -> [(tvr,e)]
    Just (x,y) -> [x,y]

workerName x = case fromId x of
    Just y -> toId (toName Val ("W@",'f':show y))
    Nothing -> toId (toName Val ("W@",'f':show x))

workWrap' :: Monad m => DataTable -> TVr -> E -> m ((TVr,E),(TVr,E))
workWrap' dataTable tvr e | wrapable cpr e = ans where
    cpr = maybe Top id (Info.lookup (tvrInfo tvr))
    sa = maybe L id (Info.lookup (tvrInfo tvr))
    ans = return ((setProperty prop_WRAPPER tvr,wrapper),(setProperty prop_WORKER tvr',worker))
    tvr' = TVr { tvrIdent = workerName (tvrIdent tvr), tvrInfo = mempty, tvrType = wt }
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
    f (Tag [n]) e as = (n,reverse as,e)
    f x y z = error $ show (x,y,z)
workWrap' _dataTable tvr e = fail "not workWrapable"


a_workWrap = toAtom "E.Simplify.WorkerWrapper"

performWorkWrap :: DataTable -> [(TVr,E)] -> ([(TVr,E)],Stats.Stat)
performWorkWrap dataTable ds = runIdentity $ Stats.runStatT (wwDs ds) where
    wwDs :: [(TVr,E)] -> Stats.StatT Identity [(TVr,E)]
    wwDs ds = liftM concat $ mapM wwDef ds
    wwDef :: (TVr,E) -> Stats.StatT Identity [(TVr,E)]
    wwDef (tvr,e) = case workWrap' dataTable tvr e of
        Just ((tx,x),(ty,y)) -> do
            Stats.mtick a_workWrap
            y' <- wwE y
            return ([ (tx,x), (ty,y') ] :: [(TVr,E)])
        Nothing -> do
            e' <- wwE e
            return ([(tvr,e')]:: [(TVr,E)])
    wwE :: E -> Stats.StatT Identity E
    wwE (ELetRec ds e) = do
        ds' <- wwDs ds
        e' <- wwE e
        return (ELetRec ds' e')
    wwE e = emapE' wwE e

