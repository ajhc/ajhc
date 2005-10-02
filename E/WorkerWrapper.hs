module E.WorkerWrapper where

import Data.Monoid

import DataConstructors
import E.E
import E.CPR
import E.Strictness
import Info.Info as Info
import Info.Types
import Name


topLike Top = True
topLike (Fun v) = topLike v
topLike _ = False


workWrap :: DataTable -> TVr -> E -> [(TVr,E)]
workWrap dataTable tvr e
    | topLike cpr && sa == L = [(tvr,e)]
    | otherwise = ans where
        cpr = maybe Top id (Info.lookup (tvrInfo tvr))
        sa = maybe L id (Info.lookup (tvrInfo tvr))
        ans = [(setProperty prop_WRAPPER tvr,wrapper),(setProperty prop_WORKER tvr',worker)]
        tvr' = TVr { tvrIdent = workerName (tvrIdent tvr), tvrInfo = mempty, tvrType = wt }
        workerName x = case fromId x of
            Just y -> toId (toName Val ("W@",'f':show y))
            Nothing -> toId (toName Val ("W@",'f':show x))
        wt = undefined
        worker = undefined
        wrapper = undefined


