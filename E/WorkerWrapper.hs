module E.WorkerWrapper(performWorkWrap) where

import Control.Monad.Writer
import Data.Monoid
import Maybe
import Monad

import DataConstructors
import E.CPR
import E.E
import E.Traverse
import E.TypeCheck()
import E.Values
import GenUtil
import Info.Info as Info
import Info.Types
import Name.Name
import Stats
import Support.CanType
import qualified E.Demand as Demand


topLike Top = True
topLike (Fun v) = topLike v
topLike _ = False


data Arg =
    Absent
    | Cons Constructor [(Arg,TVr)]
    | Plain

isPlain Plain = True
isPlain _ = False

wrappable :: Monad m =>
    DataTable   -- ^ data table
    -> TVr      -- ^ function name we want to workwrap
    -> E        -- ^ function body
    -> m (Maybe Name,E,[(Arg,TVr)])  -- ^ (CPR Constructor,Body,Args)
wrappable dataTable tvr e@ELam {} = ans where
    cpr = maybe Top id (Info.lookup (tvrInfo tvr))
    Demand.DemandSignature _ (_ Demand.:=> sa) = maybe Demand.absSig id (Info.lookup (tvrInfo tvr))
    ans = f e ( sa ++ repeat Demand.lazy) cpr []
    f (ELam t e) (Demand.S _:ss) (Fun x) ts
       | Just con <- getProduct dataTable tt = f e ss x ((Cons con (as con),t):ts)
         where
            as con = [ (Plain,t { tvrIdent = n, tvrType = st }) | st <- slotTypes dataTable (conName con) tt | n <- tmpNames Val (tvrIdent t) ]
            tt = getType t
    f (ELam t e) (Demand.Absent:ss) (Fun x) ts | isLifted (EVar t) = f e ss x ((Absent,t):ts)
    f (ELam t e) (_:ss) (Fun x) ts = f e ss x ((Plain,t):ts)
    f e _ (Tup n _) ts | isCPR n = return (Just n,e,reverse ts)
    f e _ (Tag [n]) ts | isCPR n = return (Just n,e,reverse ts)
    f e _ _ ts | any (not . isPlain . fst) ts = return (Nothing ,e,reverse ts)
    f _ _ _ _ = fail "not workwrapable"
    isCPR n | (Just [_]) <- getSiblings dataTable n = True
            | otherwise = False
wrappable _ _ _ = fail "Only lambdas are wrappable"

workerName x = case fromId x of
    Just y -> toId (toName Val ("W@",'f':show y))
    Nothing -> toId (toName Val ("W@",'f':show x))

tmpNames ns x = case fromId x of
    Just y  -> [toId (toName ns ("X@",'f':show y ++ "@" ++ show i)) | i <- [(1::Int)..] ]
    Nothing -> [toId (toName ns ("X@",'f':show x ++ "@" ++ show i)) | i <- [(1::Int)..] ]

workWrap' :: MonadStats m => DataTable -> TVr -> E -> m ((TVr,E),(TVr,E))
workWrap' _dataTable tvr _e | getProperty prop_WORKER tvr || getProperty prop_WRAPPER tvr = fail "already workwrapped"
workWrap' _dataTable tvr _e | getProperty prop_INLINE tvr || getProperty prop_SUPERINLINE tvr = fail "going to be inlined"
workWrap' _dataTable tvr _e | getProperty prop_NOINLINE tvr  = fail "not going to be inlined"
workWrap' dataTable tvr e | isJust res = ans where
    res@(~(Just (cname,body,sargs))) = wrappable dataTable tvr e
    args = snds sargs
    args' = concatMap f sargs where
        f (Absent,_) = []
        f (Plain,t) = [t]
        f (Cons c ts,_) = concatMap f ts
    lets = concatMap f sargs where
        f (Absent,t) = [(t,EError "WorkWrap.Absent" (getType t))]
        f (Plain,_) = []
        f (Cons c ts,t) = [(t,ELit (LitCons (conName c) (map EVar (snds ts)) (getType t)))] ++ concatMap f ts
    cases e = f sargs where
        f [] = e
        f ((Absent,_):rs) = f rs
        f ((Plain,_):rs) = f rs
        f ((Cons c ts,t):rs) = eCase (EVar t) [Alt (LitCons (conName c) (snds ts) (getType t)) (f (ts ++ rs))] Unknown
    ans = doTicks >> return ((setProperty prop_WRAPPER tvr,wrapper),(setProperty prop_WORKER tvr',worker))
    tvr' = TVr { tvrIdent = workerName (tvrIdent tvr), tvrInfo = mempty, tvrType = wt }
    worker = foldr ELam body' (args' ++ navar) where
        body' = eLetRec lets $ case cname of
            Just cname -> eCase body [cb] Unknown where
                cb = Alt (LitCons cname vars bodyTyp) (if isSingleton then EVar sv else (ELit $ unboxedTuple (map EVar vars)))
            Nothing -> body
    wrapper = foldr ELam ne args where
        workerCall = (foldl EAp (EVar tvr') (map EVar args' ++ navalue))
        ne | Just cname <- cname, isSingleton = cases $ eStrictLet sv workerCall  (ELit $ LitCons cname [EVar sv] bodyTyp)
           | Just cname <- cname = let ca = Alt (unboxedTuple vars) (ELit $ LitCons cname (map EVar vars) bodyTyp) in  cases $ eCase workerCall [ca] Unknown
           | otherwise = cases $ workerCall
    getName (Just x) = x
    getName Nothing  = error ("workWrap': cname = Nothing: tvr = "++show tvr)
    vars@(~[sv]) = [  tVr i t | t <- slotTypes dataTable (getName cname) bodyTyp | i <- [2,4..] ]
    isSingleton = case vars of
        [v] -> getType (getType v) == eHash
        _ -> False
    Just wt = typecheck dataTable  worker
    Just bodyTyp = typecheck dataTable body
    -- This is to add a dummy arg so workers arn't turned into updatable CAFs
    --needsArg =  all (isJust . fst) sargs && null (concat [ xs | (Just (_,xs),_) <- sargs])
    needsArg = null args'
    (navar,navalue) = if needsArg then ([tvr { tvrType = ltTuple' []}],[eTuple' []]) else ([],[])
    doTicks = do
        case cname of
            --Just n -> mtick ("E.Workwrap.CPR.{" ++ tvrShowName tvr ++ "." ++ show n ++ "}")
            Just n -> mtick ("E.Workwrap.CPR.{"  ++ show n ++ "}")
            _ -> return ()
        flip mapM_ sargs $ \ x -> case x of
            --(Just (n,_),_) ->  mtick ("E.Workwrap.arg.{" ++ tvrShowName tvr ++ "." ++ show (conName n) ++ "}")
            (Cons n _,_) -> mtick ("E.Workwrap.arg.{"  ++ show (conName n) ++ "}")
            (Absent,_) -> mtick "E.Workwrap.arg.absent"
            _ -> return ()
workWrap' _dataTable tvr e = fail "not workWrapable"


performWorkWrap :: DataTable -> [(TVr,E)] -> ([(TVr,E)],Stats.Stat)
performWorkWrap dataTable ds = runWriter (wwDs ds) where
    --wwDs :: [(TVr,E)] -> Stats.StatT Identity [(TVr,E)]
    wwDs ds = liftM concat $ mapM wwDef ds
    --wwDef :: (TVr,E) -> Stats.StatT Identity [(TVr,E)]
    wwDef (tvr,e) = case runStatT (workWrap' dataTable tvr e) of
        Just (((tx,x),(ty,y)),st) -> do
            --Stats.mtick a_workWrap
            tell st
            y' <- wwE y
            return ([ (tx,x), (ty,y') ] :: [(TVr,E)])
        Nothing -> do
            e' <- wwE e
            return ([(tvr,e')]:: [(TVr,E)])
    --wwE :: E -> Stats.StatT Identity E
    wwE (ELetRec ds e) = do
        ds' <- wwDs ds
        e' <- wwE e
        return (ELetRec ds' e')
    wwE e = emapE' wwE e

