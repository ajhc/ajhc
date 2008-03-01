module E.WorkerWrapper(performWorkWrap,workWrapProgram) where

import Control.Monad.Writer hiding(Product(..))
import Data.Monoid hiding(Product(..))
import Maybe
import Monad

import DataConstructors
import E.CPR
import E.E
import E.FreeVars
import E.Program
import E.Traverse
import E.TypeCheck()
import E.Values
import GenUtil
import Info.Types
import Name.Name
import Name.Names
import Stats hiding(null)
import Support.CanType
import Util.SetLike
import qualified E.Demand as Demand
import qualified Info.Info as Info



data Arg =
    Absent
    | Cons Constructor [(Arg,TVr)]
    | Plain

isPlain Plain = True
isPlain _ = False

fsubs Demand.None = repeat Demand.lazy
fsubs (Demand.Product xs) = xs ++ repeat Demand.lazy

wrappable :: Monad m =>
    DataTable   -- ^ data table
    -> TVr      -- ^ function name we want to workwrap
    -> E        -- ^ function body
    -> m (Maybe Name,E,[(Arg,TVr)])  -- ^ (CPR Constructor,Body,Args)
wrappable dataTable mtvr e@ELam {} = ans where
    cpr = maybe Top id (Info.lookup (tvrInfo mtvr))
    Demand.DemandSignature _ (_ Demand.:=> sa) = maybe Demand.absSig id (Info.lookup (tvrInfo mtvr))
    ans = f e ( sa ++ repeat Demand.lazy) cpr []
    g t@TVr { tvrIdent = 0 } _ = (Absent,t)
    g t (Demand.S subs)
       | Just con <- getProduct dataTable tt = (Cons con (as con),t)
         where
            as con = [ g TVr { tvrIdent = n, tvrType = st, tvrInfo = mempty } demand  | st <- slotTypes dataTable (conName con) tt | n <- tmpNames Val (tvrIdent t) | demand <- fsubs subs]
            tt = getType t
    g t Demand.Absent | isLifted (EVar t) = (Absent,t)
    g t _ = (Plain,t)
    f (ELam t e) (demand:ss) (Fun x) ts = f e ss x (g t demand:ts)
    f e _ (Tup n _) ts | isCPR n = return (Just n,e,reverse ts)
    f e _ (Tag [n]) ts | isCPR n = return (Just n,e,reverse ts)
    f e _ _ ts | any (not . isPlain . fst) ts = return (Nothing ,e,reverse ts)
    f _ _ _ _ = fail "not workwrapable"
    isCPR n | isBoxed n, onlyChild dataTable n = True
            | otherwise = False
    isBoxed n = isJust $ do
        Constructor { conInhabits = c } <- getConstructor n dataTable
        if c == s_Star then return () else do
        Constructor { conInhabits = c } <- getConstructor c dataTable
        if c == s_Star then return () else Nothing
wrappable _ _ _ = fail "Only lambdas are wrappable"

workerName x = case fromId x of
    Just y -> toId (toName Val ("W@",'f':show y))
    Nothing -> toId (toName Val ("W@",'f':show x))

tmpNames ns x = case fromId x of
    Just y  -> [toId (toName ns ("X@",'f':show y ++ "@" ++ show i)) | i <- [(1::Int)..] ]
    Nothing -> [toId (toName ns ("X@",'f':show x ++ "@" ++ show i)) | i <- [(1::Int)..] ]

workWrap' :: MonadStats m => DataTable -> TVr -> E -> m ((TVr,E),(TVr,E))
workWrap' _dataTable tvr _e
    | badProps `intersects` getProperties tvr = fail "Don't workwrap this"
    where badProps = fromList [prop_WRAPPER,prop_INLINE,prop_SUPERINLINE,prop_PLACEHOLDER,prop_NOINLINE]
workWrap' dataTable tvr e | isJust res = ans where
    res@(~(Just (cname,body,sargs))) = wrappable dataTable tvr e
    args = snds sargs
    args' = concatMap f sargs where
        f (Absent,_) = []
        f (Plain,t) = [t]
        f (Cons c ts,_) = concatMap f ts
    lets = concatMap f (zip sargs (map show naturals)) where
        f ((Absent,t),n) = [(t,EError ("WorkWrap.Absent." ++ tvrShowName tvr ++ "." ++ n) (getType t))]
        f ((Plain,_),_) = []
        f ((Cons c ts,t),n) = [(t,ELit (updateLit dataTable litCons { litName = conName c, litArgs = map EVar (snds ts), litType = getType t }))] ++ concatMap f (zip ts [ n ++ "." ++ show i | i <- naturals])
    cases e = f sargs where
        f [] = e
        f ((Absent,_):rs) = f rs
        f ((Plain,_):rs) = f rs
        f ((Cons c ts,t):rs) = eCase (EVar t) [Alt (updateLit dataTable litCons { litName = conName c, litArgs = snds ts, litType = getType t }) (f (ts ++ rs))] Unknown
    nprops = insert prop_WORKER $ getProperties tvr `intersection` fromList [prop_JOINPOINT, prop_ONESHOT]
    ans = doTicks >> return ((setProperty prop_WRAPPER tvr,wrapper),(tvr',worker))
    tvr' = putProperties nprops $ TVr { tvrIdent = workerName (tvrIdent tvr), tvrInfo = mempty, tvrType = wt }
    worker = foldr ELam body' (args' ++ navar) where
        body' = eLetRec lets $ case cname of
            Just cname -> eCase body [cb] Unknown where
                cb = Alt (updateLit dataTable litCons { litName = cname, litArgs = vars, litType = bodyTyp }) (if isSingleton then EVar sv else (ELit $ unboxedTuple (map EVar vars)))
            Nothing -> body
    wrapper = foldr ELam ne args where
        workerCall = (foldl EAp (EVar tvr') (map EVar args' ++ navalue))
        ne | Just cname <- cname, isSingleton = cases $ eStrictLet sv workerCall  (ELit $ updateLit dataTable litCons { litName = cname, litArgs = [EVar sv], litType = bodyTyp })
           | Just cname <- cname = let ca = Alt (unboxedTuple vars) (ELit $ updateLit dataTable litCons { litName = cname, litArgs = (map EVar vars), litType = bodyTyp }) in  cases $ eCase workerCall [ca] Unknown
           | otherwise = cases $ workerCall
    getName (Just x) = x
    getName Nothing  = error ("workWrap': cname = Nothing: tvr = "++show tvr)
    vars@(~[sv]) = [  tVr i t | t <- slotTypes dataTable (getName cname) bodyTyp | i <- newIds dontUseThese ]
    dontUseThese = freeIds (getType tvr) `mappend` freeIds bodyTyp --`mappend` freeIds (EAp (getType tvr) $ EAp (EVar tvr) e)
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
        let argw cn (Absent,_) = mtick $ cn ++ ".absent"
            argw cn (Cons n ts,_) = mtick  nname >> mapM_ (argw nname) ts where
                nname = cn ++ ".{"  ++ show (conName n) ++ "}"
            argw _ _ = return ()
        mapM_ (argw "E.Workwrap.arg") sargs
workWrap' _dataTable tvr e = fail "not workWrapable"


{-# NOINLINE workWrapProgram #-}
workWrapProgram :: Program -> Program
workWrapProgram prog = ans where
    (nds,stats) = performWorkWrap (progDataTable prog) (programDs prog)
    ans = programSetDs' nds prog { progStats = progStats prog `mappend` stats }

{-# NOINLINE performWorkWrap #-}
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
    wwE ELetRec { eDefs = ds, eBody =  e } = do
        ds' <- wwDs ds
        e' <- wwE e
        return (ELetRec ds' e')
    wwE e = emapE' wwE e

