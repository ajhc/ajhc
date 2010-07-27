module Grin.SSimplify(simplify,explicitRecurse) where

import qualified Data.IntSet as IS
import qualified Data.IntMap as IM
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.Identity
import Data.Maybe

import Support.Tickle
import StringTable.Atom
import Grin.Grin
import Grin.Noodle
import Util.Gen
import Util.RWS
import Util.GMap
import Util.SetLike
import Util.HasSize
import Support.CanType
import Support.FreeVars
import qualified Stats
import Stats(mtick)

-- This goes through and puts grin into a normal form, in addition, it carries out some straightforward
-- simplifications.
--
-- normalized form has the following properties
--
-- :>>= only appears in trailing position
-- Return [v0 .. vn] for n > 1 only appears in trailing position
--
-- all variables and function names are unique in their scope.


data SEnv = SEnv {
    envSubst :: IM.IntMap Val,   -- renaming substitution
    envCSE   :: Map.Map Exp (Atom,Exp),
    envPapp  :: IM.IntMap (Atom,[Val]),
    envPush  :: IM.IntMap Exp
    }
    {-! derive: Monoid !-}

newtype SState = SState { usedVars :: IS.IntSet }

data SCol = SCol {
    colStats :: Stats.Stat,
    colFreeVars :: GSet Var
    }
    {-! derive: Monoid !-}

data ExpInfo = ExpInfo {
    expFreeVars :: GSet Var,
    expUnboxing :: UnboxingResult,
    expType     :: [Ty]
    }

newtype S a = S (RWS SEnv SCol SState a)
    deriving(Monad,Functor,MonadWriter SCol, MonadReader SEnv,MonadState SState)

instance Stats.MonadStats S where
    mtickStat s = S (tell mempty { colStats = s })
    mticks' n a = S (tell mempty { colStats = Stats.singleStat n a })


tellFV v = tell mempty { colFreeVars = freeVars v }


simplify :: Grin -> IO Grin
simplify grin = do
    let (fs,_,SCol { colStats = stats}) = runRWS fun mempty SState { usedVars = mempty }
        S fun = simpFuncs (grinFunctions grin)
    return grin { grinFunctions = fs, grinStats = grinStats grin `mappend` stats }


simpFuncs :: [FuncDef] -> S [FuncDef]
simpFuncs fd = do
    let f fd@FuncDef { funcDefBody = body } = do
            body' <- simpLam body
            return $ updateFuncDefProps fd { funcDefBody = body' }
    mapM f fd

simpLam :: Lam -> S Lam
simpLam (ps :-> e) = do
    (ps,env') <- renamePattern ps
    let f col = col { colFreeVars = colFreeVars col \\ freeVars ps }
    (e,col) <- censor f $ listen $ local (env' `mappend`) $ simpExp e
    ps <- mapM (zeroVars (`member` colFreeVars col)) ps
    return (ps :-> e)


dstore x = BaseOp (StoreNode True) [x]


simpDone :: Exp -> S Exp
simpDone e = do
    pmap <- asks envPapp
    case e of
        (BaseOp (Apply ty) (Var (V vn) _:fs)) | Just (tl,gs) <- IM.lookup vn pmap -> do
            (cl,fn) <- tagUnfunction tl
            let ne = if cl == 1 then App fn (gs ++ fs) ty else dstore (NodeC (partialTag fn (cl - 1)) (gs ++ fs))
            mtick $ if cl == 1 then "Simplify.Apply.Papp.{" ++ show tl  else ("Simplify.Apply.App.{" ++ show fn)
            return ne
        (Case v ls) | isJust utypes -> ans where
            utypes@(~(Just ts)) = unboxTypes ur
            ur = foldr1 combineUnboxing [ getUnboxing e | _ :-> e <- ls ]
            ans = do
                mtick "Grin.Simplify.Unbox.case-return"
                let vs = zipWith Var [v1 ..] ts
                return (unboxModify ur (Case v ls) :>>= vs :->  (unboxRet ur vs))
        (Case v1 ls) | [v1'] :-> Case v2 ls' <- last ls, v1' == v2 || v1 == v2 -> do
            let f (p :-> e) = p :-> Return [v1] :>>= [v1'] :-> e
            mtick "Grin.Simplify.case-merge"
            return $ Case v1 (init ls ++ map f ls')
        --(e :>>= p :-> Return p') | p == p' -> do
        --    mtick "Grin.Simplify.tail-return-omit"
        --    return e
        _ -> do
            cmap <- asks envCSE
            case Map.lookup e cmap of
                Just (n,e') -> do mtick n; tellFV e'; return e'
                Nothing -> return e

simpBind :: [Val] -> Exp -> S Exp -> S Exp
simpBind p e cont = f p e where
    cse name xs = do
        (z,col) <- listen $ local (\s -> s { envCSE = Map.fromList [ (x,(toAtom name,y)) | (x,y) <- xs] `Map.union` envCSE s }) cont
        e <- simpDone e
        if isOmittable e && isEmpty (freeVars p `intersection` colFreeVars col) then do
            mtick "Simplify.Omit.Bind"
            return z
         else return $ e :>>= (p :-> z)
    cse' name xs = cse name ((e,Return p):xs)
    f p app@(BaseOp Eval [v]) =  cse' "Simplify.CSE.eval" [(BaseOp Promote [v],Return p)]
    f p (BaseOp Promote [v@Var {}]) =  cse' "Simplify.CSE.promote" [(gEval v,Return p)]
    f [p] (BaseOp Demote [v@Var {}]) =  cse' "Simplify.CSE.demote" [(BaseOp Promote [p],Return [v]),(gEval p,Return [v])]
    f [p@(Var (V vn) _)] (BaseOp (StoreNode isD) [v@(NodeC t vs)]) | not (isHoly v) = case (isD,tagUnfunction t,tagIsWHNF t) of
        (True,Nothing,_) -> cse "Simplify.CSE.return-node" []
        (True,Just (n,fn),_) -> local (\s -> s { envPapp = IM.insert vn (t,vs) (envPapp s) }) $ cse' "Simplify.CSE.return-node" []
        --(False,_,True)  -> local (\s -> s { envPush = IM.insert vn (Store v) (envPush s) }) $ cse "Simplify.CSE.store-whnf" []
        --(False,_,False) -> cse' "Simplify.CSE.store" []
        _ -> cse' "Simplify.CSE.store" []
--    f [p@(Var (V vn) _)] (Return [v@(NodeC t vs)]) | not (isHoly v) = case tagUnfunction t of
--        Nothing -> cse "Simplify.CSE.return-node" [(Return [p],Return [v]),(Store p,Store v)]
--        Just (n,fn) -> local (\s -> s { envPapp = IM.insert vn (t,vs) (envPapp s) }) $ cse' "Simplify.CSE.return-node" [(Return [p],Return [v]),(Store p,Store v)]
--    f [p@(Var (V vn) _)] (Store v@(NodeC t vs)) | not (isHoly v) = case tagIsWHNF t of
--        True -> local (\s -> s { envPush = IM.insert vn (Store v) (envPush s) }) $ cse "Simplify.CSE.store-whnf" [(BaseOp Promote [p],Return [v]),(gEval p,Return [v])]
--        False -> cse' "Simplify.CSE.store" []
    f _ _ = cse "Simplify.CSE.NOT" []

extEnv :: Var -> Val -> SEnv -> SEnv
extEnv (V vn) v s = s { envSubst = IM.insert vn v (envSubst s) }



simpExp :: Exp -> S Exp
simpExp e = f e [] where
    f (e :>>= p :-> Return p') rs | p == p' = do
        mtick "Grin.Simplify.tail-return-omit"
        f e rs
    f  (a :>>= (v :-> b)) xs = do
        env <- ask
        f a ((env,v,b):xs)

    -- simple transforms
    f (BaseOp Promote [Const x]) rs = do
        mtick "Grin.Simplify.fetch-const"
        f (Return [x]) rs
--    f (Store x) rs | valIsNF x = do
--        mtick "Grin.Simplify.store-normalform"
--        f (Return [Const x]) rs
    f (BaseOp Eval [Const n]) rs = do
        mtick "Grin.Simplify.eval-const"
        f (Return [n]) rs
    f (Error s t) rs@(_:_) = do
        mtick "Grin.Simplify.error-discard"
        let (_,_,b) = last rs
        f (Error s (getType b)) []
    f (Return [v]) ((senv,[Var vn _],b):rs) | valIsConstant v = do
        mtick "Grin.Simplify.Subst.const"
        fbind vn v senv b rs
    f (Return [v@ValUnknown {}]) ((senv,[Var vn _],b):rs) = do
        mtick "Grin.Simplify.Subst.unknown"
        fbind vn v senv b rs
    f (Return [v@Var {}]) ((senv,[Var vn _],b):rs) = do
        mtick "Grin.Simplify.Subst.var"
        fbind vn v senv b rs
--    f a@(Return [NodeC t xs]) ((senv,[NodeC t' ys],b):rs) | t == t' = do
--        mtick "Grin.Simplify.Assign.node-node"
--        dtup xs ys senv b rs
    f (Return []) ((senv,[],b):rs) = do
        mtick "Grin.Simplify.Assign.unit-unit"
        dtup [] [] senv b rs
    f a@(Return (xs@(_:_:_))) ((senv,ys,b):rs) = do
        mtick "Grin.Simplify.Assign.tuple-tuple"
        dtup xs ys senv b rs
    f (Case v@Var {} [l]) rs = do
        f (Return [v] :>>= l) rs
--    f e@(Case v ls) rs | isJust utypes  = ans where
--        utypes@(~(Just ts)) = unboxTypes ur
--        ur = foldr1 combineUnboxing [ getUnboxing e | _ :-> e <- ls ]
--        ans = do
--            mtick "Grin.Simplify.Unbox.case-return"
--            let vs = zipWith Var [v1 ..] ts
--            f (unboxModify ur (Case v ls) :>>= vs :-> Return (unboxRet ur vs)) rs
    f a ((senv,p,b):xs) = do
        a <- g a
        (p,env') <- renamePattern p
        let env'' = env' `mappend` senv
        local (const env'') $ simpBind p a (f b xs)
    f x [] = do
        e <- g x
        simpDone e
    fbind vn v senv b rs = do
        v' <- applySubst v
        local (\_ -> extEnv vn v' senv) $ f b rs

    dtup xs ys senv b rs | sameLength xs ys = do
        xs <- mapM applySubst xs
        (ys,env') <- renamePattern ys
        let env'' = env' `mappend` senv
        z <- local (const env'') $ f b rs
        ts <- mapM (return . Just) [([y],Return [x]) | x <- xs | y <- ys ]
        let h [] = z
            h ((p,v):rs) = v :>>= p :-> h rs
        return $ h [ (p,v) |  Just (p,v) <- ts]
    dtup _ _ _ _ _ = error "dtup: attempt to bind unequal lists"
    g (Case v as) = do
        v <- applySubst v
        as <- mapM simpLam as
        return $ Case v as
    g  lt@Let { expDefs = defs, expBody = body } = do
        body <- f body []
        defs <- simpFuncs defs
        let dnames = fromList $ map funcDefName defs :: GSet Atom
            isInvalid e = isEmpty (freeVars e `intersection` dnames)
        case body of
            e :>>= l :-> r | isInvalid e -> do
                mtick "Simplify.simplify.let-shrink-head"
                return $ e :>>= l :-> updateLetProps lt { expBody = r, expDefs = defs }
            e :>>= l :-> r | isInvalid r -> do
                mtick "Optimize.optimize.let-shrink-tail"
                return (updateLetProps lt { expBody = e } :>>= l :-> r)
            _ -> return $ updateLetProps lt { expBody = body, expDefs = defs }
    g x = applySubstE x



applySubstE :: Exp -> S Exp
applySubstE x = mapExpVal applySubst x

applySubst x = f x where
    f var@(Var (V v) _) = do
        env <- asks envSubst
        case IM.lookup v env of
            Just n -> tellFV n >> return n
            Nothing -> tellFV var >> return var
    f x = mapValVal f x

zeroVars fn x = f x where
    f (Var v ty) | fn v || v == v0 = return (Var v ty)
                 | otherwise = do mtick $ "Simplify.ZeroVar.{" ++ show (Var v ty); return (Var v0 ty)
    f x = mapValVal f x

renamePattern :: [Val] ->  S ([Val],SEnv)
renamePattern x = runWriterT (mapM f x) where
    f :: Val -> WriterT SEnv S Val
    f (Var v@(V vn) t) = do
        v' <- lift $ newVarName v
        let nv = Var v' t
        tell (mempty { envSubst = IM.singleton vn nv })
        return nv
    f x = mapValVal f x

newVarName :: Var -> S Var
newVarName (V 0) = return (V 0)
newVarName (V sv) = do
    s <- gets usedVars
    let nv = v sv
        v n | n `IS.member` s = v (1 + n + IS.size s)
            | otherwise = n
    modify (\e -> e { usedVars = IS.insert nv s })
    return (V nv)




isHoly (NodeC _ as) | any isValUnknown as = True
isHoly n = False

data UnboxingResult = UnErr [Ty] | UnStore !Bool !Atom [Unbox] | UnDemote Unbox | UnReturn [Unbox] | UnTail (Set.Set Atom) UnboxingResult

data Unbox =  UnConst Val | UnUnknown Ty
    deriving(Eq,Ord)

isUnUnknown UnUnknown  {} = True
isUnUnknown _ = False

instance CanType UnboxingResult [Ty] where
    getType (UnErr tys) = tys
    getType (UnReturn us) = map getType us
    getType (UnStore b _ _) = [bool b tyDNode tyINode]
    getType (UnDemote _) = [tyINode]

instance CanType Unbox Ty where
    getType (UnConst v) = getType v
    getType (UnUnknown t) = t

unboxRet :: UnboxingResult -> [Val] -> Exp
unboxRet ur vs = f ur vs where
    f (UnReturn xs) vs = Return $ let (r,[]) = g xs vs in r
    f (UnStore b c xs) vs = let (xs',[]) = g xs vs in BaseOp (StoreNode b) [NodeC c xs']
    f (UnDemote u) vs = let ([u'],[]) = g [u] vs in BaseOp Demote [u']
    f UnErr {} _ = Return []
    f _ vs = Return vs
    g [] vs = ([],vs)
    g (UnUnknown _:xs) (v:vs) = let (r,y) = g xs vs in (v:r,y)
    g (UnConst v:xs) vs = let (r,y) = g xs vs in (v:r,y)
 --   g (UnNode a ts _:xs) vs = let (ts',vs') = g ts vs; (r,y) = g xs vs' in (NodeC a ts':r,y)

unboxTypes :: UnboxingResult -> Maybe [Ty]
unboxTypes ur = f ur where
    f (UnTail {}) = Nothing
    f (UnErr []) = Nothing
    f (UnErr (_:_)) = Just []
    f (UnReturn us) | all isUnUnknown us = Nothing
    f (UnReturn xs) = Just $ concatMap h xs
    f (UnStore _ _ ts) = Just $ concatMap h ts
    f (UnDemote _) = Just [tyDNode]
    h (UnUnknown t) = [t]
    h (UnConst {}) = []

unboxModify :: UnboxingResult -> Exp -> Exp
unboxModify ur = f ur where
    Just nty = unboxTypes ur
    f UnErr {} = id
    f (UnReturn us) | all isUnUnknown us = id
    f (UnReturn xs) = runIdentity . editTail nty (g xs)
    f (UnStore _ _ us) =runIdentity . editTail nty (z us)
    f (UnDemote _) =runIdentity . editTail nty y
    g xs (Return ys) = return $ Return (concat $ zipWith h xs ys)
    h (UnUnknown _) y = [y]
    h (UnConst {}) _ = []
    z xs (BaseOp (StoreNode _) [NodeC _ ts]) = return . Return . concat $ zipWith h xs ts
    y (BaseOp Demote [x]) = return $ Return [x]
    y (Return [Const v]) = return $ Return [v]

combineUnboxing :: UnboxingResult -> UnboxingResult -> UnboxingResult
combineUnboxing ub1 ub2 = f ub1 ub2 where
    f UnErr {} x = x
    f x UnErr {} = x
    f (UnTail t1 u1) (UnTail t2 u2) = UnTail (t1 `union` t2) (f u1 u2)
    f (UnTail t1 u1) u2 = UnTail t1 (f u1 u2)
    f u1 (UnTail t2 u2) = UnTail t2 (f u1 u2)
    f (UnReturn xs) (UnReturn ys) = UnReturn (zipWith g xs ys)
    f (UnStore b1 a1 xs1) (UnStore b2 a2 xs2) | a1 == a2 = UnStore b1 a1 (zipWith g xs1 xs2)
                                              | otherwise = UnReturn [UnUnknown (bool b1 tyDNode tyINode)]
    f (UnDemote u1) (UnDemote u2) = UnDemote (g u1 u2)
    f (UnDemote u1) (UnReturn [UnConst (Const v)]) = UnDemote (UnUnknown tyDNode)
    f (UnReturn [UnConst (Const v)]) (UnDemote u1) = UnDemote (UnUnknown tyDNode)
    f x _ = UnReturn (map UnUnknown (getType x))
    --g (UnNode a1 ubs1 t1) (UnNode a2 ubs2 t2) | a1 == a2 = UnNode a1 (zipWith g ubs1 ubs2) t1
    --                                          | otherwise = UnUnknown t1
    g (UnConst v1) (UnConst v2) | v1 == v2 = UnConst v1
                                | otherwise = UnUnknown (getType v1)
    g x _ = UnUnknown (getType x)

getUnboxing :: Exp -> UnboxingResult
getUnboxing e = f e where
    f (Return rs) = UnReturn (map g rs)
    f (BaseOp (StoreNode b) [NodeC c xs]) = UnStore b c (map g xs)
    f (BaseOp Demote [v]) = UnDemote (g v)
    f (Error _ tys) = UnErr tys
    f (App f _ ts) = UnTail (singleton f) (UnErr ts)
    f (Case _ ls) = foldr1 combineUnboxing  [ f e | _ :-> e <- ls ]
    f Let { expBody = body } = f body
    f (_ :>>= _ :-> e) = f e
    f e = UnReturn (map UnUnknown $ getType e)
    g v | valIsConstant v = UnConst v
--    g (NodeC t xs) = UnNode t (map g xs) tyDNode
    g v = UnUnknown (getType v)

editTail :: Monad m => [Ty] -> (Exp -> m Exp) -> Exp -> m Exp
editTail nty mt te = f (sempty :: GSet Atom) te where
    f _ (Error s ty) = return $ Error s nty
    f lf (Case x ls) = return (Case x) `ap` mapM (g lf) ls
    f lf lt@Let {expIsNormal = False, expBody = body } = do
        body <- f lf body
        return $ updateLetProps lt { expBody = body }
    f lf lt@Let {expDefs = defs, expIsNormal = True } = do
        let nlf = lf `union` fromList (map funcDefName defs)
        mapExpExp (f nlf) lt
    f lf lt@MkCont {expLam = lam, expCont = cont } = do
        a <- g lf lam
        b <- g lf cont
        return $ lt { expLam = a, expCont = b }
    f lf (e1 :>>= p :-> e2) = do
        e2 <- f lf e2
        return $ e1 :>>= p :-> e2
    f lf e@(App a as t) | a `member` lf = return $ App a as nty
    f lf e = mt e
    g lf (p :-> e) = do e <- f lf e; return $ p :-> e

bool b x y = if b then x else y

-- this finds top level functions that call themselves recursively and turns the recursive call into a
-- local definition, allowing it to be compiled to a direct loop.

explicitRecurse
    :: Grin
    -> IO Grin
explicitRecurse grin =  mapGrinFuncsM f grin where
    f name lam | name `notMember` (freeVars lam :: GSet Atom) = return lam
    f name (as :-> e) = do
        let nname = toAtom $ "bR" ++ fromAtom name
            g (App n rs t) | n == name = App nname rs t
            g e = tickle g e
        return $ as :-> grinLet [createFuncDef True nname (as :-> g e) ] (App nname as (getType e))

