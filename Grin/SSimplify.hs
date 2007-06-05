module Grin.SSimplify(simplify) where

import qualified Data.IntSet as IS
import qualified Data.IntMap as IM
import qualified Data.Map as Map
import qualified Data.Set as Set

import Atom
import Grin.Grin
import Grin.Noodle
import Util.Gen
import Util.RWS
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
    envPapp  :: IM.IntMap (Atom,[Val])
    }
    {-! derive: Monoid !-}

newtype SState = SState { usedVars :: IS.IntSet }

data SCol = SCol {
    colStats :: Stats.Stat,
    colFreeVars :: Set.Set Var
    }
    {-! derive: Monoid !-}

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
    let f col = col { colFreeVars = colFreeVars col Set.\\ freeVars ps }
    (e,col) <- censor f $ listen $ local (env' `mappend`) $ simpExp e
    ps <- mapM (zeroVars (`Set.member` colFreeVars col)) ps
    return (ps :-> e)

simpDone :: Exp -> S Exp
simpDone e = do
    pmap <- asks envPapp
    case e of
        (App fap (Var (V vn) _:fs) ty) | fap == funcApply, Just (tl,gs) <- IM.lookup vn pmap -> do
            (cl,fn) <- tagUnfunction tl
            let ne = if cl == 1 then App fn (gs ++ fs) ty else Return [NodeC (partialTag fn (cl - 1)) (gs ++ fs)]
            mtick $ if cl == 1 then "Simplify.Apply.Papp.{" ++ show tl  else ("Simplify.Apply.App.{" ++ show fn)
            return ne
        _ -> do
            cmap <- asks envCSE
            case Map.lookup e cmap of
                Just (n,e') -> do mtick n; return e'
                Nothing -> return e

simpBind :: [Val] -> Exp -> S Exp -> S Exp
simpBind p e cont = f p e where
    cse name xs = do
        (z,col) <- listen $ local (\s -> s { envCSE = Map.fromList [ (x,(toAtom name,y)) | (x,y) <- xs] `Map.union` envCSE s }) cont
        e <- simpDone e
        if isOmittable e && Set.null (freeVars p `Set.intersection` colFreeVars col) then do
            mtick "Simplify.Omit.Bind"
            return z
         else return $ e :>>= (p :-> z)
    cse' name xs = cse name ((e,Return p):xs)
    f p app@(App a [v] _) | a == funcEval =  cse' "Simplify.CSE.eval" [(Fetch v,Return p)]
    f p (Fetch v@Var {}) =  cse' "Simplify.CSE.fetch" [(gEval v,Return p)]
    f [p@(Var (V vn) _)] (Return [v@(NodeC t vs)]) | not (isHoly v) = case tagUnfunction t of
        Nothing -> cse' "Simplify.CSE.return-node" []
        Just (n,fn) -> local (\s -> s { envPapp = IM.insert vn (t,vs) (envPapp s) }) $ cse' "Simplify.CSE.return-node" []
    f [p] (Store v@Var {})  =  cse' "Simplify.CSE.demote" [(Fetch p,Return [v]),(gEval p,Return [v])]
    f [p@(Var (V vn) _)] (Store v@(NodeC t vs)) | not (isHoly v) = case tagIsWHNF t of
        True -> cse' "Simplify.CSE.store-whnf" [(Fetch p,Return [v]),(gEval p,Return [v])]
        False -> cse' "Simplify.CSE.store" []
    f _ _ = cse "Simplify.CSE.NOT" []

extEnv :: Var -> Val -> SEnv -> SEnv
extEnv (V vn) v s = s { envSubst = IM.insert vn v (envSubst s) }



simpExp :: Exp -> S Exp
simpExp e = f e [] where
    f  (a :>>= (v :-> b)) xs = do
        env <- ask
        f a ((env,v,b):xs)

    -- simple transforms
    f (Fetch (Const x)) rs = do
        mtick "Grin.Simplify.fetch-const"
        f (Return [x]) rs
    f (Store x) rs | valIsNF x = do
        mtick "Grin.Simplify.store-normalform"
        f (Return [Const x]) rs
    f (App a [Const n] _) rs | a == funcEval = do
        mtick "Grin.Simplify.eval-const"
        f (Return [n]) rs
    f (Error s t) rs@(_:_) = do
        mtick "Grin.Simplify.error-discard"
        let (_,_,b) = last rs
        f (Error s (getType b)) []

    f (Return [v@Const {}]) ((senv,[Var vn _],b):rs) = do
        mtick "Grin.Simplify.Subst.const"
        fbind vn v senv b rs
    f (Return [v@Var {}]) ((senv,[Var vn _],b):rs) = do
        mtick "Grin.Simplify.Subst.var"
        fbind vn v senv b rs
    f a@(Return [NodeC t xs]) ((senv,[NodeC t' ys],b):rs) | t == t' = do
        mtick "Grin.Simplify.Assign.node-node"
        dtup xs ys senv b rs
    f (Return []) ((senv,[],b):rs) = do
        mtick "Grin.Simplify.Assign.unit-unit"
        dtup [] [] senv b rs
    f a@(Return (xs@(_:_:_))) ((senv,ys,b):rs) = do
        mtick "Grin.Simplify.Assign.tuple-tuple"
        dtup xs ys senv b rs
    f (Case v [l]) rs = do
        f (Return [v] :>>= l) rs
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
        return $ updateLetProps lt { expBody = body, expDefs = defs }
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
    f (NodeC t vs) = do
        vs' <- mapM f vs
        return $ NodeC t vs'
    f x = return x

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
