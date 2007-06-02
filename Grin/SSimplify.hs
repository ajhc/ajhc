module Grin.SSimplify(simplify) where

import qualified Data.IntSet as IS
import qualified Data.IntMap as IM

import Grin.Grin
import Grin.Noodle
import Util.Gen
import Util.RWS
import Support.CanType
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
    envScope :: IM.IntMap Binding
    }

newtype SState = SState { usedVars :: IS.IntSet }

newtype S a = S (RWS SEnv Stats.Stat SState a)
    deriving(Monad,Functor,MonadReader SEnv,MonadState SState)

instance Stats.MonadStats S where
    mtickStat s = S (tell s)
    mticks' n a = S (tell $ Stats.singleStat n a)


data Binding = IsBoundTo Val | Promote Val | Demote Val


instance Monoid SEnv where
    mempty = SEnv { envScope = mempty, envSubst = mempty }
    mappend sa sb = SEnv { envScope = envScope sa `IM.union` envScope sb, envSubst = envSubst sa `IM.union` envSubst sb }

simplify :: Grin -> IO Grin
simplify grin = do
    let (fs,_,stats) = runRWS fun mempty SState { usedVars = mempty }
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
    e <- local (env' `mappend`) $ simpExp e
    return (ps :-> e)


simpBind :: ([Val],Exp) -> S (Maybe ([Val],Exp))
simpBind (b,e) = f b e where
    f b (Fetch (Const x)) = do
        mtick "Grin.Simplify.fetch-const"
        return $ Just (b,Return [x])
    f b e = return $ Just (b,e)

extEnv :: Var -> Val -> SEnv -> SEnv
extEnv (V vn) v s = s { envSubst = IM.insert vn v (envSubst s) }

extScope :: Var -> Binding -> SEnv -> SEnv
extScope (V vn) v s = s { envScope = IM.insert vn v (envScope s) }

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

--    f (Store [v@Var {}]) ((senv,[Var vn _],b):rs) = do
--        v' <- applySubst v
--        local (\_ -> extScope vn (Demote v') senv) $ f b rs
--    f (Fetch [v@Var {}]) ((senv,[Var vn _],b):rs) = do
--        v' <- applySubst v
--        local (\_ -> extScope vn (Promote v') senv) $ f b rs
--    f (Store [v@(NodeC t _)]) ((senv,[Var vn _],b):rs) | tagIsWHNF t, not (isHoly v) = do
--        v' <- applySubst v
--        local (\_ -> extScope vn (Demote v') senv) $ f b rs
--    f (Return [v@(NodeC t _)]) ((senv,[Var vn _],b):rs) | tagIsWHNF t = fbind vn v senv b rs
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
        x <- simpBind (p,a)
        z <- local (const env'') $ f b xs
        case x of
            Just (p',a') -> do
                return $ a' :>>= (p' :-> z)
            Nothing -> do
                return z
    f x [] = g x

    fbind vn v senv b rs = do
        v' <- applySubst v
        local (\_ -> extEnv vn v' senv) $ f b rs

    dtup xs ys senv b rs | sameLength xs ys = do
        xs <- mapM applySubst xs
        (ys,env') <- renamePattern ys
        let env'' = env' `mappend` senv
        z <- local (const env'') $ f b rs
        ts <- mapM simpBind [([y],Return [x]) | x <- xs | y <- ys ]
        let h [] = z
            h ((p,v):rs) = v :>>= p :-> h rs
        return $ h [ (p,v) |  Just (p,v) <- ts]
    dtup _ _ _ _ _ = error "dtup: attempt to bind unequal lists"
    g (Case v as) = do
        v <- applySubst v
        as <- mapM dc as
        return $ Case v as

    g  lt@Let { expDefs = defs, expBody = body } = do
        body <- f body []
        let f def@FuncDef { funcDefName = n, funcDefBody = b } = do
                b <- dc b
                return $ createFuncDef True n b
        defs <- mapM f defs
        return $ updateLetProps lt { expBody = body, expDefs = defs }
    g x = applySubstE x

    dc (p :-> e) = do
        (p,env') <- renamePattern p
        env <- ask
        let env'' = env' `mappend` env
        z <- local (const env'') $ f e []
        return (p :-> z)


applySubstE :: Exp -> S Exp
applySubstE x = f x where
    g = applySubst
    f (App a vs t) = do
        vs' <- mapM g vs
        return $ App a vs' t
    f (Return vs) = return Return `ap` mapM g vs
    f (Prim x vs t) = do
        vs <- mapM g vs
        return $ Prim x vs t
    f (Store v) = return Store `ap` g v
    f e@Alloc { expValue = v, expCount = c } = do
        v <- g v
        c <- g c
        return e { expValue = v, expCount = c }
    f (Fetch v) = return Fetch `ap` g v
    f (Update a b) = return Update `ap` g a `ap` g b
    f e@Error {} = return e
--    f (Case e as) = do
--        e <- g e
--        return $ Case e as
--    f lt@Let {} = return lt
    f x = error $ "applySubstE: " ++ show x

applySubst x = f x where
    f var@(Var (V v) _) = do
        env <- asks envSubst
        case IM.lookup v env of
            Just n -> return n
            Nothing -> return var
    f (NodeC t vs) = return (NodeC t) `ap` mapM f vs
    f (Index a b) = return Index `ap` f a `ap` f b
    f (Const v) = return Const `ap` f v
    f (ValPrim p vs ty) = return (ValPrim p) `ap` mapM f vs `ap` return ty
    f x = return x

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
isHoly n = isHole n
