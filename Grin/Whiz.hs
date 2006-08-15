module Grin.Whiz(whiz, fizz, whizState, normalizeGrin,normalizeGrin', applySubstE, applySubst, whizExps) where

import Grin.Grin
import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Trans
import Data.Monoid
import Control.Monad.Identity
import Support.CanType

type WhizState = Either (Set.Set Int) Int
type WhizEnv = Map.Map Var Val

whizState :: WhizState
whizState = Left mempty

--normalizeGrin :: Grin -> Grin
--normalizeGrin grin@Grin { grinFunctions = fs } = grin { grinFunctions = f fs [] (Right 1) } where
--    f [] xs _ = xs
--    f ((a,(Tup vs,fn)):xs) ys set = f xs ((a,(Tup vs',fn')):ys) set' where
--        (Identity ((NodeC _ vs',fn'),set')) = whiz return return set (NodeC tagHole vs , fn)
normalizeGrin :: Grin -> Grin
normalizeGrin grin = setGrinFunctions (f (grinFuncs grin) [] (Right 1)) grin  where
    f [] xs _ = reverse xs
    f ((a,lm):xs) ys set = f xs ((a,lm'):ys) set' where
        (Identity (lm',set')) = fizz  (\_ x -> x) (return . Just) return set lm

normalizeGrin' :: Grin -> Grin
normalizeGrin' grin = setGrinFunctions (f (grinFuncs grin) []) grin  where
    f [] xs  = reverse xs
    f ((a,lm):xs) ys  = f xs ((a,lm'):ys) where
        (Identity (lm',_)) = whiz (\_ x -> x) (return . Just) return (Right 1) lm

whizExps :: Monad m => (Exp -> m Exp) -> Lam -> m Lam
whizExps f l = liftM fst $ whiz (\_ x -> x) (\(p,e) -> f e >>= \e' -> return  (Just (p,e'))) f whizState l

-- | magic traversal and flattening routine.
-- whiz traverses Grin code and right assosiates it as well as renaming and
-- repeated variables along the way.
-- in addition, it provides a nice monadic traversal of the flattened renamed code suitable
-- for a wide range of grin -> grin transformations.
-- basically, you may use 'whiz' to perform tranformations which do not require lookahead, and depend
-- only on the code that happened before.
-- note that a case is presented after all of its sub code blocks have been processed
-- Whiz also vectorizes tuple->tuple assignments, breaking them into individual assignments
-- for its components to better aid future optimizations.

whiz :: Monad m =>
    (forall a . Val -> m a -> m a)         -- ^ called for each sub-code block, such as in case statements
    -> ((Val,Exp) -> m (Maybe (Val,Exp)))  -- ^ routine to transform or omit simple bindings
    -> (Exp -> m Exp)       -- ^ routine to transform final statement in code block
    -> WhizState            -- ^ Initial state
    -> Lam                  -- ^ input lambda expression
    -> m (Lam,WhizState)
whiz sub te tf inState start = res where
    res = runStateT (dc mempty start) inState
    f (a :>>= (v :-> b)) xs env = f a ((env,v,b):xs) env
    f a@(Return (Tup xs@(_:_))) ((senv,p@(Tup ys@(_:_)),b):rs) env | length xs == length ys  = do
        Return (Tup xs) <- g env a
        (Tup ys,env') <- renamePattern p
        ts <- lift $ mapM te [(y,Return x) | x <- xs | y <- ys ]
        z <- f b rs (env' `mappend` senv)
        let h [] = z
            h ((p,v):rs) = v :>>= p :-> h rs
        return $ h [ (p,v) |  Just (p,v) <- ts]
    f a ((senv,p,b):xs) env = do
        a <- g env a
        (p,env') <- renamePattern p
        x <- lift $ te (p,a)
        z <- f b xs (env' `mappend` senv)
        case x of
            Just (p',a') -> do
                return $ a' :>>= (p' :-> z)
            Nothing -> do
                return z
    f x [] env = do
        x <- g env x
        lift $ tf x
    g env (Case v as) = do
        v <- applySubst env v
        as <- mapM (dc env) as
        return $ Case v as
--    g env lt@Let { expDefs = defs, expBody = Let { expDefs = defs', expBody = body } } = g env lt { expDefs = defs `mappend` defs', expBody = body }
    g env lt@Let { expDefs = defs, expBody = body } = do
        body <- f body [] env
        let f def@FuncDef { funcDefName = n, funcDefBody = b } = do
                b <- dc env b
                return $ createFuncDef True n b
        defs <- mapM f defs
        return lt { expBody = body, expDefs = defs }
    g env x = applySubstE env x
    dc env (p :-> e) = do
        (p,env') <- renamePattern p
        g <- get
        (z,g) <- lift $ sub p $ runStateT  (f e [] (env' `mappend` env)) g
        put g
        return (p :-> z)


-- | magic traversal and flattening routine.
-- whiz traverses Grin code and right assosiates it as well as renaming and
-- repeated variables along the way.
-- in addition, it provides a nice monadic traversal of the flattened renamed code suitable
-- for a wide range of grin -> grin transformations.
-- basically, you may use 'whiz' to perform tranformations which do not require lookahead, and depend
-- only on the code that happened before.
-- note that a case is presented after all of its sub code blocks have been processed
-- Whiz also vectorizes tuple->tuple assignments, breaking them into individual assignments
-- for its components to better aid future optimizations.
-- fizz is similar to whiz, but processes things in 'bottom-up' order.
-- fizz also removes all statements past an Error.

fizz :: Monad m =>
    (forall a . Val -> m a -> m a)         -- ^ called for each sub-code block, such as in case statements
    -> ((Val,Exp) -> m (Maybe (Val,Exp)))  -- ^ routine to transform or omit simple bindings
    -> (Exp -> m Exp)       -- ^ routine to transform final statement in code block
    -> WhizState            -- ^ Initial state
    -> Lam                  -- ^ input lambda expression
    -> m (Lam,WhizState)
fizz sub te tf inState start = res where
    res = runStateT (dc mempty start) inState
    f (a :>>= (v :-> b)) xs env = f a ((env,v,b):xs) env
    f a@(Return (Tup xs@(_:_))) ((senv,p@(Tup ys@(_:_)),b):rs) env | length xs == length ys  = do
        Return (Tup xs) <- g env a
        (Tup ys,env') <- renamePattern p
        z <- f b rs (env' `mappend` senv)
        ts <- lift $ mapM te (reverse [(y,Return x) | x <- xs | y <- ys ])
        let h [] = z
            h ((p,v):rs) = v :>>= p :-> h rs
        return $ h [ (p,v) |  Just (p,v) <- reverse ts]
    f (Error msg ty) [] env = do
        lift $ tf (Error msg ty)
    f (Error msg ty) ((_,_,b):xs) env = do
        f (Error msg (getType b)) xs env
    f a ((senv,p,b):xs) env = do
        a <- g env a
        (p,env') <- renamePattern p
        z <- f b xs (env' `mappend` senv)
        x <- lift $ te (p,a)
        case x of
            Just (p',a') -> do
                return $ a' :>>= (p' :-> z)
            Nothing -> do
                return z
    f x [] env = do
        x <- g env x
        lift $ tf x
    g env (Case v as) = do
        v <- applySubst env v
        as <- mapM (dc env) as
        return $ Case v as
    g env lt@Let { expDefs = defs, expBody = body } = do
        body <- f body [] env
        let f def@FuncDef { funcDefName = n, funcDefBody = b } = do
                b <- dc env b
                return $ createFuncDef True n b
        defs <- mapM f defs
        return lt { expBody = body, expDefs = defs }
    g env x = applySubstE env x
    dc env (p :-> e) = do
        (p,env') <- renamePattern p
        g <- get
        (z,g) <- lift $ sub p $ runStateT  (f e [] (env' `mappend` env)) g
        put g
        return (p :-> z)



applySubstE env x = f x where
    g = applySubst env
    f (App a vs t) = do
        vs' <- mapM g vs
        return $ App a vs' t
    f (Return v) = do
        v <- g v
        return $ Return v
    f (Prim x vs) = do
        vs <- mapM g vs
        return $ Prim x vs
    f (Store v) = do
        v <- g v
        return $ Store v
    f (Fetch v) = do
        v <- g v
        return $ Fetch v
    f (Update a b) = do
        a <- g a
        b <- g b
        return $ Update a b
    f e@Error {} = return e
    f (Case e as) = do
        e <- g e
        return $ Case e as
    f lt@Let {} = return lt
    f x = error $ "applySubstE: " ++ show x

applySubst env x = f x where
    f var@(Var v _)
        | Just n <- Map.lookup v env =  return n
    f (NodeC t vs) = do
        vs' <- mapM f vs
        return $ NodeC t vs'
    f (Tup vs) = do
        vs' <- mapM f vs
        return $ Tup vs'
    f (NodeV t vs) | Just (Var t' _) <- Map.lookup t env = do
        vs' <- mapM f vs
        return $ NodeV t' vs'
    f (NodeV t vs) = do
        vs' <- mapM f vs
        return $ NodeV t vs'
    f Addr {} = error "Address in subst"
    f x = return x

renamePattern :: MonadState (WhizState) m => Val ->  m (Val,WhizEnv)
renamePattern x = runWriterT (f x) where
    f :: MonadState (WhizState) m => Val -> WriterT (WhizEnv) m Val
    f (Var v t) = do
        v' <- lift $ newVarName v
        let nv = Var v' t
        tell (Map.singleton v nv)
        return nv
    f (NodeC t vs) = do
        vs' <- mapM f vs
        return $ NodeC t vs'
    f (Tup vs) = do
        vs' <- mapM f vs
        return $ Tup vs'
    f (NodeV t vs) = do
        t' <- lift $ newVarName t
        tell (Map.singleton t (Var t' TyTag))
        vs' <- mapM f vs
        return $ NodeV t' vs'
    f Addr {} = error "Address in pattern"
    f x = return x

newVarName :: MonadState WhizState m => Var -> m Var
newVarName (V sv) = do
    s <- get
    case s of
        Left s -> do
            let nv = v sv
                v n | n `Set.member` s = v (n + Set.size s)
                    | otherwise = n
            put (Left $! Set.insert nv s)
            return (V nv)
        Right n -> do
            put $! (Right $! (n + 1))
            return $ V n



