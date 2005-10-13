module Grin.Simplify(simplify) where

import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Trans
import Data.Map as Map
import Data.Monoid
import Data.Set as Set
import List

import Atom
import CanType
import FreeVars
import Grin.Grin
import Grin.Whiz
import MonoidUtil()
import Stats

-- perform a number of simple simplifications.
-- inline very small and builtin-wrapper functions
-- copy propegation
-- CSE / constant propegation
-- dispose of code unreachable via Error


at_OptSimplifyInline  = toAtom "Optimize.simplify.inline"
at_OptSimplifyCopyProp  = toAtom "Optimize.simplify.copy-propagate"
at_OptSimplifyNodeReduction  = toAtom "Optimize.simplify.node-reduction"
at_OptSimplifyDeadVar  = toAtom "Optimize.simplify.dead-var"
at_OptSimplifyTrivialCase  = toAtom "Optimize.simplify.trivial-case"
at_OptSimplifyBadAssignment  = toAtom "Optimize.simplify.bad-assignment"


simplify :: Stats -> Grin -> IO Grin
simplify stats grin = do
    gfn <- sequence [  do (x,_) <- (evalStateT (whiz fn gv f whizState l) mempty ); return (n,x) |  (n,l) <- grinFunctions grin]
    deadVars stats  grin { grinFunctions = gfn }
    where
    fn _ m = do
        s <- get
        x <- m
        put s
        return x
    f (Case x [d]) = do
        (env,_) <- get
        x <- applySubst env  x
        lift $ tick stats at_OptSimplifyTrivialCase
        return $ (Return x :>>= d)
    f x = do
        (env,_) <- get
        x <- applySubstE env  x
        inline x
    gv (p,Case x ds) = do
        (env,_) <- get
        x <- applySubst env x
        case ds of
            [] -> error "empty case"
            [d] -> do
                lift $ tick stats at_OptSimplifyTrivialCase
                return $ Just (p,Return x :>>= d)
            _ -> return $ Just (p,Case x ds)
    gv (NodeC t xs,Return (NodeC t' xs')) | t == t' = do
            lift $ tick stats at_OptSimplifyNodeReduction
            gv (Tup xs,Return (Tup xs'))
    gv (NodeC t xs,Return (NodeC t' xs')) | t /= t' = do
            lift $ tick stats at_OptSimplifyBadAssignment
            gv (NodeC t xs,Error ("Bad Assignment: " ++ show (t,t')) TyNode)
    gv (p,e) = do
        (env,_) <- get
        e <- (applySubstE env e)
        case e of
            Return v | Just n <- varBind grin p v -> do
                lift $ tick stats at_OptSimplifyCopyProp
                modify (`mappend` (n,mempty))
                return Nothing
            _ -> do
                e <- inline e
                mz <- getCS (p,e)
                modify (mappend (mempty,mz))
                return $ Just (p,e)
    funcMap = Map.fromList $ [  fn | fn <- grinFunctions grin, doInline fn]
    doInline (a,fn)
        --  | 'b':_ <- n, not ("bap" `isPrefixOf` n) = True
        --  | "fInstance@" `isPrefixOf` n = True
        | isSimple (a,fn) = True
        | otherwise = False
      --  where n = fromAtom a
    inline app@(App fn as _)
        | Just l <- Map.lookup fn funcMap = do
            lift $ tick stats at_OptSimplifyInline -- (toAtom $ fromAtom at_OptSimplifyInline ++ "." ++ fromAtom fn)
            return $ Return (Tup as) :>>= l
        | otherwise = tryCSE app
    inline x = tryCSE x
    tryCSE x = do
        (_,ce) <- get
        case Map.lookup x ce of
            Just v -> do
                lift $ tick stats (cseStat x)
                return v
            Nothing -> return x
    --getCS (b,app@(App ev _)) | ev == funcEval = return $ Map.single app (Return b)
    --getCS (b,app@(App ev _)) | ev == funcApply = return $ Map.single app (Return b)
    getCS (b,app@(App a [vr@Var {}] _)) | a == funcEval = return $ Map.fromList [(app,Return b), (Store b,Return vr)]
    getCS (b,app@App{})  = return $ Map.singleton app (Return b)
    getCS (b@Var {},Store v@(Var _ _)) = return $ Map.singleton (App funcEval [b] TyNode) (Return v)     -- TODO - only works if node stores have always been evaluated.
    getCS (b@Var {},Store v@(NodeC t _)) | tagIsWHNF t, t /= tagHole = return $ Map.fromList [(Store v,Return b),(Fetch b,Return v),(App funcEval [b] TyNode,Return v)]
    getCS (b@Var {},Store v@(NodeC t _)) | t /= tagHole = return $ Map.fromList [(Store v,Return b)]
    getCS (b@Var {},Return (Const v)) = return $ Map.fromList [(Fetch b,Return v),(App funcEval [b] TyNode,Return v)]
    getCS (b@Var {},Return v) = return $ Map.fromList [(Return b,Return v), (Store b, Store v), (Fetch b, Fetch v)]
    getCS _ = return mempty

cseStat n = toAtom $ "Optimize.simplify.cse." ++ g n where
    g App { expFunction = n } = fromAtom n
    g Fetch {} = "Fetch"
    g Store {} = "Store"
    g _ = "Misc"

varBind :: Monad m => Grin -> Val -> Val -> m (Map Var Val)
varBind _ (Var v t) nv@(Var v' t') | t == t' = return $ Map.singleton v nv
varBind _ (Lit i t) (Lit i' t') | i == i' && t == t' = return mempty
--varBind _ Unit Unit = return mempty
varBind grin (Tup xs) (Tup ys) | length xs == length ys  = liftM mconcat $ sequence $  zipWith (varBind grin) xs ys
varBind _ (Tag i) (Tag i') | i == i' = return mempty
--varBind (NodeV v vs) (NodeV t vs') = do
--    be <- sequence $  zipWith varBind vs vs'
--    b <- varBind v t
--    return (mconcat $ b:be)
varBind grin (NodeC t vs) (NodeC t' vs') | t == t' = do
    liftM mconcat $ sequence $  zipWith (varBind grin) vs vs'
varBind grin v r  | runIdentity (typecheck (grinTypeEnv grin) v) == runIdentity (typecheck (grinTypeEnv grin) r)  = fail "unvarBindable"    -- check type to be sure
varBind _ x y = error $ "varBind: " ++ show (x,y)

isSimple :: (Atom,Lam) -> Bool
isSimple (fn,x) = f (3::Int) x where
    f n _ | n <= 0 = False
    f n (p :-> a :>>= b ) = (f (n - 1) (p :-> a)) &&  (f (n - 1) b)
    f _ (_ :-> Case {}) = False
    f _ (_ :-> App { expFunction = fn' }) | fn == fn' = False
    f _ _ = True

    {-

isSimple :: (Atom,Lam) -> Bool
isSimple (fn,_ :-> x) = f x where
 f x| App fn' _ <- x , fn /= fn' = True
    | Return {} <- x = True
    | Fetch {}  <- x = True
    | Store {}  <- x = True
    | Prim {}   <- x = True
    | Error {}  <- x = True
    | Cast {}   <- x = True
    | Update {} <- x = True
    | z :>>= _ :-> Return {} <- x = f z
    | Return {} :>>= _ :-> z <- x = f z
    | Fetch {} :>>= _ :-> z <- x = f z
    | Store {} :>>= _ :-> z <- x = f z
    | Prim {} :>>= _ :-> z <- x = f z
    | Update {} :>>= _ :-> z <- x = f z
    | z :>>= _ :-> Error {} <- x = f z
    | Cast {} :>>= _ :-> z <- x = f z
    | z :>>= _ :-> Cast {} <- x = f z
    | otherwise = False
    -}


deadVars :: Stats -> Grin -> IO Grin
deadVars stats grin = do
    gfn <- sequence [  do (x,_) <- (evalStateT (fizz (grinTypeEnv grin) fn gv f whizState l) (mempty :: Set.Set Var) ); return (n,x) |  (n,l) <- grinFunctions grin]
    return $ grin { grinFunctions =  gfn }
    where
    fn _ m = m
    f x = do
        uv <- get
        put $ (Set.union uv (freeVars x))
        return x
    gv w@(v, e) | isOmittable e = do
        (uv) <- get
        if  any (`Set.member` uv) (freeVars v) then
            f e >> return (Just w)
         else lift (tick stats at_OptSimplifyDeadVar) >> return Nothing
    gv w@(vs,Case x xs) = do
        uv <- get
        put $ (Set.union uv (freeVars x))
        return (Just w)
    gv w@(_,e) = f e >> return (Just w)


isOmittable (Fetch {}) = True
isOmittable (Return {}) = True
isOmittable (Store {}) = True
isOmittable (Cast {}) = True
isOmittable (Case x ds) = all isOmittable [ e | _ :-> e <- ds ]
isOmittable _ = False

