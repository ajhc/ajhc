module Grin.Simplify(simplify) where

import Control.Monad.State
import Control.Monad.Trans
import Data.Map as Map
import Data.Monoid
import Data.Set as Set
import List

import Atom
import CharIO
import GenUtil hiding(putErrLn)
import CanType
import Doc.Pretty
import Doc.PPrint
import FreeVars
import Grin.Grin
import Grin.Show
import Grin.Whiz
import Stats
import Util.Graph
import Util.Inst()
import Util.Seq as Seq
import Util.Histogram as Hist

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

-- contains functions that should be inlined
type SimpEnv = Map.Map Atom (Atom,Lam)

simplify1 :: Stats -> SimpEnv -> (Atom,Lam) -> IO (Atom,Lam)
simplify1 stats env (n,l) = do
    (l,_) <- evalStateT (whiz fn gv f whizState l) mempty
    return (n,l)
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
            Return v | Just n <- varBind p v -> do
                lift $ tick stats at_OptSimplifyCopyProp
                modify (`mappend` (n,mempty))
                return Nothing
            _ -> do
                e <- inline e
                mz <- getCS (p,e)
                modify (mappend (mempty,mz))
                return $ Just (p,e)
    -- funcMap = Map.fromList $ [  fn | fn <- grinFunctions grin, doInline fn]
    doInline (a,fn)
        --  | 'b':_ <- n, not ("bap" `isPrefixOf` n) = True
        --  | "fInstance@" `isPrefixOf` n = True
        | isSimple (a,fn) = True
        | otherwise = False
      --  where n = fromAtom a
    inline app@(App fn as _)
        | Just (itype,l) <- Map.lookup fn env = do
            lift $ tick stats itype
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

varBind :: Monad m => Val -> Val -> m (Map Var Val)
varBind (Var v t) nv@(Var v' t') | t == t' = return $ Map.singleton v nv
varBind (Lit i t) (Lit i' t') | i == i' && t == t' = return mempty
varBind (Tup xs) (Tup ys) | length xs == length ys  = liftM mconcat $ sequence $  zipWith varBind xs ys
varBind (Tag i) (Tag i') | i == i' = return mempty
varBind (NodeC t vs) (NodeC t' vs') | t == t' = do
    liftM mconcat $ sequence $  zipWith varBind vs vs'
varBind v r | (getType v) == (getType r)  = fail "unvarBindable"    -- check type to be sure
varBind x y = error $ "varBind: " ++ show (x,y)

isSimple :: (Atom,Lam) -> Bool
isSimple (fn,x) = f (2::Int) x where
    f n _ | n <= 0 = False
    f n (p :-> a :>>= b ) = (f (n - 1) (p :-> a)) &&  (f (n - 1) b)
    f _ (_ :-> Case {}) = False
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


deadVars :: Stats -> (Atom,Lam) -> IO (Atom,Lam)
deadVars stats (n,l) = do
    (x,_) <- (evalStateT (fizz fn gv f whizState l) (mempty :: Set.Set Var) );
    return (n,x)
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


simplify ::
    Stats     -- ^ stats to update
    -> Grin   -- ^ input grin
    -> IO Grin
simplify stats grin = do
    let postEval = phaseEvalInlined (grinPhase grin)
        fs = grinFunctions grin
        uf = [ ((a,l),collectUsedFuncs l) | (a,l) <- fs ]
        graph = newGraph uf (\ ((a,_),_) -> a) (\ (_,(fi,fd)) -> (if postEval then [] else fi) ++ fd)
        rf = reachable graph (grinEntryPoints grin)
        reached = Set.fromList $ Prelude.map  (\ ((a,_),_) -> a) rf
        graph' = if postEval then graph else newGraph rf (\ ((a,_),_) -> a) (\ (_,(_,fd)) -> fd)
        (lb,os) = findLoopBreakers ( fromEnum . not . isSimple . fst) (const True) graph'
        loopBreakers = Set.fromList [ a | ((a,_),_) <- lb ]
        indirectFuncs = if postEval then Set.empty else Set.fromList (concat [ fi | (_,(fi,_)) <- rf ])
        hist =  Hist.fromList $ concat [ fd | (_,(_,fd)) <- rf ]
    let opt env a n l = do
                (_,nl) <- deadVars stats (a,l)
                (_,nl) <- simplify1 stats env (a,nl)
                return nl
                {-
        opt env a n l = do
            stats' <- Stats.new
            (_,nl) <- deadVars stats (a,l)  -- if the deadVars did not enable any other transformations we don't need to iterate as deadVars is idempotent
            (_,nl) <- simplify1 stats' env (a,nl)
            t <- Stats.getTicks stats'
            case t of
                0 -> return nl
                _ -> do
                    -- when (n > 2) $ Stats.print (show a) stats'
                    Stats.combine stats stats'
                    -- tick stats $ "Optimize.repeat.{" ++ show a ++ "}"
                    opt env a (n + 1 :: Int) nl
                    -}
        procF (out,env) ((a,_),_) | False <- a `Set.member` reached = do
            tick stats (toAtom "Optimize.dead.function")
            return (out,env)
        procF (out,env) ((a,l),_) = do
            nl <- opt env a 0 l
            let iname t = toAtom $ "Optimize.simplify.inline." ++ t ++ ".{" ++ fromAtom a  ++ "}"
                inline
                    | a `Set.member` loopBreakers = Map.empty
                    | Hist.find a hist == 1 = Map.singleton a (iname "once",nl)
                    | a `Set.member` indirectFuncs = Map.empty
                    | isSimple (a,nl) = Map.singleton a (iname "simple",nl)
                    | otherwise = Map.empty
            return ((a,nl):out , inline `Map.union` env)

    (nf,_) <- foldM procF ([],mempty) os
    return grin { grinFunctions = nf }



-- TODO have this collect CAF info ignoring updates.

collectUsedFuncs :: Lam -> ([Atom],[Atom])
collectUsedFuncs (Tup as :-> exp) = (snub $ concatMap tagToFunction (Seq.toList iu),sort $ Seq.toList du) where
    (iu,du) =  f exp
    f (e1 :>>= _ :-> e2) = f e1 `mappend` f e2
    f (App a vs _) =  (Seq.fromList (freeVars vs), Seq.singleton a)
    f (Case e alts) =  mconcat ((Seq.fromList (freeVars e) , Seq.empty):[ f e | _ :-> e <- alts])
    f e = (Seq.fromList [ v | v <- freeVars e ],Seq.empty)



