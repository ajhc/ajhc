module Grin.Simplify(simplify) where

import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Identity
import Data.Map as Map
import Data.Monoid
import Data.Set as Set
import List
import Maybe

import Atom
import GenUtil hiding(putErrLn)
import Support.CanType
import FreeVars
import Grin.Grin
import C.Prims
import Grin.Whiz
import Stats
import Util.Graph
import Util.Inst()
import Util.Seq as Seq
import Util.Histogram as Hist

-- perform a number of simple simplifications.
-- inline very small and builtin-wrapper functions
-- copy propagation
-- CSE / constant propagation
-- dispose of code unreachable via Error


at_OptSimplifyInline  = toAtom "Optimize.simplify.inline"
at_OptSimplifyCopyProp  = toAtom "Optimize.simplify.copy-propagate"
at_OptSimplifyCopyPropConst  = toAtom "Optimize.simplify.copy-propagate-const"
at_OptSimplifyNodeReduction  = toAtom "Optimize.simplify.node-reduction"
at_OptSimplifyDeadVar  = toAtom "Optimize.simplify.dead-var"
at_OptSimplifyConstApply  = toAtom "Optimize.simplify.const-apply"
at_OptSimplifyConstFetch  = toAtom "Optimize.simplify.const-fetch"
at_OptSimplifyConstEval  = toAtom "Optimize.simplify.const-eval"
at_OptSimplifyTrivialCase  = toAtom "Optimize.simplify.trivial-case"
at_OptSimplifyBadAssignment  = toAtom "Optimize.simplify.bad-assignment"
at_OptSimplifyHoleAssignment  = toAtom "Optimize.simplify.hole-assignment"
at_OptSimplifyConstStore  = toAtom "Optimize.simplify.const-store"
at_OptSimplifyConstUpdate  = toAtom "Optimize.simplify.const-update"

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
        x <- gs x
        inline x
    gs (Update Const {} Var {}) = do
        lift $ tick stats at_OptSimplifyConstUpdate
        gs (Return unit)
    gs (Store n) | valIsNF n = do
        lift $ tick stats at_OptSimplifyConstStore
        gs (Return (Const n))
    gs (App a [n@NodeC {},v] typ) | a == funcApply = do
        lift $ tick stats at_OptSimplifyConstApply
        gs (doApply n v typ)
    gs (App a [Const n] typ) | a == funcEval = do
        lift $ tick stats at_OptSimplifyConstEval
        gs (Return n)
    gs (Fetch (Const n)) = do
        lift $ tick stats at_OptSimplifyConstFetch
        gs (Return n)
    gs x = return x
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
    gv (NodeC t xs,Return (NodeC t' [])) |  t' == tagHole = do
            lift $ tick stats at_OptSimplifyHoleAssignment
            gv (Tup xs, Return $ Tup $ Prelude.map (properHole . getType) xs)
    gv (NodeC t xs,Return (NodeC t' xs')) | t /= t' = do
            lift $ tick stats at_OptSimplifyBadAssignment
            gv (NodeC t xs,Error ("Bad Assignment: " ++ show (t,t')) TyNode)
    gv (p,e) = do
        (env,_) <- get
        e <- (applySubstE env e)
        e <- gs e
        case e of
            Return v | valIsNF v, Just n <- varBind' p v -> do
                lift $ tick stats at_OptSimplifyCopyPropConst
                modify (`mappend` (n,mempty))
                return Nothing
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
    getCS (b,app@(App a [vr@Var {}] _)) | a == funcEval = return $ Map.fromList [(app,Return b), (Store b,Return vr)]
    getCS (b,app@App{})  = return $ Map.singleton app (Return b)
    --getCS (b@Var {},Store v@(Var _ _)) = return $ Map.singleton (App funcEval [b] TyNode) (Return v)     -- TODO - only works if node stores have always been evaluated.
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

doApply (NodeC t xs) y typ | Just (n,v) <- tagUnfunction t = case n of
    1 -> (App v (xs ++ [y]) typ)
    _ -> Return (NodeC (partialTag v (n - 1)) (xs ++ [y]))
doApply n y typ = error $ show ("doApply", n,y,typ)

doEval n@(NodeC t xs) typ
    | tagIsWHNF t = Return n
    | tagIsSuspFunction t = App (tagFlipFunction t) xs typ
doEval n typ = error $ show ("doEval", n,typ)


-- This only binds variables to variables
varBind :: Monad m => Val -> Val -> m (Map Var Val)
varBind (Var v t) nv@(Var v' t') | t == t' = return $ Map.singleton v nv
varBind (Lit i t) (Lit i' t') | i == i' && t == t' = return mempty
varBind (Tup xs) (Tup ys) | length xs == length ys  = liftM mconcat $ sequence $  zipWith varBind xs ys
varBind (Tag i) (Tag i') | i == i' = return mempty
varBind (NodeC t vs) (NodeC t' vs') | t == t' = do
    liftM mconcat $ sequence $  zipWith varBind vs vs'
varBind v r | (getType v) == (getType r)  = fail "unvarBindable"    -- check type to be sure
varBind x y = error $ "varBind: " ++ show (x,y)

-- This binds variables to anything
varBind' :: Monad m => Val -> Val -> m (Map Var Val)
varBind' (Var v t) nv | t == getType nv = return $ Map.singleton v nv
varBind' (Lit i t) (Lit i' t') | i == i' && t == t' = return mempty
varBind' (Tup xs) (Tup ys) | length xs == length ys  = liftM mconcat $ sequence $  zipWith varBind' xs ys
varBind' (Tag i) (Tag i') | i == i' = return mempty
varBind' (NodeC t vs) (NodeC t' vs') | t == t' = do
    liftM mconcat $ sequence $  zipWith varBind' vs vs'
varBind' v r | (getType v) == (getType r)  = fail "unvarBind'able"    -- check type to be sure
varBind' x y = error $ "varBind': " ++ show (x,y)

isSimple :: (Atom,Lam) -> Bool
isSimple (fn,x) = f (2::Int) x where
    f n _ | n <= 0 = False
    f n (p :-> a :>>= b ) = (f (n - 1) (p :-> a)) &&  (f (n - 1) b)
    f _ (_ :-> Case {}) = False
    f _ _ = True

isManifestNode :: Monad m => Exp -> m [Atom]
isManifestNode (Return (NodeC t _)) = return [t]
isManifestNode Error {} = return []
isManifestNode (Case _ ls) = do
    cs <- Prelude.mapM isManifestNode [ e | _ :-> e <- ls ]
    return $ concat cs
isManifestNode (_ :>>= _ :-> e) = isManifestNode e
isManifestNode _ = fail "not manifest node"

manifestNodes as = Prelude.map (isManifestNode . lamExp) as

lamExp (_ :-> e) = e
lamBind (b :-> _) = b

isVar Var {} = True
isVar _ = False

isKnown NodeC {} = True
isKnown Lit {} = True
isKnown _ = False

optimize1 ::  (Atom,Lam) -> StatM Lam
optimize1 (n,l) = g l where
    g (b :-> e) = f e >>= return . (b :->)
    f (e :>>= Tup [] :-> Return (Tup []) :>>= lr) = do
        mtick "Optimize.optimize.unit-unit"
        f (e :>>= lr)
    f (e :>>= Tup [] :-> Return (Tup [])) = do
        mtick "Optimize.optimize.unit-unit"
        f e
    f (Store t :>>= v :-> Fetch v' :>>= lr) | v == v' = do
        mtick "Optimize.optimize.store-fetch"
        f (Store t :>>= v :-> Return t :>>= lr)
    f (Update v t :>>= Tup [] :-> Fetch v' :>>= lr) | v == v' = do
        mtick "Optimize.optimize.update-fetch"
        f (Update v t :>>= Tup [] :-> Return t :>>= lr)
    f (Return t@NodeC {} :>>= v :-> App fa [v',a] typ :>>= lr) | fa == funcApply, v == v' = do
        mtick "Optimize.optimize.return-apply"
        f (Return t :>>= v :-> doApply t a typ :>>= lr)
    f (Return t@NodeC {} :>>= v :-> App fa [v',a] typ) | fa == funcApply, v == v' = do
        mtick "Optimize.optimize.return-apply"
        f (Return t :>>= v :-> doApply t a typ)
    f (Store t@NodeC {} :>>= v :-> App fa [v'] typ :>>= lr) | fa == funcEval, v == v' = do
        mtick "Optimize.optimize.store-eval"
        f (Store t :>>= v :-> doEval t typ :>>= lr)
    f (Store t@NodeC {} :>>= v :-> App fa [v'] typ) | fa == funcEval, v == v' = do
        mtick "Optimize.optimize.store-eval"
        f (Store t :>>= v :-> doEval t typ)
    f (Update v t@NodeC {} :>>= Tup [] :-> App fa [v'] typ :>>= lr) | fa == funcEval, v == v' = do
        mtick "Optimize.optimize.update-eval"
        f (Update v t :>>= Tup [] :-> doEval t typ :>>= lr)
    f (Update v t@NodeC {} :>>= Tup [] :-> App fa [v'] typ) | fa == funcEval, v == v' = do
        mtick "Optimize.optimize.update-eval"
        f (Update v t :>>= Tup [] :-> doEval t typ)
    f (Case n as) | isKnown n = do
        kc <- knownCase n as
        f kc
    f (Case n as :>>= lr) | isKnown n = do
        kc <- knownCase n as
        f (kc :>>= lr)
    f (Return n :>>= b :-> Case b' as :>>= lr) | isKnown n, b == b' = do
        c <- knownCase n as
        r <- f (c :>>= lr)
        return (Return n :>>= b :-> r)
    f (Return n :>>= b :-> Case b' as ) | isKnown n, b == b' = do
        kc <- knownCase n as
        r <- f kc
        return (Return n :>>= b :-> r)
    f (Case x as :>>= Tup [] :-> (Case x' as') :>>= lr) | x == x', not $ any (isVar . lamBind) as = do
        c <- caseCombine x as as'
        f (c :>>= lr)
    f (Case x as :>>= Tup [] :-> (Case x' as')) | x == x', not $ any (isVar . lamBind) as = do
        c <- caseCombine x as as'
        f c
        {-
    f (Case x as :>>= b :-> m) | count (/= Just []) (manifestNodes as) <= 1 = do
        mtick "Optimize.optimize.case-pullin"
        f $ Case x [ x :-> (e :>>= b :-> m) |  x :-> e <- as ]
        -}
    f (Case x as :>>= v@(Var vnum _) :-> rc@(Case v' as') :>>= lr) | v == v', count (== Nothing ) (Prelude.map (isManifestNode . lamExp) as) <= 1, not (vnum `Set.member` freeVars lr) = do
        c <- caseHoist x as v as' (getType rc)
        f (c :>>= lr)
    f (Case x as :>>= v :-> rc@(Case v' as')) | v == v', count (== Nothing ) (Prelude.map (isManifestNode . lamExp) as) <= 1 = do
        ch <- caseHoist x as v as' (getType rc)
        f ch
    f (e1 :>>= _ :-> err@Error {}) | isErrOmittable e1 = do
        mtick "Optimize.optimize.del-error"
        return err
    f (e1 :>>= l :-> e2) = do
        e1' <- f e1
        e2' <- f e2
        return (e1' :>>= l :-> e2')
    f (Case x as) = do
       as' <- sequence [ f e >>= return . (b :->)| b :-> e <- as ]
       return $ Case x as'
    f e = return e
    caseHoist x as v as' ty = do
        mtick $ "Optimize.optimize.case-hoist" -- .{" ++ show (Prelude.map (isManifestNode . lamExp) as :: [Maybe [Atom]])
        let nc = Case x [ b :-> e :>>= v :-> Case v as' | b :-> e <- as ]
            z (Error s _) = Error s ty
            z (e1 :>>= b :-> e2) = e1 :>>= b :-> z e2
            z e = e :>>= v :-> Case v as'
        return nc
    knownCase n@(NodeC t vs) as = do
        mtick $ "Optimize.optimize.known-case.{" ++ show t
        --let f [] = error $ "no known case:" ++ show (n,as)
        let f [] =  Error "known-case: No known case" (getType (Case n as))
            f ((v@Var {} :-> b):_) = Return n :>>= v :-> b
            f ((NodeC t' vs' :-> b):_) | t == t' =  Return (Tup vs) :>>= Tup vs' :-> b
            -- f ((NodeC t' vs' :-> b):_) | t == t' = let (xs,ys) = unzip [ (Var x t,y) | (x,y@(Var _ t)) <- Map.toList mp] in Return (Tup ys) :>>= Tup xs :-> b
            f (_:as) = f as
        return $ f as
    knownCase n@(Lit l _) as = do
        mtick $ "Optimize.optimize.known-case.{" ++ show n
        let f [] =  Error "known-case: No known case" (getType (Case n as))
            f ((v@Var {} :-> b):_) = Return n :>>= v :-> b
            f ((Lit l' _ :-> b):_) | l == l' = b
            f (_:as) = f as
        return $ f as
    caseCombine x as as' = do
        mtick $ "Optimize.optimize.case-combine"
        let etags = [ bd | bd@(NodeC t _ :-> _) <- as, t `notElem` [ t | NodeC t _ :-> _ <- as' ] ]
            as'' = Prelude.map f as'
            f (v@Var {} :-> b) = v :-> Case v etags :>>= unit :-> b
            f (n@(NodeC t _) :-> b) = case [ a | a@(NodeC t' _ :-> _) <-  as, t == t'] of
                [bind :-> body] -> n :-> Return n :>>= bind :-> body :>>= unit :-> b
            -- f r
        return $ Case x as''

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
isOmittable Prim { expPrimitive = Primitive { primAPrim = aprim } } = aprimIsCheap aprim
isOmittable (Case x ds) = all isOmittable [ e | _ :-> e <- ds ]
isOmittable (e1 :>>= _ :-> e2) = isOmittable e1 && isOmittable e2
isOmittable _ = False

isErrOmittable Update {} = True
isErrOmittable (e1 :>>= _ :-> e2) = isErrOmittable e1 && isErrOmittable e2
isErrOmittable (Case x ds) = all isErrOmittable [ e | _ :-> e <- ds ]
isErrOmittable x = isOmittable x

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
                let Identity nl'' = whizExps return nl
                -- putDocM CharIO.putErr (prettyFun (a,nl''))
                let (nl',stat) = runStatM (optimize1 (a,nl''))
                tickStat stats stat
                return nl'
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
            nl <- opt env a (0::Int) l
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





