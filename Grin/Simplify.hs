module Grin.Simplify(simplify) where

import Char
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Trans
import Data.Monoid
import List
import Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set

import Atom
import C.Prims
import GenUtil hiding(putErrLn,replicateM_)
import Grin.Grin
import Grin.Noodle
import Grin.Whiz
import qualified Util.Histogram as Hist
import Stats hiding(combine)
import Support.CanType
import Support.FreeVars
import Support.Tuple
import Util.HasSize
import Util.Graph
import Util.Inst()
import Util.Seq as Seq

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
at_OptSimplifyCastLit  = toAtom "Optimize.simplify.cast-lit"
at_OptSimplifyConstUpdate  = toAtom "Optimize.simplify.const-update"
at_OptSimplifyEnumAssignment  = toAtom "Optimize.simplify.enum-assignment"

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
    gs (Prim Primitive { primAPrim = APrim CCast {} _, primType = (_,nty) } [Lit i _]) = do
        lift $ tick stats at_OptSimplifyCastLit
        return $ Return (Lit i nty)
    gs (Store n) | valIsNF n, not (valIsMutable n) = do
        lift $ tick stats at_OptSimplifyConstStore
        gs (Return (Const n))
    gs (App a [n@NodeC {},v] typ) | a == funcApply = do
        lift $ tick stats at_OptSimplifyConstApply
        gs (doApply Return True n v typ)
    gs (Store (NodeC t [Const x@NodeC {},y])) | Just 1 <- fromBap t = do --  App a [n@NodeC {},v] typ) | a == funcApply = do
        lift $ tick stats "Optimize.simplify.const-lazy-apply"
        gs (doApply Store False x y TyNode)
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
    gv (NodeV v [],Return (NodeC t' [])) = do
            lift $ tick stats at_OptSimplifyEnumAssignment
            gv (Var v TyTag, Return (Tag t'))
    gv (NodeV v [],Return (NodeV v' [])) = do
            lift $ tick stats at_OptSimplifyEnumAssignment
            gv (Var v TyTag, Return (Var v' TyTag))
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
    --getCS (b,app@App{})  = return $ Map.singleton app (Return b)
    --getCS (b@Var {},Store v@(Var _ _)) = return $ Map.singleton (App funcEval [b] TyNode) (Return v)     -- TODO - only works if node stores have always been evaluated.
    getCS (b@Var {},Store v@(NodeC t _)) | not (isMutableNodeTag t), tagIsWHNF t, t /= tagHole = return $ Map.fromList [(Store v,Return b),(Fetch b,Return v),(App funcEval [b] TyNode,Return v)]
    getCS (b@Var {},Store v@(NodeC t _)) | not (isMutableNodeTag t), t /= tagHole = return $ Map.fromList [(Store v,Return b)]
    --getCS (b@Var {},Store v@(NodeC t as)) | Just (0,fn) <- tagUnfunction t = return $ Map.fromList [(Store v,Return b),(App funcEval [b] TyNode, App fn as TyNode :>>= n1 :-> Update b n1 :>>= unit :-> Return n1)]
    getCS (b@Var {},Store v@(NodeC t as)) | Just (0,fn) <- tagUnfunction t = return $ Map.fromList [(Store v,Return b)]
    getCS (b@Var {},Return (Const v)) = return $ Map.fromList [(Fetch b,Return v),(App funcEval [b] TyNode,Return v)]
    getCS (b@Var {},Return v) = return $ Map.fromList [(Return b,Return v), (Store b, Store v), (Fetch b, Fetch v)]
    getCS _ = return mempty

cseStat n = toAtom $ "Optimize.simplify.cse." ++ g n where
    g App { expFunction = n } = fromAtom n
    g Fetch {} = "Fetch"
    g Store {} = "Store"
    g _ = "Misc"

doApply ret strict (NodeC t xs) y typ | Just (n,v) <- tagUnfunction t = case n of
    1 | strict -> (App v (xs ++ [y]) typ)
    _ -> ret (NodeC (partialTag v (n - 1)) (xs ++ [y]))
doApply _ _ n y typ = error $ show ("doApply", n,y,typ)

doEval n@(NodeC t xs) typ
    | tagIsWHNF t = Return n
    | tagIsSuspFunction t = App (tagFlipFunction t) xs typ
doEval n typ = error $ show ("doEval", n,typ)


fromBap :: Monad m => Atom -> m Int
fromBap t | 'B':'a':'p':'_':(n:ns) <- toString t, isDigit n = return $ read (n:takeWhile isDigit ns)
fromBap t = fail "not Bap"

-- This only binds variables to variables
varBind :: Monad m => Val -> Val -> m (Map.Map Var Val)
varBind (Var v t) nv@(Var v' t') | t == t' = return $ Map.singleton v nv
varBind (Lit i t) (Lit i' t') | i == i' && t == t' = return mempty
varBind (Tup xs) (Tup ys) | length xs == length ys  = liftM mconcat $ sequence $  zipWith varBind xs ys
varBind (Tag i) (Tag i') | i == i' = return mempty
varBind (NodeC t vs) (NodeC t' vs') | t == t' = do
    liftM mconcat $ sequence $  zipWith varBind vs vs'
varBind v r | (getType v) == (getType r)  = fail "unvarBindable"    -- check type to be sure
varBind x y = error $ "varBind: " ++ show (x,y)

-- This binds variables to anything
varBind' :: Monad m => Val -> Val -> m (Map.Map Var Val)
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
    f _ (_ :-> Let {}) = False
    f _ (_ :-> MkCont {}) = False
    f _ _ = True


manifestNodes as = Prelude.map (isManifestNode . lamExp) as

data UnboxingResult = UnboxTag | UnboxTup (Atom,[Ty]) | UnboxConst Val
    deriving(Eq,Ord)

isCombinable :: Monad m => Bool -> Exp -> m UnboxingResult
isCombinable postEval e = ans where
    ans = do
        mn <- f mempty e
        equal mn
    equal [] = fail "empty isCombinable"
    equal [x] = return x
    equal (x:y:rs) = if x == y then equal (y:rs) else fail "not equal"
    f lf (Return (NodeV t [])) | postEval = return [UnboxTag]
    f lf (Return (NodeC t [])) | postEval = return [UnboxTag]
    f lf (Return z) | z /= unit && valIsConstant z = return [UnboxConst z]
    f lf (Return (NodeC t xs)) = return [UnboxTup (t,map getType xs)]
    f lf Error {} = return []
    f lf (Case _ ls) = do
        cs <- Prelude.mapM (f lf) [ e | _ :-> e <- ls ]
        return $ concat cs
    f lf (_ :>>= _ :-> e) = f lf e
    f lf Let { expBody = body, expIsNormal = False } = f lf body
    f lf (App a _ _) | a `Set.member` lf = return []
    f lf Let { expBody = body, expDefs = defs, expIsNormal = True } = ans where
        nlf = lf `Set.union` Set.fromList (map funcDefName defs)
        ans = do
            xs <- mapM (f nlf . lamExp . funcDefBody) defs
            b <- f nlf body
            return (concat (b:xs))
    f _ _ = fail "not combinable"



combineLam postEval nty (p :-> e) = p :-> combine postEval nty e where
combine postEval nty exp = editTail nty f exp where
    f (Return (NodeV t [])) = Return (Var t TyTag)
    f (Return (NodeC t [])) | postEval  = Return (Tag t)
    f (Return v) | valIsConstant v  = Return unit
    f (Return (NodeC t xs)) = Return (tuple xs)
    f lt@Let { expBody = body } = updateLetProps lt { expBody = combine postEval nty body }
    f e = error $ "combine: " ++ show (postEval,nty,e)

editTail :: Ty -> (Exp -> Exp) -> Exp -> Exp
editTail nty mt te = f mempty te where
    f _ (Error s ty) = Error s nty
    f lf (Case x ls) = Case x (map (g lf) ls)
    f _ lt@Let {expIsNormal = False } = mt lt
    f lf lt@Let {expDefs = defs, expBody = body, expIsNormal = True } = updateLetProps lt { expBody = f nlf body, expDefs = defs' } where
        nlf = lf `Set.union` Set.fromList (map funcDefName defs)
        defs' = [ updateFuncDefProps d { funcDefBody = g nlf (funcDefBody d) } | d <- defs ]
    f lf lt@MkCont {expLam = lam, expCont = cont } = lt { expLam = g lf lam, expCont = g lf cont }
    f lf (e1 :>>= p :-> e2) = e1 :>>= p :-> f lf e2
    f lf e@(App a as t) | a `Set.member` lf = App a as nty
    f lf e = mt e
    g lf (p :-> e) = p :-> f lf e


isKnown Tag {} = True
isKnown NodeC {} = True
isKnown Lit {} = True
isKnown _ = False

mapExp f (b :-> e) = b :-> f e

sizeLam (b :-> exp) = sizeExp exp
sizeExp (x :>>= y) = sizeExp x + sizeLam y
sizeExp (Case e as) = 1 + sum (map sizeLam as)
sizeExp Let { expDefs = defs, expBody = body } = sizeExp body + sum (map (sizeLam . funcDefBody) defs)
sizeExp MkCont { expCont = l1, expLam = l2 } = 1 + sizeLam l1 + sizeLam l2
sizeExp x = 1

optimize1 ::  Bool -> (Atom,Lam) -> StatM Lam
optimize1 postEval (n,l) = g l where
    g (b :-> e) = f e >>= return . (b :->)
--    f (Case e as :>>= lam)  | (sizeLam lam - 1) * length as <= 3 = do
--        mtick "Optimize.optimize.case-pullin"
--        return (Case e (map (mapExp (:>>= lam)) as))
    f (Return t@NodeC {} :>>= v@Var {} :-> Update w v' :>>= lr) | v == v' = do
        mtick "Optimize.optimize.return-update"
        f (Return t :>>= v :-> Update w t :>>= lr)
    f (Return t@NodeV {} :>>= v@Var {} :-> Update w v' :>>= lr) | v == v' = do
        mtick "Optimize.optimize.return-update"
        f (Return t :>>= v :-> Update w t :>>= lr)
    f (e :>>= v1 :-> Return v2) | (isTup v1 || isVar v1) && v1 == v2 = do
        mtick "Optimize.optimize.unit-unit"
        f e
    f (Store t :>>= v :-> Fetch v' :>>= lr) | v == v' = do
        mtick "Optimize.optimize.store-fetch"
        f (Store t :>>= v :-> Return t :>>= lr)
    f (Store t :>>= v@(Var vr _) :-> Update  v' w :>>= lr) | v == v', vr `notElem` freeVars w = do
        mtick "Optimize.optimize.store-update"
        f (Store w :>>= v :-> Return unit :>>= lr)
    f (Update v t :>>= Tup [] :-> Fetch v' :>>= lr) | v == v' = do
        mtick "Optimize.optimize.update-fetch"
        f (Update v t :>>= Tup [] :-> Return t :>>= lr)
    f (Return t@NodeC {} :>>= v :-> App fa [v',a] typ :>>= lr) | fa == funcApply, v == v' = do
        mtick "Optimize.optimize.return-apply"
        f (Return t :>>= v :-> doApply Return True t a typ :>>= lr)
    f (Return t@NodeC {} :>>= v :-> App fa [v',a] typ) | fa == funcApply, v == v' = do
        mtick "Optimize.optimize.return-apply"
        f (Return t :>>= v :-> doApply Return True t a typ)
    f (Store t@NodeC {} :>>= v :-> App fa [v'] typ :>>= lr) | not (valIsMutable t), fa == funcEval, v == v' = do
        mtick "Optimize.optimize.store-eval"
        f (Store t :>>= v :-> doEval t typ :>>= lr)
    f (Store t@NodeC {} :>>= v :-> App fa [v'] typ) | not (valIsMutable t), fa == funcEval, v == v' = do
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
    f (cc@Case {} :>>= v :-> Return v' :>>= NodeC t as :-> lr ) | v == v' = do
        mtick "Optimize.optimize.case-hoist-return"
        let (va:_) = [ v | v <- [v1..], not $ v `Set.member` fv ]
            var = Var va TyNode
            fv = freeVars as
            mc = modifyTail ( var :-> Return var :>>=  NodeC t as :-> Return (tuple as))
        f (mc cc :>>= tuple as :-> Return (NodeC t as) :>>= v :-> lr)
    f (lt@Let { expIsNormal = True } :>>= v :-> Return v' :>>= NodeC t as :-> lr ) | v == v' = do
        mtick "Optimize.optimize.let-hoist-return"
        let (va:_) = [ v | v <- [v1..], not $ v `Set.member` fv ]
            var = Var va TyNode
            fv = freeVars as
            mc = modifyTail ( var :-> Return var :>>=  NodeC t as :-> Return (tuple as))
        f (mc lt :>>= tuple as :-> Return (NodeC t as) :>>= v :-> lr)

    f lt@Let { expDefs = defs, expBody = e :>>= l :-> r } | Set.null (freeVars r `Set.intersect` (Set.fromList $ map funcDefName defs)) = do
        mtick "Optimize.optimize.let-shrink-tail"
        f (updateLetProps lt { expBody = e } :>>= l :-> r)
--    f lt@(Let { expDefs = defs, expBody = e :>>= l :-> r } :>>= lr) | Set.null (freeVars r `Set.intersect` (Set.fromList $ map funcDefName defs)) = do
--        mtick "Optimize.optimize.let-shrink-tail"
--        f ((updateLetProps lt { expBody = e } :>>= l :-> r) :>>= lr)
    f lt@Let { expDefs = defs, expBody = e :>>= l :-> r } | Set.null (freeVars e `Set.intersect` (Set.fromList $ map funcDefName defs)) = do
        mtick "Optimize.optimize.let-shrink-head"
        f (e :>>= l :-> updateLetProps lt { expBody = r })
    f (Case x as :>>= v@(Var vnum _) :-> rc@(Case v' as') :>>= lr) | v == v', count (== Nothing ) (Prelude.map (isManifestNode . lamExp) as) <= 1, not (vnum `Set.member` freeVars lr) = do
        c <- caseHoist x as v as' (getType rc)
        f (c :>>= lr)
    f (Case x as :>>= v :-> rc@(Case v' as')) | v == v', count (== Nothing ) (Prelude.map (isManifestNode . lamExp) as) <= 1 = do
        ch <- caseHoist x as v as' (getType rc)
        f ch

    -- case unboxing
    f (cs@(Case x as) :>>= lr) | Just UnboxTag <- isCombinable postEval cs = do
        mtick "Optimize.optimize.case-unbox-tag"
        let fv = freeVars cs `Set.union` freeVars [ p | p :-> _ <- as ]
            (va:_vr) = [ v | v <- [v1..], not $ v `Set.member` fv ]
        return ((Case x (map (combineLam postEval TyTag) as) :>>= Var va TyTag :-> Return (NodeV va [])) :>>= lr)
    f (cs@(Case x as) :>>= lr) | Just (UnboxTup (t,ts)) <- isCombinable postEval cs = do
        mtick $ "Optimize.optimize.case-unbox-node.{" ++ show t
        let fv = freeVars cs `Set.union` freeVars [ p | p :-> _ <- as ]
            vs = [ v | v <- [v1..], not $ v `Set.member` fv ]
            vars = [ Var v t | v <- vs | t <- ts ]
        return ((Case x (map (combineLam postEval (tuple ts)) as) :>>= tuple vars  :-> Return (NodeC t vars)) :>>= lr)
    f (cs@(Case x as) :>>= lr) | Just (UnboxConst val) <- isCombinable postEval cs = do
        mtick $ "Optimize.optimize.case-unbox-const.{" ++ show val
        return ((Case x (map (combineLam postEval tyUnit) as) :>>= unit :-> Return val) :>>= lr)


    -- let pullin
    f (cs@Let { expIsNormal = True } :>>= lr) |  sizeLTE 1 (filter (/= ReturnError) (getReturnInfo cs)) = do
            mtick "Optimize.optimize.let-pullin"
            return $ modifyTail lr cs
    -- case pullin
    f (cs@Case {} :>>= lr) |  sizeLTE 1 (filter (/= ReturnError) (getReturnInfo cs)) = do
            mtick "Optimize.optimize.case-pullin"
            return $ modifyTail lr cs


    -- let unboxing
    f (cs@Let {} :>>= lr) | Just comb <- isCombinable postEval cs = case comb of
        UnboxTag -> do
            mtick "Optimize.optimize.let-unbox-tag"
            let (va:_vr) = [ v | v <- [v1..], not $ v `Set.member` fv ]
            return ((combine postEval TyTag cs :>>= Var va TyTag :-> Return (NodeV va [])) :>>= lr)
        UnboxTup (t,ts) -> do
            mtick $ "Optimize.optimize.let-unbox-node.{" ++ show t
            let vs = [ v | v <- [v1..], not $ v `Set.member` fv ]
                vars = [ Var v t | v <- vs | t <- ts ]
            return ((combine postEval (tuple ts) cs :>>= tuple vars  :-> Return (NodeC t vars)) :>>= lr)
        UnboxConst val -> do
            mtick $ "Optimize.optimize.let-unbox-const.{" ++ show val
            return ((combine postEval tyUnit cs :>>= unit :-> Return val) :>>= lr)
       where fv = freeVars cs `Set.union` freeVars [ p | p :-> _ <- map funcDefBody (expDefs cs) ]

    f cs@(Case x as) | postEval && all isEnum [ p | p :-> _ <- as] = do
        mtick "Optimize.optimize.case-enum"
        let fv = freeVars cs `Set.union` freeVars [ p | p :-> _ <- as ]
            (va:vb:_vr) = [ v | v <- [v1..], not $ v `Set.member` fv ]
        f (Return x :>>= NodeV va [] :-> Case (Var va TyTag) (Prelude.map (untagPat vb) as))

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
    f e@Let {} = mapExpExp f e
    f e = return e
    caseHoist x as v as' ty = do
        mtick $ "Optimize.optimize.case-hoist" -- .{" ++ show (Prelude.map (isManifestNode . lamExp) as :: [Maybe [Atom]])
        let nc = Case x [ b :-> e :>>= v :-> Case v as' | b :-> e <- as ]
            z (Error s _) = Error s ty
            z (e1 :>>= b :-> e2) = e1 :>>= b :-> z e2
            z e = e :>>= v :-> Case v as'
        return nc
    knownCase n@(NodeC t vs) as = do
        mtick $ "Optimize.optimize.known-case-node.{" ++ show t
        --let f [] = error $ "no known case:" ++ show (n,as)
        let f [] =  Error "known-case: No known case" (getType (Case n as))
            f ((v@Var {} :-> b):_) = Return n :>>= v :-> b
            f ((NodeC t' vs' :-> b):_) | t == t' =  Return (Tup vs) :>>= Tup vs' :-> b
            -- f ((NodeC t' vs' :-> b):_) | t == t' = let (xs,ys) = unzip [ (Var x t,y) | (x,y@(Var _ t)) <- Map.toList mp] in Return (Tup ys) :>>= Tup xs :-> b
            f (_:as) = f as
        return $ f as
    knownCase n@(Lit l _) as = do
        mtick $ "Optimize.optimize.known-case-lit.{" ++ show n
        let f [] =  Error "known-case: No known case" (getType (Case n as))
            f ((v@Var {} :-> b):_) = Return n :>>= v :-> b
            f ((Lit l' _ :-> b):_) | l == l' = b
            f (_:as) = f as
        return $ f as
    knownCase (Tag t) as = do
        mtick $ "Optimize.optimize.known-case-tag.{" ++ show t
        let f [] =  Error "known-case: No known case" (getType (Case (Tag t) as))
            f ((v@Var {} :-> b):_) = Return (Tag t) :>>= v :-> b
            f ((Tag t' :-> b):_) | t == t' = b
            f (_:as) = f as
        return $ f as
    caseCombine x as as' = do
        mtick $ "Optimize.optimize.case-combine"
        let etags = [ bd | bd@(NodeC t _ :-> _) <- as, t `notElem` [ t | NodeC t _ :-> _ <- as' ] ]
            ttags = [ bd | bd@(Tag t:-> _) <- as, t `notElem` [ t | Tag t :-> _ <- as' ] ]
            as'' = Prelude.map f as'
            f (v@Var {} :-> b) | getType v == TyTag = v :-> Case v ttags :>>= unit :-> b
            f (v@Var {} :-> b) = v :-> Case v etags :>>= unit :-> b
            f (n@(NodeC t _) :-> b) = case [ a | a@(NodeC t' _ :-> _) <-  as, t == t'] of
                [bind :-> body] -> n :-> Return n :>>= bind :-> body :>>= unit :-> b
            f (n@(Tag t) :-> b) = case [ a | a@(Tag t' :-> _) <-  as, t == t'] of
                [bind :-> body] -> n :-> Return n :>>= bind :-> body :>>= unit :-> b
            -- f r
        return $ Case x as''

isEnum (NodeC t []) = True
isEnum (Var t TyNode) = True
isEnum _ = False

untagPat _ (NodeC t [] :-> e) = Tag t :-> e
untagPat vb (v@Var{} :-> e) = Var vb TyTag :-> Return (NodeV vb []) :>>= v :-> e


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
    gv w@(Tup vs,Case x xs) = do
        uv <- get
        put $ (Set.union uv (freeVars x))
        let used v = any (`Set.member` uv) (freeVars v)
        case partition used vs of
            (_,[]) -> return $ Just w
            (nvs,unused) -> do
                replicateM_ (length unused) $ lift (tick stats "Optimize.simplify.dead-var-case-tup")
                let ml = modifyTail (tuple vs :-> Return (tuple nvs))
                return (Just (tuple nvs,ml (Case x xs) ))
    gv w@(_,e) = f e >> return (Just w)



{-# NOINLINE simplify #-}

simplify ::
    Stats     -- ^ stats to update
    -> Grin   -- ^ input grin
    -> IO Grin
simplify stats grin = do
    let postEval = phaseEvalInlined (grinPhase grin)
        fs = grinFuncs grin
        uf = [ ((a,l),collectUsedFuncs l) | (a,l) <- fs ]
        graph = newGraph uf (\ ((a,_),_) -> a) (\ (_,(fi,fd)) -> (if postEval then [] else fi) ++ fd)
        rf = reachable graph (grinEntryPointNames grin)
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
                let (nl',stat) = runStatM (optimize1 postEval (a,nl''))
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
        --procF (out,env) ((a,_),_) | False <- a `Set.member` reached = do
        --    tick stats (toAtom "Optimize.dead.function")
        --    return (out,env)
        procF (out,env) ((a,l),_) = do
            nl <- opt env a (0::Int) l
            let iname t = toAtom $ "Optimize.simplify.inline." ++ t ++ ".{" ++ fromAtom a  ++ "}"
                inline
                    | a `elem` noInline = Map.empty
                    | a `Set.member` loopBreakers = Map.empty
                    | Hist.find a hist == 1 = Map.singleton a (iname "once",nl)
                    | a `Set.member` indirectFuncs = Map.empty
                    | isSimple (a,nl) = Map.singleton a (iname "simple",nl)
                    | otherwise = Map.empty
            return ((a,nl):out , inline `Map.union` env)

    (nf,_) <- foldM procF ([],mempty) os
    return $ setGrinFunctions nf grin


noInline = [toAtom "fData.IORef.readIORef", toAtom "fData.IORef.writeIORef"]


-- TODO have this collect CAF info ignoring updates.

collectUsedFuncs :: Lam -> ([Atom],[Atom])
collectUsedFuncs (Tup as :-> exp) = (snub $ concatMap tagToFunction (Seq.toList iu),sort $ Seq.toList du) where
    (iu,du) =  f exp
    f (e1 :>>= _ :-> e2) = f e1 `mappend` f e2
    f (App a vs _) =  (Seq.fromList (freeVars vs), Seq.singleton a)
    f (Case e alts) =  mconcat ((Seq.fromList (freeVars e) , Seq.empty):[ f e | _ :-> e <- alts])
    f e = (Seq.fromList [ v | v <- freeVars e ],Seq.empty)






