module Grin.EvalInline(
    createEval,
    createApply,
    createEvalApply,
    UpdateType(..)
    ) where


import Control.Monad.Identity
import List
import qualified Data.Set as Set
import qualified Data.Map as Map


import Atom
import Grin.Grin
import GenUtil
import Support.FreeVars(freeVars)
import Support.CanType(getType)
import C.Arch
import Util.Once
import Util.UniqueMonad

data UpdateType =
    NoUpdate                  -- ^ no update is performed
    | TrailingUpdate          -- ^ an update is placed after the whole evaluation
    | HoistedUpdate Val
    | SwitchingUpdate [Atom]

mapExp f (b :-> e) = b :-> f e

-- create an eval suitable for inlining.
createEval :: UpdateType -> TyEnv -> [Tag] -> Lam
createEval shared  te ts'
    | null cs = p1 :-> Error "Empty Eval" TyNode
    | all tagIsWHNF [ t | t <- ts , tagIsTag t] = p1 :-> Fetch p1
    | NoUpdate <- shared, [t] <- ts = p1 :-> Fetch p1 :>>= f t
    | TrailingUpdate <- shared, [ot] <- ofts = p1 :->
        Fetch p1 :>>= n2 :->
        Case n2 (mapExp (:>>= n3 :-> Update p1 n3 :>>= unit :-> Return n3) (f ot):map f whnfts)
    | TrailingUpdate <- shared = p1 :->
        Fetch p1 :>>= n2 :->
        Case n2 cs :>>= n3 :->
        Update p1 n3 :>>= unit :->
        Return n3
    | HoistedUpdate (NodeC t [v]) <- shared = p1 :->
        Fetch p1 :>>= n2 :->
        Case n2 cs :>>= v :->
        Return (NodeC t [v])
    | HoistedUpdate (NodeC t vs) <- shared = p1 :->
        Fetch p1 :>>= n2 :->
        Case n2 cs :>>= Tup vs :->
        Return (NodeC t vs)
    | NoUpdate <- shared = p1 :->
        Fetch p1 :>>= n2 :->
        Case n2 cs
    | SwitchingUpdate sts <- shared, [ot] <- ofts = p1 :->
        Fetch p1 :>>= n2 :->
        Case n2 (mapExp (:>>= sup p1 sts) (f ot):map f whnfts)
    | SwitchingUpdate sts <- shared = let
            lf = createEval NoUpdate te ts
            cu t | tagIsTag t && tagIsWHNF t = return ans where
                (ts,_) = runIdentity $ findArgsType te t
                vs = [ Var v ty |  v <- [V 4 .. ] | ty <- ts]
                ans = NodeC t vs :-> Update p1 (NodeC t vs)
            cu t = error $ "not updatable:" ++ show t
        in (p1 :-> (Return p1 :>>= lf) :>>= sup p1 sts) --  n3 :-> Case n3 (concatMap cu sts) :>>= unit :-> Return n3)
    where
    ts = sortUnder toPackedString ts'
    sup p sts = let
            cu t | tagIsTag t && tagIsWHNF t = return ans where
                (ts,_) = runIdentity $ findArgsType te t
                vs = [ Var v ty |  v <- [V 4 .. ] | ty <- ts]
                ans = NodeC t vs :-> Update p1 (NodeC t vs)
            cu t = error $ "not updatable:" ++ show t
        in (n3 :-> Case n3 (concatMap cu sts) :>>= unit :-> Return n3)
    cs = [f t | t <- ts, tagIsTag t, isGood t ]
    isGood t | tagIsWHNF t, HoistedUpdate (NodeC t' _) <- shared, t /= t' = False
    isGood _ = True
    (whnfts,ofts) = partition tagIsWHNF (filter tagIsTag ts)
    g t vs
        | tagIsWHNF t, HoistedUpdate (NodeC t' [v]) <- shared  = case vs of
            [x] -> Return x
            _ -> error "createEval: bad thing"
        | tagIsWHNF t, HoistedUpdate (NodeC t' vars) <- shared  = Return (Tup vs)
        | tagIsWHNF t = Return (NodeC t vs)
        | 'F':fn <- fromAtom t  = ap ('f':fn) vs
        | 'B':fn <- fromAtom t  = ap ('b':fn) vs
        | otherwise = Error ("Bad Tag: " ++ fromAtom t) TyNode
    f t = (NodeC t vs :-> g t vs ) where
        (ts,_) = runIdentity $ findArgsType te t
        vs = [ Var v ty |  v <- [V 4 .. ] | ty <- ts]
    ap n vs
    --    | shared =  App (toAtom $ n) vs :>>= n3 :-> Update p1 n3 :>>= unit :-> Return n3
        | HoistedUpdate udp@(NodeC t []) <- shared = App fname vs ty :>>= n3 :-> Update p1 udp
        | HoistedUpdate udp@(NodeC t [v]) <- shared = App fname vs ty :>>= n3 :-> Return n3 :>>= udp :-> (Update p1 udp :>>= unit :-> Return v)
        | HoistedUpdate udp@(NodeC t vars) <- shared = App fname vs ty :>>= n3 :-> (Return n3 :>>= udp :-> (Update p1 udp) :>>= unit :-> Return (Tup vars))
        | otherwise = App fname vs ty
     where
        fname = toAtom n
        Just (_,ty) = findArgsType te fname

createApply :: Ty -> Ty -> TyEnv -> [Tag] -> Lam
createApply argType retType te ts'
    | null cs && argType == tyUnit = Tup [n1] :-> Error ("Empty Apply:" ++ show ts)  retType
    | null cs = Tup [n1,a2] :-> Error ("Empty Apply:" ++ show ts)  retType
    | argType == tyUnit = Tup [n1] :-> Case n1 cs
    | otherwise = Tup [n1,a2] :-> Case n1 cs
    where
    ts = sortUnder toPackedString ts'
    a2 = Var v2 argType
    cs = [ f t | t <- ts, tagGood t]
    tagGood t | Just TyTy { tyThunk = TyPApp mt w } <- findTyTy te t =
         (Just argType == mt || (argType == tyUnit && Nothing == mt)) && (fmap snd $ findArgsType te w) == Just retType
    tagGood _ = False
--    tagGood t | Just (n,fn) <- tagUnfunction t, n > 0 = let
--        ptag = argType == ts !! (length ts - n)
--        rtag = retType == TyNode || (n == 1 && rt == retType)
--        (ts,rt) = runIdentity $ findArgsType te fn
--        in rtag && ptag
    f t = (NodeC t vs :-> g ) where
        (ts,_) = runIdentity $ findArgsType te t
        vs = [ Var v ty |  v <- [v3 .. ] | ty <- ts]
        Just (n,fn) = tagUnfunction t
        a2s = if argType == tyUnit then [] else [a2]
        g | n == 1 =  App fn (vs ++ a2s) ty
          | n > 1 = Return $ NodeC (partialTag fn (n - 1)) (vs ++ a2s)
          | otherwise = error "createApply"
         where
            Just (_,ty) = findArgsType te fn

{-# NOINLINE createEvalApply #-}
createEvalApply :: Grin -> IO Grin
createEvalApply grin = do
    let eval = (funcEval,Tup [earg] :-> ebody) where
            earg :-> ebody  =  createEval TrailingUpdate (grinTypeEnv grin) tags
        tags = Set.toList $ ftags `Set.union` plads
        ftags = freeVars (map (lamExp . snd) $ grinFuncs grin)
        plads = Set.fromList $ concatMap mplad (Set.toList ftags)
        mplad t | Just (n,tag) <- tagUnfunction t, n > 1 = t:mplad (partialTag tag (n - 1))
        mplad t = [t]
    appMap <- newOnceMap
    let f (ls :-> exp) = do
            exp' <- g exp
            return $ ls :-> exp'
        g (exp :>>= lam) = do
            exp' <- g exp
            lam' <- f lam
            return (exp' :>>= lam')
        g (Case v ls) = do
            ls' <- mapM f ls
            return $ Case v ls'
        g (App fn [fun] ty) | fn == funcApply = do
            fn' <- runOnceMap appMap (tyUnit,ty) $ do
                u <- newUniq
                return (toAtom $ "bapply_" ++ show u)
            return (App fn' [fun] ty)
        g (App fn [fun,arg] ty) | fn == funcApply = do
            fn' <- runOnceMap appMap (getType arg,ty) $ do
                u <- newUniq
                return (toAtom $ "bapply_" ++ show u)
            return (App fn' [fun,arg] ty)
        g x = return x
    funcs <- mapMsnd f (grinFuncs grin)
    as <- onceMapToList appMap
    let (apps,ntyenv) = unzip $ map cf as
        cf ((targ,tret),name) | targ == tyUnit = ((name,appBody),(name,tyTy { tySlots = [TyNode],tyReturn = tret })) where
            appBody = createApply targ tret (grinTypeEnv grin) tags
        cf ((targ,tret),name) = ((name,appBody),(name,tyTy { tySlots = [TyNode,targ],tyReturn = tret })) where
            appBody = createApply targ tret (grinTypeEnv grin) tags
        TyEnv tyEnv = grinTypeEnv grin
        appTyEnv = Map.fromList ntyenv
    return $ setGrinFunctions (apps ++ funcs) grin { grinTypeEnv = TyEnv (tyEnv `Map.union` appTyEnv) }





