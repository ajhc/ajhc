module Grin.EvalInline(
    createEval,
    createApply,
    UpdateType(..)
    ) where


import Grin.Grin
import Control.Monad.Identity
import Atom
import Char

data UpdateType = NoUpdate | TrailingUpdate | HoistedUpdate Val | SwitchingUpdate [Atom]

-- create an eval suitable for inlining.
createEval :: UpdateType -> TyEnv -> [Tag] -> Lam
createEval shared  te ts

    | null cs = p1 :-> Error "Empty Eval" TyNode
    | all tagIsWHNF [ t | t <- ts , tagIsTag t] = p1 :-> Fetch p1
    | TrailingUpdate <- shared = p1 :->
        Fetch p1 :>>= n2 :->
        Case n2 cs :>>= n3 :->
        Update p1 n3 :>>= unit :->
        Return n3
    | HoistedUpdate (NodeC t [v]) <- shared = p1 :->
        Fetch p1 :>>= n2 :->
        Case n2 cs :>>= v :->
        Return (NodeC t [v])
    | HoistedUpdate (NodeC t []) <- shared = p1 :->
        Fetch p1 :>>= n2 :->
        Case n2 cs :>>= Tup [] :->
        Return (NodeC t [])
    | HoistedUpdate (NodeC t vs) <- shared = p1 :->
        Fetch p1 :>>= n2 :->
        Case n2 cs :>>= Tup vs :->
        Return (NodeC t vs)
    | NoUpdate <- shared = p1 :->
        Fetch p1 :>>= n2 :->
        Case n2 cs
    | SwitchingUpdate sts <- shared = let
            lf = createEval NoUpdate te ts
            cu t | tagIsTag t && tagIsWHNF t = return ans where
                (ts,_) = runIdentity $ findArgsType te t
                vs = [ Var v ty |  v <- [V 4 .. ] | ty <- ts]
                ans = NodeC t vs :-> Update p1 (NodeC t vs)
            cu t = error $ "not updatable:" ++ show t
        in (p1 :-> (Return p1 :>>= lf) :>>= n3 :-> Case n3 (concatMap cu sts) :>>= unit :-> Return n3)
    where
    cs = [f t | t <- ts, tagIsTag t, isGood t ]
    isGood t | tagIsWHNF t, HoistedUpdate (NodeC t' _) <- shared, t /= t' = False
    isGood _ = True
    g t vs
        | tagIsWHNF t, HoistedUpdate (NodeC t' [v]) <- shared  = case vs of
            [x] -> Return x
            _ -> error "createEval: bad thing"
        | tagIsWHNF t, HoistedUpdate (NodeC t' []) <- shared  = case vs of
            [] -> Return (Tup [])
            _ -> error "createEval: bad thing"
        | tagIsWHNF t, HoistedUpdate (NodeC t' vars) <- shared  = Return (Tup vs)
        | tagIsWHNF t = Return n2
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
createApply argType retType te ts
    | null cs = Tup [n1,a2] :-> Error ("Empty Apply:" ++ show ts)  retType
    | otherwise = Tup [n1,a2] :-> Case n1 cs
    where
    a2 = Var v2 argType
    cs = [ f t | t <- ts, tagIsPartialAp t]
    f t = (NodeC t vs :-> g ) where
        (ts,_) = runIdentity $ findArgsType te t
        vs = [ Var v ty |  v <- [v3 .. ] | ty <- ts]
        ('P':cs) = fromAtom t
        (n','_':rs) = span isDigit cs
        n = read n'
        g
            | n == (1::Int) =  App fname (vs ++ [a2]) ty
            | n > 1 = Return $ NodeC (toAtom $ 'P':show (n - 1) ++ "_" ++ rs) (vs ++ [a2])
            | otherwise = error "createApply"
         where
            fname = (toAtom $ 'f':rs)
            Just (_,ty) = findArgsType te fname


