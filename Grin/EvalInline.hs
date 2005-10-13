module Grin.EvalInline(
    createEval,
    createApply,
    UpdateType(..)
    ) where


import Grin.Grin
import Control.Monad.Identity
import Atom
import Char

data UpdateType = NoUpdate | TrailingUpdate | HoistedUpdate Val

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
    | otherwise = p1 :->
        Fetch p1 :>>= n2 :->
        Case n2 cs
    where
    cs = [f t | t <- ts, tagIsTag t, isGood t ]
    isGood t | tagIsWHNF t, HoistedUpdate (NodeC t' _) <- shared, t /= t' = False
    isGood _ = True
    g t vs
        | tagIsWHNF t, HoistedUpdate (NodeC t' [v]) <- shared  = case vs of
            [x] -> Return x
            _ -> error "createEval: bad thing"
        | tagIsWHNF t = Return n2
        | 'F':fn <- fromAtom t  = ap ('f':fn) vs
        | 'B':fn <- fromAtom t  = ap ('b':fn) vs
        | otherwise = Error ("Bad Tag: " ++ fromAtom t) TyNode
    f t = (NodeC t vs :-> g t vs ) where
        (ts,_) = runIdentity $ findArgsType te t
        vs = [ Var v ty |  v <- [V 4 .. ] | ty <- ts]
    ap n vs
    --    | shared =  App (toAtom $ n) vs :>>= n3 :-> Update p1 n3 :>>= unit :-> Return n3
        | HoistedUpdate udp@(NodeC t [v]) <- shared = App fname vs ty :>>= n3 :-> Return n3 :>>= udp :-> Update p1 udp :>>= unit :-> Return v
        | HoistedUpdate udp <- shared = App fname vs ty :>>= n3 :-> (Return n3 :>>= udp :-> Update p1 udp) :>>= unit :-> Return n3
        | otherwise = App fname vs ty
     where
        fname = toAtom n
        Just (_,ty) = findArgsType te fname

createApply :: TyEnv -> [Tag] -> Lam
createApply te ts
    | null cs = Tup [n1,p2] :-> Error ("Empty Apply:" ++ show ts)  TyNode
    | otherwise = Tup [n1,p2] :-> Case n1 cs
    where
    cs = [ f t | t <- ts, tagIsPartialAp t]
    f t = (NodeC t vs :-> g ) where
        (ts,_) = runIdentity $ findArgsType te t
        vs = [ Var v ty |  v <- [v3 .. ] | ty <- ts]
        ('P':cs) = fromAtom t
        (n','_':rs) = span isDigit cs
        n = read n'
        g
            | n == (1::Int) =  App fname (vs ++ [p2]) ty
            | n > 1 = Return $ NodeC (toAtom $ 'P':show (n - 1) ++ "_" ++ rs) (vs ++ [p2])
            | otherwise = error "createApply"
         where
            fname = (toAtom $ 'f':rs)
            Just (_,ty) = findArgsType te fname


