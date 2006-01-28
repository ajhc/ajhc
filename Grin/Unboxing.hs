module Grin.Unboxing(unboxReturnValues) where

import Maybe
import Monad
import qualified Data.Map as Map
import qualified Data.Set as Set

import Atom
import GenUtil
import Grin.Grin
import Support.CanType
import Support.Tuple
import Util.Graph


tailcalls :: Lam -> Set.Set Atom
tailcalls (_ :-> e) = f e where
    f (_ :>>= l) = tailcalls l
    f App { expFunction = fn } = Set.singleton fn
    f Case { expAlts = as } = Set.unions (map tailcalls as)
    f _ = Set.empty

unboxingCandidate :: Item -> Bool
unboxingCandidate item = isJust (unboxFunction undefined item)


isEnum (NV _ []) = True
isEnum _ = False


unboxFunction :: Monad m => Atom -> Item -> m (Exp -> Exp, Exp -> Exp, Ty, Item)
unboxFunction _ x | getType x == tyUnit = fail "unboxFunction: return type is already ()"
-- get rid of any fully constant values in return
unboxFunction fn item | any isLeft rvs = return (unboxReturn, unboxCall, returnType, nvs) where
    vs = fromTuple item
    rvs = [ case constantItem v of Just x -> Left x ; _ -> Right v | v <- vs ]
    nvs = tuple (rights rvs)
    returnType = getType nvs
    unboxReturn e = e :>>= tuple vars :-> Return (tuple vars')
    unboxCall (App a as _) | a == fn = App a as returnType :>>= tuple vars' :-> Return (tuple [ case x of Left x -> x ; Right _ -> v |  v <- vars | x <- rvs ])
    vars  = [Var v t | v <- [v1 ..] | t <- map getType vs ]
    vars' = concat [ perhapsM (isRight r) (Var v t)  | v <- [v1 ..] | t <- map getType vs | r <- rvs ]

-- unbox enumerated types
unboxFunction fn (NodeValue vs) | all isEnum (Set.toList vs) = return (unboxReturn, unboxCall, TyTag, itemTag) where
    unboxReturn (Return (NodeC t [])) = Return (Tag t)
    unboxReturn e = e :>>= nodev :-> Return var
    unboxCall (App a as ty) = App a as TyTag :>>= var :-> Return nodev
    var = Var v1 TyTag
    nodev = NodeV v1 []

-- returning a known node type
unboxFunction fn (NodeValue vs) | [NV t args] <- Set.toList vs  =  let
    returnType = tuple (map getType args)
    unboxReturn (Return (NodeC t' xs))
        | t == t' = Return (tuple xs)
        | otherwise = error "returning wrong node"
    unboxReturn e = e :>>= NodeC t vars :-> Return (tuple vars)
    unboxCall (App a as _) | a == fn = App a as returnType :>>= tuple vars :-> Return (NodeC t vars)
    vars  = [Var v t | v <- [v1 ..] | t <- map getType args ]
    in return (unboxReturn, unboxCall, returnType, tuple args)

unboxFunction _ item = fail "function not unboxable" -- (id,id,getType item)

constantItem (NodeValue vs) | [NV t xs] <- Set.toList vs  = do
    xs <- mapM constantItem xs
    return (NodeC t xs)
constantItem (TupledValue xs) = do
    xs <- mapM constantItem xs
    return (Tup xs)
constantItem (HeapValue vs) | [HV _ (Right val)] <- Set.toList vs  = do
    return (Const val)
constantItem _ = fail "not constant item"

{-# NOINLINE unboxReturnValues #-}
unboxReturnValues :: Grin -> IO Grin
unboxReturnValues grin = do
    let tcgraph = newGraph [ (n, Set.toList $ tailcalls body) | (n,body) <- grinFunctions grin] fst snd
        ubc a | Just v <- Map.lookup a (grinReturnTags grin) = unboxingCandidate v
        ubc _ = False
        cfns = filter ubc (fsts $ grinFunctions grin)
        pf fn | Just item <- Map.lookup fn (grinReturnTags grin) =
            do x <- unboxFunction fn item ; return $ Map.singleton fn x
        fns = Map.unions $ concatMap pf cfns
        retTag fn _ | Just (_,_,_,ret) <- Map.lookup fn fns = ret
        retTag _ x = x
        retTe fn (ts,_) | Just (_,_,ret,_) <- Map.lookup fn fns = (ts,ret)
        retTe _ x = x
        mtenv (TyEnv mp) = TyEnv $ Map.mapWithKey retTe mp
        doFunc (fn,lam) | Just (unboxReturn,_,_,_) <- Map.lookup fn fns = doFunc' (fn,convertReturns unboxReturn lam)
        doFunc (fn,lam) = doFunc' (fn,lam)
        doFunc' (fn,lam) = (fn, convertApps doApp lam)
        doApp ap@(App fn _ _) | Just (_,f,_,_) <- Map.lookup fn fns = f ap
        doApp e = e
    putStrLn "Unboxed return values"
    mapM_ putStrLn [ "  " ++ show fn ++ " - " ++  show nt | (fn,(_,_,nt,_)) <- Map.toList fns]

    let newgrin = grin {
        grinReturnTags = Map.mapWithKey retTag (grinReturnTags grin),
        grinTypeEnv = mtenv (grinTypeEnv grin),
        grinFunctions = map doFunc (grinFunctions grin)
        }
    if Map.null fns then return newgrin else unboxReturnValues newgrin


convertReturns unboxReturn lam = g lam where
    g (l :-> e) = l :-> f e
    f (e :>>= l) = e :>>= g l
    f e@Case { expAlts = as } = e { expAlts = map g as }
    f e = unboxReturn e

convertApps doApp lam = g lam where
    g (l :-> e) = l :-> f e
    f (e :>>= l) = f e :>>= g l
    f e@Case { expAlts = as } = e { expAlts = map g as }
    f e = doApp e

