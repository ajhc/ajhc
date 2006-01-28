module Grin.Unboxing(unboxReturnValues) where

import Grin.Grin
import qualified Data.Set as Set
import qualified Data.Map as Map
import CanType
import GenUtil
import Atom
import Maybe
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
-- any fully constant values are done first
unboxFunction fn x | getType x /= tyUnit, Just v <- constantItem  x  =  let
    returnType = tyUnit
    unboxReturn Return {} = Return unit
    unboxReturn e = e :>>= var :-> Return unit
    unboxCall (App a as _) | a == fn = App a as returnType :>>= unit :-> Return v
    var = Var v1 (getType x)
    in return (unboxReturn, unboxCall, returnType, TupledValue [])
--unboxFunction fn (NodeValue vs) | all isEnum (Set.toList vs) = (unboxReturn, unboxCall, TyTag) where
--    unboxReturn (Return (NodeC t [])) = Return t
--    unboxReturn (App a as ty) | a == fn = App a as TyTag
--    unboxCall (App a as ty) = (App a as TyTag :>>= (Var v1 TyTag) :-> Return (NodeV v1 []))

-- returning node of exactly one value
unboxFunction fn (NodeValue vs) | [NV t [arg]] <- Set.toList vs  =  let
    returnType = getType arg
    unboxReturn (Return (NodeC t' [x])) | t == t' = Return x
                                        | otherwise = error "returning wrong node"
    unboxReturn e = e :>>= NodeC t [var] :-> Return var
    unboxCall (App a as _) | a == fn = App a as returnType :>>= var :-> Return (NodeC t [var])
    var = Var v1 returnType
    in return (unboxReturn, unboxCall, returnType, arg)
-- returning known node of several arguments
unboxFunction fn (NodeValue vs) | [NV t args] <- Set.toList vs  =  let
    returnType = TyTup (map getType args)
    unboxReturn (Return (NodeC t' xs)) | t == t' = Return (Tup xs)
                                        | otherwise = error "returning wrong node"
    unboxReturn e = e :>>= NodeC t vars :-> Return (Tup vars)
    unboxCall (App a as _) | a == fn = App a as returnType :>>= Tup vars :-> Return (NodeC t vars)
    vars  = [Var v t | v <- [v1 ..] | t <- map getType args ]
    in return (unboxReturn, unboxCall, returnType, TupledValue args)

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
    --putStrLn "Candidate Unboxings"
    --mapM_ print cfns

    let pf fn | Just item <- Map.lookup fn (grinReturnTags grin) =
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

    return grin {
        grinReturnTags = Map.mapWithKey retTag (grinReturnTags grin),
        grinTypeEnv = mtenv (grinTypeEnv grin),
        grinFunctions = map doFunc (grinFunctions grin)
        }


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

