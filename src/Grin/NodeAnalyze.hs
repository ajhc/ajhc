
-- a fast, straightforward points to analysis
-- meant to determine nodes that are always in whnf
-- and find out evals or applys that always
-- apply to a known value

module Grin.NodeAnalyze(nodeAnalyze) where

import Control.Monad.Trans
import Control.Monad.Identity hiding(join)
import Control.Monad(forM, forM_, when)
import Control.Monad.RWS(MonadWriter(..), RWS(..))
import Control.Monad.RWS hiding(join)
import Data.Monoid
import Data.Maybe
import IO
import Text.Printf
import qualified Data.Map as Map
import qualified Data.Set as Set

import Grin.Grin hiding(V)
import Grin.Lint
import Grin.Noodle
import Grin.Whiz
import Options
import StringTable.Atom
import Support.CanType
import Support.FreeVars
import Support.Tickle
import Util.Gen
import Util.SetLike
import Util.UnionSolve
import Util.UniqueMonad
import qualified Stats



data NodeType
    = WHNF       -- ^ guarenteed to be a WHNF
    | Lazy       -- ^ a suspension, a WHNF, or an indirection to a WHNF
    deriving(Eq,Ord,Show)


data N = N !NodeType (Topped (Set.Set Atom))
    deriving(Eq)

instance Show N where
    show (N nt ts) = show nt ++ "-" ++ f ts  where
        f Top = "[?]"
        f (Only x) = show (Set.toList x)

instance Fixable NodeType where
    isBottom x = x == WHNF
    isTop x = x == Lazy
    join x y = max x y
    meet x y = min x y
    eq = (==)
    lte x y = x <= y


instance Fixable N where
    isBottom (N a b) = isBottom a && isBottom b
    isTop (N a b) = isTop a && isTop b
    join  (N x y) (N x' y') = N (join x x') (join y y')
    meet  (N x y) (N x' y') = N (meet x x') (meet y y')
    lte   (N x y) (N x' y') = lte x x' && lte y y'


data V = V Va Ty | VIgnore
    deriving(Eq,Ord)

data Va =
    Vr !Var
    | Fa !Atom !Int
    | Fr !Atom !Int
    deriving(Eq,Ord)

vr v t = V (Vr v) t
fa n i t = V (Fa n i) t
fr n i t = V (Fr n i) t

class NodeLike a where
    isGood :: a -> Bool

instance NodeLike Ty where
    isGood TyNode = True
    isGood TyINode = True
    isGood _ = False

instance NodeLike Val where
    isGood v = isGood (getType v)

instance NodeLike V where
    isGood (V _ t) = isGood t
    isGood _ = False

instance NodeLike (Either V b) where
    isGood (Left n) = isGood n
    isGood _ = True

instance Show V where
    showsPrec _ (V (Vr v) ty) = shows (Var v ty)
    showsPrec _ (V (Fa a i) _) = shows (a,i)
    showsPrec _ (V (Fr a i) _) = shows (i,a)
    showsPrec _ VIgnore = showString "IGN"

newtype M a = M (RWS TyEnv (C N V) Int a)
    deriving(Monad,Functor,MonadWriter (C N V))

runM :: Grin -> M a -> C N V
runM grin (M w) = case runRWS w (grinTypeEnv grin) 1 of
    (_,_,w) -> w


{-# NOINLINE nodeAnalyze #-}
nodeAnalyze :: Grin -> IO Grin
nodeAnalyze grin' = do
    let cs = runM grin $ do
            mapM_ doFunc (grinFuncs grin)
            mapM_ docaf (grinCafs grin)
            doFunc (toAtom "@initcafs",[] :-> initCafs grin)
        grin = renameUniqueGrin grin'
        docaf (v,tt) | True = tell $ Right top `equals` Left (V (Vr v) TyINode)
                     | otherwise = return ()
    --putStrLn "----------------------------"
    --print cs
    --putStrLn "----------------------------"
    --putStrLn "-- NodeAnalyze"
    (rm,res) <- solve (const (return ())) cs
    --(rm,res) <- solve putStrLn cs
    --putStrLn "----------------------------"
    --mapM_ (\ (x,y) -> putStrLn $ show x ++ " -> " ++ show y) (Map.toList rm)
    --putStrLn "----------------------------"
    --mapM_ print (Map.elems res)
    --putStrLn "----------------------------"
    let cmap = Map.map (fromJust . flip Map.lookup res) rm
    (grin',stats) <- Stats.runStatT $ tickleM (fixupfs cmap (grinTypeEnv grin)) grin
    return $ transformFuncs (fixupFuncs (grinSuspFunctions grin) (grinPartFunctions grin) cmap) grin' { grinStats = stats `mappend` grinStats grin' }


data Todo = Todo !Bool [V] | TodoNothing

initCafs grin = f (grinCafs grin) (Return []) where
        f ((v,node):rs) rest = BaseOp Overwrite [(Var v TyINode),node] :>>= [] :-> f rs rest
        f [] rest = rest

doFunc :: (Atom,Lam) -> M ()
doFunc (name,arg :-> body) = ans where
    ans :: M ()
    ans = do
        let rts = getType body
        forMn_ rts $ \ (t,i) -> dVar (fr name i t) t
        forMn_ arg $ \ (~(Var v vt),i) -> do
            dVar (vr v vt) vt
            tell $ cAnnotate "FunArg" $ Left (fa name i vt) `equals` Left (vr v vt)
        fn (Todo True [ fr name i t | i <- naturals | t <- rts ]) body
    -- restrict values of TyNode type to be in WHNF
    dVar v TyNode = do
        tell $ Left v `islte` Right (N WHNF Top)
    dVar _ _ = return ()
    -- set concrete values for vars based on their type only
    -- should only be used in patterns
    zVar s v TyNode = tell $ cAnnotate ("zVar - tynode " ++ s) $ Left (vr v TyNode) `equals` Right (N WHNF Top)
    zVar s v t = tell $ cAnnotate ("zVar - inode " ++ s) $ Left (vr v t) `equals` Right top
    fn :: Todo -> Exp -> M ()
    fn ret body = f body where
        f (x :>>= [Var v vt] :-> rest) = do
            dVar (vr v vt) vt
            gn (Todo True [vr v vt]) x
            f rest
        f (x :>>= vs@(_:_:_) :-> rest) = do
            vs' <- forM vs $ \ (Var v vt) -> do
                dVar (vr v vt) vt
                return $ vr v vt
            gn (if all (== VIgnore) vs' then TodoNothing else Todo True vs') x
            f rest
        f (x :>>= v :-> rest) = do
            forM_ (Set.toList $ freeVars v) $ \ (v,vt) -> zVar "Bind" v vt
            gn TodoNothing x
            f rest
        f body = gn ret body
    isfn _ x y | not (isGood x) = mempty
    isfn (Todo True  _) x y = cAnnotate "isfn True" $ Left x `equals` y
    isfn (Todo False _) x y = Left x `isgte` y
    --isfn (Todo _ _) x y = Left x `isgte` y
    isfn TodoNothing x y =  mempty
    equals x y | isGood x && isGood y = Util.UnionSolve.equals x y
               | otherwise = mempty
    isgte x y | isGood x && isGood y = Util.UnionSolve.isgte x y
              | otherwise = mempty
    islte x y | isGood x && isGood y = Util.UnionSolve.islte x y
              | otherwise = mempty
    gn ret head = f head where
        fl ret (v :-> body) = do
            forM_ (Set.toList $ freeVars v) $ \ (v,vt) -> zVar "Alt" v vt
            fn ret body
        dunno ty = do
            dres [Right (if TyNode == t then N WHNF Top else top) | t <- ty ]
        dres res = do
            case ret of
                Todo b vs -> forM_ (zip vs res) $ \ (v,r) -> tell (isfn ret v r)
                _ -> return ()
        f (_ :>>= _) = error $ "Grin.NodeAnalyze: :>>="
        f (Case v as)
            | Todo _ n <- ret = mapM_ (fl (Todo False n)) as
            | TodoNothing <- ret = mapM_ (fl TodoNothing) as
        f (BaseOp Eval [x]) = do
            dres [Right (N WHNF Top)]
        f (BaseOp (Apply ty) xs) = do
            mapM_ convertVal xs
            dunno ty
        f (App { expFunction = fn, expArgs = vs, expType = ty }) = do
            vs' <- mapM convertVal vs
            forMn_ (zip vs vs') $ \ ((tv,v),i) -> do
                tell $ v `islte` Left (fa fn i (getType tv))
            dres [Left $ fr fn i t | i <- [ 0 .. ] | t <- ty ]
        f (Call { expValue = Item fn _, expArgs = vs, expType = ty }) = do
            vs' <- mapM convertVal vs
            forMn_ (zip vs vs') $ \ ((tv,v),i) ->  do
                tell $ v `islte` Left (fa fn i (getType tv))
            dres [Left $ fr fn i t | i <- [ 0 .. ] | t <- ty ]
        f (Return x) = do mapM convertVal x >>= dres
        f (BaseOp (StoreNode _) w) = do mapM convertVal w >>= dres
        f (BaseOp Promote [w]) = do
            ww <- convertVal w
            tell $ ww `islte` Right (N WHNF Top)
            dres [ww]
        f (BaseOp Demote [w]) = do
            ww <- convertVal w
            tell $ ww `islte` Right (N WHNF Top)
            dres [ww]
        f Error {} = return ()
        f Prim { expArgs = as, expType = ty } = mapM_ convertVal as >> dunno ty
        f Alloc { expValue = v } | getType v == TyNode = do
            v' <- convertVal v
            dres [v']
        f Alloc { expValue = v } | getType v == tyINode = do
            convertVal v
            dunno [TyPtr tyINode]
        f NewRegion { expLam = _ :-> body } = fn ret body
        f (BaseOp Overwrite [Var vname ty,v]) | ty == TyINode = do
            v' <- convertVal v
            tell $ Left (vr vname ty) `isgte` v'
            dres []
        f e@(BaseOp Overwrite vs) = do mapM_ convertVal vs >> dunno (getType e)
        f e@(BaseOp PokeVal vs) = do mapM_ convertVal vs >> dunno (getType e)
        f e@(BaseOp PeekVal vs) = do mapM_ convertVal vs  >> dunno (getType e)
        f Let { expDefs = ds, expBody = e } = do
            mapM_ doFunc (map (\x -> (funcDefName x, funcDefBody x)) ds)
            fn ret e
        f exp = error $ "NodeAnalyze.f: " ++ show exp


    convertVal (Const n@(NodeC _ _)) = convertVal n
    convertVal (Const _) = return $ Right (N WHNF Top)
    convertVal (NodeC t vs) = case tagUnfunction t of
        Nothing -> do
            mapM_ convertVal vs
            return $ Right (N WHNF (Only $ Set.singleton t))
        Just (n,fn) -> do
            vs' <- mapM convertVal vs
            forMn_ (zip vs vs') $ \ ((vt,v),i) -> do
                tell $ v `islte` Left (fa fn i (getType vt))
            forM_ [0 .. n - 1 ] $ \i -> do
               tell $ Right top `islte` Left (fa fn (length vs + i) TyINode)
            return $ Right (N (if n == 0 then Lazy else WHNF) (Only $ Set.singleton t))
    convertVal (Var v t) = return $ Left (vr v t)
    convertVal v | isGood v = return $ Right (N Lazy Top)
    convertVal Lit {} = return $ Left VIgnore
    convertVal ValPrim {} = return $ Left VIgnore
    convertVal Index {} = return $ Left VIgnore
    convertVal Item {} = return $ Left VIgnore
    convertVal ValUnknown {} = return $ Left VIgnore
    convertVal v = error $ "convertVal " ++ show v

bottom = N WHNF (Only (Set.empty))
top = N Lazy Top

data WhatToDo
    = WhatDelete
    | WhatUnchanged
    | WhatConstant Val
    | WhatSubs Ty (Val -> Exp) (Val -> Exp)

isWhatUnchanged WhatUnchanged = True
isWhatUnchanged _ = False


transformFuncs :: (Atom -> [Ty] -> Maybe [Ty] -> (Maybe [WhatToDo],Maybe [WhatToDo])) -> Grin -> Grin
transformFuncs fn grin = grin'' where
    grin'' =  grin' { grinTypeEnv = extendTyEnv (grinFunctions grin') (grinTypeEnv grin') }
    grin' = setGrinFunctions (nfs $ grinFuncs grin) grin
    nfs ds = map fs ds
    fs (n,l@(ps :-> e)) = (n,f (fn n (map getType ps) (Just $ getType e)) l)
    f (Nothing,Nothing) (p :-> e) = p :-> j e
    f (Just ats,rts') (p :-> e) = p' :-> e' where
        rts = maybe (map (const WhatUnchanged) (getType e)) id rts'
        p' = concatMap f (zip p ats) where
            f (v,WhatUnchanged) = [v]
            f (_,WhatDelete) = []
            f (_,WhatConstant _) = []
            f (Var v _,WhatSubs nty _ _) = [Var v nty]
        e' =  g (zip p ats) (j e)
        g ((_,WhatUnchanged):xs) e = g xs e
        g ((_,WhatDelete):xs) e = g xs e
        g ((vr,WhatConstant c):xs) e = Return [c] :>>= [vr] :-> g xs e
        g ((Var v vt,WhatSubs nt _ ft):xs) e = ft (Var v nt) :>>= [Var v vt] :-> g xs e
        g [] e = e :>>= rvs :-> h (zip rvs rts) (drop (length (getType e)) [v1 .. ]) [] where
            rvs = zipWith Var [v1 .. ] (getType e)
        h ((r,WhatUnchanged):xs) vs rs = h xs vs (r:rs)
        h ((r,WhatDelete):xs) vs rs = h xs vs rs
        h ((r,WhatConstant _):xs) vs rs = h xs vs rs
        h ((r,WhatSubs nty tt _):xs) (v:vs) rs = tt r :>>= [Var v nty] :-> h xs vs (Var v nty:rs)
        h [] _ rs = Return (reverse rs)

    j app@(BaseOp (StoreNode False) [NodeC a xs]) = res where
        res = if isNothing ats' then app else e'
        ats = maybe (repeat WhatUnchanged) id ats'
        (ats',_) = fn (tagFlipFunction a) (map getType xs) Nothing
        lvars = zipWith Var [ v1 .. ] (map getType xs)
        e' = Return xs :>>= lvars :-> f (zip lvars ats) []

        f ((v,WhatUnchanged):xs) rs = f xs (v:rs)
        f ((_,WhatDelete):xs) rs = f xs rs
        f ((_,WhatConstant _):xs) rs = f xs rs
        f ((Var v oty,WhatSubs nty tt _):xs) rs = tt (Var v oty) :>>= [Var v nty] :-> f xs (Var v nty:rs)
        f [] rs = BaseOp (StoreNode False) [NodeC a (reverse rs)]



    j app@(App a xs ts) = res where
        res = if isNothing ats' && isNothing rts'  then app else e'
        ats = maybe (repeat WhatUnchanged) id ats'
        rts = maybe (repeat WhatUnchanged) id rts'
        (ats',rts') = fn a (map getType xs) (Just ts)
        lvars = zipWith Var [ v1 .. ] (map getType xs)
        e' = Return xs :>>= lvars :-> f (zip lvars ats) []

        f ((v,WhatUnchanged):xs) rs = f xs (v:rs)
        f ((_,WhatDelete):xs) rs = f xs rs
        f ((_,WhatConstant _):xs) rs = f xs rs
        f ((Var v oty,WhatSubs nty tt _):xs) rs = tt (Var v oty) :>>= [Var v nty] :-> f xs (Var v nty:rs)
        f [] rs = App a (reverse rs) ts' :>>= rvars :-> g (zip rvars' rts) rvars []

        g [] [] rs = Return (reverse rs)
        g ((_,WhatUnchanged):xs) (n:ns) rs = g xs ns (n:rs)
        g ((v,WhatDelete):xs) vs rs = Return [ValUnknown (getType v)] :>>= [v] :-> g xs vs (v:rs)
        g ((v,WhatConstant c):xs) vs rs = Return [c] :>>= [v] :-> g xs vs (v:rs)
        g ((v,WhatSubs _ _ ft):xs) (n:ns) rs = ft n :>>= [v] :-> g xs ns (v:rs)


        rvars = zipWith Var [ v1 .. ] ts'
        rvars' = zipWith Var (drop (length rvars) [ v1 .. ]) ts
        ts' = concatMap g (zip ts rts) where
            g (t,WhatUnchanged) = [t]
            g (t,WhatConstant _) = []
            g (t,WhatDelete) = []
            g (t,WhatSubs nty _ _) = [nty]

    j Let { expDefs = ds, expBody = e } =  grinLet [ updateFuncDefProps d { funcDefBody = snd $ fs (funcDefName d, funcDefBody d) } | d <- ds ] (j e)

    j e = runIdentity $ mapExpExp (return . j) e


fixupFuncs sfuncs pfuncs cmap  = ans where
    ans a as jrs | a `Set.member` pfuncs = (Nothing,Nothing)
                 | a `Set.member` sfuncs = (Just aargs,Nothing)
                 | otherwise =  (Just aargs,fmap rargs jrs) where
        aargs = map (bool pnode WhatUnchanged) largs
        largs = map (lupArg fa a) (zip as [0 ..  ])
        rargs rs = map (bool pnode WhatUnchanged) (map (lupArg fr a) (zip rs [0 ..  ]))
    lupArg fa a (x,i) =  case (x,Map.lookup (fa a i x) cmap) of
        (TyINode,Just (ResultJust _ (N WHNF _))) -> True
        (TyINode,Just ResultBounded { resultLB = Just (N WHNF _) }) -> True
        (TyINode,Just ResultBounded { resultLB = Nothing }) -> True
        _ -> False
    pnode = WhatSubs TyNode (\v -> BaseOp Promote [v]) (\v -> BaseOp Demote [v])

fixupfs cmap tyEnv l = tickleM f (l::Lam) where
    lupVar (Var v t) =  case Map.lookup (vr v t) cmap of
        _ | v < v0 -> fail "nocafyet"
        Just (ResultJust _ lb) -> return lb
        Just ResultBounded { resultLB = Just lb } -> return lb
        Just ResultBounded { resultLB = Nothing } -> return bottom
        _ -> fail "lupVar"
    pstuff x arg n@(N w t) = liftIO $ when verbose (printf "-- %s %s %s\n" x (show arg) (show n))
    f a@(BaseOp Eval [arg]) | Just n <- lupVar arg = case n of
        N WHNF _ -> do
            pstuff "eval" arg n
            Stats.mtick (toAtom "Optimize.NodeAnalyze.eval-promote")
            return (BaseOp Promote [arg])
        _ -> return a
    f a@(BaseOp (Apply ty) (papp:args)) | Just nn <- lupVar papp = case nn of
        N WHNF tset | Only set <- tset, [sv] <- Set.toList set, TagPApp n fn <- tagInfo sv, Just (ts,_) <- findArgsType tyEnv sv -> do
            pstuff "apply" papp nn
            case (n,args) of
                (1,[arg]) -> do
                    Stats.mtick (toAtom "Optimize.NodeAnalyze.apply-inline")
                    let va = Var v1 (getType arg)
                        vars = zipWith Var [ v2 .. ] ts
                    return $ Return [arg,papp] :>>= [va,NodeC sv vars] :-> App fn (vars ++ [va]) ty
                (1,[]) -> do
                    Stats.mtick (toAtom "Optimize.NodeAnalyze.apply-inline")
                    let vars = zipWith Var [ v2 .. ] ts
                    return $ Return [papp] :>>= [NodeC sv vars] :-> App fn vars ty
                (pn,[arg]) -> do
                    Stats.mtick (toAtom "Optimize.NodeAnalyze.apply-inline")
                    let va = Var v1 (getType arg)
                        vars = zipWith Var [ v2 .. ] ts
                    return $ Return [arg,papp] :>>= [va,NodeC sv vars] :-> dstore (NodeC (partialTag fn (pn - 1)) (vars ++ [va]))
                (pn,[]) -> do
                    Stats.mtick (toAtom "Optimize.NodeAnalyze.apply-inline")
                    let vars = zipWith Var [ v2 .. ] ts
                    return $ Return [papp] :>>= [NodeC sv vars] :-> dstore (NodeC (partialTag fn (pn - 1)) vars)
                _ -> return a
        _ -> return a
    f e = mapExpExp f e

dstore x = BaseOp (StoreNode True) [x]


renameUniqueGrin :: Grin -> Grin
renameUniqueGrin grin = res where
    (res,()) = evalRWS (execUniqT 1 ans) ( mempty :: Map.Map Atom Atom) (fromList [ x | (x,_) <- grinFuncs grin ] :: Set.Set Atom)
    ans = do tickleM f grin
    f (l :-> b) = g b >>= return . (l :->)
    g a@App  { expFunction = fn } = do
        m <- lift ask
        case mlookup fn m of
            Just fn' -> return a { expFunction = fn' }
            _ -> return a
    g a@Call { expValue = Item fn t } = do
        m <- lift ask
        case mlookup fn m of
            Just fn' -> return a { expValue = Item fn' t }
            _ -> return a
    g (e@Let { expDefs = defs }) = do
        (defs',rs) <- liftM unzip $ flip mapM defs $ \d -> do
            (nn,rs) <- newName (funcDefName d)
            return (d { funcDefName = nn },rs)
        local (fromList rs `mappend`) $  mapExpExp g e { expDefs = defs' }
    g b = mapExpExp g b
    newName a = do
        m <- lift get
        case member a m of
            False -> do lift $ modify (insert a); return (a,(a,a))
            True -> do
            let cfname = do
                uniq <- newUniq
                let fname = toAtom $ show a  ++ "-" ++ show uniq
                if fname `member` (m :: Set.Set Atom) then cfname else return fname
            nn <- cfname
            lift $ modify (insert nn)
            return (nn,(a,nn))


bool x y b = if b then x else y
