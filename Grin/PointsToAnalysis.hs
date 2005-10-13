module Grin.PointsToAnalysis(grinInlineEvalApply) where

import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Writer
import Data.IORef
import Data.Monoid
import List(sort,intersperse)
import Maybe
import Monad
import qualified Data.Map as Map
import qualified Data.Set as Set

import Atom
import CharIO
import Doc.DocLike
import Fixer
import GenUtil
import Grin.EvalInline
import Grin.Grin
import Grin.HashConst
import Grin.Linear
import Options
import qualified Doc.Chars as U
import qualified FlagDump as FD
import UniqueMonad
import Util.Once


sameLength (_:xs) (_:ys) = sameLength xs ys
sameLength [] [] = True
sameLength _ _ = False

data HeapType = Constant | SharedEval | UnsharedEval | Reference | RecursiveThunk
    deriving(Eq,Ord,Show)

-- These names make no sense
-- this analysis could probably be strongly typed.
data Pos =
    Union [Pos]
    | Variable {-# UNPACK #-} !Var
    | Func {-# UNPACK #-} !Atom
    | Basic
    | PCase Pos [(Atom,Pos)] Pos
    | PIf {-# UNPACK #-} !Bool Pos Atom Pos
    | Ptr {-# UNPACK #-}!Int
    | Down Pos {-# UNPACK #-}!Atom {-# UNPACK #-}!Int
    | DownTup Pos {-# UNPACK #-}!Int
    | Arg {-# UNPACK #-} !Atom {-# UNPACK #-}!Int
    | Con {-# UNPACK #-} !Atom [Pos]
    | Tuple [Pos]
    | Complex {-# UNPACK #-}!Atom [Pos]
    deriving(Ord,Eq)

instance Show Pos where
    showsPrec n (Variable v) xs = showsPrec n v xs
    showsPrec n (Func a) xs = U.lArrow ++ showsPrec n a  xs
    showsPrec _ Basic xs = 'B':'A':'S':xs
    showsPrec n (Ptr i) xs = '*':showsPrec n i xs
    showsPrec n (Down p a i) xs = show p ++ U.dArrow ++ show a ++ U.dArrow ++ show i ++ xs
    showsPrec n (DownTup p i) xs = show p ++ U.dArrow ++ show i ++ xs
    showsPrec n (Arg p i) xs = show p ++ U.rArrow ++ show i ++ xs
    showsPrec n (Con p i) xs = show p ++ show i ++ xs
    showsPrec n (Tuple ps) xs = (parens $ hcat (intersperse "," $ map show ps)) ++ xs
    showsPrec n (Complex a p) xs = show a ++ tupled (map show p) ++ xs
    showsPrec n (Union ps) xs =  text "{" ++ hcat (intersperse "," $ map show ps) ++ "}" ++ xs
    showsPrec n (PCase p as p') xs = text "case" <+> shows p <+> shows as <+> shows p'  $ xs
    showsPrec n (PIf True p a p') xs = text "if" <+> shows a <+> U.elem <+>  shows p <+> text "then"  <+> shows p' $ xs
    showsPrec n (PIf False p a p') xs = text "if" <+> shows a <+> U.notElem <+>  shows p <+> text "then"  <+> shows p' $ xs

instance Monoid Pos where
    mempty = Union []
    mappend (Union []) x = x
    mappend x (Union []) = x
    mappend (Union xs) (Union ys) = mconcat (xs ++ ys)
    mappend (Union xs) x = mconcat (x:xs)
    mappend x (Union xs) = mconcat (x:xs)
    mappend x y = mconcat [x,y]
    mconcat xs = f (snub xs) [] where
        f [] [] = Union []
        f [] [x] = x
        f [] xs = Union xs
        f (Tuple ps:Tuple ps':xs) ys | sameLength ps ps'  = f (Tuple [ mappend x y | x <- ps | y <- ps']:xs) ys
        f (Con a ps:Con a' ps':xs) ys | a == a' && sameLength ps ps'  = f (Con a [ mappend x y | x <- ps | y <- ps']:xs) ys
        f (DownTup (Tuple vs) n:xs) ys = f ((vs !! n):xs) ys
        f (x:xs) ys = f xs (x:ys)


data ValueSet = VsEmpty | VsNodes (Map.Map (Atom,Int) ValueSet) (Set.Set Atom)  | VsHeaps !(Set.Set Int) | VsBas String
    deriving(Eq,Ord)
    {-! derive: is !-}

getHeaps' s VsEmpty = Set.empty
getHeaps' s (VsHeaps h) = h
getHeaps' s x = error $ "getHeaps: " ++ s ++ " " ++ show x

getHeaps VsEmpty = Set.empty
getHeaps (VsHeaps s) = s
getHeaps x = error $ "getHeaps: " ++ show x

getNodes VsEmpty = Set.empty
getNodes (VsNodes _ s) = s
getNodes x = error $ "getNodes: " ++ show x

getNodeArgs VsEmpty = Map.empty
getNodeArgs (VsNodes s _) = s
getNodeArgs x = error $ "getNodeArgs: " ++ show x

vsBas = VsBas ""
setNodes [] = VsEmpty
setNodes xs = pruneNodes $ VsNodes (Map.fromList $ concat [ [ ((n,i),a) | a <- as | i <- [0..] ] | (n,as) <- xs]) (Set.fromList (fsts xs))
setHeaps [] = VsEmpty
setHeaps xs = VsHeaps (Set.fromList xs)

pruneNodes (VsNodes x y) = VsNodes (Map.filter (not . isBottom) x) y
pruneNodes x = x

instance Monoid ValueSet where
    mempty = VsEmpty
    mappend VsEmpty x = x
    mappend x VsEmpty = x
    mappend (VsBas a) (VsBas b) = VsBas a
    --mappend (VsBas a) (VsBas b) = VsBas (a ++ b)
    mappend (VsHeaps a) (VsHeaps b) = VsHeaps (Set.union a b)
    mappend (VsNodes a a') (VsNodes b b') = pruneNodes $ VsNodes (Map.unionWith mappend a b) (Set.union a' b')
    mappend x y = error $ "mappend: " ++ show x <+> show y

instance Fixable ValueSet where
    bottom = mempty
    lub = mappend
    isBottom VsEmpty = True
    isBottom (VsHeaps s) | Set.null s = True
    isBottom (VsNodes n s) | Map.null n && Set.null s = True
    isBottom _ = False
    minus a VsEmpty = a
    minus VsEmpty _ = VsEmpty
    minus (VsBas _) (VsBas _) = VsEmpty
    minus (VsHeaps h1) (VsHeaps h2) = VsHeaps (h1 Set.\\ h2)
    minus (VsNodes n1 w1) (VsNodes n2 w2) = pruneNodes $ VsNodes (Map.fromList $ concat [
            case Map.lookup (a,i) n2 of
                Just v' ->  [((a,i),v `minus` v')]
                Nothing ->  [((a,i),v)]
        | ((a,i),v) <- Map.toList n1 ] ) (w1 Set.\\ w2)
    minus x y = error $ "minus: " ++ show x <+> show y

instance Show ValueSet where
    showsPrec x VsEmpty = \xs -> '{':'}':xs
    showsPrec x (VsBas a) = \xs -> '(':'B':'a':'s':':':a ++ ")" ++ xs
    showsPrec x (VsHeaps s)
        | Set.size s > 7  = braces (hcat (intersperse (char ',') $ map tshow  (take 7 $ Set.toAscList s)) <> text ",...")
        | otherwise  = braces (hcat (intersperse (char ',') $ map tshow  ( Set.toAscList s)) )

    showsPrec x (VsNodes n s) = braces (hcat (intersperse (char ',') $ (map f $ snub $ fsts  (Map.keys n) ++ Set.toList s) )) where
        f a = (if a `Set.member` s then tshow a else char '#' <> tshow a) <> tshow (g a)
        g a = sort [ (i,v) | ((a',i),v) <- Map.toList n, a' == a ]


data PointsTo = PointsTo {
    ptVars :: Map.Map Var ValueSet,
    ptFunc :: Map.Map Atom ValueSet,
    ptHeap :: Map.Map Int ValueSet,
    ptHeapType :: Map.Map Int HeapType
    }
    deriving(Show)
    {-! derive: Monoid, update !-}

pointsToStats :: PointsTo -> String
pointsToStats pt = text "PointsTo Analysis results:" <$> buildTable ["Total", "Empty", "Basic", "Max", "Average" ] [f "Variables" (ptVars pt), f "Functions" (ptFunc pt), f "Heap" (ptHeap pt)] where
    f n mp = {- text n <> char ':' <+> -}  vs n (Map.elems mp)
    vs n xs = (n,[tshow $ length xs, show (count isVsEmpty xs),show (count isVsBas xs),show (maximum $ 0:map num xs), show ((fromIntegral (sum (map num xs)) ::Double ) / fromIntegral (length xs))] )
    num (VsNodes x s) = Set.size s
    num (VsHeaps x) = Set.size x
    num _ = 0




data PointsToEq = PointsToEq {
    varEq  :: [(Var, Pos)],
    funcEq :: [(Atom,Pos)],
    heapEq :: [(Int,(HeapType,Pos))],
    updateEq :: [(Pos,Pos)],
    applyEq :: [(Pos,Pos)],
    appEq  :: [(Atom,[Pos])]

    }
    deriving(Show)
    {-! derive: Monoid, update !-}

flattenPointsToEq eq = varEq_u f . funcEq_u f . heapEq_u h . appEq_u g $ eq  where
    f xs = [ (x, mconcat $ snds xs)  | xs@((x,_):_) <- sortGroupUnder fst xs]
    --g xs = [ (x, map mconcat $ transpose (snds xs))  | xs@((x,_):_) <- sortGroupUnder fst xs]
    g xs = xs
    h xs = [ (x, (t,mconcat $ snds $ snds xs))  | xs@((x,(t,_)):_) <- sortGroupUnder fst xs]




--newHeap ht p@(Con a ps)
--    | tagIsSuspFunction a, Identity t <- tagToFunction a = newHeap' ht (mappend p (Func t))
newHeap ht p = newHeap' ht p


newHeap' ht p = do
    h <- newUniq
    tell mempty { heapEq = [(h,(ht,p))] }
    return (Ptr h)

bind (Var v _) p = tell mempty { varEq = [(v, p)] }
bind (NodeC t [Lit {}]) _ = return ()
bind (NodeC t vs) p | sameLength vs vs' = tell mempty { varEq = vs' }  where
    vs' = [ (v,if basicType ty then Basic else Down p t i) | Var v ty <- vs | i <- [0..] ]
    basicType (Ty _) = True
    basicType _ = False
bind (Tup []) _ = return ()
bind (Tup vs) p | sameLength vs vs' = tell mempty { varEq = vs'  }  where
    vs' = [ (v,if basicType ty then Basic else DownTup p i) | Var v ty <- vs | i <- [0..] ]
    basicType (Ty _) = True
    basicType _ = False
bind x y = error $ unwords ["bind:",show x,show y]

analyze :: Grin -> IO PointsTo
analyze grin@(Grin { grinTypeEnv = typeEnv, grinFunctions = grinFunctions, grinCafs = cafs }) = do
    wdump FD.Progress $ CharIO.putErrLn "Linear nodes analysis..."
    lr <- Grin.Linear.grinLinear grin

    let f (eq,hc) (n,l) | n == funcEval = (eq,hc)
        f (eq,hc) (n,l) | n == funcApply = (eq,hc)
        f (eq,hc) (n,l) = mapFst (mappend eq) $ collect (Map.fromList lr) hc (mh eq + 1) n l
        mh PointsToEq { heapEq = xs } = maximum $ 1:fsts xs
        --toHEq (NodeC t []) | not (tagIsWHNF t) = return (SharedEval,Union [Con t [], func (fromAtom t) ] )
        toHEq (NodeC t []) | not (tagIsWHNF t) = return (SharedEval,Con t []  )
        toHEq node = toPos node >>= return . (,) Constant
        (heapEq',hc') = runState (sequence [ toHEq node >>= return . (,) h | (v,node) <- cafs | h <- [1..] ]) emptyHcHash
        eq = mempty {
            --heapEq = [ (h,(SharedEval,Union [Con t [], func (fromAtom t) ] )) | (v,NodeC t []) <- cafs | h <- [1..] ],
            --varEq =  [ (v,Ptr h) | (v,NodeC t []) <- cafs | h <- [1..] ]
            heapEq = heapEq', -- [ (h,toHEq node) | (v,node) <- cafs | h <- [1..] ],
            varEq =  [ (v,Ptr h) | (v,_) <- cafs | h <- [1..] ]
            }
        (neq,hc) = mapFst flattenPointsToEq $ foldl f  (eq,hc') grinFunctions
        func ('B':xs) = Func $ toAtom $ 'b':xs
        func ('F':xs) = Func $ toAtom $ 'f':xs
        func x = error $ "func:" ++ x
    when (dump FD.Eval) $ do
        CharIO.putStrLn "vars:"
        mapM_ CharIO.print $ sort $ varEq neq
        CharIO.putStrLn "apps:"
        mapM_ CharIO.print $ Map.toList (Map.fromListWith (zipWith mappend) (appEq neq))
        CharIO.putStrLn "funcs:"
        mapM_ CharIO.print $ sort $ funcEq neq
        CharIO.putStrLn "updates:"
        mapM_ CharIO.print $ sort $ updateEq neq
        CharIO.putStrLn "heaps:"
        mapM_ CharIO.print $ sort $ heapEq neq
        let vm = Map.fromList (varEq neq)
            (HcHash _ mp) = hc
            cheaps = sort [ ((-x),setNodes [(t,(map z xs))]) | (HcNode t xs,x) <- Map.toList mp ] where
            z (Right n) = setHeaps [(-n)]
            z (Left (Var v _)) = case Map.lookup v vm of
                Just (Ptr h) -> setHeaps [h]
                _ -> error "cheaps"
            z (Left x) = VsBas (show x)
        mapM_ CharIO.print $ sort $ cheaps
        CharIO.putStrLn "applys:"
        mapM_ CharIO.print $ sort $ applyEq neq
    doTime "findFixpoint" $ findFixpoint' grin hc neq

-- create an eval suitable for inlining.
createStore ::  TyEnv -> [Tag] -> Lam
createStore  te ts
    | null cs = n1 :-> Error "Empty Store" (TyPtr TyNode)
    | otherwise = n1 :->
        Case n1 cs
    where
    cs = [f t | t <- ts, tagIsTag t ]
    f t = (NodeC t vs :-> Store (NodeC t vs)) where
        (ts,_) = runIdentity $ findArgsType te t
        vs = [ Var v ty |  v <- [V 4 .. ] | ty <- ts]

grinInlineEvalApply :: Grin -> IO Grin
grinInlineEvalApply  grin@(Grin { grinTypeEnv = typeEnv, grinFunctions = grinFunctions, grinCafs = cafs }) = do
    pt <- analyze grin
    wdump FD.Progress $ do
        CharIO.putStrLn (pointsToStats pt)
    wdump FD.Eval $ do
        CharIO.putStrLn "funcs:"
        mapM_ CharIO.print [ v  | v@(_,_) <-  Map.toList (ptFunc pt)]
        CharIO.putStrLn "vars:"
        mapM_ CharIO.print [ v  | v@(_,_) <-  Map.toList (ptVars pt)]
        CharIO.putStrLn "heap:"
        mapM_ CharIO.print [ v  | v@(_,_) <-  Map.toList (ptHeap pt)]

    let f (l :-> e) = l :-> g e
        g (App a [vr@(Var v _)] _ :>>= vb :-> Return vb' :>>= node@(NodeC {}) :-> e)
            | vb == vb', a == funcEval = (Return vr :>>= createEval (HoistedUpdate node) typeEnv (tagsp v)) :>>= vb :-> Return vb' :>>= node :-> g e
        g (e1 :>>= l) = g e1 :>>= f l
        g (App a [vr@(Var v _)] _)
            | a == funcEval = Return vr :>>= createEval TrailingUpdate typeEnv (tagsp v)
        g app@(App a [vr@(Var v _),y] _)
            | a == funcApply = case (tags v) of
                Just ts ->  Return (Tup [vr,y]) :>>= createApply typeEnv ts
                Nothing -> error $ "InlineEvalApply: " ++ show app
        g n@(App a _ _)
            | a == funcApply || a == funcEval = error $ "Invalid evap: " ++ show n
        g (Store vr@(Var v _)) | Just ts <- tags v = Return vr :>>= createStore typeEnv ts
        g st@(Store (Var {})) = Error ("Store of basic: " ++ show st) (TyPtr TyNode)
        g (Case v@(Var vr _) xs) = docase v (map f xs) (tags vr)
        g (Case v xs) = Case v (map f xs)
        g x = x
        tags v = if isVsBas x then Nothing else Just [ t | t <- Set.toList vs] where
              vs = getNodes   x
              x = case Map.lookup v (ptVars pt) of
                Just x -> x
                Nothing -> error $ "Tags: " ++ show v
        tagsp v = snub (concat [ f n |  n <- Set.toList vs ]) where
            f n = [ t | t <- Set.toList $ getNodes h ]  where
                Just h = Map.lookup  n (ptHeap pt)
            vs = getHeaps x
            Just x = Map.lookup v (ptVars pt)
        docase v xs Nothing =  Case v xs
        docase _ ((_ :-> x):_) (Just []) = Error "No Valid alternatives. This Should Not be reachable." (runIdentity $ tc typeEnv x)
        --docase v xs (Just ts) | null vs && any (`notElem` ns') ts = error $ "Odd Case: " ++ show (v,ns',ts)  where
        --    (ns,vs) = span isNodeC xs
        --    ns' = [ t | NodeC t _ :-> _ <- ns ]
        --    isNodeC (NodeC {} :-> _) = True
        --   isNodeC _ = False
        docase v xs (Just ts) | not (null ns && null vs) = if length ns == length ts  then Case v ns else Case v (ns ++ vs) where
            (ns,vs) = span isNodeC (filter g xs)
            g (NodeC t _ :-> _) = t `elem` ts
            g (Var {} :-> _ ) = True
            g _ = False
            isNodeC (NodeC {} :-> _) = True
            isNodeC _ = False
            --simple (NodeC t [Lit {}] :-> _) = False
            --simple (NodeC t _ :-> _) = True
        docase _ ((_ :-> x):_) _ = Error "No Valid alternatives. This Should Not be reachable." (runIdentity $ tc typeEnv x)
        docase _ _ _ = error $ "docase: strange argument"
    return grin { grinFunctions = map (mapSnd f) grinFunctions }

collect :: Map.Map Var W -> HcHash -> Int -> Atom -> Lam -> (PointsToEq,HcHash)
collect lmap hc st fname (Tup vs :-> exp')
    | sameLength avs vs = (eq { funcEq = (fname,v):funcEq eq, varEq = varEq eq ++ avs },hc')   where
    avs = [ (v,Arg fname n) |  Var v _ <- vs | n <- [0..] ]
    --((v,eq),hc') = execUniq st $ (runStateT ((runWriterT (f exp'))) hc)
    ((v,hc'),eq) = execUniq st $ (runWriterT (runStateT (f exp') hc))
    --((v,hc'),eq) = runWriter $ execUniqT st $ (runStateT  (f exp') hc)
    --tell x = lift $ Control.Monad.Writer.tell x
    isHole (Con t _) | t == tagHole = True
    isHole _ = False

    f (Store { expValue = val } :>>= var@(Var v _) :-> exp2) = do
        p <- toPos val
        p' <- if Map.lookup v lmap == Just One then newHeap UnsharedEval p else newHeap SharedEval p
        bind var p'
        f exp2

    f (exp :>>= v :-> exp2) = do
        p <- g exp
        bind v p
        f exp2
    f exp = g exp

    g (App fe [v] _) | fe == funcEval = do
        x <- toPos v
        tell mempty { appEq = [(funcEval,[x])] }
        return $ Complex funcEval [Complex funcFetch [x]]
    g (App fe [v,x] _) | fe == funcApply = do
        v <- toPos v
        x <- toPos x
        tell mempty { applyEq = [(v,x)] }
        return $ Complex funcApply [v,x]
        --return $ Complex funcEval (Complex funcApply x)

    g (App a vs _) | a `notElem` [funcEval,funcApply]  = do
        vs' <- mapM toPos vs
        tell mempty { appEq = [(a,vs')] }
        return $ Func a
    g Return { expValue = n@(NodeC _ (_:_)) } = do
        p@(Con a ts) <- toPos n
        --case fromAtom a of
        --    'F':rs -> tell mempty { appEq = [(toAtom ('f':rs),ts)] }
        --    'B':rs -> tell mempty { appEq = [(toAtom ('b':rs),ts)] }
        --    _ -> return ()
        return p
    g (Return { expValue = val }) = toPos val
    g Store { expValue = NodeC t _ } | t == tagHole = do
        newHeap RecursiveThunk mempty
    g Store { expValue = n@(NodeC _ (_:_)) } = do
        p@(Con a ts) <- toPos n
        --case fromAtom a of
        --    'F':rs -> tell mempty { appEq = [(toAtom ('f':rs),ts)] }
        --    'B':rs -> tell mempty { appEq = [(toAtom ('b':rs),ts)] }
        --    _ -> return ()
        newHeap SharedEval p
    g (Store { expValue = val }) = do
        v <- toPos val
        newHeap SharedEval v
    g Fetch { expAddress = val } = do
        p <- toPos val
        return $ Complex funcFetch [p]
    g (Prim p vs)
        | Just as <- primRets p = return $ Union [ Con a [] | a <- as]
        | (_,TyTup []) <- primType p = return Basic
        | (_,TyTup ts) <- primType p = return $ Tuple (replicate (length ts) Basic)
        | otherwise = return Basic
    g (Cast v _) = toPos v
    g (Error {}) = return mempty
    g (Case d ls) = do
        p <- toPos d
        --xs <- sequence [ bind v p >> f exp |  v :-> exp <- ls ]
        let f'' bnd tg exp = do
                (v,w) <- listen (bnd >> f exp)
                let t x = PIf True p tg x -- [(tg,x)] mempty
                    z xs = [ (t x,t y) |  (x,y) <- xs ]
                    z' as = [  (a,map t ts)   |  (a,ts) <- as   ]
                tell (applyEq_u z $ updateEq_u z $ appEq_u z' $  w)
                return v
            f' bnd _ exp = bnd >> f exp
        xs <- sequence [  f' (bind v p) t exp >>= \x -> return (t,x) |  v@(NodeC t _) :-> exp <- ls ]
        els <- sequence [ bind v p >> f exp |  v@(Var _ _) :-> exp <- ls ]
        let els' = head (els ++ [mempty])
        if (length xs + length els == length ls) then
            return (PCase p xs els')
              else sequence [ f e | _ :-> e <- ls ] >>= return . mconcat
        --return $ mconcat xs
    g (Update p v) = do
        p <- toPos p
        v <- toPos v
        tell mempty { updateEq = [(p,v)] }
        return Basic
    g x = error $ unwords ["g",show x]
collect _ _ _ _ _ = error "collect: bad argument"

toPos (NodeC tag vs) = do
    vs' <- mapM toPos vs
    return $ Con tag vs'
toPos (Const v) = do
    (_,h) <- newConst' True v
    return $ Ptr (-h)
--    p <- toPos v
--    newHeap Constant p
toPos (Tup []) = return Basic
toPos (Tup xs) = do
    vs' <- mapM toPos xs
    return $ Tuple vs'
toPos (Lit {}) = return Basic
toPos (Var v _)  = return $ Variable v
toPos u | u == unit = return Basic
toPos x  = error $ unwords ["toPos:",show x]



hcHashGetNodes (HcHash _ hc) = [ (x,n) | (n,x) <- Map.toList hc ]


tupleName = toAtom ""

constPos Basic = return vsBas
constPos (Con a []) = return (setNodes [(a,[])])
constPos (Con a xs) = do
    cs <- mapM constPos xs
    return (setNodes [(a,cs)])
constPos (Tuple []) = return $ VsBas "()"
constPos (Tuple ts) = constPos (Con tupleName ts)
constPos (Union cs) = do
    cs' <- mapM constPos cs
    return (mconcat cs')
constPos (Ptr i)  = return $ setHeaps [i]
constPos _ = fail "not a constant Pos"

findFixpoint' :: Grin -> HcHash -> PointsToEq -> IO PointsTo
findFixpoint' grin (HcHash _ mp) eq = do
    fr <- newFixer
    let cmap eql = do
            vs <- flip mapM eql $ \ (v,p) -> do
                x <- newValue fr bottom
                return (v,(x,p))
            return $ Map.fromList vs
    varMap <- cmap (varEq eq)
    funcMap <- cmap (funcEq eq)
    heapMap <- cmap (heapEq eq)
    argMap <- newIORef mempty


    let cheaps = Map.fromList [ ((-x),setNodes [(t,(map z xs))]) | (HcNode t xs,x) <- Map.toList mp ] where
        z (Right n) = setHeaps [(-n)]
        z (Left (Var v _)) = case Map.lookup v varMap of
            Just (_,(Ptr h)) -> setHeaps [h]
            _ -> error "cheaps"
        z (Left i) = VsBas (show i)

    let procPos self p = pp p where
            pp p | Just c <- constPos p = self `isSuperSetOf` value c
            pp p | Just e <- simplePos p = self `isSuperSetOf` e
            pp (Union ps) = mapM_ pp ps
            pp (Tuple ts) = pp (Con tupleName ts)
            pp (DownTup p n) = pp (Down p tupleName n)
            pp (PIf True p a t) = do
                p' <- newVal p
                t' <- newVal t
                conditionalRule (Set.member a . getNodes) p' $ do self `isSuperSetOf` t'
            pp (PCase p vs e) = do
                p' <- newVal p
                e' <- newVal e
                flip mapM_ vs $ \ (a,w) -> do
                    w' <- newVal w
                    conditionalRule (Set.member a . getNodes) p' $ do self `isSuperSetOf` w'
                once <- newOnce
                conditionalRule (\x -> not $ or [ Set.member a (getNodes x) | (a,_) <- vs]) p' $ do runOnce once (self `isSuperSetOf` e')
            pp cc@(Complex a [p])
                | a == funcEval = do
                    p' <- newVal p
                    modifiedSuperSetOf self p' (\n -> pruneNodes $ VsNodes (Map.filterWithKey (\ (t,_) _ -> tagIsWHNF t) (getNodeArgs n)) (Set.filter tagIsWHNF (getNodes n)))
                    dynamicRule p' $ \p -> do
                        flip mapM_ (Map.toList $ getNodeArgs p) $ \ ((n,i),v) -> do
                            when (tagIsSuspFunction n) $ do
                                a <- getArg (tagFlipFunction n) i
                                a `isSuperSetOf` value v
                | a == funcFetch = do
                    p' <- newVal p
                    dynamicRule p' $ \v -> flip mapM_ (Set.toList (getHeaps' ("funcFetch" ++ show cc) v)) $ \u -> do
                        case Map.lookup u heapMap of
                            Just (x,_) -> self `isSuperSetOf` x
                            Nothing -> do
                                z <- Map.lookup u cheaps
                                self `isSuperSetOf` value z
            pp cc@(Complex a [v,x]) | a == funcApply = do
                v' <- newVal v
                x' <- newVal x
                modifiedSuperSetOf self v' $ \v -> let
                    ns = Set.fromList $ concatMap incp (Set.toList (getNodes v))
                    as = Map.fromList $  [ ((nn,i),v) | ((n,i),v) <- Map.toList (getNodeArgs v), nn <- incp n ]
                   in VsNodes as ns

                dynamicRule v' $ \v -> do
                    flip mapM_ (concat [  fmap ((,) n) (incp n)  | n <- (allNodes v) ]) $ \(on,n) -> do
                        (ts,_) <- findArgsType (grinTypeEnv grin) n
                        --let mm = Map.fromList $ concat [ Map.lookup (on,i) (getNodeArgs v) >>= return . ((,) (n,i)) |  i <- [0 .. length ts ]]
                        --self `isSuperSetOf` value (pruneNodes $ VsNodes mm mempty)
                        modifiedSuperSetOf self x' $ \x ->
                                pruneNodes $ VsNodes (Map.singleton (n,length ts - 1) x) Set.empty
                        return ()
                    flip mapM_ (Set.toList (getNodes v)) $ \n -> do
                         case tagUnfunction n of
                            Just (1,fn) -> self `isSuperSetOf` (fst $ runIdentity $ Map.lookup fn funcMap)
                            _ -> return ()
                    --sequence_ $ concat [  papp'' n i a | ((n,i),a) <- Map.toList (getNodeArgs v) ]
            pp (Down p a i) = do
                p' <- newVal p
                modifiedSuperSetOf self p' $ \p -> case Map.lookup (a,i) (getNodeArgs p) of
                    Just v -> v
                    Nothing -> mempty
            pp arg@(Arg a i) = do
                x <- getArg a i
                self `isSuperSetOf` x
            pp (Con n as) = do
                as'' <- mapM newVal as
                self `isSuperSetOf` value (VsNodes mempty (Set.singleton n))
                flip mapM_ (zip [(0 :: Int) ..] as'') $ \ (i,a) -> do
                    modifiedSuperSetOf self a $ \a' -> pruneNodes $ VsNodes (Map.singleton (n,i) a') (Set.singleton n)
            pp e = fail $ "pp: " ++ show e
            incp t | Just (n,fn) <- tagUnfunction t, n > 1 = return (partialTag fn (n - 1))
            incp _ = fail "not incp"
            allNodes x = snub $ (Set.toList $ getNodes x) ++ (fsts $ Map.keys (getNodeArgs x))
        procUpdate p1 p2 = do
            p1' <- newVal p1
            p2' <- newVal p2
            dynamicRule p1' $ \p1 -> flip mapM_ (Set.toList (getHeaps' "update" p1)) $ \h ->
                case Map.lookup h heapMap of
                    Just (e,_) -> e `isSuperSetOf` p2'
                    Nothing -> return ()
        procApply xp1 xp2 = do
            p1' <- newVal xp1
            p2' <- newVal xp2
            dynamicRule p1' $ \p1 -> do
                argMap <- readIORef argMap
                flip mapM_ (Map.toList (getNodeArgs p1)) $ \ ((a,i),v) -> do
                    case tagUnfunction a of
                        Just (1,fn) -> do
                            case Map.lookup (fn,i) argMap of
                                Just arg -> do
                                    arg `isSuperSetOf` value v
                                _  -> return ()
                        _ -> return ()

                flip mapM_ (Set.toList (getNodes p1)) $ \ a -> do
                    case tagUnfunction a of
                        Just (1,fn) -> do
                            case Map.lookup (fn,length (fst $ runIdentity $  findArgsType (grinTypeEnv grin) fn) - 1) argMap of
                                Just arg -> arg `isSuperSetOf` p2'
                                _ -> return ()
                        _ -> return ()
        procApp a [p] | a == funcEval = do
            p' <- newVal p
            dynamicRule p' $ \p -> flip mapM_ (Set.toList (getHeaps p)) $ \h -> do
                case Map.lookup h heapMap of
                    Just (e',(x,_)) | True || x /= UnsharedEval -> dynamicRule e' $ \e -> do
                        flip mapM_ (fsts [ runIdentity $ Map.lookup (tagFlipFunction n) funcMap | n <- (Set.toList $ getNodes e), tagIsSuspFunction n ]) $ \z -> do
                            e' `isSuperSetOf` z
                    _ -> return ()

        procApp a ps = do
            unless (tagIsFunction a) $ fail "procApp: not function"
            argMap <- readIORef argMap
            flip mapM_ (zip [0..] ps) $ \ (i,p) -> do
                case Map.lookup (a,i) argMap of
                    Just v -> procPos v p
                    Nothing -> return ()

        simplePos p | Just x <- constPos p = return $ value x
        simplePos var@(Variable v) = case Map.lookup v varMap of
            Just (x,_) -> return x
            Nothing -> error $ "varMap has no var:" ++ show var
        simplePos (Func v) = case Map.lookup v funcMap of
            Just (x,_) -> return x
            Nothing -> error "funcMap has no var"
        simplePos _ = fail "this pos is not simple"
        getArg a i = do
            when (not $ tagIsFunction a) $ fail "getArg: tag not function"
            am <- readIORef argMap
            case Map.lookup (a,i) am of
                Just e -> return e
                Nothing -> do
                    x <- newValue fr mempty
                    modifyIORef argMap (Map.insert (a,i) x)
                    return x
        newVal p | Just v <- simplePos p = return v
        newVal p = do
            v <- newValue fr mempty
            procPos v p
            return v

    flip mapM_ (Map.elems varMap) $ \ (e,p) -> procPos e p
    flip mapM_ (Map.elems funcMap) $ \ (e,p) -> procPos e p
    flip mapM_ (Map.elems heapMap) $ \ (e,(_,p)) -> procPos e p
    mapM_ (uncurry procUpdate) (updateEq eq)
    mapM_ (uncurry procApply) (applyEq eq)
    mapM_ (uncurry procApp) (appEq eq)

    CharIO.putStrLn "About to solve fixpoint.."
    findFixpoint fr

    let readMap m = fmap Map.fromList $ flip mapM (Map.toList m) $ \ (v,(e,_)) -> do
                x <- readValue e
                return (v,x)
    ptVars <- readMap varMap
    ptFunc <- readMap funcMap
    ptHeap <- readMap heapMap

    wdump FD.Eval $ do
        CharIO.putStrLn "argMap"
        argMap <- readIORef argMap
        mapM_  (\ (ai,x) -> readValue x >>= \x' -> CharIO.print (ai,x')) (Map.toList argMap)

    return PointsTo {
        ptVars = ptVars,
        ptFunc = ptFunc,
        ptHeap = ptHeap `Map.union`  cheaps,
        ptHeapType = Map.fromList [ (h,t) | (h,(t,_)) <- heapEq eq ]
        }

