module Grin.PointsToAnalysis(grinInlineEvalApply) where

import Atom
import CharIO
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Writer
import Data.Monoid
import DDataUtil()
import Doc.DocLike
import GenUtil
import Grin.Grin
import Grin.HashConst
import List(sort)
import List(intersperse)
import Maybe
import Monad
import Options
import Data.IORef
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Doc.Chars as U
import qualified FlagDump as FD
import UniqueMonad
import Grin.EvalInline
import Fixer

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


data ValueSet = VsEmpty | VsNodes (Map.Map (Atom,Int) ValueSet) (Set.Set Atom)  | VsHeaps !(Set.Set Int) | VsBas
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

vsBas = VsBas
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
    mappend VsBas VsBas = VsBas
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
    minus VsBas VsBas = VsEmpty
    minus (VsHeaps h1) (VsHeaps h2) = VsHeaps (h1 Set.\\ h2)
    minus (VsNodes n1 w1) (VsNodes n2 w2) = pruneNodes $ VsNodes (Map.fromList $ concat [
            do v' <- Map.lookup (a,i) n2
               let m =  v `minus` v'
               if isBottom m then [] else [((a,i),m)]
        | ((a,i),v) <- Map.toList n1 ] ) (w1 Set.\\ w2)
    minus x y = error $ "minus: " ++ show x <+> show y

instance Show ValueSet where
    showsPrec x VsEmpty = \xs -> '{':'}':xs
    showsPrec x VsBas = \xs -> 'B':'a':'s':xs
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




newHeap ht p@(Con a ps)
    | tagIsSuspFunction a, Identity t <- tagToFunction a = newHeap' ht (mappend p (Func t))
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
    let f (eq,hc) (n,l) | n == funcEval = (eq,hc)
        f (eq,hc) (n,l) | n == funcApply = (eq,hc)
        f (eq,hc) (n,l) = mapFst (mappend eq) $ collect hc (mh eq + 1) n l
        mh PointsToEq { heapEq = xs } = maximum $ 1:fsts xs
        toHEq (NodeC t []) | not (tagIsWHNF t) = return (SharedEval,Union [Con t [], func (fromAtom t) ] )
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
        g (App a [vr@(Var v _)] :>>= vb :-> Return vb' :>>= node@(NodeC {}) :-> e)
            | vb == vb', a == funcEval = (Return vr :>>= createEval (HoistedUpdate node) typeEnv (tagsp v)) :>>= vb :-> Return vb' :>>= node :-> g e
        g (e1 :>>= l) = g e1 :>>= f l
        g (App a [vr@(Var v _)])
            | a == funcEval = Return vr :>>= createEval TrailingUpdate typeEnv (tagsp v)
        g app@(App a [vr@(Var v _),y])
            | a == funcApply = case (tags v) of
                Just ts ->  Return (Tup [vr,y]) :>>= createApply typeEnv ts
                Nothing -> error $ "InlineEvalApply: " ++ show app
        g n@(App a _)
            | a == funcApply || a == funcEval = error $ "Invalid evap: " ++ show n
        g (Store vr@(Var v _)) | Just ts <- tags v = Return vr :>>= createStore typeEnv ts
        g st@(Store (Var {})) = Error ("Store of basic: " ++ show st) (TyPtr TyNode)
        g (Case v@(Var vr _) xs) = docase v (map f xs) (tags vr)
        g (Case v xs) = Case v (map f xs)
        g x = x
        tags v = if x == vsBas then Nothing else Just [ t | t <- Set.toList vs] where
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

collect :: HcHash -> Int -> Atom -> Lam -> (PointsToEq,HcHash)
collect hc st fname (Tup vs :-> exp')
    | sameLength avs vs = (eq { funcEq = (fname,v):funcEq eq, varEq = varEq eq ++ avs },hc')   where
    avs = [ (v,Arg fname n) |  Var v _ <- vs | n <- [0..] ]
    --((v,eq),hc') = execUniq st $ (runStateT ((runWriterT (f exp'))) hc)
    ((v,hc'),eq) = execUniq st $ (runWriterT (runStateT (f exp') hc))
    --((v,hc'),eq) = runWriter $ execUniqT st $ (runStateT  (f exp') hc)
    --tell x = lift $ Control.Monad.Writer.tell x
    f (exp :>>= v :-> exp2) = do
        p <- g exp
        bind v p
        f exp2
    f exp = g exp

    g (App fe [v]) | fe == funcEval = do
        x <- toPos v
        --tell mempty { appEq = [(funcEval,[x])] }
        return $ Complex funcEval [Complex funcFetch [x]]
    g (App fe [v,x]) | fe == funcApply = do
        v <- toPos v
        x <- toPos x
        tell mempty { applyEq = [(v,x)] }
        return $ Complex funcApply [v,x]
        --return $ Complex funcEval (Complex funcApply x)

    g (App a vs ) | a `notElem` [funcEval,funcApply]  = do
        vs' <- mapM toPos vs
        tell mempty { appEq = [(a,vs')] }
        return $ Func a
    g Return { expValue = n@(NodeC _ (_:_)) } = do
        p@(Con a ts) <- toPos n
        case fromAtom a of
            'F':rs -> tell mempty { appEq = [(toAtom ('f':rs),ts)] }
            'B':rs -> tell mempty { appEq = [(toAtom ('b':rs),ts)] }
            _ -> return ()
        return p
    g (Return { expValue = val }) = toPos val
    g Store { expValue = NodeC t _ } | t == tagHole = do
        newHeap RecursiveThunk mempty
    g Store { expValue = n@(NodeC _ (_:_)) } = do
        p@(Con a ts) <- toPos n
        case fromAtom a of
            'F':rs -> tell mempty { appEq = [(toAtom ('f':rs),ts)] }
            'B':rs -> tell mempty { appEq = [(toAtom ('b':rs),ts)] }
            _ -> return ()
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
collect _ _ _ _ = error "collect: bad argument"

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
constPos (Tuple []) = return vsBas
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
        z (Left _) = vsBas

    let procPos self p = pp p where
            pp p | Just c <- constPos p = self `isSuperSetOf` value c
            pp p | Just e <- simplePos p = self `isSuperSetOf` e
            pp (Union ps) = mapM_ pp ps
            pp (Tuple ts) = pp (Con tupleName ts)
            pp (DownTup p n) = pp (Down p tupleName n)
            pp (PIf True p a t) = do
                p' <- newVal p
                t' <- newVal t
                --conditionalRule (Set.member a . getNodes) p' $ do self `isSuperSetOf` t' -- TODO
                self `isSuperSetOf` t'
            pp (PCase p vs e) = do
                p' <- newVal p
                e' <- newVal e
                flip mapM_ vs $ \ (a,w) -> do
                    w' <- newVal w
                    --conditionalRule (Set.member a . getNodes) p' $ do self `isSuperSetOf` w'  -- TODO
                    self `isSuperSetOf` w'
                self `isSuperSetOf` e' -- TODO make this better
                -- conditionalRule (\x -> not $ or [ Set.member a (getNodes x) | (a,_) <- vs]) p' $ do self `isSuperSetOf` e'  -- TODO, should only fire once
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
                    as = Map.fromList $ concat [
                            do nn <- incp n
                               return ((nn,i),v)
                        | ((n,i),v) <- Map.toList (getNodeArgs v)]
                   in VsNodes as ns

                dynamicRule v' $ \v -> do
                    flip mapM_ (concat [  fmap ((,) n) (incp n)  | n <- (allNodes v) ]) $ \(on,n) -> do
                        (ts,_) <- findArgsType (grinTypeEnv grin) n
                        let mm = Map.fromList $ concat [ Map.lookup (on,i) (getNodeArgs v) >>= return . ((,) (n,i)) |  i <- [0 .. length ts ]]
                        self `isSuperSetOf` value (pruneNodes $ VsNodes mm mempty)
                        modifiedSuperSetOf self x' $ \x ->
                                pruneNodes $ VsNodes (Map.singleton (n,length ts - 1) x) Set.empty
                    sequence_ $ concat [  papp'' n i a | ((n,i),a) <- Map.toList (getNodeArgs v) ]
                    sequence_ $ concat [  papp' n x'  | n <- Set.toList (getNodes v) ]
            pp (Down p a i) = do
                p' <- newVal p
                modifiedSuperSetOf self p' $ \p -> case Map.lookup (a,i) (getNodeArgs p) of
                    Just v -> v
                    Nothing -> mempty
            pp arg@(Arg a i) = do
                x <- getArg a i
                self `isSuperSetOf` x
            pp (Con n as) = do
                as'' <- mapM newVal as ;
                self `isSuperSetOf` value (VsNodes mempty (Set.singleton n))
                flip mapM_ (zip [(0 :: Int) ..] as'') $ \ (i,a) -> do
                    modifiedSuperSetOf self a $ \a' -> pruneNodes $ VsNodes (Map.singleton (n,i) a') (Set.singleton n)
            pp e = fail $ "pp: " ++ show e
            papp'' t i a
                | Just (1,fn) <- tagUnfunction t = return $ do
                    av <- getArg fn i
                    av `isSuperSetOf` value a
                | otherwise = fail "not papp''"
            papp' t x'
                | Just (1,fn) <- tagUnfunction t = return $ do
                    self `isSuperSetOf` (fst $ runIdentity $ Map.lookup fn funcMap) -- cp (Func (toAtom $ 'f':xs))
                    (ts,_) <- findArgsType (grinTypeEnv grin) fn
                    av <- getArg fn (length ts - 1)
                    av `isSuperSetOf` x'
                | otherwise = fail "not papp'"
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
        procApply p1 p2 = do
            p1' <- newVal p1
            p2' <- newVal p2
            dynamicRule p1' $ \p1 -> do
                argMap <- readIORef argMap
                flip mapM_ (Map.toList (getNodeArgs p1)) $ \ ((a,i),v) -> do
                    case tagUnfunction a of
                        Just (1,fn) -> do
                            case Map.lookup (fn,i) argMap of
                                Nothing -> return ()
                                Just arg -> arg `isSuperSetOf` value v
                        _ -> return ()

                flip mapM_ (Set.toList (getNodes p1)) $ \ a -> do
                    case tagUnfunction a of
                        Just (1,fn) -> do
                            case Map.lookup (fn,length (fst $ runIdentity $  findArgsType (grinTypeEnv grin) fn) - 1) argMap of
                                Just arg -> arg `isSuperSetOf` p2'
                                Nothing -> return ()
                        _ -> return ()
        procApp a ps = do
            unless (tagIsFunction a) $ fail "procApp: not function"
            argMap <- readIORef argMap
            flip mapM_ (zip [0..] ps) $ \ (i,p) -> do
                case Map.lookup (a,i) argMap of
                    Just v -> procPos v p
                    Nothing -> return ()

        simplePos p | Just x <- constPos p = return $ value x
        simplePos (Variable v) = liftM fst $ Map.lookup v varMap
        simplePos (Func v) = liftM fst $  Map.lookup v funcMap
        simplePos _ = fail "this pos is not simple"
        getArg a i = do
            CharIO.print ("getArg", a, i)
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
            CharIO.print ("Creating newval", p)
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

    return PointsTo {
        ptVars = ptVars,
        ptFunc = ptFunc,
        ptHeap = ptHeap `Map.union`  cheaps,
        ptHeapType = Map.fromList [ (h,t) | (h,(t,_)) <- heapEq eq ]
        }

{-
    fs = vars ++ heaps ++ funcs
    fs' = fsts fs
    vars = [ (Lv x,cp' y) | (x,y) <- varEq eq ]
    heaps = [ (Lh x,\env -> cp' y env >>= \z -> getUpdates env x >>= return . mappend z ) | (x,(_,y)) <- heapEq eq ] ++ cheaps
    cheaps = [ (Lh (-x),\_ -> return $ setNodes  [(t,(map z xs))]) | (HcNode t xs,x) <- Map.toList mp ] where
        z (Right n) = setHeaps [(-n)]
        z (Left _) = vsBas
    funcs = [ (Lf x,cp' y) | (x,y) <- funcEq eq ]
    fmp = Map.fromListWith (zipWith mappend) $ appEq eq
    --valMap = Map.fromList (zip fs' [(0::Int)..])
    varsMap = Map.fromList  [ (v,i) | (Lv v,_) <- vars | i <- [0..] ]
    heapsMap = Map.fromList [ (v,i) | (Lh v,_) <- heaps | i <- [length vars ..]]
    funcsMap = Map.fromList [ (v,i) | (Lf v,_) <- funcs | i <- [length vars + length heaps ..]]
    getUpdates env p = do
        let e (v,x) = do
                ns <- cp' v env
                case Set.member p (getHeaps ns) of
                    True -> cp' x env
                    False -> return mempty
        ep <-  mapM e (updateEq eq)
        return $ mconcat ep
    cp' v env = cp v where
        --getVal h = getVal' env (Map.find h valMap)
        cp (Union ps) = fmap mconcat (mapM cp ps)
        cp (Variable v) = getVal' env x where
            --Just x =  (Map.lookup v varsMap)
            x = case (Map.lookup v varsMap) of
                Just x -> x
                Nothing -> error $ "Can't find var: " ++ show v
        cp (Func a) = getVal' env x where
            x = case (Map.lookup a funcsMap) of
                Just x -> x
                Nothing -> error $ "Can't find func: " ++ show a
        cp Basic = return $ vsBas
        cp (Ptr a) = return $ setHeaps [a]
        cp (PIf True p tg v) = do
            w <- cp p
            case Map.lookup tg (getNodes w) of
                Nothing -> return mempty
                Just _ -> cp v
        cp (PIf False p tg v) = do
            w <- cp p
            case Map.lookup tg (getNodes w) of
                Just _ -> return mempty
                Nothing -> cp v
        cp (PCase p xs e) = do
            w <- cp p
            let mp = Map.fromList xs
            xs <- sequence [ maybe (cp e) cp (Map.lookup t mp) |  t <-  Map.keys $ getNodes w]
            return $ mconcat xs
        cp x@(Down p a i) = do
            vs <-  cp p
            when (vs == VsBas) $ fail ("VsBas: " ++ show x)
            case Map.lookup a (getNodes vs) of
                Just as -> return (as !! i)
                Nothing -> return mempty
                --[ as !! i |  (a',as) <- vs, a' == a ]
        cp (DownTup p i) = cp (Down p (toAtom "") i)
        cp z@(Arg a i) = do
            da <- case Map.lookup a fmp of
                Just ps -> cp (ps !! i) >>= return . (:[])
                Nothing -> return []
            ep <- return []
            {-
            ea <- case Map.lookup funcEval fmp of
                Just [p] -> do
                    ns <-   cp p
                    hs <- mapM getVal [ Lh h |  h <- Set.toAscList $ getHeaps ns ]
                    let pts = [ as !! i |  Just as <-  (map (Map.lookup (partialTag a 0) . getNodes) hs)]
                    return $  pts
                Nothing -> return []
            -}
            let  e (v,x) = do
                    let pt = partialTag a 1
                    ns <-  cp v
                    when (ns == VsBas) $ fail ("VsBas: " ++ show z)
                    case Map.lookup pt (getNodes ns) of
                        Just as | length as == i -> cp x >>= return . (:[])
                        Just as -> return [as !! i]
                        Nothing -> return []
            ep <- if 'f' == head (fromAtom a) then mapM e (applyEq eq) else return []
            return (mconcat $ da  ++ concat ep)
        cp (Con a ps) = do
            ps' <- mapM cp ps
            return $ setNodes [(a,ps')]
        cp (Tuple []) = return VsBas
        cp (Tuple ps) = cp (Con (toAtom "") ps)
        cp (Complex a [p])
            | a == funcFetch = do
                hs <- cp p
                vs <- mapM (getVal' env) [ maybe (error "Can't find heap") id $ Map.lookup n heapsMap | n <- Set.toAscList $ getHeaps hs]
                return $ mconcat vs
            | a == funcEval = do
                vs <-  cp p
                return $ VsNodes (Map.filterWithKey (\t _ -> tagIsWHNF t) (getNodes vs))
        cp (Complex a [v,x])
            | a == funcApply = do
                vs <-  cp v
                xs <- case  [ papp (fromAtom t) as  | (t,as) <- Map.toList (getNodes vs), tagIsPartialAp t ]  of
                    [] -> return []
                    xs -> do
                        x' <- cp x
                        mapM ($ x') xs
                return $ mconcat xs


        papp ('P':'1':'_':xs) _ _ = getVal' env (runIdentity $ Map.lookup (toAtom $ 'f':xs) funcsMap) -- cp (Func (toAtom $ 'f':xs))
        papp ('P':cs) as x | (n','_':rs) <- span isDigit cs, n <- read n', n > 1 =  return $ setNodes [((toAtom $ 'P':(show $ n -  (1::Int)) ++ "_" ++ rs),(as ++ [x]))]

findFixpoint' :: HcHash -> PointsToEq -> IO PointsTo
findFixpoint' (HcHash _ mp) eq = fmap cpt (solve' mempty (snds fs)) where
    cpt xs = PointsTo {
        ptVars = Map.fromList [ (v,x) | (Lv v,x) <- zip fs' xs ],
        ptFunc = Map.fromList [ (v,x) | (Lf v,x) <- zip fs' xs ],
        ptHeap = Map.fromList [ (v,x) | (Lh v,x) <- zip fs' xs ],
        ptHeapType = Map.fromList [ (h,t) | (h,(t,_)) <- heapEq eq ]
        }
    fs = vars ++ heaps ++ funcs
    fs' = fsts fs
    vars = [ (Lv x,cp' y) | (x,y) <- varEq eq ]
    heaps = [ (Lh x,\env -> cp' y env >>= \z -> getUpdates env x >>= return . mappend z ) | (x,(_,y)) <- heapEq eq ] ++ cheaps
    cheaps = [ (Lh (-x),\_ -> return $ setNodes  [(t,(map z xs))]) | (HcNode t xs,x) <- Map.toList mp ] where
        z (Right n) = setHeaps [(-n)]
        z (Left _) = vsBas
    funcs = [ (Lf x,cp' y) | (x,y) <- funcEq eq ]
    fmp = Map.fromListWith (zipWith mappend) $ appEq eq
    --valMap = Map.fromList (zip fs' [(0::Int)..])
    varsMap = Map.fromList  [ (v,i) | (Lv v,_) <- vars | i <- [0..] ]
    heapsMap = Map.fromList [ (v,i) | (Lh v,_) <- heaps | i <- [length vars ..]]
    funcsMap = Map.fromList [ (v,i) | (Lf v,_) <- funcs | i <- [length vars + length heaps ..]]
    getUpdates env p = do
        let e (v,x) = do
                ns <- cp' v env
                case Set.member p (getHeaps ns) of
                    True -> cp' x env
                    False -> return mempty
        ep <-  mapM e (updateEq eq)
        return $ mconcat ep
    cp' v env = cp v where
        --getVal h = getVal' env (Map.find h valMap)
        cp (Union ps) = fmap mconcat (mapM cp ps)
        cp (Variable v) = getVal' env x where
            --Just x =  (Map.lookup v varsMap)
            x = case (Map.lookup v varsMap) of
                Just x -> x
                Nothing -> error $ "Can't find var: " ++ show v
        cp (Func a) = getVal' env x where
            x = case (Map.lookup a funcsMap) of
                Just x -> x
                Nothing -> error $ "Can't find func: " ++ show a
        cp Basic = return $ vsBas
        cp (Ptr a) = return $ setHeaps [a]
        cp (PIf True p tg v) = do
            w <- cp p
            case Map.lookup tg (getNodes w) of
                Nothing -> return mempty
                Just _ -> cp v
        cp (PIf False p tg v) = do
            w <- cp p
            case Map.lookup tg (getNodes w) of
                Just _ -> return mempty
                Nothing -> cp v
        cp (PCase p xs e) = do
            w <- cp p
            let mp = Map.fromList xs
            xs <- sequence [ maybe (cp e) cp (Map.lookup t mp) |  t <-  Map.keys $ getNodes w]
            return $ mconcat xs
        cp x@(Down p a i) = do
            vs <-  cp p
            when (vs == VsBas) $ fail ("VsBas: " ++ show x)
            case Map.lookup a (getNodes vs) of
                Just as -> return (as !! i)
                Nothing -> return mempty
                --[ as !! i |  (a',as) <- vs, a' == a ]
        cp (DownTup p i) = cp (Down p (toAtom "") i)
        cp z@(Arg a i) = do
            da <- case Map.lookup a fmp of
                Just ps -> cp (ps !! i) >>= return . (:[])
                Nothing -> return []
            ep <- return []
            {-
            ea <- case Map.lookup funcEval fmp of
                Just [p] -> do
                    ns <-   cp p
                    hs <- mapM getVal [ Lh h |  h <- Set.toAscList $ getHeaps ns ]
                    let pts = [ as !! i |  Just as <-  (map (Map.lookup (partialTag a 0) . getNodes) hs)]
                    return $  pts
                Nothing -> return []
            -}
            let  e (v,x) = do
                    let pt = partialTag a 1
                    ns <-  cp v
                    when (ns == VsBas) $ fail ("VsBas: " ++ show z)
                    case Map.lookup pt (getNodes ns) of
                        Just as | length as == i -> cp x >>= return . (:[])
                        Just as -> return [as !! i]
                        Nothing -> return []
            ep <- if 'f' == head (fromAtom a) then mapM e (applyEq eq) else return []
            return (mconcat $ da  ++ concat ep)
        cp (Con a ps) = do
            ps' <- mapM cp ps
            return $ setNodes [(a,ps')]
        cp (Tuple []) = return VsBas
        cp (Tuple ps) = cp (Con (toAtom "") ps)
        cp (Complex a [p])
            | a == funcFetch = do
                hs <- cp p
                vs <- mapM (getVal' env) [ maybe (error "Can't find heap") id $ Map.lookup n heapsMap | n <- Set.toAscList $ getHeaps hs]
                return $ mconcat vs
            | a == funcEval = do
                vs <-  cp p
                return $ VsNodes (Map.filterWithKey (\t _ -> tagIsWHNF t) (getNodes vs))
        cp (Complex a [v,x])
            | a == funcApply = do
                vs <-  cp v
                xs <- case  [ papp (fromAtom t) as  | (t,as) <- Map.toList (getNodes vs), tagIsPartialAp t ]  of
                    [] -> return []
                    xs -> do
                        x' <- cp x
                        mapM ($ x') xs
                return $ mconcat xs


        papp ('P':'1':'_':xs) _ _ = getVal' env (runIdentity $ Map.lookup (toAtom $ 'f':xs) funcsMap) -- cp (Func (toAtom $ 'f':xs))
        papp ('P':cs) as x | (n','_':rs) <- span isDigit cs, n <- read n', n > 1 =  return $ setNodes [((toAtom $ 'P':(show $ n -  (1::Int)) ++ "_" ++ rs),(as ++ [x]))]

findFixpoint'''' :: Grin -> HcHash -> PointsToEq -> IO PointsTo
findFixpoint'''' grin hcHash eq = do
    (xs,ms) <- convertPos grin hcHash eq
    xs <- mapM fixupOUnion xs
    when (dump FD.Eval) $ do
        mapM_ CharIO.print [ (l,xs !! i) | (l,i) <- ms ]
    fr <- newFixer
    zs <- mapM (const (newValue fr bottom)) xs
    let zarr ::  Array Index (Fixer.Value ValueSet)
        zarr = (listArray (0,length zs - 1) zs)
    let ptagMap = Map.fromList [ (partialTag v 1,zarr!x) | (Lf v,x) <- ms, 'f' == head ( fromAtom v) ]
    flip mapM_ (zip (Array.elems zarr) xs) $ \ (self,u) -> do
        let (init,act) = go ptagMap zarr self u
        act
        propegateValue init self
    findFixpoint fr
    rs <- mapM Fixer.readValue zs
    when (dump FD.Eval) $ do
        mapM_ CharIO.print [ (l,rs !! i) | (l,i) <- ms ]
    let mp x = arr!x
        arr =  listArray (0, length rs - 1) rs
    return  PointsTo {
        ptVars = Map.fromList [ (v,mp x) | (Lv v,x) <- ms ],
        ptFunc = Map.fromList [ (v,mp x) | (Lf v,x) <- ms ],
        ptHeap = Map.fromList [ (v,mp x) | (Lh v,x) <- ms ],
        ptHeapType = Map.fromList [ (h,t) | (h,(t,_)) <- heapEq eq ]
        } where
    go ptagMap zarr self u = cu u
        where
        env = undefined
        cu (OUnion v ps) = (v,mapM_ cp ps)
        cp (OVal i) = self `isSuperSetOf` (zarr!i)
        cp _ = undefined
        {-
        cp (OIf i (Left t) x) = do
            v <- cu i
            case t `Set.member` getHeaps v of
                True -> cu x
                False -> return mempty
        cp (OIf i (Right t) x) = do
            v <- cu i
            case t `Map.lookup` getNodes v of
                Just _ -> cu x
                Nothing -> return mempty
        cp (OCase i xs els) = do
            v <- liftM getNodes $ Fixit.getVal env (zarr!i)
            let f (t,x) = case Map.lookup t v of
                    Just _ -> [cu x]
                    Nothing ->  []
            xs <- sequence $  concatMap f xs
            x <- cu els   -- TODO should only do else case if no match
            return (mconcat $ x:xs)
            --case xs of
            --    [] -> cu els
            --    _ -> return $ mconcat xs
        cp exp@(ODown x t i) = do
            nds <- liftM getNodes $ cu x
            case Map.lookup t nds of
                Just ps | i >= length ps -> error $ "ODown i to large: " ++ show exp ++ show ps
                Just as -> return $ as !! i
                Nothing -> return mempty
        cp (ONode a ps) = do
            ps <- mapM cu ps
            return (setNodes [(a,ps)])
        cp (OFetch p) = do
            hp <- liftM getHeaps $ cu p
            vs <- mapM (Fixit.getVal env . (zarr !)) (Set.toList hp)
            return $ mconcat vs
        cp (ORestrictEval p) = do
            vs <- cu p
            return $ VsNodes (Map.filterWithKey (\t _ -> tagIsWHNF t) (getNodes vs))
        cp (OApply v x) = do
            vs <- cu v
            xs <- case  [ papp ( t) as  | (t,as) <- Map.toList (getNodes vs), tagIsPartialAp t ]  of
                [] -> return []
                xs -> do
                    x' <- cu x
                    mapM ($ x') xs
            return $ mconcat xs

        papp t _ _ | Just x <- Map.lookup t ptagMap = Fixit.getVal env x
        papp t  as x | ('P':cs) <- fromAtom t, (n','_':rs) <- span isDigit cs, n <- read n', n > 1 =  return $ setNodes [((toAtom $ 'P':(show $ n -  (1::Int)) ++ "_" ++ rs),(as ++ [x]))]
        -}
-}

{-
    f (exp :>>= Var v _ :-> exp2) = do
        p <- g exp
        tell mempty { varEq = [(v, p)] }
        f exp2
    f (exp :>>= NodeC t vs :-> exp2) = do
        p <- g exp
        tell mempty { varEq = [ (v,Down p t i) | Var v _ <- vs | i <- [1..] ] }
        f exp2
-}
{-
data Value =
    Node Atom [ValueSet]
    | Bas
    | Heap Int
    deriving(Eq,Ord)

newtype ValueSet = ValueSet (Set.Set Value)



instance Eq ValueSet where
    a == b = valueSetToList a == valueSetToList b
    a /= b = valueSetToList a /= valueSetToList b

instance Ord ValueSet where
    compare a b = compare (valueSetToList a) (valueSetToList b)


instance Show Value where
    showsPrec x (Heap n) = showsPrec x n
    showsPrec x Bas = \xs -> ('B':'a':'s':xs)
    showsPrec x (Node a vs)
        | a == toAtom "" = tupled  (map (showsPrec x) vs)
        | otherwise = showsPrec x a . tupled (map (showsPrec x) vs)

instance Show ValueSet where
    showsPrec x vs'
        | length vs > 10 = showsPrec x (take 10 vs) . ("... " ++)
        | otherwise = showsPrec x vs
        where vs = valueSetToList vs'


--valueSetToList (ValueSet vs) = vs
--valueSet vs = ValueSet (snub vs)

valueSetToList (ValueSet vs) = Set.toAscList vs
valueSet vs = ValueSet (Set.fromList vs)



instance Monoid ValueSet where
    mempty = ValueSet Set.empty
    mappend x y = mconcat [x,y]
    mconcat [] = mempty
    mconcat [x] = x
    mconcat xs =  f [] $ Set.toAscList (Set.unions [ vs | ValueSet vs <- xs]) where
        f xs [] = valueSet xs
        f xs (Node a vs:Node b vs':xs') | a == b = f xs (Node a (zipWith mappend vs vs'):xs')
        f xs (y:ys) = f (y:xs) ys

instance Monoid ValueSet where
    mempty = ValueSet []
    mappend x y = mconcat [x,y]
    mconcat xs = ValueSet $ f [] $ snub $ concat [ vs | ValueSet vs <- xs] where
        f xs [] = xs
        f xs (Node a vs:Node b vs':xs') | a == b = f xs (Node a (zipWith mappend vs vs'):xs')
        f xs (y:ys) = f (y:xs) ys

findFixpoint :: HcHash -> PointsToEq -> IO PointsTo
findFixpoint (HcHash _ mp) eq = fmap cpt (solve mempty fs) where
    cpt xs = PointsTo {
        ptVars = Map.fromList [ (v,x) | (Lv v,x) <- xs ],
        ptFunc = Map.fromList [ (v,x) | (Lf v,x) <- xs ],
        ptHeap = Map.fromList [ (v,x) | (Lh v,x) <- xs ],
        ptHeapType = Map.fromList [ (h,t) | (h,(t,_)) <- heapEq eq ]
        }
    fs = vars ++ heaps ++ funcs
    vars = [ (Lv x,cp y) | (x,y) <- varEq eq ]
    heaps = [ (Lh x,cp y) | (x,(_,y)) <- heapEq eq ] ++ cheaps
    cheaps = [ (Lh (-x),return $ valueSet [Node t (map z xs)]) | (HcNode t xs,x) <- Map.toList mp ] where
        z (Right n) = valueSet [Heap (-n)]
        z (Left _) = valueSet [Bas]
    funcs = [ (Lf x,cp y) | (x,y) <- funcEq eq ]
    fmp = Map.fromList $ appEq eq
    cp (Union ps) = fmap mconcat (mapM cp ps)
    cp (Variable v) = getVal (Lv v)
    cp (Func a) = getVal (Lf a)
    cp Basic = return $ valueSet [Bas]
    cp (Ptr a) = return $ valueSet [Heap a]
    cp (Down p a i) = do
        vs <- fmap valueSetToList $ cp p
        return $ mconcat [ as !! i | Node a' as <- vs, a' == a ]
    cp (DownTup p i) = do
        vs <- fmap valueSetToList $ cp p
        return $ mconcat [ as !! i | Node a' as <- vs, a' == toAtom "" ]
    cp (Arg a i) = do
        da <- case Map.lookup a fmp of
            Just ps -> cp (ps !! i) >>= return . (:[])
            Nothing -> return []
        ea <- case Map.lookup funcEval fmp of
            Just [p] -> do
                ns <- fmap valueSetToList $  cp p
                hs <- mapM getVal [ Lh h |  Heap h <- ns ]
                let pts = [ as !! i |  Node t as <- concat (map valueSetToList  hs), t == partialTag a 0]
                return $  pts
            Nothing -> return []
        ep <- case Map.lookup funcApply fmp of
            Just [v,x] -> do
                ns <- fmap valueSetToList $ cp v
                x <- cp x
                let pts = [ (as ++ [x]) !! i |  Node t as <- ns, head (fromAtom a) == 'f',t == partialTag a 1]
                return $ pts
            Nothing -> return []
        return (mconcat $ da ++ ea ++ ep)
    cp (Con a ps) = do
        ps' <- mapM cp ps
        return $ valueSet [Node a ps']
    cp (Tuple ps) = do
        ps' <- mapM cp ps
        return $ valueSet [Node (toAtom "") ps']
    cp (Complex a [p])
        | a == funcFetch = do
            vs <- fmap valueSetToList $  cp p
            vs <- mapM getVal [ Lh n | Heap n <- vs]
            return $ mconcat vs
        | a == funcEval = do
            vs <- fmap valueSetToList $  cp p
            return $ valueSet [ n | n@(Node t _) <- vs, tagIsWHNF t ]
    cp (Complex a [v,x])
        | a == funcApply = do
            vs <- fmap valueSetToList $  cp v
            x' <- cp x
            xs <- sequence [ papp (fromAtom t) as x' | Node t as <- vs, tagIsPartialAp t ]
            return $ mconcat xs

    papp ('P':'1':'_':xs) _ _ = cp (Func (toAtom $ 'f':xs))
    papp ('P':cs) as x | (n','_':rs) <- span isDigit cs = return $ valueSet [Node (toAtom $ 'P':(show $ read n' - (1::Int)) ++ "_" ++ rs) (as ++ [x])]
findFixpoint :: HcHash -> PointsToEq -> IO PointsTo
findFixpoint (HcHash _ mp) eq = fmap cpt (solve mempty fs) where
    cpt xs = PointsTo {
        ptVars = Map.fromList [ (v,x) | (Lv v,x) <- xs ],
        ptFunc = Map.fromList [ (v,x) | (Lf v,x) <- xs ],
        ptHeap = Map.fromList [ (v,x) | (Lh v,x) <- xs ],
        ptHeapType = Map.fromList [ (h,t) | (h,(t,_)) <- heapEq eq ]
        }
    fs = vars ++ heaps ++ funcs
    vars = [ (Lv x,cp y) | (x,y) <- varEq eq ]
    heaps = [ (Lh x,cp y >>= \z -> getUpdates x >>= return . mappend z ) | (x,(_,y)) <- heapEq eq ] ++ cheaps
    cheaps = [ (Lh (-x),return $ setNodes  [(t,(map z xs))]) | (HcNode t xs,x) <- Map.toList mp ] where
        z (Right n) = setHeaps [(-n)]
        z (Left _) = vsBas
    funcs = [ (Lf x,cp y) | (x,y) <- funcEq eq ]
    fmp = Map.fromListWith (zipWith mappend) $ appEq eq
    getUpdates p = do
        let e (v,x) = do
                ns <- cp v
                case Set.member p (getHeaps ns) of
                    True -> cp x
                    False -> return mempty
        ep <-  mapM e (updateEq eq)
        return $ mconcat ep
    cp (Union ps) = fmap mconcat (mapM cp ps)
    cp (Variable v) = getVal (Lv v)
    cp (Func a) = getVal (Lf a)
    cp Basic = return $ vsBas
    cp (Ptr a) = return $ setHeaps [a]
    cp x@(Down p a i) = do
        vs <-  cp p
        when (vs == VsBas) $ fail ("VsBas: " ++ show x)
        case Map.lookup a (getNodes vs) of
            Just as -> return (as !! i)
            Nothing -> return mempty
            --[ as !! i |  (a',as) <- vs, a' == a ]
    cp (DownTup p i) = cp (Down p (toAtom "") i)
    cp z@(Arg a i) = do
        da <- case Map.lookup a fmp of
            Just ps -> cp (ps !! i) >>= return . (:[])
            Nothing -> return []
        ep <- return []
        {-
        ea <- case Map.lookup funcEval fmp of
            Just [p] -> do
                ns <-   cp p
                hs <- mapM getVal [ Lh h |  h <- Set.toAscList $ getHeaps ns ]
                let pts = [ as !! i |  Just as <-  (map (Map.lookup (partialTag a 0) . getNodes) hs)]
                return $  pts
            Nothing -> return []
        -}
        let  e (v,x) = do
                let pt = partialTag a 1
                ns <-  cp v
                when (ns == VsBas) $ fail ("VsBas: " ++ show z)
                pts <- case Map.lookup pt (getNodes ns) of
                    Just as | length as == i -> cp x >>= return . (:[])
                    Just as -> return [as !! i]
                    Nothing -> return []
                return $ pts
        ep <- if 'f' == head (fromAtom a) then mapM e (applyEq eq) else return []
        return (mconcat $ da  ++ concat ep)
    cp (Con a ps) = do
        ps' <- mapM cp ps
        return $ setNodes [(a,ps')]
    cp (Tuple []) = return VsBas
    cp (Tuple ps) = cp (Con (toAtom "") ps)
    cp (Complex a [p])
        | a == funcFetch = do
            hs <- cp p
            vs <- mapM getVal [ Lh n | n <- Set.toAscList $ getHeaps hs]
            return $ mconcat vs
        | a == funcEval = do
            vs <-  cp p
            return $ VsNodes (Map.filterWithKey (\t _ -> tagIsWHNF t) (getNodes vs))
    cp (Complex a [v,x])
        | a == funcApply = do
            vs <-  cp v
            x' <- cp x
            xs <- sequence [ papp (fromAtom t) as x' | (t,as) <- Map.toList (getNodes vs), tagIsPartialAp t ]
            return $ mconcat xs

    papp ('P':'1':'_':xs) _ _ = getVal (Lf (toAtom $ 'f':xs)) -- cp (Func (toAtom $ 'f':xs))
    papp ('P':cs) as x | (n','_':rs) <- span isDigit cs, n <- read n', n > 1 = return $ setNodes [((toAtom $ 'P':(show $ n -  (1::Int)) ++ "_" ++ rs),(as ++ [x]))]

-}

{-
convertPos :: Grin -> HcHash -> PointsToEq -> IO ([OUnion],[(L,Int)])
convertPos grin hcHash eq = return (xs,ys) where
    ys = [ (fh l,i) | (i,l,_) <- wholeMap ]
    fh (Lh h) = Lh $ convertHeap h
    fh x = x
    xs = snds $  sortUnder fst [ (i,p) | (i,_,p) <- wholeMap ]
    vars = (Lv app_var,apps):[ (Lv x,cp y) | (x,y) <- varEq eq ]
    heaps = [ (Lh x, cp y `mappend` getUpdates ht (convertHeap x) ) | (x,(ht,y)) <- heapEq eq ] ++ cheaps where
        cheaps = [ (Lh (-x),oNode t (map z xs)) | (x,HcNode t xs) <- hcHashGetNodes hcHash ] where
        z (Right n) = oConst $ setHeaps [(convertHeap (-n))]
        z (Left (Var v _)) = oVar $ convertVar v
        z (Left (Lit _ _)) = oConst vsBas
        z (Left (Tag t)) = oConst vsBas
        oNode t [] = oConst (setNodes [(t,[])])
        oNode t xs = oVal (ONode t xs)
    funcs = [ (Lf x,cp y) | (x,y) <- Map.toList $ Map.fromListWith mappend $ funcEq eq ]
    wholeMap = [ (i,x,y) |  (x,y) <- (vars ++ heaps ++ funcs) | i <- [0..] ]
    varsMap = Map.fromList  [ (v,i) | (i,Lv v,_) <- wholeMap  ]
    heapsMap = Map.fromList [ (v,i) | (i,Lh v,_) <- wholeMap ]
    funcsMap = Map.fromList [ (v,i) | (i,Lf v,_) <- wholeMap ]
    convertVar v | Just x <- Map.lookup v varsMap = x
    convertVar v | otherwise = error $ "convertVar: " ++ show v
    convertHeap v | Just x <- Map.lookup v heapsMap = x
    convertFunc v | Just x <- Map.lookup v funcsMap = x
    convertFunc v = error $ "convertFunc: " ++ show v
    funcMap = Map.fromListWith (zipWith mappend) $ appEq eq
    getUpdates RecursiveThunk p =
        let e (x,c) = OIf (cp x) (Left p) (cp c)
        in OUnion mempty (map e (updateEq eq))
    getUpdates _ _ = mempty
    cp (Func a) = oVar (convertFunc a)
    cp (Variable a) = oVar (convertVar a)
    cp (Ptr h) = oConst (setHeaps [convertHeap h])
    cp (Union ps) = mconcat $ map cp ps
    cp Basic = oConst vsBas
    cp (PIf True (x) tg v) = oVal (OIf (cp x) (Right tg) (cp v))
    cp (PCase (Variable x) xs e) = oVal (OCase (convertVar x) [ (t,cp v) | (t,v) <- xs ] (cp e))
    cp (Down x a i) = oVal (ODown (cp x) a i)
    cp (DownTup x i) = oVal (ODown (cp x) (toAtom "") i)
    cp (Con a []) = oConst (setNodes [(a,[])])
    cp (Con a ps) = oVal (ONode a (map cp ps))
    cp (Tuple []) = oConst vsBas
    cp (Tuple ps) = cp (Con (toAtom "") ps)
    cp (Complex a [p])
        | a == funcFetch = oVal (OFetch (cp p))
        | a == funcEval = oVal (ORestrictEval (cp p))
    cp (Complex a [v,x]) | a == funcApply = oVal $ OApply (cp v) (cp x)
    cp exp@(Arg a i) = mconcat (asd:cps) where
        asd = case Map.lookup a funcMap of
            Just ps | i >= length ps -> error $ "Arg i to large: " ++ show exp
            Just ps -> cp (ps !! i)
            Nothing -> mempty
        pt = partialTag a 1
        cps | 'f':_ <- fromAtom a, i < length as - 1 = [oVal (ODown (oVar appVar) pt i)]
            | 'f':_ <- fromAtom a = map f (applyEq eq)
            | otherwise = []
        --f (v,x)
        --    | i == length as - 1 =  oVal (OIf (cp v) (Right pt) (cp x))
        --    | otherwise = oVal (ODown (cp (v)) pt i)
        f (v,x) = oVal (OIf (cp v) (Right pt) (dpt v x))
        dpt _ x | i == length as - 1 = cp x
        dpt v x = oVal (ODown (cp (v)) pt i)
        Identity (as,_) = findArgsType (grinTypeEnv grin) a
    apps = mconcat [ cp v |  (v,_) <- (applyEq eq)]
    appVar = convertVar app_var

type Index = Int

-- Optimized DataFlow equations
data OPos =
    OVal !Index
    | ODown OUnion !Tag !Int
    | OIf OUnion (Either Index Tag) OUnion
    | ONode !Atom [OUnion]
    | OCase !Index [(Tag,OUnion)] OUnion
    | ORestrictEval  OUnion
    | OFetch OUnion
    | OApply OUnion OUnion
        deriving(Eq,Ord,Show)

data OUnion = OUnion ValueSet [OPos]
        deriving(Eq,Ord,Show)
        {-! derive: Monoid !-}

normalizeOUnion (OUnion vs ops) = OUnion vs (snub ops)

fixupOPos (OApply x y) = do
    x <- fixupOUnion x
    y <- fixupOUnion y
    return $ OApply x y
fixupOPos (OFetch x) = do
    x <- fixupOUnion x
    return $ OFetch x
fixupOPos (ORestrictEval x) = do
    x <- fixupOUnion x
    return $ ORestrictEval x
fixupOPos (ODown x a i) = do
    x <- fixupOUnion x
    return $ ODown x a i
fixupOPos (OIf x a y) = do
    x <- fixupOUnion x
    y <- fixupOUnion y
    --a <- evaluate a
    return $ OIf x a y
fixupOPos (ONode a xs) = do
    xs <- mapM fixupOUnion xs
    return $ ONode a xs
fixupOPos (OCase a xs els) = do
    xs <- sequence [ fixupOUnion x >>= return . (,) t | (t,x) <- xs]
    els <- fixupOUnion els
    return $ OCase a xs els
fixupOPos x = return x

fixupOUnion :: OUnion -> IO OUnion
fixupOUnion (OUnion vs xs) = do
    xs <- mapM fixupOPos xs
    --xs <- mapM evaluate (snub xs)
    return $ ((OUnion $ vs) $ xs)


oVal x = OUnion mempty [x]
oVar x = oVal (OVal x)
oConst x = OUnion x []



data L = Lv {-# UNPACK #-} !Var | Lh {-# UNPACK #-} !Int | Lf {-# UNPACK #-} !Atom
    deriving(Ord,Eq)

instance Show L where
    showsPrec n (Lv v) = showsPrec n v
    showsPrec n (Lh v) = showsPrec n v
    showsPrec n (Lf v) = showsPrec n v


-}
