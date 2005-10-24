module Grin.DeadFunctions(deadFunctions) where

import Control.Monad.Writer
import Data.Graph
import Data.Monoid
import List
import Monad
import qualified Data.Map as Map
import qualified Data.Set as Set

import Atom
import CanType
import FindFixpoint
import FreeVars
import GenUtil hiding(replicateM_)
import Grin.Grin
import Grin.Whiz
import Util.Inst()
import Stats
import Util.Seq as Seq

concatMapM f xs = liftM concat $ mapM f xs

data ArgInfo = Used | Unused | Passed [(Atom,Int)]
    deriving(Eq,Ord,Show)

instance Monoid ArgInfo where
    mempty = Unused
    mappend Unused b = b
    mappend b Unused = b
    mappend Used _ = Used
    mappend _ Used = Used
    mappend (Passed xs) (Passed ys) = Passed (snub (xs ++ ys))

data FunctionInfo = FunctionInfo {
    functionName :: Atom,
    functionBody :: Lam,
    functionArity :: Int,
    functionUnusedArgs :: [Int],
    functionArgInfo :: [ArgInfo],
    functionCafsUsed :: [Var],
    functionCalls :: [Atom]
}


instance Show FunctionInfo where
    showsPrec d (FunctionInfo aa ab ac ad bb ae af) = showParen (d >= 10)
              (showString "FunctionInfo" . showChar '{' .
               showString "functionName" . showChar '=' . showsPrec 10 aa
               . showChar ',' . showChar '\n' .
               showString "functionArity" . showChar '=' . showsPrec 10 ac
               . showChar ',' . showChar '\n' .
               showString "functionUnusedArgs" . showChar '=' . showsPrec 10 ad
               . showChar ',' . showChar '\n' .
               showString "functionArgInfo" . showChar '=' . showsPrec 10 bb
               . showChar ',' . showChar '\n' .
               showString "functionCafsUsed" . showChar '=' . showsPrec 10 ae
               . showChar ',' . showChar '\n' .
               showString "functionCalls" . showChar '=' . showsPrec 10 af
               . showChar '}' . showChar '\n')


getFunctionInfo cafs indirect (a,b@(Tup as :-> e)) = FunctionInfo {
    functionName = a,
    functionBody = b,
    functionArity = length as,
    functionUnusedArgs = uuargs,
    functionArgInfo = arginfo,
    functionCafsUsed = cused,
    functionCalls = fc

    }  where
        fc | indirect =  snub $ concatMap tagToFunction (Set.toList ts) ++ ef
           | otherwise = filter tagIsFunction (Set.toList ts)
        (vs,ts) = freeVars e
        ef =  concatMap tagToFunction  (freeVars (nc))
        uuargs = [ i | (Var v _,i) <- zip as [0..], not $ v `Set.member` vs]
        cused = [ v | v@(V n) <- Set.toList vs, n < 0 ]
        arginfo = collectArgInfo b
        nc = [ y | (x,y) <- cafs, x `elem` cused ]


-- | Remove dead functions

deadFunctions ::
    Stats   -- ^ stats to update with what was done
    -> [Atom]  -- ^ roots
    -> Grin    -- ^ input
    -> IO Grin -- ^ output
deadFunctions stats keeps grin = do
    let -- (graph,lv,kv) = graphFromEdges [ (gf, functionName gf, functionCalls gf) |  gf <- map (getFunctionInfo (grinCafs grin) indirect ) $ grinFunctions grin ]
        -- reach = [ x|  (x,_,_) <- map lv $ snub $ concatMap (reachable graph) (map la keeps)]
        -- rs = Set.fromList (map functionName reach)
        -- | Whether to count indirect function calls. (used before eval\/apply inlining)
        indirect = not $ phaseEvalInlined $ grinPhase grin
        -- la a = case kv a  of
        --    Just n -> n
        --    Nothing -> error $ "DeadFunctions, CannotFind: " ++ show a
        fs = map (getFunctionInfo (grinCafs grin) indirect ) $ grinFunctions grin
        -- fs =  [ f | f@(a,_) <- grinFunctions grin, a `Set.member` rs ]
        --cu = Set.fromList $ concatMap functionCafsUsed fs
        --(nc,uuc) = List.partition ((`Set.member` cu) . fst)  (grinCafs grin)
    -- ticks stats (length (grinFunctions grin) - length reach) (toAtom "Optimize.dead.function")
    --ticks stats (length uuc) (toAtom "Optimize.dead.caf")
    fs <- findDeadCode stats  fs

    fs <- mapM (removeDeadArgs stats fs) [ (functionName f, functionBody f) |  f <- fs]
    return $ grin { grinFunctions = fs }



findFixpoint :: (Show a,Ord a,Eq b,Monoid b) => Maybe String -> (x -> a) -> ((a -> Ms b b) -> x -> Ms b b) -> [x] -> IO [(x,b)]
findFixpoint str en fn xs = ans where
    ans = do
        rs <- solve str mempty is
        return $ zip xs rs
    mp = Map.fromList [ (en x,i) | x <- xs | i <- [0..]]
    f a | Just x <- Map.lookup a mp = getVal x
    f a | otherwise = return $ mempty
    f a | otherwise = error $ "findFixpoint: Cannot find " ++ show a
    is = map (fn f) xs




findDeadCode stats fs = ans where
    is = [ ((functionName f,i),functionArgInfo f !! i) | f <- fs, i <- [0 .. functionArity f - 1]  ]
    ans = do
        zs <- findFixpoint Nothing fst c is
        ua <- concatMapM rs zs
        let mp = Map.fromList $ [ (x,snds xs) | (x,xs) <- sortGroupUnderF fst ua]
            z f | Just x <- Map.lookup (functionName f) mp = f { functionUnusedArgs = x }
            z f@(FunctionInfo { functionUnusedArgs = [] }) = f
        return $ map z fs
    st = Set.fromList $ map functionName fs
    c getVal (_,f) = g getVal f
    g getVal Used = return True
    g getVal Unused = return False
    g getVal (Passed xs) | any (not . (`Set.member` st)) (fsts xs) = return True
    g getVal (Passed xs)  = mapM getVal xs >>= return . or
    rs ((x,Passed _), False) = do
        --CharIO.print x
        tick stats $ toAtom "Optimize.dead.rec-arg"
        rs' (x,False)
    rs ((x,_),y) = rs' (x,y)
    rs' (x,True) = return []
    rs' (x,False) = return [x]


pHole = Const (NodeC tagHole [])

removeDeadArgs stats fs (a,l) =  whizExps f l >>= return . (,) a where
    f (App fn as ty) = do
        as <- dff fn as
        return $ App fn as ty
    f (Return (NodeC fn as)) | Just fn' <- tagToFunction fn = do
        as <- dff fn' as
        return $ Return (NodeC fn as)
    f (Store (NodeC fn as)) |  Just fn' <- tagToFunction fn = do
        as <- dff fn' as
        return $ Store (NodeC fn as)
    f (Update p (NodeC fn as)) |  Just fn' <- tagToFunction fn = do
        as <- dff fn' as
        return $ Update p (NodeC fn as)
    f x = return x
    dff fn as = mapM df  (zip as [0..]) where
        xs = lup fn
        deadVal (Lit 0 _) = True
        deadVal x =  x `elem` [pHole,Tag tagHole]
        df (a,i) | not (deadVal a) && i `elem` xs  = do
            tick stats $ toAtom "Optimize.dead.arg"
            case getType a of
                TyPtr TyNode -> return pHole
                TyTag -> return (Tag tagHole)
                ty@(Ty _) -> return (Lit 0 ty)
            --return $ Const (NodeC (toAtom $ "@hole:" ++ show (fn,i)) [])-- pHole
        df (a,_)  = return a
    lup fn = case Map.lookup fn m of
        Just x -> x
        Nothing -> []
    m = Map.fromList [  (functionName x,functionUnusedArgs x) | x <- fs, not $ null (functionUnusedArgs x) ]

groupConcatFst xs = [ (x,mconcat $ snds xs) | (x,xs) <- sortGroupUnderF fst xs]

-- TODO make this see through store and fetches
collectArgInfo :: Lam -> [ArgInfo]
collectArgInfo exp@(Tup as :-> _) = ans where
    ws = Map.fromList $ groupConcatFst $ Seq.toList $ execWriter (whizExps f exp)
    lv x = case Map.lookup x ws of
        Just x -> x
        Nothing -> Unused
    ans = [ lv x |  Var x _ <- as ]
    f e = g e >> return e
    g (App a [e] _) | a == funcEval =  tell (Seq.fromList [ (v,Used) | v <- freeVars e ])
    g (App a [x,y] _) | a == funcApply =  tell (Seq.fromList [ (v,Used) | v <- freeVars (x,y) ])
    g (App a vs _) = tell (Seq.fromList $ concat [ [ (x,Passed [(a,i)]) | x <- freeVars v] | v <- vs | i <- [0..] ])
    g (Store (NodeC x vs)) | Just a <- tagToFunction x = tell (Seq.fromList $ concat [ [ (x,Passed [(a,i)]) | x <- freeVars v] | v <- vs | i <- [0..] ])
    g (Return (NodeC x vs)) | Just a <- tagToFunction x = tell (Seq.fromList $ concat [ [ (x,Passed [(a,i)]) | x <- freeVars v] | v <- vs | i <- [0..] ])
    g (Update _ (NodeC x vs)) | Just a <- tagToFunction x = tell (Seq.fromList $ concat [ [ (x,Passed [(a,i)]) | x <- freeVars v] | v <- vs | i <- [0..] ])
    g (Case e _) = tell (Seq.fromList [ (v,Used) | v <- freeVars e ])
    g e = tell (Seq.fromList [ (v,Used) | v <- freeVars e ])
    g _ = return ()


