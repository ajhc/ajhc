module Grin.StorageAnalysis(storeAnalyze) where

import Control.Monad
import Control.Monad.Writer
import Data.Maybe

import Support.FreeVars
import Support.CanType
import Grin.Lint
import StringTable.Atom
import Support.Tickle
import Util.UniqueMonad
import Util.UnionSolve
import Grin.Grin
import Grin.Noodle
import Util.Gen
import qualified Data.Map as Map
import qualified Data.Set as Set

data T = S | E
    deriving(Eq,Show)

instance Fixable T where
    join S S = S
    join _ _ = E

    meet E E = E
    meet _ _ = S

    isTop x = E == x
    isBottom x = S == x

    eq = (==)

    lte E S = False
    lte _ _ = True

data Vr
    = Vb !Var         -- ^ inner variable
    | Va !Atom !Int   -- ^ function argument
    | Vr !Var         -- ^ region variable
    deriving(Eq,Ord)

instance Show Vr where
    showsPrec _ (Vb v) = shows v
    showsPrec _ (Va a i) = shows (a,i)
    showsPrec _ (Vr (V n)) = showChar 'r' . shows n


{-# NOINLINE storeAnalyze #-}
storeAnalyze :: Grin -> IO Grin
storeAnalyze grin = do
    dumpGrin "storeAnalyze1" grin
    let (grin',cs) = execUniq1 $ runWriterT (mapGrinFuncsM firstLam grin)
    dumpGrin "storeAnalyze2" grin'
    --(rm,res) <- solve (const $ return ()) cs
    (rm,res) <- solve putStrLn cs
    putStrLn "----------------------------"
    mapM_ (\ (x,y) -> putStrLn $ show x ++ " -> " ++ show y) (Map.toList rm)
    putStrLn "----------------------------"
    mapM_ print (Map.elems res)
    putStrLn "----------------------------"
    let cmap = Map.map (lower . fromJust . flip Map.lookup res) rm
        lower (ResultJust _ j) = j
        lower ResultBounded { resultLB = Nothing } = S
    mapM_ (\ (x,y) -> putStrLn $ show x ++ " -> " ++ show y) (Map.toList cmap)
    return grin'


isHeap TyNode = True
isHeap TyINode = True
--isHeap TyPtr {} = True
isHeap _ = False

firstLam fname lam = g Nothing fname lam where
    g wtd fname (as :-> body) = do
        tell $ mconcat [ Left (Vb v) `equals` Left (Va fname n) | (n,Var v t) <- zip naturals as, isHeap t ]
        let f wtd (BaseOp (StoreNode sh) [n@(NodeC _ vs)]) = do
                vu <- V `liftM` newUniq
                g wtd [[Vr vu]]
                tell $ mconcat [ Left (Vr vu) `islte` Left v | v' <- toVs vs, v <- v'  ]
                return (BaseOp (StoreNode sh) [n,Var vu TyRegion])
            f wtd (e :>>= as :-> body) = do
                e' <- f (Just as) e
                body' <- f wtd body
                return (e' :>>= as :-> body')
            f wtd (Case e as) = Case e `liftM` mapM (tickleM  (f wtd)) as
            f wtd (Return xs) = g wtd (toVs xs) >> return (Return xs)
            f wtd e@(BaseOp Promote xs) = g wtd (toVs xs) >> return e
            f wtd e@(BaseOp Demote xs) = g wtd (toVs xs) >> return e
            f wtd e@(BaseOp Redirect xs) = g Nothing (toVs xs) >> return e
            f wtd e@(App fn vs ty) = do
                tell $ mconcat [ Left (Va fn n) `islte` Left (Vb v) | (n,Var v t) <- zip naturals vs, isHeap t ]
                return e
            f wtd e@(Let { expDefs = defs, expBody = b }) = do
                defs' <- mapM (tickleM (g' wtd)) defs
                b <- f wtd b
                return $ updateLetProps e { expDefs = defs', expBody = b }
            f wtd e = return e

            g Nothing vs = tell $ mconcat [ Right E `islte` Left v | v' <- vs, v <- v' ]
            g (Just as) vs = tell $ mconcat [ Left a `islte` Left v | (a',v') <- zip (toVs as) vs, a <- a', v <- v']

            toVs :: [Val] -> [[Vr]]
            toVs xs = f xs [] where
                f [] rs = reverse rs
                f (x:xs) rs = f xs (Set.toList (Set.map (Vb . fst) $ Set.filter (isHeap . snd) (freeVars x)):rs)
        b <- f wtd body
        return (as :-> b)
    g' wtd (fname,b) = do
        b <- g wtd fname b
        return (fname,b)





