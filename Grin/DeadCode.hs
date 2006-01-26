module Grin.DeadCode(deadCode) where


import Grin.Grin
import qualified Stats
import Atom
import Monad
import FreeVars
import Fixer.Fixer
import Data.Monoid
import GenUtil
import Util.Gen
import Grin.Whiz
import Fixer.Supply


implies :: Value Bool -> Value Bool -> Rule
implies x y = y `isSuperSetOf` x

deadCode ::
    Stats.Stats   -- ^ stats to update with what was done
    -> [Atom]  -- ^ roots
    -> Grin    -- ^ input
    -> IO Grin -- ^ output
deadCode stats roots grin = do
    fixer <- newFixer
    usedFuncs <- newSupply fixer
    usedArgs <- newSupply fixer
    usedCafs <- newSupply fixer
    -- set all roots as used
    flip mapM_ roots $ \r -> do
        addRule $ value True `implies` sValue usedFuncs r
    let postInline = phaseEvalInlined (grinPhase grin)

    -- using a CAF implies using its function if pre-inlining
    unless postInline $ flip mapM_ (grinCafs grin) $ \ (var,~(NodeC a [])) -> do
        x <- supplyValue usedCafs var
        f <- supplyValue usedFuncs (tagFlipFunction a)
        addRule $ x `implies` f

    mapM_ (go fixer usedFuncs usedArgs usedCafs postInline) (grinFunctions grin)
    calcFixpoint "Dead Code" fixer
    supplyReadValues usedArgs >>= mapM_ print
    supplyReadValues usedFuncs >>= mapM_ print
    supplyReadValues usedCafs >>= mapM_ print
    return grin

combineArgs fn as = [ ((fn,n),a) | (n,a) <- zip [0..] as]

go fixer usedFuncs usedArgs usedCafs postInline (fn,~(Tup as) :-> body) = ans where
    ans = do
        usedVars <- newSupply fixer

        flip mapM_ (combineArgs fn as) $ \ (ap,Var v _) -> do
            x <- supplyValue usedArgs ap
            v <- supplyValue usedVars v
            addRule $ v `implies` x
        -- a lot of things are predicated on this so that CAFS are not held on to unnecesarily
        fn' <- supplyValue usedFuncs fn
        let useVar v | v < v0 = addRule $ fn' `implies` sValue usedCafs v
            useVar v = addRule $ fn' `implies` sValue usedVars v
            varValue v | v < v0 = sValue usedCafs v
                       | otherwise = sValue usedVars v
            f e = g e >> return e
            g (App a [e] _) | a == funcEval =  mapM_ useVar (freeVars e)
            g (App a [x,y] _) | a == funcApply =  mapM_ useVar (freeVars (x,y))
            g (Case e _) =  mapM_ useVar (freeVars e)
            g Prim { expArgs = as } = mapM_ useVar (freeVars as)
            g (App a vs _) = do
                addRule $ conditionalRule id fn' $ mconcat [ mconcatMap (implies (sValue usedArgs fn) . varValue) (freeVars a) | (fn,a) <- combineArgs a vs]
                addRule $ fn' `implies` sValue usedFuncs a
            g (Update ~(Var v _) n@(NodeC x vs))
                | v < v0 = do
                    v' <- supplyValue usedCafs v
                    addRule $ conditionalRule id v' $ doNode n
                | otherwise = addRule $ doNode n
            doNode = undefined
            h (p,e) = g e >> return (Just (p,e))


{-
            g (Store (NodeC x vs)) | Just a <- tagToFunction x = tell (Seq.fromList $ concat [ [ (x,Passed [(a,i)]) | x <- freeVars v] | v <- vs | i <- [0..] ])
            g (Return (NodeC x vs)) | Just a <- tagToFunction x = tell (Seq.fromList $ concat [ [ (x,Passed [(a,i)]) | x <- freeVars v] | v <- vs | i <- [0..] ])
            g (Update _ (NodeC x vs)) | Just a <- tagToFunction x = tell (Seq.fromList $ concat [ [ (x,Passed [(a,i)]) | x <- freeVars v] | v <- vs | i <- [0..] ])
            g (Case e _) = tell (Seq.fromList [ (v,Used) | v <- freeVars e ])
            g e = tell (Seq.fromList [ (v,Used) | v <- freeVars e ])
            g _ = return ()
            -}

        (nl,_) <- whiz (\_ -> id) h f whizState (Tup as :-> body)
        return nl




