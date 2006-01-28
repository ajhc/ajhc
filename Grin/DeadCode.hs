module Grin.DeadCode(deadCode) where


import Data.Monoid
import Monad
import qualified Data.Set as Set

import Atom
import Fixer.Fixer
import Fixer.Supply
import FreeVars
import Grin.Grin
import Grin.Whiz
import Support.CanType
import Stats hiding(print)
import Util.Gen


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
    --supplyReadValues usedFuncs >>= mapM_ print
    ua <- supplyReadValues usedArgs
    uc <- supplyReadValues usedCafs
    let cafSet = fg uc
        argSet = fg ua
        fg xs = Set.fromList [ x | (x,True) <- xs ]
    newCafs <- flip mconcatMapM (grinCafs grin) $ \ (x,y) -> do
        u <- readSValue usedCafs x
        if u then return [(x,y)] else return []
    newFuncs <- flip mconcatMapM (grinFunctions grin) $ \ (x,y) -> do
        u <- readSValue usedFuncs x
        --if not u then tick stats ("Optimize.dead-code.func.{" ++ show x) >> return [] else do
        if not u then tick stats "Optimize.dead-code.func" >> return [] else do
        r <- runStatIO stats $ removeDeadArgs postInline cafSet argSet (x,y)
        return [r]

    return grin { grinCafs = newCafs, grinFunctions = newFuncs }

combineArgs fn as = [ ((fn,n),a) | (n,a) <- zip [0 :: Int ..] as]

go fixer usedFuncs usedArgs usedCafs postInline (fn,~(Tup as) :-> body) = ans where
    ans = do
        usedVars <- newSupply fixer

        flip mapM_ (combineArgs fn as) $ \ (ap,Var v _) -> do
            x <- supplyValue usedArgs ap
            v <- supplyValue usedVars v
            addRule $ v `implies` x
        -- a lot of things are predicated on this so that CAFS are not held on to unnecesarily
        fn' <- supplyValue usedFuncs fn
        let varValue v | v < v0 = sValue usedCafs v
                       | otherwise = sValue usedVars v
            f e = g e >> return e
            g (App a [e] _) | a == funcEval =  addRule (doNode e)
            g (App a [x,y] _) | a == funcApply =  addRule (doNode x `mappend` doNode y)
            g (Case e _) =  addRule (doNode e)
            g Prim { expArgs = as } = addRule (mconcatMap doNode as)
            g (App a vs _) = do
                addRule $ conditionalRule id fn' $ mconcat [ mconcatMap (implies (sValue usedArgs fn) . varValue) (freeVars a) | (fn,a) <- combineArgs a vs]
                addRule $ fn' `implies` sValue usedFuncs a
                addRule (mconcatMap doConst vs)
            g (Update (Var v _) n@(~(NodeC x vs)))
                | v < v0 = do
                    v' <- supplyValue usedCafs v
                    addRule $ conditionalRule id v' $ doNode n
                | otherwise = addRule $ doNode n
            g (Store n) = addRule $ doNode n
            g (Fetch x) = addRule $ doNode x
            g (Cast x _) = addRule $ doNode x
            g Error {} = return ()
            -- TODO - handle function and case return values smartier.
            g (Return n) = addRule $ doNode n
            g x = error $ "deadcode.g: " ++ show x
            h' (p,e) = h (p,e) >> return (Just (p,e))
            h (p,Store v) = addRule $ mconcat $ [ conditionalRule id  (varValue pv) (doNode v) | pv <- freeVars p]
            h (p,Return v) = addRule $ mconcat $ [ conditionalRule id  (varValue pv) (doNode v) | pv <- freeVars p]
            h (p,Fetch v) = addRule $ mconcat $ [ conditionalRule id  (varValue pv) (doNode v) | pv <- freeVars p]
            h (p,Cast v _) = addRule $ mconcat $ [ conditionalRule id  (varValue pv) (doNode v) | pv <- freeVars p]
            --h (p,Fetch v) = addRule $ mconcat $ [ mconcatMap (implies (varValue pv) . varValue) (freeVars v) | pv <- freeVars p]
            --h (p,Cast v _) = addRule $ mconcat $ [ mconcatMap (implies (varValue pv) . varValue) (freeVars v) | pv <- freeVars p]
            h (p,e) = g e
            doNode (NodeC n as) | not postInline, Just (x,fn) <- tagUnfunction n  = mappend (mconcatMap doConst as) $ mconcat (implies fn' (sValue usedFuncs fn):[ mconcatMap (implies (sValue usedArgs fn) . varValue) (freeVars a) | (fn,a) <- combineArgs fn as])
            doNode x = doConst x `mappend` mconcatMap (implies fn' . varValue) (freeVars x)
            doConst _ | postInline  = mempty
            doConst (Const n) = doNode n
            doConst (Tup ns) = mconcatMap doConst ns
            doConst (NodeC n as) = mconcatMap doConst as
            doConst (NodeV n as) = mconcatMap doConst as
            doConst _ = mempty


{-
            g (Store (NodeC x vs)) | Just a <- tagToFunction x = tell (Seq.fromList $ concat [ [ (x,Passed [(a,i)]) | x <- freeVars v] | v <- vs | i <- [0..] ])
            g (Return (NodeC x vs)) | Just a <- tagToFunction x = tell (Seq.fromList $ concat [ [ (x,Passed [(a,i)]) | x <- freeVars v] | v <- vs | i <- [0..] ])
            g (Update _ (NodeC x vs)) | Just a <- tagToFunction x = tell (Seq.fromList $ concat [ [ (x,Passed [(a,i)]) | x <- freeVars v] | v <- vs | i <- [0..] ])
            g (Case e _) = tell (Seq.fromList [ (v,Used) | v <- freeVars e ])
            g e = tell (Seq.fromList [ (v,Used) | v <- freeVars e ])
            g _ = return ()
            -}

        (nl,_) <- whiz (\_ -> id) h' f whizState (Tup as :-> body)
        return nl


removeDeadArgs :: MonadStats m => Bool -> (Set.Set Var) -> (Set.Set (Atom,Int)) -> (Atom,Lam) -> m (Atom,Lam)
removeDeadArgs postInline usedCafs usedArgs (a,l) =  whizExps f l >>= return . (,) a where
    f (App fn as ty) | fn `notElem` [funcApply, funcEval] = do
        as <- dff fn as
        as <- mapM clearCaf as
        return $ App fn as ty
    f (Return (NodeC fn as)) | Just fn' <- tagToFunction fn = do
        as <- dff' fn' as
        as <- mapM clearCaf as
        return $ Return (NodeC fn as)
    f (Store (NodeC fn as)) |  Just fn' <- tagToFunction fn = do
        as <- dff' fn' as
        as <- mapM clearCaf as
        return $ Store (NodeC fn as)
    f (Update (Var v (TyPtr TyNode)) _) | deadCaf v = do
        mtick $ toAtom "Optimize.dead-code.caf-update"
        return $ Return unit
    f (Update p (NodeC fn as)) |  Just fn' <- tagToFunction fn = do
        as <- dff' fn' as
        as <- mapM clearCaf as
        return $ Update p (NodeC fn as)
    f x = return x
    dff' fn as | postInline = return as
    dff' fn as = dff fn as
    dff fn as = mapM df  (zip as [0..]) where
        deadVal (Lit 0 _) = True
        deadVal x =  isHole x
        df (a,i) | not (deadVal a) && not (Set.member (fn,i) usedArgs) = do
            mtick $ toAtom "Optimize.dead-code.func-arg"
            return $ properHole (getType a)
        df (a,_)  = return a
    clearCaf (Var v (TyPtr TyNode)) | deadCaf v = do
        mtick $ toAtom "Optimize.dead-code.caf-arg"
        return (properHole (TyPtr TyNode))
    clearCaf (Tup xs) = do
        xs <- mapM clearCaf xs
        return $ Tup xs
    clearCaf (NodeC a xs) = do
        xs <- mapM clearCaf xs
        return $ NodeC a xs
    clearCaf (NodeV a xs) = do
        xs <- mapM clearCaf xs
        return $ NodeV a xs
    clearCaf (Const a) = do
        a <- clearCaf a
        return $ Const a
    clearCaf x = return x
    deadCaf v =  v < v0 && not (v `Set.member` usedCafs)


