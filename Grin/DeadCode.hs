module Grin.DeadCode(deadCode) where


import Data.Monoid
import Monad
import qualified Data.Set as Set
import qualified Data.Map as Map

import Atom
import Fixer.Fixer
import Fixer.Supply
import FreeVars
import GenUtil
import Grin.Grin
import Grin.Whiz
import Stats hiding(print)
import Support.CanType
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
    pappFuncs <- newValue fixer bottom
    suspFuncs <- newValue fixer bottom
    -- set all roots as used
    flip mapM_ roots $ \r -> do
        addRule $ value True `implies` sValue usedFuncs r
    let postInline = phaseEvalInlined (grinPhase grin)

    -- using a CAF implies using its function if pre-inlining
    unless postInline $ flip mapM_ (grinCafs grin) $ \ (var,~(NodeC a [])) -> do
        x <- supplyValue usedCafs var
        f <- supplyValue usedFuncs (tagFlipFunction a)
        addRule $ x `implies` f

    mapM_ (go fixer pappFuncs suspFuncs usedFuncs usedArgs usedCafs postInline) (grinFunctions grin)
    calcFixpoint "Dead Code" fixer
    ua <- supplyReadValues usedArgs
    uc <- supplyReadValues usedCafs
    uf <- supplyReadValues usedFuncs
    pappFuncs <- readValue pappFuncs
    suspFuncs <- readValue suspFuncs
    let cafSet = fg uc
        argSet = fg ua
        funSet = fg uf
        directFuncs =  funSet Set.\\ suspFuncs Set.\\ pappFuncs
        fg xs = Set.fromList [ x | (x,True) <- xs ]
    newCafs <- flip mconcatMapM (grinCafs grin) $ \ (x,y) -> do
        if x `Set.member` cafSet then return [(x,y)] else tick stats "Optimize.dead-code.caf" >> return []
    newFuncs <- flip mconcatMapM (grinFunctions grin) $ \ (x,y) -> do
        if not $ x `Set.member` funSet then tick stats "Optimize.dead-code.func" >> return [] else do
        r <- runStatIO stats $ removeDeadArgs postInline funSet directFuncs cafSet argSet (x,y)
        return [r]
    let (TyEnv mp) = grinTypeEnv grin
    mp' <- flip mconcatMapM (Map.toList mp) $ \ (x,(ts,rt)) -> case Just x  of
        Just _ | tagIsFunction x, not $ x `Set.member` funSet -> return []
        Just fn | fn `Set.member` directFuncs -> do
            let da (t,i)
                    | Set.member (fn,i) argSet = return [t]
                    | otherwise = tick stats ("Optimize.dead-code.arg-func.{" ++ show x ++ "-" ++ show i) >> return []
            ts' <- mconcatMapM da (zip ts naturals)
            return [(x,(ts',rt))]
        _ -> return [(x,(ts,rt))]


    --putStrLn "partialapplied:"
    --mapM_ print $ Set.toList pappFuncs
    --putStrLn "suspended:"
    --mapM_ print $ Set.toList suspFuncs
    --putStrLn "none:"
    --mapM_ print $ Set.toList $ funSet Set.\\ suspFuncs Set.\\ pappFuncs
    return grin {
        grinCafs = newCafs,
        grinFunctions = newFuncs,
        grinPartFunctions = pappFuncs,
        grinTypeEnv = TyEnv $ Map.fromList mp',
        grinSuspFunctions = suspFuncs
        }

combineArgs fn as = [ ((fn,n),a) | (n,a) <- zip [0 :: Int ..] as]

go fixer pappFuncs suspFuncs usedFuncs usedArgs usedCafs postInline (fn,~(Tup as) :-> body) = ans where
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
            h (p,e) = g e
            doNode (NodeC n as) | not postInline, Just (x,fn) <- tagUnfunction n  = let
                consts = (mconcatMap doConst as)
                usedfn = implies fn' (sValue usedFuncs fn)
                suspfn | x > 0 = conditionalRule id fn' (pappFuncs `isSuperSetOf` value (Set.singleton fn))
                       | otherwise = conditionalRule id fn' (suspFuncs `isSuperSetOf` value (Set.singleton fn))
                in mappend consts $ mconcat (usedfn:suspfn:[ mconcatMap (implies (sValue usedArgs fn) . varValue) (freeVars a) | (fn,a) <- combineArgs fn as])
            doNode x = doConst x `mappend` mconcatMap (implies fn' . varValue) (freeVars x)
            doConst _ | postInline  = mempty
            doConst (Const n) = doNode n
            doConst (Tup ns) = mconcatMap doConst ns
            doConst (NodeC n as) = mconcatMap doConst as
            doConst (NodeV n as) = mconcatMap doConst as
            doConst _ = mempty

        (nl,_) <- whiz (\_ -> id) h' f whizState (Tup as :-> body)
        return nl


removeDeadArgs :: MonadStats m => Bool -> Set.Set Atom -> Set.Set Atom -> (Set.Set Var) -> (Set.Set (Atom,Int)) -> (Atom,Lam) -> m (Atom,Lam)
removeDeadArgs postInline funSet directFuncs usedCafs usedArgs (a,l) =  whizExps f (margs l) >>= return . (,) a where
    margs (Tup as :-> e) | a `Set.member` directFuncs = (Tup (removeArgs a as) :-> e)
    margs x = x
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
    dff' fn as | fn `Set.member` directFuncs = return as
    dff' fn as = dff'' fn as
    dff fn as | fn `Set.member` directFuncs = return (removeArgs fn as)
    dff fn as = dff'' fn as
    dff'' fn as | not (fn `Set.member` funSet) = return as -- if function was dropped, we don't have argument use information.
    dff'' fn as = mapM df  (zip as naturals) where
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
    deadCaf v = v < v0 && not (v `Set.member` usedCafs)
    deadVal (Lit 0 _) = True
    deadVal x = isHole x
    removeArgs fn as = concat [ perhapsM ((fn,i) `Set.member` usedArgs) a | a <- as | i <- naturals ]


