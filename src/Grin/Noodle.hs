module Grin.Noodle where

-- various routines for manipulating and exploring grin code.

import Control.Monad.Writer
import qualified Data.Set as Set

import C.Prims
import Debug.Trace
import Grin.Grin
import Options(flint)
import StringTable.Atom(Atom())
import Support.CanType
import Support.FreeVars
import Support.Tickle
import Util.GMap
import Util.Gen
import Util.HasSize
import Util.SetLike

modifyTail :: Lam -> Exp -> Exp
modifyTail lam@(_ :-> lb) te = f (sempty :: GSet Atom) te where
    lamFV = freeVars lam :: GSet Var
    f lf e | False && trace ("modifyTail: " ++ show (lf,e)) False = undefined
    f _ (Error s ty) = Error s (getType lb)
    f lf (Case x ls) = Case x (map (g lf) ls)
    f _ lt@Let {expIsNormal = False } = lt :>>= lam
    f lf lt@Let {expDefs = defs, expBody = body, expIsNormal = True } = updateLetProps lt { expBody = f nlf body, expDefs = defs' } where
        nlf = lf `union` fromList (map funcDefName defs)
        defs' = [ updateFuncDefProps d { funcDefBody = g nlf (funcDefBody d) } | d <- defs ]
    f lf lt@MkCont {expLam = lam, expCont = cont } = lt { expLam = g lf lam, expCont = g lf cont }
    f lf (e1 :>>= p :-> e2) = e1 :>>= p :-> f lf e2
    f lf e@(App a as t) | a `member` lf = App a as (getType lb)
    f lf e = e :>>= lam
    g lf (p :-> e) | flint && not (isEmpty $ intersection (freeVars p) lamFV) = error "modifyTail: lam floated inside bad scope"
    g lf (p :-> e) = p :-> f lf e

instance Tickleable Exp Lam where
    tickleM = mapBodyM
instance Tickleable Exp Exp where
    tickleM = mapExpExp
instance Tickleable Val Exp where
    tickleM = mapExpVal
instance Tickleable Val Val where
    tickleM = mapValVal
    tickleM_ = mapValVal_
instance Tickleable Lam Grin where
    tickleM f grin = liftM (`setGrinFunctions` grin) $ mapM  (\x -> do nb <- f (funcDefBody x); return (funcDefName x, nb)) (grinFunctions grin)
instance Tickleable Lam FuncDef where
    tickleM f fd = funcDefBody_uM f fd
instance Tickleable (Atom,Lam) FuncDef where
    tickleM f fd@FuncDef { funcDefName = n, funcDefBody = b } = do
    (n',b') <- f (n,b)
    return $  updateFuncDefProps fd { funcDefBody = b', funcDefName = n' }

mapBodyM :: Monad m => (Exp -> m Exp) -> Lam -> m Lam
mapBodyM f (x :-> y) = f y >>= return . (x :->)

mapExpVal :: Monad m => (Val -> m Val) -> Exp -> m Exp
mapExpVal g x = f x where
    f (App a vs t) = return (App a) `ap` mapM g vs `ap` return t
    f (BaseOp a vs) = return (BaseOp a) `ap` mapM g vs
    f (Return vs) = return Return `ap` mapM g vs
    f (Prim x vs t) = return (Prim x) `ap` mapM g vs `ap` return t
    f e@Alloc { expValue = v, expCount = c } = do
        v <- g v
        c <- g c
        return e { expValue = v, expCount = c }
    f (Case v as) = do
        v <- g v
        return (Case v as)
    f e = return e

mapValVal fn x = f x where
    f (NodeC t vs) = return (NodeC t) `ap` mapM fn vs
    f (Index a b) = return Index `ap` fn a `ap` fn b
    f (Const v) = return Const `ap` fn v
    f (ValPrim p vs ty) = return (ValPrim p) `ap` mapM fn vs `ap` return ty
    f x = return x

mapValVal_ fn x = f x where
    f (NodeC t vs) = mapM_ fn vs
    f (Index a b) = fn a >> fn b >> return ()
    f (Const v) = fn v >> return ()
    f (ValPrim p vs ty) =  mapM_ fn vs >> return ()
    f _ = return ()

mapExpLam fn e = f e where
    f (a :>>= b) = return (a :>>=) `ap` fn b
    f (Case e as) = return (Case e) `ap` mapM fn as
    f lt@Let { expDefs = defs } = do
        defs' <- forM defs $ \d -> do
            b <- fn $ funcDefBody d
            return $ updateFuncDefProps d { funcDefBody = b }
        return $ updateLetProps lt { expDefs = defs' }
    f nr@NewRegion { expLam = lam } = do
        lam <- fn lam
        return $ nr { expLam = lam }
    f e@MkCont { expCont = c, expLam = l } = do
        c <- fn c
        l <- fn l
        return $ e { expCont = c, expLam = l }
    f e = return e

mapExpExp fn e = f e where
    f (a :>>= b) = return (:>>=) `ap` fn a `ap` g b
    f l@Let { expBody = b, expDefs = defs } = do
        b <- fn b
        return updateLetProps `ap` (mapExpLam g l { expBody = b })
    f (GcRoots vs e) = return (GcRoots vs) `ap` fn e
    f e = mapExpLam g e
    g (l :-> e) = return (l :->) `ap` fn e

mapFBodies f xs = mapM f' xs where
    f' fd@FuncDef { funcDefBody = l :-> r } = do
        r' <- f r
        return $  updateFuncDefProps fd { funcDefBody = l :-> r' }

funcDefBody_uM f fd@FuncDef { funcDefBody = b } = do
    b' <- f b
    return $  updateFuncDefProps fd { funcDefBody = b' }

grinFunctions_s nf grin = grin { grinFunctions = nf }

--------------------------
-- examining and reporting
--------------------------

isManifestNode :: Monad m => Exp -> m [Atom]
isManifestNode e = f (sempty :: GSet Atom) e where
    f lf _ | False && trace ("isManifestNode: " ++ show lf) False = undefined
    f lf (Return [(NodeC t _)]) = return [t]
    f lf Error {} = return []
    f lf (App a _ _) | a `member` lf = return []
    f lf Let { expBody = body, expIsNormal = False } = f lf body
    f lf Let { expBody = body, expDefs = defs, expIsNormal = True } = ans where
        nlf = lf `union` fromList (map funcDefName defs)
        ans = do
            xs <- mapM (f nlf . lamExp . funcDefBody) defs
            b <- f nlf body
            return (concat (b:xs))
    f lf (Case _ ls) = do
        cs <- Prelude.mapM (f lf) [ e | _ :-> e <- ls ]
        return $ concat cs
    f lf (_ :>>= _ :-> e) = isManifestNode e
    f lf _ = fail "not manifest node"

-- | Is a Val constant?
valIsConstant :: Val -> Bool
valIsConstant (NodeC _ xs) = all valIsConstant xs
valIsConstant Lit {} = True
valIsConstant Const {} = True
valIsConstant (Var v _) | v < v0 = True
valIsConstant (Index v t) = valIsConstant v && valIsConstant t
valIsConstant ValPrim {} = True
valIsConstant _ = False

-- NOPs will not produce any code at run-time so we can tail-call through them.
isNop (BaseOp Promote _) = True
isNop (BaseOp Demote _) = True
isNop _ = False

isOmittable (BaseOp Promote _) = True
isOmittable (BaseOp Demote _) = True
isOmittable (BaseOp PeekVal _) = True
isOmittable (BaseOp ReadRegister _) = True
isOmittable (BaseOp NewRegister _) = True
isOmittable (BaseOp GcPush _) = True  -- omittable because if we don't use the returned gc context, then we don't need to push to begin with
isOmittable (BaseOp (StoreNode _) _) = True
isOmittable Alloc {} = True
isOmittable (Return {}) = True
isOmittable Prim { expPrimitive = aprim } = aprimIsCheap aprim
isOmittable (Case x ds) = all isOmittable [ e | _ :-> e <- ds ]
isOmittable Let { expBody = x } = isOmittable x
isOmittable (e1 :>>= _ :-> e2) = isOmittable e1 && isOmittable e2
isOmittable _ = False

isErrOmittable (BaseOp Overwrite _) = True
isErrOmittable (BaseOp PokeVal _) = True
isErrOmittable (BaseOp WriteRegister _) = True
isErrOmittable (e1 :>>= _ :-> e2) = isErrOmittable e1 && isErrOmittable e2
isErrOmittable (Case x ds) = all isErrOmittable [ e | _ :-> e <- ds ]
isErrOmittable x = isOmittable x

-- collect tail and normally called functions
-- expression (tail called, non tail called)
collectFuncs :: Exp -> (Set.Set Atom,Set.Set Atom)
collectFuncs exp = runWriter (cfunc exp) where
        clfunc (l :-> r) = cfunc r
        cfunc e | False && trace ("isManifestNode: " ++ show e) False = undefined
        cfunc (e :>>= v :-> op@(BaseOp _ v')) | isNop op && v == v' = do cfunc e
        cfunc (e :>>= y) = do
            xs <- cfunc e
            tell xs
            clfunc y
        cfunc (App a _ _) = return (singleton a)
        cfunc (Case _ as) = do
            rs <- mapM clfunc as
            return (mconcat rs)
        cfunc Let { expFuncCalls = (tail,nonTail) } = do
            tell nonTail
            return tail
        cfunc Error {} = return mempty
        cfunc Prim {} = return mempty
        cfunc Return {} = return mempty
        cfunc BaseOp {} = return mempty
        cfunc Alloc {} = return mempty
        cfunc GcRoots { expBody = b} = cfunc b
        cfunc NewRegion { expLam = l } = clfunc l
        cfunc MkCont { expCont = l1, expLam = l2 } = do
            a <- clfunc l1
            b <- clfunc l2
            return (a `mappend` b)
        cfunc x = error "Grin.Noodle.collectFuncs: unknown"

grinLet defs body = updateLetProps Let {
    expDefs = defs,
    expBody = body,
    expInfo = mempty,
    expNonNormal = undefined,
    expIsNormal = undefined,
    expFuncCalls = undefined }

updateLetProps Let { expDefs = [], expBody = body } = body
updateLetProps lt@Let { expBody = body, expDefs = defs } =
        lt {
            expFuncCalls = (tail \\ myDefs, nonTail \\ myDefs),
            expNonNormal = notNormal,
            expIsNormal = isEmpty notNormal
            } where
    (tail,nonTail) = mconcatMap collectFuncs (body : map (lamExp . funcDefBody) defs)
    notNormal =  nonTail `intersection` (fromList $ map funcDefName defs)
    myDefs = fromList $ map funcDefName defs
updateLetProps e = e

data ReturnInfo = ReturnNode (Maybe Atom,[Ty]) | ReturnConst Val | ReturnCalls Atom | ReturnOther | ReturnError
    deriving(Eq,Ord)

getReturnInfo :: Exp -> [ReturnInfo]
getReturnInfo  e = ans where
    ans = execWriter (f (sempty :: GSet Atom) e)
    tells x = tell [x]
    f lf (Return [(NodeC t as)]) = tells (ReturnNode (Just t,map getType as))
    f lf (Return [z]) | valIsConstant z = tell [ReturnConst z]
    f lf Error {} = tells ReturnError
    f lf (Case _ ls) = do Prelude.mapM_ (f lf) [ e | _ :-> e <- ls ]
    f lf (_ :>>= _ :-> e) = f lf e
    f lf Let { expBody = body, expIsNormal = False } = f lf body
    f lf (App a _ _) | a `member` lf = return ()
    f lf Let { expBody = body, expDefs = defs, expIsNormal = True } = ans where
        nlf = lf `union` fromList (map funcDefName defs)
        ans = do
            mapM_ (f nlf . lamExp . funcDefBody) defs
            f nlf body
    f _ (App a _ _) = tells $ ReturnCalls a
    f _ e = tells ReturnOther

mapGrinFuncsM :: Monad m => (Atom -> Lam -> m Lam) -> Grin -> m Grin
mapGrinFuncsM f grin = liftM (`setGrinFunctions` grin) $ mapM  (\x -> do nb <- f (funcDefName x) (funcDefBody x); return (funcDefName x, nb)) (grinFunctions grin)
