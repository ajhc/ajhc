module Grin.Noodle where

-- various routines for manipulating and exploring grin code.

import Control.Monad.Writer
import Data.Monoid
import qualified Data.Set as Set

import Support.FreeVars
import Atom(Atom(),toAtom)
import Grin.Val
import Name.Names
import Options(flint)
import C.Prims
import Util.Gen
import Grin.Grin
import Support.CanType
import Debug.Trace


modifyTail :: Lam -> Exp -> Exp
modifyTail lam@(_ :-> lb) te = f mempty te where
    lamFV = freeVars lam :: Set.Set Var
    f lf e | False && trace ("modifyTail: " ++ show (lf,e)) False = undefined
    f _ (Error s ty) = Error s (getType lb)
    f lf (Case x ls) = Case x (map (g lf) ls)
    f _ lt@Let {expIsNormal = False } = lt :>>= lam
    f lf lt@Let {expDefs = defs, expBody = body, expIsNormal = True } = updateLetProps lt { expBody = f nlf body, expDefs = defs' } where
        nlf = lf `Set.union` Set.fromList (map funcDefName defs)
        defs' = [ updateFuncDefProps d { funcDefBody = g nlf (funcDefBody d) } | d <- defs ]
    f lf lt@MkCont {expLam = lam, expCont = cont } = lt { expLam = g lf lam, expCont = g lf cont }
    f lf (e1 :>>= p :-> e2) = e1 :>>= p :-> f lf e2
    f lf e@(App a as t) | a `Set.member` lf = App a as (getType lb)
    f lf e = e :>>= lam
    g lf (p :-> e) | flint && not (Set.null $ Set.intersection (freeVars p) lamFV) = error "modifyTail: lam floated inside bad scope"
    g lf (p :-> e) = p :-> f lf e


mapBodyM f (x :-> y) = f y >>= return . (x :->)

mapExpExp f (a :>>= v :-> b) = do
    a <- f a
    b <- f b
    return (a :>>= v :-> b)
mapExpExp f (Case e as) = do
    as' <- mapM (mapBodyM f) as
    return (Case e as')
mapExpExp f l@Let { expBody = b, expDefs = defs } = do
    b <- f b
    defs' <- mapFBodies f defs
    return $ updateLetProps l { expBody = b, expDefs = defs' }
mapExpExp _ x = return x

mapFBodies f xs = mapM f' xs where
    f' fd@FuncDef { funcDefBody = l :-> r } = do
        r' <- f r
        return $  updateFuncDefProps fd { funcDefBody = l :-> r' }


--------------------------
-- examining and reporting
--------------------------

isManifestNode :: Monad m => Exp -> m [Atom]
isManifestNode e = f mempty e where
    f lf _ | False && trace ("isManifestNode: " ++ show lf) False = undefined
    f lf (Return (Tag t)) = return [t]
    f lf (Return (NodeC t _)) = return [t]
    f lf Error {} = return []
    f lf (App a _ _) | a `Set.member` lf = return []
    f lf Let { expBody = body, expIsNormal = False } = f lf body
    f lf Let { expBody = body, expDefs = defs, expIsNormal = True } = ans where
        nlf = lf `Set.union` Set.fromList (map funcDefName defs)
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
valIsConstant (Tup xs) = all valIsConstant xs
valIsConstant (NodeC t _) | isMutableNodeTag t = False
valIsConstant (NodeC _ xs) = all valIsConstant xs
valIsConstant Tag {} = True
valIsConstant Lit {} = True
valIsConstant Const {} = True
valIsConstant (Var v _) | v < v0 = True
valIsConstant (Index v t) = valIsConstant v && valIsConstant t
valIsConstant ValPrim {} = True
valIsConstant _ = False

-- | Is type mutable (currently IORef)
isMutableNodeTag :: Tag -> Bool
isMutableNodeTag _ = False
--isMutableNodeTag t = t ==  convertName dc_Ref

valIsMutable (NodeC t _) = isMutableNodeTag t || t == tagHole
valIsMutable NodeC {} = False
valIsMutable _ = True



isOmittable (Fetch {}) = True
isOmittable (Return {}) = True
isOmittable (Store x) | getType x /= TyNode = False
isOmittable (Store (NodeC n _)) | isMutableNodeTag n || n == tagHole = False
isOmittable (Store {}) = True
isOmittable Prim { expPrimitive = Primitive { primAPrim = aprim } } = aprimIsCheap aprim
isOmittable (Case x ds) = all isOmittable [ e | _ :-> e <- ds ]
isOmittable Let { expBody = x } = isOmittable x
isOmittable (e1 :>>= _ :-> e2) = isOmittable e1 && isOmittable e2
isOmittable _ = False

isErrOmittable Update {} = True
isErrOmittable (e1 :>>= _ :-> e2) = isErrOmittable e1 && isErrOmittable e2
isErrOmittable (Case x ds) = all isErrOmittable [ e | _ :-> e <- ds ]
isErrOmittable x = isOmittable x



-- collect tail called, and normally called functions

collectFuncs :: Exp -> (Set.Set Atom,Set.Set Atom)
collectFuncs exp = runWriter (cfunc exp) where
        clfunc (l :-> r) = cfunc r
        cfunc e | False && trace ("isManifestNode: " ++ show e) False = undefined
        cfunc (e :>>= y) = do
            xs <- cfunc e
            tell xs
            clfunc y
        cfunc (App a _ _) = return (Set.singleton a)
        cfunc (Case _ as) = do
            rs <- mapM clfunc as
            return (mconcat rs)
        cfunc Let { expFuncCalls = (tail,nonTail) } = do
            tell nonTail
            return tail
        cfunc Fetch {} = return mempty
        cfunc Error {} = return mempty
        cfunc Prim {} = return mempty
        cfunc Return {} = return mempty
        cfunc Store {} = return mempty
        cfunc Update {} = return mempty
        cfunc Alloc {} = return mempty
        cfunc NewRegion { expLam = l } = clfunc l
        cfunc MkCont { expCont = l1, expLam = l2 } = do
            a <- clfunc l1
            b <- clfunc l2
            return (a `mappend` b)

grinLet defs body = updateLetProps Let {
    expDefs = defs,
    expBody = body,
    expInfo = mempty,
    expNonNormal = undefined,
    expIsNormal = undefined,
    expFuncCalls = undefined }

updateLetProps Let { expDefs = [], expBody = body } = body
updateLetProps lt@Let { expBody = body, expDefs = defs } = lt { expFuncCalls = (tail Set.\\ myDefs, nonTail Set.\\ myDefs), expNonNormal = notNormal, expIsNormal = Set.null notNormal } where
    (tail,nonTail) = mconcatMap collectFuncs (body : map (lamExp . funcDefBody) defs)
    notNormal =  nonTail `Set.intersection` (Set.fromList $ map funcDefName defs)
    myDefs = Set.fromList $ map funcDefName defs
--    retInfo = filter (`notElem` [myDefs]) concatMap (getReturnInfo . lamExp . funcDefBody) (body:defs)
updateLetProps e = e


data ReturnInfo = ReturnNode (Maybe Atom,[Ty]) | ReturnConst Val | ReturnCalls Atom | ReturnOther | ReturnError
    deriving(Eq,Ord)

getReturnInfo :: Exp -> [ReturnInfo]
getReturnInfo  e = ans where
    ans = execWriter (f mempty e)
    tells x = tell [x]
    f lf (Return (NodeV t as)) = tells (ReturnNode (Nothing,map getType as))
    f lf (Return (NodeC t as)) = tells (ReturnNode (Just t,map getType as))
    f lf (Return z) | valIsConstant z = tell [ReturnConst z]
    f lf Error {} = tells ReturnError
    f lf (Case _ ls) = do Prelude.mapM_ (f lf) [ e | _ :-> e <- ls ]
    f lf (_ :>>= _ :-> e) = f lf e
    f lf Let { expBody = body, expIsNormal = False } = f lf body
    f lf (App a _ _) | a `Set.member` lf = return ()
    f lf Let { expBody = body, expDefs = defs, expIsNormal = True } = ans where
        nlf = lf `Set.union` Set.fromList (map funcDefName defs)
        ans = do
            mapM_ (f nlf . lamExp . funcDefBody) defs
            f nlf body
    f _ (App a _ _) = tells $ ReturnCalls a
    f _ e = tells ReturnOther


