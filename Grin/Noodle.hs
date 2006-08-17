module Grin.Noodle where

-- various routines for manipulating and exploring grin code.

import Control.Monad.Writer
import Data.Monoid
import qualified Data.Set as Set

import Atom(Atom(),toAtom)
import C.Prims
import Grin.Grin
import Support.CanType


modifyTail :: Lam -> Exp -> Exp
modifyTail lam@(_ :-> lb) e = f e where
    f (Error s ty) = Error s (getType lb)
    f (Case x ls) = Case x (map g ls)
    f lt@Let {expBody = body } = lt { expBody = f body }
    f lt@MkCont {expLam = lam, expCont = cont } = lt { expLam = g lam, expCont = g cont }
    f (e1 :>>= p :-> e2) = e1 :>>= p :-> f e2
    f e = e :>>= lam
    g (p :-> e) = p :-> f e


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
    return l { expBody = b, expDefs = defs' }
mapExpExp _ x = return x

mapFBodies f xs = mapM f' xs where
    f' fd@FuncDef { funcDefBody = l :-> r } = do
        r' <- f r
        return $  updateFuncDefProps fd { funcDefBody = l :-> r' }


--------------------------
-- examining and reporting
--------------------------

isManifestNode :: Monad m => Exp -> m [Atom]
isManifestNode (Return (Tag t)) = return [t]
isManifestNode (Return (NodeC t _)) = return [t]
isManifestNode Error {} = return []
isManifestNode (Case _ ls) = do
    cs <- Prelude.mapM isManifestNode [ e | _ :-> e <- ls ]
    return $ concat cs
isManifestNode (_ :>>= _ :-> e) = isManifestNode e
isManifestNode _ = fail "not manifest node"


-- | Is a Val constant?
valIsConstant :: Val -> Bool
valIsConstant (Tup xs) = all valIsConstant xs
valIsConstant (NodeC t _) | isMutableNodeTag t = False
valIsConstant (NodeC _ xs) = all valIsConstant xs
valIsConstant Tag {} = True
valIsConstant Lit {} = True
valIsConstant Const {} = True
valIsConstant (Var v _) | v < v0 = True
valIsConstant ValPrim {} = True
valIsConstant _ = False

-- | Is type mutable (currently IORef)
isMutableNodeTag :: Tag -> Bool
isMutableNodeTag t = t == ref_tag where ref_tag = toAtom "CData.IORef.IORef"

valIsMutable (NodeC t _) = isMutableNodeTag t
valIsMutable _ = False



isOmittable (Fetch {}) = True
isOmittable (Return {}) = True
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
        cfunc (e :>>= y) = do
            xs <- cfunc e
            tell xs
            clfunc y
        cfunc (App a _ _) = return (Set.singleton a)
        cfunc (Case _ as) = do
            rs <- mapM clfunc as
            return (mconcat rs)
        cfunc Let { expDefs = defs, expBody = body } = do
            b <- cfunc body
            rs <- mapM (clfunc . funcDefBody) defs
            return $ mconcat (b:rs)
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

grinLet defs body = updateLetProps Let { expDefs = defs, expBody = body, expInfo = mempty }

updateLetProps Let { expDefs = [], expBody = body } = body
updateLetProps lt@Let {} = lt
updateLetProps e = e


