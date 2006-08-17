module Grin.Noodle where

-- various routines for manipulating and exploring grin code.

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


