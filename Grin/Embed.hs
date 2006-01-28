module Grin.Embed((.>>=),(.>>),GG,VVar,TyNode,TyTag,TyPtr,TyRaw,embedTest) where

import Grin.Grin
import Util.UniqueMonad
import Support.CanType
import Atom
import GenUtil
import Grin.Show
import Doc.Pretty


newtype GG = GG { unGG :: (Uniq Exp) }

data TyNode
data TyTag
data TyPtr a
data TyRaw a
data TyUnknown

runGG :: Int -> Uniq Exp -> Exp
runGG n x | n <= 0 = runGG 1 x
runGG s (GG x) = fst (runUniq s x)

unLam :: forall a . Valable a => ( a -> GG ) -> Uniq Lam
unLam f = do
    (x,v) <- varUp (error "cannot unlamunknown")
    gb <- unGG $ f x
    return $ v :-> gb

dunLam :: forall a . Valable a => Ty -> ( a -> GG ) -> Uniq Lam
dunLam ty f = do
    (x,v) <- varUp ty
    gb <- unGG $ f x
    return $ v :-> gb

{-
data TyTup -- ???
data Val :: * where
    Tag :: Tag -> Val TyTag
    Const :: Val x -> Val (TyPtr x)
    Lit :: Number -> Val (TyBasic)
    Var :: Var -> Val a
    NodeC :: Tag -> ???? -> Val TyNode
    Tup :: ???? -> Val ???
-}

infixr 1  .>>=, .>>

(.>>=) :: Valable a => GG -> (a -> GG) -> GG
(.>>=) (GG g1) f2 = GG $ do
    ga <- g1
    (x,v) <- varUp (getType ga)
    gb <- unGG $ f2 x
    return $ ga :>>= v :-> gb

(.>>) :: GG -> GG -> GG
(.>>) g1 g2 = g1 .>>= \ ( _ :: TyUnknown ) -> g2

newtype VVar a = VVar Val

class Valable a where
    varUp :: Ty -> Uniq (a,Val)

vvarUp :: forall a . Ty -> Uniq (VVar a,Val)
vvarUp TyTup {} = error "vvarUp tuple"
vvarUp ty = do
    vv <- newVal ty
    return (VVar vv,vv)

newVal (TyTup tys) = do
    vs <- mapM newVal tys
    return (Tup vs)
newVal ty = do
    i <- newUniq
    return (Var (V i) ty)

{-
instance Valable a where
    varUp ty = do
        vv <- newVal ty
        return (undefined,vv)
-}

instance Valable TyUnknown where
    varUp ty = do
        vv <- newVal ty
        return (undefined,vv)

instance TyBasic a => Valable (VVar (TyRaw a)) where
    varUp _ = vvarUp (Ty (rawType (undefined :: a)))


instance Valable (VVar TyNode) where
    varUp _ = vvarUp TyNode
instance Valable (VVar TyTag) where
    varUp _ = vvarUp TyTag
instance (Valable a, Valable b) => Valable (a,b) where
    varUp (TyTup [x,y]) = do
        (vva,va) <- varUp x
        (vvb,vb) <- varUp y
        return ((vva,vvb),Tup [va,vb])

class TyBasic a where
    rawType :: a -> Atom

data RawInt
type Rint = TyRaw RawInt

instance TyBasic RawInt where
    rawType _ = (toAtom "int")

gCase :: VVal a -> [Uniq Lam] -> GG
gCase (VVal v) ls = GG $ do
    ls <- sequence ls
    return $ Case v ls

gReturn :: VVal a -> GG
gReturn (VVal v) = GG $ return $ Return v

app2 :: Atom -> VVal a -> VVal b -> GG
app2 n (VVal a) (VVal b) = GG $ return (App n [a,b])


lLam :: Val -> Uniq Exp -> Uniq Lam
lLam v ue = do
    e <- ue
    return $ v :-> e

fact :: (Rint,Rint) -> GG
fact (n,r) = gCase n [unLam $ \ (x :: Rint) -> primMinus n 1 .>>= \ n' -> primTimes n r .>>= \r' -> app (toAtom "fact") [n',r'], lLam 1 (gReturn r) ]


embedTest = do
    putDoc $ prettyFun (toAtom "fact",execUniq1 $ unLam fact)



