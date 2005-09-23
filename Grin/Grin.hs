{-# OPTIONS -funbox-strict-fields #-}

module Grin.Grin(
    tagIsWHNF,
    tagIsPartialAp,
    tagIsTag,
    tagIsSuspFunction,
    tagIsFunction,
    tagToFunction,
    tagFlipFunction,
    tagUnfunction,
    valIsNF,
    gApply,
    partialTag,
    gEval,
    tagApply,
    tagArrow,
    tagHole,
    Exp(..),
    Ty(..),
    Tag,
    TyEnv(..),
    Val(..),
    v0,v1,v2,v3,
    p0,p1,p2,p3,
    n0,n1,n2,n3,
    Var(..),
    sequenceG_,
    funcEval,
    funcFetch,
    funcApply,
    funcInitCafs,
    Grin(..),
    HasType(..),
    Primitive(..),
    Builtin,
    Props(..),
    Lam(..),
    unit,
    tyUnit,
    funcMain,
    findArgsType, findArgs) where

import Atom
import Boolean.Algebra
import Char
import Control.Monad.Identity
import C.Prims
import Data.IORef
import Data.Monoid
import Doc.DocLike
import FreeVars
import GenUtil
import List(isPrefixOf)
import Number
import Prelude hiding((&&),(||),not,and,or,any,all)
import qualified Data.Map as Map
import qualified Data.Set as Set
import VConsts

-- Extremely simple first order monadic code with basic type system.  similar
-- to GRIN except for the explicit typing on variables. Note, that certain
-- haskell types become Grin values, however, nothing may be done with types other
-- than examining them. (types may not be constructed at run-time) ( do we need
-- this for polymorphic recursion? )


newtype TyEnv = TyEnv (Map.Map Atom ([Ty],Ty))
    deriving(Monoid)


tagApply = toAtom "Bap"
tagArrow = toAtom "TPrelude.->"
tagHole = toAtom "@hole"
funcApply = toAtom "@apply"
funcEval = toAtom "@eval"
funcFetch = toAtom "@fetch"
funcInitCafs = toAtom "@initcafs"
funcMain = toAtom "@main"

gEval x = App funcEval [x]
gApply x y = App funcApply [x,y]


instance TypeNames Ty where
    tIntzh = Ty (toAtom "int")
    tCharzh = Ty (toAtom "uint32_t")
    tStar = Ty (toAtom "*")

data Ty =
    TyTag           -- ^ a lone tag
    | TyPtr Ty      -- ^ pointer to a heap location which contains its argument
    | TyNode        -- ^ a whole tagged node
    | Ty Atom       -- ^ a basic type
    | TyTup [Ty]    -- ^ unboxed list of values
    | TyUnknown     -- ^ an unknown possibly undefined type, All of these must be eliminated by code generation
    deriving(Eq,Ord)

instance Show Ty where
    show TyTag = "T"
    show (Ty a) = fromAtom a
    show TyNode = "N"
    show (TyPtr t) = '&':show t
    show (TyTup []) = "()"
    show (TyTup ts) =  show ts
    show TyUnknown = "?"


type Tag = Atom

newtype Var = V Int
    deriving(Eq,Ord,Enum)

instance Show Var where
    showsPrec _ (V n) xs = 'v':shows n xs

a @>> b = a :>>= (unit :-> b)
sequenceG_ [] = Return unit
sequenceG_ (x:xs) = foldl (@>>) x xs




infixr 1  :->, :>>=


tyUnit = TyTup []
unit = Tup []

data Lam = Val :-> Exp
    deriving(Eq,Ord,Show)

data Exp =
     Exp :>>= !Lam
    | App Atom [Val]  -- ^ this handles applications of functions and builtins
    | Prim Primitive [Val]
    | Case Val [Lam]
    | Return { expValue :: Val }
    | Store { expValue :: Val }
    | Fetch { expAddress :: Val }
    | Update { expAddress :: Val, expValue :: Val }
    | Error String Ty -- ^ abort with an error message, non recoverably.
    | Cast Val Ty     -- ^ reinterpret Val as a different type, also used to box\/unbox lifted types
    deriving(Eq,Show,Ord)

data Val =
    NodeC !Tag [Val]
    | NodeV !Var [Val]
    | Tag !Tag
    | Const Val         -- ^ pointer to constant data, only Lit, Tag, and NodeC may be children
    | Lit !Number Ty
    | Var !Var Ty
    | Tup [Val]
    | ValPrim APrim
    | Addr {-# UNPACK #-} !(IORef Val)  -- used only in interpreter
    deriving(Eq,Show,Ord)


instance Show (IORef a) where
    show _ = "IORef"
instance Ord (IORef a) where
    compare a b = EQ



data Grin = Grin {
    grinTypeEnv :: TyEnv,
    grinFunctions :: [(Atom,Lam)],
--    grinPrimitives :: [(Primitive,Builtin)],
    grinCafs :: [(Var,Val)]
}

data Flag = No | Maybe | Yes
    deriving(Eq,Ord,Enum,Show)


instance Monoid Flag where
    mempty = No
    mappend a b = max a b
    mconcat xs = maximum xs


instance SemiBooleanAlgebra Flag where
    (&&) = max
    Yes || Yes = Yes
    No || No = No
    _ || _ = Maybe


data Primitive = Primitive {
    primName :: Atom,
    primRets :: Maybe [Atom],
    primType :: (Ty,Ty),
    primAPrim :: APrim
    --primProps :: Props
    --primCallingConvention :: (),
    } deriving(Show)

instance Eq Primitive where
    a == b = primName a == primName b
    a /= b = primName a /= primName b

instance Ord Primitive where
    compare a b = compare (primName a) (primName b)

data Props = Props {
    hasSideEffects :: Flag,  -- ^ has side effects
    causesError    :: Flag,  -- ^ contains Error or aborting primitive
    allocsMem      :: Flag   -- ^ calls store (does not count as side effect)
    } deriving(Show)

instance Monoid Props where
    mempty = Props mempty mempty mempty
    Props x y z `mappend` Props a b c = Props (mappend x a) (mappend y b) (mappend z c)
instance SemiBooleanAlgebra Props where
    Props x y z && Props a b c = Props ((&&) x a) ((&&) y b) ((&&) z c)
    Props x y z || Props a b c = Props ((||) x a) ((||) y b) ((||) z c)


propsMaybe = Props { hasSideEffects = Maybe, causesError = Maybe, allocsMem = Maybe }

props :: Exp -> Props
props (x :>>= (_ :-> y)) = props x && props y
props (Case _ xs) = or1 [ props x | _ :-> x <- xs ]
props Return {} = mempty
props Store {} = mempty { allocsMem = Yes }
props Fetch {} = mempty
props Update {} = mempty { hasSideEffects = Yes }
props Error {} = mempty { causesError = Yes }
props Cast {} = mempty
props _ = error "props"


type Builtin = [Val] -> IO Val

partialTag :: Tag -> Int -> Tag
partialTag v c = case fromAtom v of
    ('f':xs) | 0 <- c ->   toAtom $ 'F':xs
             | c > 0 ->  toAtom $ 'P':show c ++ "_" ++ xs
    ('T':xs) | 0 <- c ->  v
             | c > 0 ->  toAtom $ 'Y':show c ++ "_" ++ xs
    ('b':xs) | 0 <- c ->  toAtom $ 'B':xs
    _ -> error $  "partialTag: " ++ show (v,c)



tagUnfunction :: Monad m => Tag -> m (Int, Tag)
tagUnfunction t
    | tagIsSuspFunction t = return (0,tagFlipFunction t)
    | tagIsFunction t = return (0,t)
    | ('P':zs) <- t', (n@(_:_),'_':rs) <- span isDigit zs = return (read n, toAtom ('f':rs))
    where t' = fromAtom t
tagUnfunction _ = fail "Tag does not represent function"



tagFlipFunction t
    | 'F':xs <- t' = toAtom $ 'f':xs
    | 'B':xs <- t' = toAtom $ 'b':xs
    | 'f':xs <- t' = toAtom $ 'F':xs
    | 'b':xs <- t' = toAtom $ 'B':xs
    | otherwise = error "Cannot FLIP non function."
    where t' = fromAtom t

tagIsSuspFunction t
    | 'F':_ <- t' = True
    | 'B':_ <- t' = True
    | otherwise = False
    where t' = fromAtom t

tagToFunction t
    | t == funcMain = return t
    | t == funcInitCafs = return t
    | 'F':xs <- t' = return $ toAtom $ 'f':xs
    | 'B':xs <- t' = return $ toAtom $ 'b':xs
    | 'f':_ <- t' = return t
    | 'b':_ <- t' = return t
    | 'P':is <- t', ('_':xs) <- dropWhile isDigit is = return $ toAtom $ 'f':xs
    | otherwise = fail $ "Not Function: " ++ t'
    where t' = fromAtom t

tagIsFunction t
    | t == funcMain = True
    | t == funcInitCafs = True
    | 'f':_ <- t' = True
    | 'b':_ <- t' = True
    | otherwise = False
    where t' = fromAtom t

tagIsPartialAp t
    | 'P':_ <- t' = True
    | otherwise = False
    where t' = fromAtom t

tagIsTag t
    | 'P':_ <- t' = True
    | 'T':_ <- t' = True
    | 'C':_ <- t' = True
    | 'F':_ <- t' = True
    | 'B':_ <- t' = True
    | 'Y':_ <- t' = True
    | otherwise = False
    where t' = fromAtom t

tagIsWHNF t
    | 'P':_ <- t' = True
    | 'T':_ <- t' = True
    | 'C':_ <- t' = True
    | 'Y':_ <- t' = True
    | otherwise = False
    where t' = fromAtom t

valIsNF (NodeC t vs) = tagIsWHNF t && all valIsNF vs
valIsNF (Tup xs) = all valIsNF xs
valIsNF (Tag _) = True
--valIsNF Unit = True
valIsNF Const {} = True
valIsNF Lit {} = True
valIsNF _ = False


{-

-- create an eval suitable for inlining.
createEval' :: Bool -> TyEnv -> [Tag] -> Lam
createEval' shared  te ts

    | null cs = p1 :-> Error "Empty Eval" TyNode
    | all tagIsWHNF [ t | t <- ts , tagIsTag t] = p1 :-> Fetch p1
    | otherwise = p1 :->
        Fetch p1 :>>= n2 :->
        Case n2 cs :>>= n3 :->
        Update p1 n3 :>>= unit :->
        Return n3
    where
    cs = [f t | t <- ts, tagIsTag t ]
    g t vs
        | tagIsWHNF t = Return n2
        | 'F':fn <- fromAtom t  = ap ('f':fn) vs
        | 'B':fn <- fromAtom t  = ap ('b':fn) vs
        | otherwise = Error ("Bad Tag: " ++ fromAtom t) TyNode
    f t = (NodeC t vs :-> g t vs ) where
        (ts,_) = runIdentity $ findArgsType te t
        vs = [ Var v ty |  v <- [V 4 .. ] | ty <- ts]
    ap n vs
    --    | shared =  App (toAtom $ n) vs :>>= n3 :-> Update p1 n3 :>>= unit :-> Return n3
        | otherwise = App (toAtom $ n) vs


createEval :: TyEnv -> [Tag] -> Exp
createEval  te ts
    | null cs = Error ("Empty Eval:" ++ show ts) TyNode
    | otherwise =
        Fetch p1 :>>= n2 :->
        Case n2 cs :>>= n3 :->
        Update p1 n3 :>>= unit :->
        Return n3
    where
    cs = [f t | t <- ts, tagIsTag t ]
    g t vs
        | tagIsWHNF t = Return n2
        | 'F':fn <- fromAtom t  = App (toAtom $ 'f':fn) vs -- :>>= n3 :-> Update p1 n3 :>>= unit :-> Return n3
        | 'B':fn <- fromAtom t  = App (toAtom $ 'b':fn) vs
        | otherwise = Error ("Bad Tag: " ++ fromAtom t) TyNode
    f t = (NodeC t vs :-> g t vs ) where
        (ts,_) = runIdentity $ findArgsType te t
        vs = [ Var v ty |  v <- [V 4 .. ] | ty <- ts]
        -}


---------
-- Look up stuff in the typing environment.
---------

findArgsType (TyEnv m) a | Just x <-  Map.lookup a m = return x
findArgsType (TyEnv m) a | ('F':rs) <- fromAtom a = case Map.lookup (toAtom ('f':rs)) m of
    Just x -> return x
    Nothing -> fail $ "findArgsType: " ++ show a
findArgsType (TyEnv m) a | ('B':rs) <- fromAtom a = case Map.lookup (toAtom ('b':rs)) m of
    Just x -> return x
    Nothing -> fail $ "findArgsType: " ++ show a
findArgsType (TyEnv m) a | ('P':rs) <- fromAtom a, (ns,'_':rs) <- span isDigit rs  = case Map.lookup (toAtom ('f':rs)) m of
    Just (ts,n) -> return (take (length ts - read ns) ts,n)
    Nothing -> fail $ "findArgsType: " ++ show a
findArgsType (TyEnv m) a | ('Y':rs) <- fromAtom a, (ns,'_':rs) <- span isDigit rs  = case Map.lookup (toAtom ('T':rs)) m of
    Just (ts,n) -> return (take (length ts - read ns) ts,n)
    Nothing -> fail $ "findArgsType: " ++ show a
--findArgsType _ a | a == tagApply = return ([TyPtr TyNode,TyPtr TyNode],TyNode)
findArgsType _ a | a == toAtom "TAbsurd#" = return ([],TyNode)
findArgsType _ a | a == funcEval = return ([TyPtr TyNode],TyNode)
findArgsType _ a | a == funcApply = return ([TyNode, TyPtr TyNode],TyNode)
findArgsType _ a | a == funcMain = return ([],tyUnit)
findArgsType _ a | a == tagHole = return ([],TyNode)
findArgsType _ a | "@hole" `isPrefixOf` fromAtom a  = return ([],TyNode)
findArgsType _ a =  fail $ "findArgsType: " ++ show a

findType (TyEnv m) a = case Map.lookup a m of
    Nothing -> fail $ "findType: " ++ show a
    Just (_,x) -> return x
findArgs m a = case findArgsType m a of
    Nothing -> fail $ "findArgs: " ++ show a
    Just (as,_) -> return as

v0 = V 0
v1 = V 1
v2 = V 2
v3 = V 3

n0 = Var v0 TyNode
n1 = Var v1 TyNode
n2 = Var v2 TyNode
n3 = Var v3 TyNode

p0 = Var v0 (TyPtr TyNode)
p1 = Var v1 (TyPtr TyNode)
p2 = Var v2 (TyPtr TyNode)
p3 = Var v3 (TyPtr TyNode)



instance ConNames Val where
    vTrue = NodeC (toAtom "CPrelude.True") []
    vFalse = NodeC (toAtom "CPrelude.False") []
    vUnit =  NodeC (toAtom "CPrelude.()") []
    vOrdering x = NodeC (toAtom $ "CPrelude." ++ show x) []

-- typechecking
class HasType a where
    typecheck :: Monad m => TyEnv -> a -> m Ty
    tc :: Monad m => TyEnv -> a -> m Ty
    tc = typecheck


instance HasType a => HasType [a] where
    typecheck _ [] = fail "empty list"
    typecheck te xs = do
        ts <- mapM (typecheck te) xs
        foldl1M_ (same "list") ts
        return (head ts)
    tc _ [] = fail "empty list"
    tc te (x:_) = tc te x


same _ t1 t2 | t1 == t2 = return t1
same msg t1 t2 = fail $ "Types not the same:" <+> msg <+> tshow t1 <+> tshow t2

typLam te (x :-> y) = do
    x <- typecheck te x
    y <- typecheck te y
    return (x,y)



instance HasType Exp where
    typecheck te (e :>>= (v :-> e2)) = do
        t1 <- typecheck te e
        t2 <- typecheck te v
        same (":>>=" <+> show e <+> show v) t1 t2
        typecheck te e2
    typecheck te n@(Prim p as) = do
        let (TyTup as',t') = primType p
        as'' <- mapM (typecheck te) as
        if as'' == as' then return t' else
            fail $ "Prim: arguments do not match " ++ show n
    typecheck te a@(App fn as) = do
        (as',t') <- findArgsType te fn
        as'' <- mapM (typecheck te) as
        if as'' == as' then return t' else
            fail $ "App: arguments do not match: " ++ show a
    typecheck te (Store v) = do
        t <- typecheck te v
        return (TyPtr t)
    typecheck te (Return v) = do
        typecheck te v
    typecheck te (Fetch v) = do
        (TyPtr t) <- typecheck te v
        return t
    typecheck te (Error _ t) = return t
    typecheck te e@(Update w v) = do
        (TyPtr t) <- typecheck te w
        t' <- typecheck te v
        same (show e) t t'
        return tyUnit
    typecheck _ (Case _ []) = fail "empty case"
    typecheck te (Case v as) = do
        tv <- typecheck te v
        (ps,es) <- liftM unzip $ mapM (typLam te) as
        foldl1M_ (same "case pat") (tv:ps)
        foldl1M (same $ "case exp: " ++ show (map head $ sortGroupUnder fst (zip es as)) ) (es)
    typecheck te (Cast _ t) = return t
    tc te (_ :>>= (_ :-> e)) = tc te e
    tc te e = typecheck te e

instance HasType Val where
    typecheck _ (Tag _) = return TyTag
--    typecheck _ Unit = return tyUnit
    typecheck _ (Var _ t) = return t
    typecheck _ (Lit _ t) = return t
    typecheck _ (NodeV {}) = return TyNode
    typecheck te (Tup xs) = do
        xs <- mapM (typecheck te) xs
        return $ TyTup xs
    typecheck x (Const t) = do
        v <- typecheck x t
        return (TyPtr v)
--    typecheck _ (NodeC {}) = return TyNode
    typecheck _ (Addr _) = return $ TyPtr (error "typecheck: Addr")
    typecheck _ (ValPrim _) = error "ValPrim"
    typecheck te n@(NodeC tg as) = do
        (as',_) <- findArgsType te tg
        as'' <- mapM (typecheck te) as
        if as'' == as' then return TyNode else
            fail $ "NodeC: arguments do not match " ++ show n ++ show (as'',as')

instance FreeVars Lam (Set.Set Var) where
    freeVars (x :-> y) = freeVars y Set.\\ freeVars x

instance  FreeVars Exp (Set.Set Var,Set.Set Tag) where
    freeVars x = (freeVars x, freeVars x)

instance FreeVars Val (Set.Set Var) where
    freeVars (NodeC t xs) = freeVars xs
    freeVars (NodeV _ xs) = freeVars xs
    freeVars (Const v) = freeVars v
    freeVars (Var v _) = Set.singleton v
    freeVars (Tup vs) = freeVars vs
    freeVars _ = Set.empty


instance FreeVars Exp (Set.Set Var) where
    freeVars (a :>>= b) = freeVars (a,b)
    freeVars (App a vs) =  freeVars vs
    freeVars (Case x xs) = freeVars (x,xs)
    freeVars (Return v) = freeVars v
    freeVars (Store v) = freeVars v
    freeVars (Fetch v) = freeVars v
    freeVars (Update x y) = freeVars (x,y)
    freeVars (Cast x _) = freeVars x
    freeVars (Prim _ x) = freeVars x
    freeVars Error {} = Set.empty

instance FreeVars Exp [Var] where
    freeVars e = Set.toList $ freeVars e
instance FreeVars Val [Var] where
    freeVars e = Set.toList $ freeVars e
instance FreeVars Lam [Var] where
    freeVars e = Set.toList $ freeVars e

instance FreeVars Val (Set.Set Tag) where
    freeVars (NodeC t xs) = Set.singleton t `Set.union` freeVars xs
    freeVars (NodeV _ xs) = freeVars xs
    freeVars (Tup xs) = freeVars xs
    freeVars (Tag t) = Set.singleton t
    freeVars (Const v) = freeVars v
    freeVars _ = Set.empty

instance FreeVars Val [Tag] where
    freeVars v = Set.toList $ freeVars v

instance FreeVars Exp [Tag] where
    freeVars v = Set.toList $ freeVars v

instance FreeVars Lam (Set.Set Tag) where
    freeVars (a :-> b) = freeVars (a,b)


instance FreeVars Exp (Set.Set Tag) where
    freeVars (a :>>= b) = freeVars (a,b)
    freeVars (App a vs) = Set.singleton a `Set.union` freeVars vs
    freeVars (Case x xs) = freeVars (x,xs)
    freeVars (Return v) = freeVars v
    freeVars (Store v) = freeVars v
    freeVars (Fetch v) = freeVars v
    freeVars (Update x y) = freeVars (x,y)
    freeVars (Cast x _) = freeVars x
    freeVars (Prim _ x) = freeVars x
    freeVars Error {} = Set.empty



