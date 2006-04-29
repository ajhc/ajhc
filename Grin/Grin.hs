{-# OPTIONS_GHC -funbox-strict-fields #-}

module Grin.Grin(
    Exp(..),
    Grin(..),
    HeapType(..),
    HeapValue(HV),
    Item(..),
    Lam(..),
    NodeValue(NV),
    Phase(..),
    Primitive(..),
    Tag,
    Ty(..),
    TyEnv(..),
    Val(..),
    Var(..),
    emptyGrin,
    findArgs,
    findArgsType,
    funcApply,
    funcEval,
    funcFetch,
    funcInitCafs,
    funcMain,
    gApply,
    gEval,
    isMutableNodeTag,
    isHole,
    n0,n1,n2,n3,
    p0,p1,p2,p3,
    partialTag,
    phaseEvalInlined,
    properHole,
    sequenceG_,
    tagFlipFunction,
    tagHole,
    valToItem,
    tagIsFunction,
    tagIsPartialAp,
    tagIsSuspFunction,
    tagIsTag,
    tagIsWHNF,
    tagToFunction,
    tagUnfunction,
    tyUnit,
    combineItems,
    unit,
    mapBodyM,
    mapExpExp,
    itemTag,
    v0,v1,v2,v3,lamExp,lamBind,
    isVar,isTup,modifyTail,valIsConstant,
    valIsNF
    ) where

import Char
import Control.Monad.Identity
import Data.IORef
import Data.Monoid
import List(isPrefixOf)
import Prelude hiding((&&),(||),not,and,or,any,all)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Atom
import Boolean.Algebra
import C.Prims
import Doc.DocLike
import Support.FreeVars
import GenUtil
import Name.VConsts
import Number
import Support.CanType
import Support.Tuple

-- Extremely simple first order monadic code with basic type system.  similar
-- to GRIN except for the explicit typing on variables. Note, that certain
-- haskell types become Grin values, however, nothing may be done with types other
-- than examining them. (types may not be constructed at run-time) ( do we need
-- this for polymorphic recursion? )


newtype TyEnv = TyEnv (Map.Map Atom ([Ty],Ty))
    deriving(Monoid)


tagHole = toAtom "@hole"
funcApply = toAtom "@apply"
funcEval = toAtom "@eval"
funcFetch = toAtom "@fetch"
funcInitCafs = toAtom "@initcafs"
funcMain = toAtom "@main"

gEval x = App funcEval [x] TyNode
gApply x y = App funcApply [x,y] TyNode


instance TypeNames Ty where
    tIntzh = Ty (toAtom "int")
    tCharzh = Ty (toAtom "HsChar")
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
     Exp :>>= Lam
    | App { expFunction :: Atom, expArgs :: [Val], expType :: Ty }    -- ^ this handles applications of functions and builtins
    | Prim { expPrimitive :: Primitive, expArgs :: [Val] }
    | Case { expValue :: Val, expAlts :: [Lam] }
    | Return { expValue :: Val }
    | Store { expValue :: Val }
    | Fetch { expAddress :: Val }
    | Update { expAddress :: Val, expValue :: Val }
    | Error { expError :: String, expType :: Ty }      -- ^ abort with an error message, non recoverably.
    deriving(Eq,Show,Ord)

data Val =
    NodeC !Tag [Val]
    | NodeV !Var [Val]
    | Tag !Tag
    | Const Val         -- ^ pointer to constant data, only Lit, Tag, and NodeC may be children
    | Lit !Number Ty
    | Var !Var Ty
    | Tup [Val]
    | ValPrim APrim [Val] Ty
    | Addr {-# UNPACK #-} !(IORef Val)  -- used only in interpreter
    deriving(Eq,Ord)


instance Show (IORef a) where
    show _ = "IORef"
instance Ord (IORef a) where
    compare a b = EQ

instance Show Val where
    -- showsPrec _ s | Just st <- fromVal s = text $ show (st::String)
    showsPrec _ (NodeC t []) = parens $ (fromAtom t)
    showsPrec _ (NodeC t vs) = parens $ (fromAtom t) <+> hsep (map shows vs)
    showsPrec _ (NodeV (V i) vs) = parens $ char 't' <> tshow i <+> hsep (map shows vs)
    showsPrec _ (Tag t) = (fromAtom t)
    showsPrec _ (Var (V i) t)
        | TyPtr _ <- t = char 'p' <> tshow i
        | TyNode <- t = char 'n' <> tshow i
        | Ty _ <- t  = char 'l' <> tshow i
        | TyTag <- t  = char 't' <> tshow i
        | otherwise = char 'v' <> tshow i
    showsPrec _ (Lit i t) | t == tCharzh, Just x <- toIntegral i = tshow (chr x)
    showsPrec _ (Lit i _)  = tshow i
    showsPrec _ (Tup xs)  = tupled $ map shows xs
    showsPrec _ (Const v) = char '&' <> shows v
    showsPrec _ (Addr _) = text "<ref>"
    showsPrec _ (ValPrim aprim xs _) = tshow aprim <> tupled (map tshow xs)

data Phase = PhaseInit | PostInlineEval
    deriving(Show,Eq,Ord,Enum)

phaseEvalInlined PostInlineEval = True
phaseEvalInlined _ = False


data Grin = Grin {
    grinEntryPoints :: [Atom],
    grinPhase :: Phase,
    grinTypeEnv :: TyEnv,
    grinFunctions :: [(Atom,Lam)],
    grinReturnTags :: Map.Map Atom Item,
    grinArgTags :: Map.Map Atom [Item],
    grinSuspFunctions :: Set.Set Atom,
    grinPartFunctions :: Set.Set Atom,
    grinCafs :: [(Var,Val)]
}


emptyGrin = Grin {
    grinEntryPoints = [],
    grinPhase = PhaseInit,
    grinTypeEnv = mempty,
    grinFunctions = [],
    grinReturnTags = mempty,
    grinArgTags = mempty,
    grinSuspFunctions = mempty,
    grinPartFunctions = mempty,
    grinCafs = mempty
}

mapBodyM f (x :-> y) = f y >>= return . (x :->)

mapExpExp f (a :>>= v :-> b) = do
    a <- f a
    b <- f b
    return (a :>>= v :-> b)
mapExpExp f (Case e as) = do
    as' <- mapM (mapBodyM f) as
    return (Case e as')
mapExpExp _ x = return x

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
    primType :: ([Ty],Ty),
    primAPrim :: APrim
    } deriving(Show)

instance Eq Primitive where
    a == b = primName a == primName b
    a /= b = primName a /= primName b

instance Ord Primitive where
    compare a b = compare (primName a) (primName b)






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
valIsNF Const {} = True
valIsNF Lit {} = True
valIsNF _ = False

properHole x = case x of
    TyPtr TyNode -> Const (properHole TyNode)
    TyTag -> (Tag tagHole)
    ty@(Ty _) -> (Lit 0 ty)
    TyNode -> (NodeC tagHole [])

isHole x = x `elem` map properHole [TyPtr TyNode, TyNode, TyTag]

---------
-- Look up stuff in the typing environment.
---------

findArgsType (TyEnv m) a | Just x <-  Map.lookup a m = return x
findArgsType (TyEnv m) a | ('Y':rs) <- fromAtom a, (ns,'_':rs) <- span isDigit rs  = case Map.lookup (toAtom ('T':rs)) m of
    Just (ts,n) -> return (take (length ts - read ns) ts,n)
    Nothing -> fail $ "findArgsType: " ++ show a
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



instance CanTypeCheck TyEnv a Ty => CanTypeCheck TyEnv [a] Ty where
    typecheck _ [] = fail "empty list"
    typecheck te xs = do
        ts <- mapM (typecheck te) xs
        foldl1M_ (same "list") ts
        return (head ts)


same _ t1 t2 | t1 == t2 = return t1
same msg t1 t2 = fail $ "Types not the same:" <+> parens msg <+> parens (tshow t1) <+> parens (tshow t2)

typLam te (x :-> y) = do
    x <- typecheck te x
    y <- typecheck te y
    return (x,y)



instance CanTypeCheck TyEnv Exp Ty where
    typecheck te (e :>>= (v :-> e2)) = do
        t1 <- typecheck te e
        t2 <- typecheck te v
        same (":>>=" <+> show e <+> show v) t1 t2
        typecheck te e2
    typecheck te n@(Prim p as) = do
        let (as',t') = primType p
        as'' <- mapM (typecheck te) as
        if as'' == as' then return t' else
            fail $ "Prim: arguments do not match " ++ show n
    typecheck te ap@(App fn [v,a] t) | fn == funcApply = do
        [v',a'] <- mapM (typecheck te) [v,a]
        if v' == TyNode then return t
         else fail $ "App apply arg doesn't match: " ++ show ap
    typecheck te a@(App fn as t) = do
        (as',t') <- findArgsType te fn
        as'' <- mapM (typecheck te) as
        if t' == t then
            if as'' == as' then return t' else
                fail $ "App: arguments do not match: " ++ show (a,as',t')
         else fail $ "App: results do not match: " ++ show (a,t,(as',t'))
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

instance CanTypeCheck TyEnv Val Ty where
    typecheck _ (Tag _) = return TyTag
    typecheck _ (Var _ t) = return t
    typecheck _ (Lit _ t) = return t
    typecheck _ (NodeV {}) = return TyNode
    typecheck te (Tup xs) = do
        xs <- mapM (typecheck te) xs
        return $ TyTup xs
    typecheck x (Const t) = do
        v <- typecheck x t
        return (TyPtr v)
    typecheck _ (Addr _) = return $ TyPtr (error "typecheck: Addr")
    typecheck _ (ValPrim _ _ ty) = return ty
    typecheck te n@(NodeC tg as) = do
        (as',_) <- findArgsType te tg
        as'' <- mapM (typecheck te) as
        if as'' == as' then return TyNode else
            fail $ "NodeC: arguments do not match " ++ show n ++ show (as'',as')

instance CanType Exp Ty where
    getType (_ :>>= (_ :-> e2)) = getType e2
    getType (Prim p _) = snd (primType p)
    getType App { expType = t } = t
    getType (Store v) = TyPtr (getType v)
    getType (Return v) = getType v
    getType (Fetch v) = case getType v of
        TyPtr t -> t
        _ -> error "Exp.getType: fetch of non-pointer type"
    getType (Error _ t) = t
    getType (Update w v) = tyUnit
    getType (Case _ []) = error "empty case"
    getType (Case _ ((_ :-> e):_)) = getType e

instance CanType Val Ty where
    getType (Tag _) = TyTag
    getType (Var _ t) = t
    getType (Lit _ t) = t
    getType (NodeV {}) = TyNode
    getType (Tup xs) = TyTup (map getType xs)
    getType (Const t) = TyPtr (getType t)
    getType (NodeC {}) = TyNode
    getType (Addr _) = TyPtr (error "typecheck: Addr")
    getType (ValPrim _ _ ty) = ty

instance FreeVars Lam (Set.Set Var) where
    freeVars (x :-> y) = freeVars y Set.\\ freeVars x

instance  FreeVars Exp (Set.Set Var,Set.Set Tag) where
    freeVars x = (freeVars x, freeVars x)

instance FreeVars Val (Set.Set Var) where
    freeVars (NodeC t xs) = freeVars xs
    freeVars (NodeV v xs) = Set.insert v $ freeVars xs
    freeVars (Const v) = freeVars v
    freeVars (Var v _) = Set.singleton v
    freeVars (Tup vs) = freeVars vs
    freeVars _ = Set.empty


instance FreeVars Exp (Set.Set Var) where
    freeVars (a :>>= b) = freeVars (a,b)
    freeVars (App a vs _) =  freeVars vs
    freeVars (Case x xs) = freeVars (x,xs)
    freeVars (Return v) = freeVars v
    freeVars (Store v) = freeVars v
    freeVars (Fetch v) = freeVars v
    freeVars (Update x y) = freeVars (x,y)
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
    freeVars (App a vs _) = Set.singleton a `Set.union` freeVars vs
    freeVars (Case x xs) = freeVars (x,xs)
    freeVars (Return v) = freeVars v
    freeVars (Store v) = freeVars v
    freeVars (Fetch v) = freeVars v
    freeVars (Update x y) = freeVars (x,y)
    freeVars (Prim _ x) = freeVars x
    freeVars Error {} = Set.empty


-- Points to information

data HeapType = Constant | SharedEval | UnsharedEval | Reference | RecursiveThunk
    deriving(Eq,Ord)


data Item = HeapValue (Set.Set HeapValue) | NodeValue (Set.Set NodeValue) | BasicValue Ty | TupledValue [Item]
    deriving(Ord,Eq)
data HeapValue = HV Int (Either (HeapType,Item) Val)  -- either a heap location or a constant
data NodeValue = NV Tag [Item]
    deriving(Ord,Eq)

valToItem (Const v) = HeapValue (Set.singleton (HV (-1) (Right v)))
valToItem (NodeC t as) = NodeValue (Set.singleton (NV t (map valToItem as)))
valToItem (Lit _ ty) = BasicValue ty
valToItem (Tup as) = TupledValue (map valToItem as)
valToItem (Tag _) = BasicValue TyTag

itemTag = BasicValue TyTag

instance CanType Item Ty where
    getType (HeapValue _) = TyPtr TyNode
    getType NodeValue {} = TyNode
    getType (BasicValue ty) = ty
    getType (TupledValue xs) = TyTup (map getType xs)


-- heap locations are given a unique integer to break cycles.
instance Eq HeapValue where
    (HV x _) == (HV y _) = x == y
instance Ord HeapValue where
    compare (HV x _) (HV y _) = compare x y

combineItem :: Item -> Item -> Item
combineItem (BasicValue ty) (BasicValue ty') | ty == ty' = BasicValue ty
combineItem (HeapValue s1) (HeapValue s2) = HeapValue (Set.union s1 s2)
combineItem (NodeValue ns1) (NodeValue ns2) = NodeValue ns where
    ns2map = Map.fromAscList [ (t,NV t as)| NV t as <- (Set.toAscList ns2)]
    ns = Set.fromAscList [ NV t1 (zipWith combineItem as1 as2) | NV t1 as1 <- Set.toAscList ns1, NV _ as2 <- Map.lookup t1 ns2map  ] `Set.union` ns1

combineItems :: [Item] -> Item
combineItems [] = error "cannot combine no items"
combineItems xs = foldl1 combineItem xs


instance Tuple Val where
    tupleMany vs = Tup vs

instance Tuple Ty where
    tupleMany ts = TyTup ts


instance FromTuple Val where
    fromTuple (Tup vs) = vs
    fromTuple v = [v]

instance FromTuple Ty where
    fromTuple (TyTup ts) = ts
    fromTuple v = [v]


instance Tuple Item where
    tupleMany vs = TupledValue vs

instance FromTuple Item where
    fromTuple (TupledValue ts) = ts
    fromTuple x = [x]

lamExp (_ :-> e) = e
lamBind (b :-> _) = b

isVar Var {} = True
isVar _ = False

isTup Tup {} = True
isTup _ = False

modifyTail lam@(_ :-> lb) e = f e where
    f (Error s ty) = Error s (getType lb)
    f (Case x ls) = Case x (map g ls)
    f (e1 :>>= p :-> e2) = e1 :>>= p :-> f e2
    f e = e :>>= lam
    g (p :-> e) = p :-> f e

ref_tag =  (toAtom "CData.IORef.IORef")

isMutableNodeTag t = t == ref_tag

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
