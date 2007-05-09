{-# OPTIONS_GHC -funbox-strict-fields #-}

module Grin.Grin(
    Callable(..),
    Exp(..),
    FuncDef(..),
    FuncProps(..),
    Grin(..),
    HeapType(..),
    TyThunk(..),
    HeapValue(HV),
    Item(..),
    Lam(..),
    NodeValue(NV),
    Phase(..),
    Primitive(..),
    Tag,
    updateFuncDefProps,
    Ty(..),
    TyEnv(..),
    TyTy(..),
    tyTy,
    Val(..),
    Var(..),
    extendTyEnv,
    createFuncDef,
    setGrinFunctions,
    combineItems,
    grinFuncs,
    emptyGrin,
    findArgs,
    findArgsType,
    findTyTy,
    funcApply,
    funcEval,
    funcFetch,
    funcInitCafs,
    funcMain,
    gEval,
    grinEntryPointNames,
    isHole,
    isValUnknown,
    isVar,isTup,
    itemTag,
    n0,n1,n2,n3,
    p0,p1,p2,p3,
    partialTag,
    phaseEvalInlined,
    properHole,
    tagFlipFunction,
    tagHole,
    tagIsFunction,
    tagIsPartialAp,
    tagIsSuspFunction,
    tagIsTag,
    tagIsWHNF,
    tagToFunction,
    tagUnfunction,
    tyUnit,
    unit,
    v0,v1,v2,v3,lamExp,lamBind,
    valToItem,
    valIsNF
    ) where

import Char
import Control.Monad.Identity
import Data.IORef
import Data.Monoid
import List(isPrefixOf)
import Prelude
import qualified Data.Map as Map
import qualified Data.Set as Set

import Atom
import C.FFI
import C.Prims
import Doc.DocLike
import GenUtil
import Name.VConsts
import Number
import Options
import Support.CanType
import Support.FreeVars
import Support.Tuple
import Util.Perhaps
import qualified Info.Info as Info

-- Extremely simple first order monadic code with basic type system.  similar
-- to GRIN except for the explicit typing on variables. Note, that certain
-- haskell types become Grin values, however, nothing may be done with types other
-- than examining them. (types may not be constructed at run-time) ( do we need
-- this for polymorphic recursion? )

data TyThunk =
    TyNotThunk                 -- ^ not the thunk
    | TyPApp (Maybe Ty) Atom   -- ^ can be applied to (possibly) an argument, and what results
    | TySusp Atom              -- ^ can be evaluated and calls what function
    deriving(Eq,Show)

data TyTy = TyTy {
    tySlots :: [Ty],
    tyReturn :: Ty,
    tyThunk :: TyThunk,
    tySiblings :: Maybe [Atom]
}

tyTy = TyTy { tySlots = [], tyReturn = TyUnknown, tySiblings = Nothing, tyThunk = TyNotThunk }

newtype TyEnv = TyEnv (Map.Map Atom TyTy)
    deriving(Monoid)


tagHole = toAtom "@hole"
funcApply = toAtom "@apply"
funcEval = toAtom "@eval"
funcFetch = toAtom "@fetch"
funcInitCafs = toAtom "@initcafs"
funcMain = toAtom "@main"

gEval :: Val -> Exp
gEval x = App funcEval [x] TyNode


instance TypeNames Ty where
    tIntzh = Ty (toAtom "int")
    tCharzh = Ty (toAtom "HsChar")
    tStar = Ty (toAtom "*")

data Callable = Continuation | Function | Closure | LocalFunction | Primitive'
    deriving(Eq,Ord,Show)


type Tag = Atom

newtype Var = V Int
    deriving(Eq,Ord,Enum)

instance Show Var where
    showsPrec _ (V n) xs = 'v':shows n xs



infixr 1  :->, :>>=


tyUnit = TyTup []
unit = Tup []

data Lam = Val :-> Exp
    deriving(Eq,Ord,Show)

data Exp =
     Exp :>>= Lam                                                         -- ^ Sequencing - the same as >>= for monads.
    | App       { expFunction  :: Atom, expArgs :: [Val], expType :: Ty } -- ^ Application of functions and builtins
    | Prim      { expPrimitive :: Primitive, expArgs :: [Val] }           -- ^ Primitive operation
    | Case      { expValue :: Val, expAlts :: [Lam] }                     -- ^ Case statement
    | Return    { expValue :: Val }                                       -- ^ Return a value
    | Store     { expValue :: Val }                                       -- ^ Allocate a new heap node
    | Fetch     { expAddress :: Val }                                     -- ^ Load given heap node
    | Update    { expAddress :: Val, expValue :: Val }                    -- ^ Update given heap node
    | Error     { expError :: String, expType :: Ty }                     -- ^ Abort with an error message, non recoverably.
    | Call      { expValue :: Val,
                  expArgs :: [Val],
                  expType :: Ty,
                  expJump :: Bool,                                        -- ^ Jump is equivalent to a call except it deallocates the region it resides in before transfering control
                  expFuncProps :: FuncProps,
                  expInfo :: Info.Info }                                  -- ^ Call or jump to a callable
    | NewRegion { expLam :: Lam, expInfo :: Info.Info }                   -- ^ create a new region and pass it to its argument
    | Alloc     { expValue :: Val,
                  expCount :: Val,
                  expRegion :: Val,
                  expInfo :: Info.Info }                                  -- ^ allocate space for a number of values in the given region
    | Let       { expDefs :: [FuncDef],
                  expBody :: Exp,
                  expFuncCalls :: (Set.Set Atom,Set.Set Atom),            -- ^ cache
                  expIsNormal :: Bool,                                    -- ^ cache, True = definitely normal, False = maybe normal
                  expInfo :: Info.Info }                                  -- ^ A let of local functions
    | MkClosure { expValue :: Val,
                  expArgs :: [Val],
                  expRegion :: Val,
                  expType :: Ty,
                  expInfo :: Info.Info }                   -- ^ create a closure
    | MkCont    { expCont :: Lam,                          -- ^ the continuation routine
                  expLam :: Lam,                           -- ^ the computation that is passed the newly created computation
                  expInfo :: Info.Info }                   -- ^ Make a continuation, always allocated on region encompasing expLam
    deriving(Eq,Show,Ord)

data Val =
    NodeC !Tag [Val]          -- ^ Complete node with constant tag
    | NodeV !Var [Val]        -- ^ Complete node with variable tag
    | Tag !Tag                -- ^ Single tag
    | Const Val               -- ^ pointer to constant data, only Lit, Tag, and NodeC may be children
    | Lit !Number Ty          -- ^ Literal
    | Var !Var Ty             -- ^ Variable
    | Tup [Val]               -- ^ Unboxed tuple
    | ValPrim APrim [Val] Ty  -- ^ Primitive value
    | Index Val Val           -- ^ A pointer incremented some number of values (Index v 0) == v
    | Item Atom Ty            -- ^ Specific named thing. function, global, region, etc..
    | ValUnknown Ty           -- ^ Unknown or empty value
    | Addr {-# UNPACK #-} !(IORef Val)  -- ^ Used only in interpreter
    deriving(Eq,Ord)

data Ty =
    TyTag                      -- ^ a lone tag
    | TyPtr Ty                 -- ^ pointer to a heap location which contains its argument
    | TyNode                   -- ^ a whole tagged node
    | Ty Atom                  -- ^ a basic type
    | TyTup [Ty]               -- ^ unboxed list of values
    | TyCall Callable [Ty] Ty  -- ^ something call,jump, or cut-to-able
    | TyRegion                 -- ^ a region
    | TyUnknown                -- ^ an unknown possibly undefined type, All of these must be eliminated by code generation
    deriving(Eq,Ord)


data FuncDef = FuncDef {
    funcDefName  :: Atom,
    funcDefBody  :: Lam,
    funcDefCall  :: Val,
    funcDefProps :: FuncProps
    } deriving(Eq,Ord,Show)

createFuncDef local name body@(Tup args :-> rest)  = updateFuncDefProps FuncDef { funcDefName = name, funcDefBody = body, funcDefCall = call, funcDefProps = funcProps } where
    call = Item name (TyCall (if local then LocalFunction else Function) (map getType args) (getType rest))


updateFuncDefProps fd@FuncDef { funcDefBody = body@(Tup args :-> rest) } =  fd { funcDefProps = props } where
    props = (funcDefProps fd) { funcFreeVars = freeVars body, funcTags = freeVars body, funcType = (map getType args,getType rest) }

grinFuncs grin = map (\x -> (funcDefName x, funcDefBody x)) (grinFunctions grin)
setGrinFunctions xs _grin | flint && hasRepeatUnder fst xs = error $ "setGrinFunctions: grin has redundent defeninitions" ++ show (fsts xs)
setGrinFunctions xs grin = grin { grinFunctions = map (uncurry (createFuncDef False)) xs }


extendTyEnv ds (TyEnv env) = TyEnv (Map.fromList xs `mappend` env) where
    xs = [ (funcDefName d,tyTy { tySlots = ss, tyReturn = r }) |  d <- ds, let (ss,r) = funcType $ funcDefProps d]

-- cached info
data FuncProps = FuncProps {
    funcInfo    :: Info.Info,
    funcFreeVars :: Set.Set Var,
    funcTags    :: Set.Set Tag,
    funcType    :: ([Ty],Ty),
    funcExits   :: Perhaps,      -- ^ function quits the program
    funcCuts    :: Perhaps,      -- ^ function cuts to a value
    funcAllocs  :: Perhaps,      -- ^ function allocates memory
    funcCreates :: Perhaps,      -- ^ function allocates memory and stores or returns it
    funcLoops   :: Perhaps       -- ^ function may loop
    }
    deriving(Eq,Ord,Show)

funcProps = FuncProps {
    funcInfo = mempty,
    funcFreeVars = mempty,
    funcTags = mempty,
    funcExits = Maybe,
    funcCuts = Maybe,
    funcAllocs = Maybe,
    funcCreates = Maybe,
    funcLoops = Maybe
    }


instance Show Ty where
    show TyTag = "T"
    show (Ty a) = fromAtom a
    show TyNode = "N"
    show (TyPtr t) = '&':show t
    show (TyTup []) = "()"
    show (TyTup ts) =  tupled (map show ts)
    show TyRegion = "R"
    show (TyCall c as rt) = show c <> tupled (map show as) <+> "->" <+> show rt
    show TyUnknown = "?"

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
    showsPrec _ (Index v o) = shows v <> char '[' <> shows o <> char ']'
    showsPrec _ (Var (V i) t)
        | TyPtr t <- t = char 'p' <> shows (Var (V i) t)
        | TyNode <- t = char 'n' <> tshow i
        | t == tCharzh = char 'c' <> tshow i
        | t == tIntzh  = char 'i' <> tshow i
        | Ty _ <- t  = char 'l' <> tshow i
        | TyTag <- t  = char 't' <> tshow i
        | otherwise = char 'v' <> tshow i
    showsPrec _ (Lit i t) | t == tCharzh, Just x <- toIntegral i = tshow (chr x)
    showsPrec _ (Lit i _)  = tshow i
    showsPrec _ (Tup xs)  = tupled $ map shows xs
    showsPrec _ (Const v) = char '&' <> shows v
    showsPrec _ (Item a  ty) = tshow a <> text "::" <> tshow ty
    showsPrec _ (ValUnknown ty) = text "?::" <> tshow ty
    showsPrec _ (Addr _) = text "<ref>"
    showsPrec _ (ValPrim aprim xs _) = tshow aprim <> tupled (map tshow xs)

data Phase = PhaseInit | PostInlineEval | PostAeOptimize | PostDevolve
    deriving(Show,Eq,Ord,Enum)

phaseEvalInlined e = e >= PostInlineEval


data Grin = Grin {
    grinEntryPoints :: Map.Map Atom FfiExport,
    grinPhase :: Phase,
    grinTypeEnv :: TyEnv,
    grinFunctions :: [FuncDef],
    grinReturnTags :: Map.Map Atom Item,
    grinArgTags :: Map.Map Atom [Item],
    grinSuspFunctions :: Set.Set Atom,
    grinPartFunctions :: Set.Set Atom,
    grinCafs :: [(Var,Val)]
}


emptyGrin = Grin {
    grinEntryPoints = mempty,
    grinPhase = PhaseInit,
    grinTypeEnv = mempty,
    grinFunctions = [],
    grinReturnTags = mempty,
    grinArgTags = mempty,
    grinSuspFunctions = mempty,
    grinPartFunctions = mempty,
    grinCafs = mempty
}

grinEntryPointNames = Map.keys . grinEntryPoints




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

isValUnknown ValUnknown {} = True
isValUnknown _ = False


---------
-- Look up stuff in the typing environment.
---------

findTyTy (TyEnv m) a | Just tyty <-  Map.lookup a m = return tyty
findTyTy (TyEnv m) a | ('Y':rs) <- fromAtom a, (ns,'_':rs) <- span isDigit rs  = case Map.lookup (toAtom ('T':rs)) m of
    Just TyTy { tySlots = ts, tyReturn = n } -> return tyTy { tySlots = take (length ts - read ns) ts, tyReturn = n }
    Nothing -> fail $ "findArgsType: " ++ show a
findTyTy _ a | "@hole" `isPrefixOf` fromAtom a  = return tyTy { tySlots = [], tyReturn = TyNode }
findTyTy _ a =  fail $ "findArgsType: " ++ show a

findArgsType m a = liftM (\tyty -> (tySlots tyty,tyReturn tyty)) (findTyTy m a)

findType m a = case findArgsType m a of
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
    typecheck te ap@(App fn [v] t) | fn == funcApply = do
        [v'] <- mapM (typecheck te) [v]
        if v' == TyNode then return t
         else fail $ "App apply arg doesn't match: " ++ show ap
    typecheck te ap@(App fn [v] t) | fn == funcEval = do
        v' <- typecheck te v
        if v' == TyPtr TyNode then return t
         else fail $ "App eval arg doesn't match: " ++ show ap
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
    typecheck te Alloc { expValue = v } = do
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
    typecheck te (Let { expDefs = defs, expBody = body }) = do
        let nte = extendTyEnv defs te
        mapM_ (typecheck nte) [ b | FuncDef { funcDefBody = _ :-> b } <- defs ]
        typecheck nte body

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
    typecheck te (Index v offset) = do
        t <- typecheck te v
        Ty _ <- typecheck te offset
        return t
    typecheck _ (ValUnknown ty) = return ty
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
    getType NewRegion { expLam = _ :-> body } = getType body
    getType Alloc { expValue = v } = TyPtr (getType v)
    getType Let { expBody = body } = getType body
    getType MkCont { expLam = _ :-> rbody } = getType rbody
    getType Call { expType = ty } = ty

instance CanType Val Ty where
    getType (Tag _) = TyTag
    getType (Var _ t) = t
    getType (Lit _ t) = t
    getType (Index v _) = getType v
    getType (NodeV {}) = TyNode
    getType (Tup xs) = TyTup (map getType xs)
    getType (Const t) = TyPtr (getType t)
    getType (NodeC {}) = TyNode
    getType (Addr _) = TyPtr (error "typecheck: Addr")
    getType (ValPrim _ _ ty) = ty
    getType (ValUnknown ty) = ty
    getType (Item _ ty) = ty

instance FreeVars Lam (Set.Set Var) where
    freeVars (x :-> y) = freeVars y Set.\\ freeVars x
instance FreeVars Lam (Set.Set (Var,Ty)) where
    freeVars (x :-> y) = freeVars y Set.\\ freeVars x

instance  FreeVars Exp (Set.Set Var,Set.Set Tag) where
    freeVars x = (freeVars x, freeVars x)

instance FreeVars Val (Set.Set Var) where
    freeVars (NodeC t xs) = freeVars xs
    freeVars (NodeV v xs) = Set.insert v $ freeVars xs
    freeVars (Const v) = freeVars v
    freeVars (Index a b) = freeVars (a,b)
    freeVars (Var v _) = Set.singleton v
    freeVars (Tup vs) = freeVars vs
    freeVars _ = Set.empty

instance FreeVars Val (Set.Set (Var,Ty)) where
    freeVars (NodeC t xs) = freeVars xs
    freeVars (NodeV v xs) = Set.insert (v,TyTag) $ freeVars xs
    freeVars (Const v) = freeVars v
    freeVars (Index a b) = freeVars (a,b)
    freeVars (Var v t) = Set.singleton (v,t)
    freeVars (Tup vs) = freeVars vs
    freeVars _ = Set.empty

instance FreeVars FuncProps (Set.Set Var) where
    freeVars FuncProps { funcFreeVars = fv } = fv

instance FreeVars FuncProps (Set.Set Tag) where
    freeVars FuncProps { funcTags = fv } = fv

instance FreeVars FuncProps a => FreeVars FuncDef a where
    freeVars fd = freeVars (funcDefProps fd)

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
    freeVars Let { expDefs = fdefs, expBody = body } = mconcat (map (funcFreeVars . funcDefProps) fdefs) `mappend` freeVars body
    freeVars NewRegion { expLam = l } = freeVars l
    freeVars Alloc { expValue = v, expCount = c, expRegion = r } = freeVars (v,c,r)
    freeVars Call { expValue = v, expArgs = as } = freeVars (v:as)
    freeVars MkClosure { expValue = v, expArgs = as, expRegion = r } = freeVars (v,as,r)
    freeVars MkCont { expCont = v, expLam = as} = freeVars (v,as)

instance FreeVars Exp (Set.Set (Var,Ty)) where
    freeVars (a :>>= b) = freeVars (a,b)
    freeVars (App a vs _) =  freeVars vs
    freeVars (Case x xs) = freeVars (x,xs)
    freeVars (Return v) = freeVars v
    freeVars (Store v) = freeVars v
    freeVars (Fetch v) = freeVars v
    freeVars (Update x y) = freeVars (x,y)
    freeVars (Prim _ x) = freeVars x
    freeVars Error {} = Set.empty
    freeVars Let { expDefs = fdefs, expBody = body } = mconcat (map (freeVars . funcDefBody) fdefs) `mappend` freeVars body
    freeVars NewRegion { expLam = l } = freeVars l
    freeVars Alloc { expValue = v, expCount = c, expRegion = r } = freeVars (v,c,r)
    freeVars Call { expValue = v, expArgs = as } = freeVars (v:as)
    freeVars MkClosure { expValue = v, expArgs = as, expRegion = r } = freeVars (v,as,r)
    freeVars MkCont { expCont = v, expLam = as} = freeVars (v,as)

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
    freeVars (Index a b) = freeVars (a,b)
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
    freeVars Let { expDefs = fdefs, expBody = body } = mconcat (map (funcTags . funcDefProps) fdefs) `mappend` freeVars body
    freeVars NewRegion { expLam = l } = freeVars l
    freeVars Alloc { expValue = v, expCount = c, expRegion = r } = freeVars (v,c,r)
    freeVars Call { expValue = v, expArgs = as } = freeVars (v:as)
    freeVars MkClosure { expValue = v, expArgs = as, expRegion = r } = freeVars (v,as,r)
    freeVars MkCont { expCont = v, expLam = as} = freeVars (v,as)


-- Points to information

data HeapType = Constant | SharedEval | UnsharedEval | Reference | RecursiveThunk
    deriving(Eq,Ord)


data Item = HeapValue (Set.Set HeapValue) | NodeValue (Set.Set NodeValue) | BasicValue Ty | TupledValue [Item]
    deriving(Ord,Eq)
data HeapValue = HV Int (Either (HeapType,Item) Val)  -- either a heap location or a constant
data NodeValue = NV Tag [Item]
    deriving(Ord,Eq)

valToItem (Index v _) = valToItem v
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



