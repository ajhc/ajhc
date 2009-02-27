{-# OPTIONS_GHC -funbox-strict-fields #-}

module Grin.Grin(
    Callable(..),
    Exp(..),
    FuncDef(..),
    FuncProps(..),
    Grin(..),
    TyThunk(..),
    Lam(..),
    Phase(..),
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
    grinFuncs,
    emptyGrin,
    tyINode,
    tyDNode,
    findArgs,
    findArgsType,
    findTyTy,
    funcApply,
    funcEval,
    gEval,
    grinEntryPointNames,
    isHole,
    isValUnknown,
    isVar,
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
    v0,v1,v2,v3,lamExp,lamBind,
    valIsNF
    ) where

import Char
import Control.Monad.Identity
import Data.Monoid
import List(isPrefixOf)
import Prelude
import qualified Data.Map as Map
import qualified Data.Set as Set

import StringTable.Atom
import C.FFI
import C.Prims
import Doc.DocLike
import GenUtil
import Name.VConsts
import Cmm.Number
import Options
import Support.CanType
import Support.FreeVars
import Util.Perhaps
import qualified Info.Info as Info
import qualified Cmm.Op as Op
import qualified Stats

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
    tyReturn :: [Ty],
    tyThunk :: TyThunk,
    tySiblings :: Maybe [Atom]
}

tyTy = TyTy { tySlots = [], tyReturn = [], tySiblings = Nothing, tyThunk = TyNotThunk }

newtype TyEnv = TyEnv (Map.Map Atom TyTy)
    deriving(Monoid)


tagHole = toAtom "@hole"
funcApply = toAtom "@apply"
funcEval = toAtom "@eval"

gEval :: Val -> Exp
gEval x = App funcEval [x] [TyNode]

-- lazy node sptr_t
tyINode = TyPtr TyNode
-- strict node wptr_t
tyDNode = TyNode


instance TypeNames Ty where
    tIntzh = TyPrim (Op.bits32) -- Ty (toAtom "int")
    tEnumzh = TyPrim (Op.bits16) -- Ty (toAtom "int")
    tCharzh = TyPrim (Op.bits32) -- Ty (toAtom "HsChar")
    --tStar = Ty (toAtom "*")

data Callable = Continuation | Function | Closure | LocalFunction | Primitive'
    deriving(Eq,Ord,Show)


type Tag = Atom

newtype Var = V Int
    deriving(Eq,Ord,Enum)

instance Show Var where
    showsPrec _ (V n) xs = 'v':shows n xs



infixr 1  :->, :>>=


data Lam = [Val] :-> Exp
    deriving(Eq,Ord,Show)

data Exp =
     Exp :>>= Lam                                                         -- ^ Sequencing - the same as >>= for monads.
    | App       { expFunction  :: Atom,
                  expArgs :: [Val],
                  expType :: [Ty] }                                       -- ^ Application of functions and builtins
    | Prim      { expPrimitive :: APrim,
                  expArgs :: [Val],
                  expType :: [Ty] }                                       -- ^ Primitive operation
    | Case      { expValue :: Val, expAlts :: [Lam] }                     -- ^ Case statement
    | Return    { expValues :: [Val] }                                    -- ^ Return a value
    | Store     { expValue :: Val }                                       -- ^ Allocate a new heap node
    | Fetch     { expAddress :: Val }                                     -- ^ Load given heap node
    | Update    { expAddress :: Val, expValue :: Val }                    -- ^ Update given heap node
    | Error     { expError :: String, expType :: [Ty] }                   -- ^ Abort with an error message, non recoverably.
    | Call      { expValue :: Val,
                  expArgs :: [Val],
                  expType :: [Ty],
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
                  expNonNormal :: Set.Set Atom,                           -- ^ cache, a superset of functions called in non-tail call position.
                  expInfo :: Info.Info }                                  -- ^ A let of local functions
    | MkClosure { expValue :: Val,
                  expArgs :: [Val],
                  expRegion :: Val,
                  expType :: [Ty],
                  expInfo :: Info.Info }                   -- ^ create a closure
    | MkCont    { expCont :: Lam,                          -- ^ the continuation routine
                  expLam :: Lam,                           -- ^ the computation that is passed the newly created computation
                  expInfo :: Info.Info }                   -- ^ Make a continuation, always allocated on region encompasing expLam
    deriving(Eq,Show,Ord)

data Val =
    NodeC !Tag [Val]          -- ^ Complete node with constant tag
    | Const Val               -- ^ pointer to constant data, only Lit, Tag, and NodeC may be children
    | Lit !Number Ty          -- ^ Literal
    | Var !Var Ty             -- ^ Variable
    | Unit                    -- ^ Empty value used as placeholder
    | ValPrim APrim [Val] Ty  -- ^ Primitive value
    | Index Val Val           -- ^ A pointer incremented some number of values (Index v 0) == v
    | Item Atom Ty            -- ^ Specific named thing. function, global, region, etc..
    | ValUnknown Ty           -- ^ Unknown value
    deriving(Eq,Ord)

data Ty =
    TyPtr Ty                 -- ^ pointer to a heap location which contains its argument
    | TyNode                   -- ^ a whole tagged node
    | TyPrim Op.Ty             -- ^ a basic type
    | TyUnit                   -- ^ type of Unit
    | TyCall Callable [Ty] [Ty]  -- ^ something call,jump, or cut-to-able
    | TyRegion                 -- ^ a region
    | TyUnknown                -- ^ an unknown possibly undefined type, All of these must be eliminated by code generation
    deriving(Eq,Ord)



data FuncDef = FuncDef {
    funcDefName  :: Atom,
    funcDefBody  :: Lam,
    funcDefCall  :: Val,
    funcDefProps :: FuncProps
    } deriving(Eq,Ord,Show)

createFuncDef local name body@(args :-> rest)  = updateFuncDefProps FuncDef { funcDefName = name, funcDefBody = body, funcDefCall = call, funcDefProps = funcProps } where
    call = Item name (TyCall (if local then LocalFunction else Function) (map getType args) (getType rest))


updateFuncDefProps fd@FuncDef { funcDefBody = body@(args :-> rest) } =  fd { funcDefProps = props } where
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
    funcType    :: ([Ty],[Ty]),
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
    funcType = undefined,
    funcExits = Maybe,
    funcCuts = Maybe,
    funcAllocs = Maybe,
    funcCreates = Maybe,
    funcLoops = Maybe
    }


instance Show Ty where
    show TyNode = "N"
    show (TyPtr t) = '&':show t
    show (TyUnit) = "()"
    show (TyPrim t) = show t
    show TyRegion = "R"
    show (TyCall c as rt) = show c <> tupled (map show as) <+> "->" <+> show rt
    show TyUnknown = "?"


instance Show Val where
    -- showsPrec _ s | Just st <- fromVal s = text $ show (st::String)
    showsPrec _ (NodeC t []) = parens $ (fromAtom t)
    showsPrec _ (NodeC t vs) = parens $ (fromAtom t) <+> hsep (map shows vs)
    showsPrec _ (Index v o) = shows v <> char '[' <> shows o <> char ']'
    showsPrec _ (Var (V i) t)
        | TyPtr TyNode <- t = text "ni" <> tshow i
        | TyNode <- t = text "nd" <> tshow i
        | TyPtr (TyPtr TyNode) <- t = text "np" <> tshow i
        | TyPrim Op.TyBool <- t  = char 'b' <> tshow i
        | TyPrim (Op.TyBits _ Op.HintFloat) <- t  = char 'f' <> tshow i
        | TyPrim (Op.TyBits _ Op.HintCharacter) <- t  = char 'c' <> tshow i
        | TyPrim (Op.TyBits (Op.Bits 8)  _) <- t  = char 'o' <> tshow i      -- octet
        | TyPrim (Op.TyBits (Op.Bits 16)  _) <- t  = char 'h' <> tshow i     -- half
        | TyPrim (Op.TyBits (Op.Bits 32)  _) <- t  = char 'w' <> tshow i     -- word
        | TyPrim (Op.TyBits (Op.Bits 64)  _) <- t  = char 'd' <> tshow i     -- doubleword
        | TyPrim (Op.TyBits (Op.Bits 128)  _) <- t  = char 'q' <> tshow i    -- quadword
        | TyPrim (Op.TyBits (Op.BitsArch Op.BitsPtr)  _) <- t  = char 'p' <> tshow i
        | TyPrim (Op.TyBits (Op.BitsArch Op.BitsMax)  _) <- t  = char 'm' <> tshow i
        | TyPrim (Op.TyBits _ _) <- t  = char 'l' <> tshow i
        | otherwise = char 'v' <> tshow i
    showsPrec _ (Lit i _)  = tshow i
    showsPrec _ Unit  = showString "()"
    showsPrec _ (Const v) = char '&' <> shows v
    showsPrec _ (Item a  ty) = tshow a <> text "::" <> tshow ty
    showsPrec _ (ValUnknown ty) = text "?::" <> tshow ty
    showsPrec _ (ValPrim aprim xs _) = tshow aprim <> tupled (map tshow xs)

data Phase = PhaseInit | PostInlineEval | PostAeOptimize | PostDevolve
    deriving(Show,Eq,Ord,Enum)

phaseEvalInlined e = e >= PostInlineEval


data Grin = Grin {
    grinEntryPoints :: Map.Map Atom FfiExport,
    grinPhase :: !Phase,
    grinTypeEnv :: TyEnv,
    grinFunctions :: [FuncDef],
    grinSuspFunctions :: Set.Set Atom,
    grinPartFunctions :: Set.Set Atom,
    grinStats :: !Stats.Stat,
    grinCafs :: [(Var,Val)]
}


emptyGrin = Grin {
    grinEntryPoints = mempty,
    grinPhase = PhaseInit,
    grinTypeEnv = mempty,
    grinFunctions = [],
    grinSuspFunctions = mempty,
    grinPartFunctions = mempty,
    grinStats = mempty,
    grinCafs = mempty
}

grinEntryPointNames = Map.keys . grinEntryPoints


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
    | 'F':xs <- t' = return $ toAtom $ 'f':xs
    | 'B':xs <- t' = return $ toAtom $ 'b':xs
    | 'f':_ <- t' = return t
    | 'b':_ <- t' = return t
    | 'P':is <- t', ('_':xs) <- dropWhile isDigit is = return $ toAtom $ 'f':xs
    | otherwise = fail $ "Not Function: " ++ t'
    where t' = fromAtom t

tagIsFunction t
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
valIsNF Const {} = True
valIsNF Lit {} = True
valIsNF _ = False

properHole x = case x of
    TyPtr TyNode -> Const (properHole TyNode)
    ty@(TyPrim _) -> (Lit 0 ty)
    ~TyNode -> (NodeC tagHole [])

isHole x = x `elem` map properHole [TyPtr TyNode, TyNode]

isValUnknown ValUnknown {} = True
isValUnknown _ = False


---------
-- Look up stuff in the typing environment.
---------

findTyTy (TyEnv m) a | Just tyty <-  Map.lookup a m = return tyty
findTyTy (TyEnv m) a | ('Y':rs) <- fromAtom a, (ns,'_':rs) <- span isDigit rs  = case Map.lookup (toAtom ('T':rs)) m of
    Just TyTy { tySlots = ts, tyReturn = n } -> return tyTy { tySlots = take (length ts - read ns) ts, tyReturn = n }
    Nothing -> fail $ "findArgsType: " ++ show a
findTyTy _ a | "@hole" `isPrefixOf` fromAtom a  = return tyTy { tySlots = [], tyReturn = [TyNode] }
findTyTy _ a =  fail $ "findArgsType: " ++ show a

findArgsType m a = liftM (\tyty -> (tySlots tyty,tyReturn tyty)) (findTyTy m a)

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


instance CanType e t => CanType [e] [t] where
    getType es = map getType es

instance CanType Exp [Ty] where
    getType (_ :>>= (_ :-> e2)) = getType e2
    getType (Prim _ _ ty) = ty
    getType App { expType = t } = t
    getType (Store v) = [TyPtr (getType v)]
    getType (Return v) = getType v
    getType (Fetch v) = case getType v of
        TyPtr t -> [t]
        _ -> error "Exp.getType: fetch of non-pointer type"
    getType (Error _ t) = t
    getType (Update w v) = []
    getType (Case _ []) = error "empty case"
    getType (Case _ ((_ :-> e):_)) = getType e
    getType NewRegion { expLam = _ :-> body } = getType body
    getType Alloc { expValue = v } = [TyPtr (getType v)]
    getType Let { expBody = body } = getType body
    getType MkCont { expLam = _ :-> rbody } = getType rbody
    getType Call { expType = ty } = ty
    getType MkClosure { expType = ty } = ty

instance CanType Val Ty where
    getType (Var _ t) = t
    getType (Lit _ t) = t
    getType (Index v _) = getType v
    getType Unit = TyUnit
    getType (Const t) = TyPtr (getType t)
    getType (NodeC {}) = TyNode
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
    freeVars (Const v) = freeVars v
    freeVars (Index a b) = freeVars (a,b)
    freeVars (Var v _) = Set.singleton v
    freeVars _ = Set.empty

instance FreeVars Val (Set.Set (Var,Ty)) where
    freeVars (NodeC t xs) = freeVars xs
    freeVars (Const v) = freeVars v
    freeVars (Index a b) = freeVars (a,b)
    freeVars (Var v t) = Set.singleton (v,t)
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
    freeVars (Prim _ x _) = freeVars x
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
    freeVars (Prim _ x _) = freeVars x
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
    freeVars (Index a b) = freeVars (a,b)
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
    freeVars (Prim _ x _) = freeVars x
    freeVars Error {} = Set.empty
    freeVars Let { expDefs = fdefs, expBody = body } = mconcat (map (funcTags . funcDefProps) fdefs) `mappend` freeVars body
    freeVars NewRegion { expLam = l } = freeVars l
    freeVars Alloc { expValue = v, expCount = c, expRegion = r } = freeVars (v,c,r)
    freeVars Call { expValue = v, expArgs = as } = freeVars (v:as)
    freeVars MkClosure { expValue = v, expArgs = as, expRegion = r } = freeVars (v,as,r)
    freeVars MkCont { expCont = v, expLam = as} = freeVars (v,as)


lamExp (_ :-> e) = e
lamBind (b :-> _) = b

isVar Var {} = True
isVar _ = False




