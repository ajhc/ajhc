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
    BaseOp(..),
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
    tagInfo,
    TagInfo(..),
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

gEval :: Val -> Exp
gEval x = BaseOp Eval [x]

-- lazy node sptr_t
tyINode = TyINode
-- strict node wptr_t
tyDNode = TyNode


instance TypeNames Ty where
    tIntzh = TyPrim (Op.bits32)
    tEnumzh = TyPrim (Op.bits16)
    tCharzh = TyPrim (Op.bits32)

data Callable = Continuation | Function | Closure | LocalFunction | Primitive'
    deriving(Eq,Ord,Show)


type Tag = Atom

newtype Var = V Int
    deriving(Eq,Ord,Enum)

instance Show Var where
    showsPrec _ (V n) xs = 'v':shows n xs



{-

data VCont = VCont Val VContext

data VContext = PrimApp PrimApp VCont | Decons Tag Int VCont | ContUnknown


-}

infixr 1  :->, :>>=

-- The basic operations of our monad
--
-- PeekVal and PokeVal differ from the primitive peek and poke in that the Val
-- varients operate on node references, while the primitive versions work on
-- raw memory with unboxed pointers.
--

data BaseOp
    = Demote                -- turn a node into an inode, always okay
    | Promote               -- turn an inode into a node, the inode _must_ already be a valid node
    | Eval                  -- evaluate an inode, returns a node representing the evaluated value. Bool is whether to update the inode
    | Apply [Ty]            -- apply a partial application to a value, returning the given type
    | StoreNode !Bool       -- create a new node, Bool is true if it should be an direct node, the second val is the region
    | Redirect              -- write an indirection over its first argument to point to its second one
    | Overwrite             -- overwrite an existing node with new data (the tag must match what was used for the initial Store)
    | PeekVal               -- read a value from a pointed to location
    | PokeVal               -- write a value to a pointed to location
    | Consume               -- consume a value, depending on the back end this may be used to free memory
    deriving(Eq,Ord,Show)

data Lam = [Val] :-> Exp
    deriving(Eq,Ord,Show)

data Exp =
     Exp :>>= Lam                                                         -- ^ Sequencing - the same as >>= for monads.
    | BaseOp    { expBaseOp :: BaseOp,
                  expArgs :: [Val]
                }
    | App       { expFunction  :: Atom,
                  expArgs :: [Val],
                  expType :: [Ty] }                                       -- ^ Application of functions and builtins
    | Prim      { expPrimitive :: APrim,
                  expArgs :: [Val],
                  expType :: [Ty] }                                       -- ^ Primitive operation
    | Case      { expValue :: Val, expAlts :: [Lam] }                     -- ^ Case statement
    | Return    { expValues :: [Val] }                                    -- ^ Return a value
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
    | GcRoots   { expValues :: [Val],                  -- ^ add some new variables to the GC roots for a subcomputation
                  expBody :: Exp }
    deriving(Eq,Show,Ord)

data Val =
    NodeC !Tag [Val]          -- ^ Complete node, of type TyNode
    | Const Val               -- ^ constant data, only Lit, Const and NodeC may be children. of type TyINode
    | Lit !Number Ty          -- ^ Literal
    | Var !Var Ty             -- ^ Variable
    | Unit                    -- ^ Empty value used as placeholder
    | ValPrim APrim [Val] Ty  -- ^ Primitive value
    | Index Val Val           -- ^ A pointer incremented some number of values (Index v 0) == v
    | Item Atom Ty            -- ^ Specific named thing. function, global, region, etc..
    | ValUnknown Ty           -- ^ Unknown or unimportant value
    deriving(Eq,Ord)

data Ty =
    TyPtr Ty                     -- ^ pointer to a memory location which contains its argument
    | TyNode                     -- ^ a whole node
    | TyINode                    -- ^ a whole possibly indirect node
    | TyAttr Ty Ty               -- ^ attach an attribute to a type
    | TyAnd Ty Ty                -- ^ boolean conjunction of types
    | TyOr  Ty Ty                -- ^ boolean disjunction of types
    | TyPrim Op.Ty               -- ^ a basic type
    | TyUnit                     -- ^ type of Unit
    | TyCall Callable [Ty] [Ty]  -- ^ something call,jump, or cut-to-able
    | TyRegion                   -- ^ a region
    | TyUnknown                  -- ^ an unknown possibly undefined type, All of these must be eliminated by code generation
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
setGrinFunctions xs _grin | flint && hasRepeatUnder fst xs = error $ "setGrinFunctions: grin has redundent definitions" ++ show (fsts xs)
setGrinFunctions xs grin = grin { grinFunctions = map (uncurry (createFuncDef False)) xs }


extendTyEnv ds (TyEnv env) = TyEnv (Map.fromList xs `mappend` env) where
    xs = [ (funcDefName d,tyTy { tySlots = ss, tyReturn = r }) |  d <- ds, let (ss,r) = funcType $ funcDefProps d]
      ++ [ (tagFlipFunction (funcDefName d),tyTy { tySlots = ss, tyReturn = r }) |  d <- ds, let (ss,r) = funcType $ funcDefProps d, r == [TyNode]]

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
    show TyINode = "I"
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
        | TyINode <- t = text "ni" <> tshow i
        | TyNode <- t = text "nd" <> tshow i
        | TyRegion <- t = text "r" <> tshow i
--        | TyPtr TyINode <- t = text "np" <> tshow i
        | TyPtr t' <- t = text "p" <> shows (Var (V i) t')
        | TyPrim Op.TyBool <- t  = char 'b' <> tshow i
        | TyPrim (Op.TyBits _ Op.HintFloat) <- t  = char 'f' <> tshow i
        | TyPrim (Op.TyBits _ Op.HintCharacter) <- t  = char 'c' <> tshow i
        | TyPrim (Op.TyBits (Op.Bits 8)  _) <- t  = char 'o' <> tshow i      -- octet
        | TyPrim (Op.TyBits (Op.Bits 16)  _) <- t  = char 'h' <> tshow i     -- half
        | TyPrim (Op.TyBits (Op.Bits 32)  _) <- t  = char 'w' <> tshow i     -- word
        | TyPrim (Op.TyBits (Op.Bits 64)  _) <- t  = char 'd' <> tshow i     -- doubleword
        | TyPrim (Op.TyBits (Op.Bits 128)  _) <- t  = char 'q' <> tshow i    -- quadword
        | TyPrim (Op.TyBits (Op.BitsArch Op.BitsPtr)  _) <- t  = text "bp" <> tshow i
        | TyPrim (Op.TyBits (Op.BitsArch Op.BitsMax)  _) <- t  = text "bm" <> tshow i
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

data TagInfo
    = TagPApp !Int !Atom   -- partial application, number is how many more arguments needed
    | TagSusp !Bool !Atom  -- a suspended version of the function, true if an update is required
    | TagDataCons          -- data constructor
    | TagTypeCons          -- type constructor
    | TagTypePApp !Int Tag -- type partial app
    | TagFunc

tagInfo t = case fromAtom t of
    'F':xs ->  TagSusp True (toAtom $ 'f':xs)
    'B':xs ->  TagSusp True (toAtom $ 'b':xs)
    'f':_  -> TagFunc
    'b':_  -> TagFunc
    'P':is | (n@(_:_),('_':xs)) <- span isDigit is -> TagPApp (read n) (toAtom $ 'f':xs)
    'Y':is | (n@(_:_),('_':xs)) <- span isDigit is -> TagTypePApp (read n) (toAtom $ 'T':xs)
    'C':_ -> TagDataCons
    'T':_ -> TagTypeCons
    t -> error $ "tagInfo: bad tag " ++  t


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
    TyINode -> Const (properHole TyNode)
    ty@(TyPrim _) -> (Lit 0 ty)
    ~TyNode -> (NodeC tagHole [])

isHole x = x `elem` map properHole [TyINode, TyNode]

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

p0 = Var v0 TyINode
p1 = Var v1 TyINode
p2 = Var v2 TyINode
p3 = Var v3 TyINode


instance CanType e t => CanType [e] [t] where
    getType es = map getType es

instance CanType Exp [Ty] where
    getType (_ :>>= (_ :-> e2)) = getType e2
    getType (Prim _ _ ty) = ty
    getType App { expType = t } = t
    getType (BaseOp Overwrite _) = []
    getType (BaseOp Redirect _) = []
    getType (BaseOp Promote _) = [TyNode]
    getType (BaseOp Demote _) = [TyINode]
    getType (BaseOp Eval _) = [TyNode]
    getType (BaseOp (StoreNode b) _) = if b then [TyNode] else [TyINode]
    getType (BaseOp (Apply ty) _) = ty
    getType (BaseOp PeekVal [v]) = case getType v of
        TyPtr t -> [t]
        _ -> error "Exp.getType: PeekVal of non-pointer type"
    getType (Return v) = getType v
    getType (Error _ t) = t
    getType (Case _ []) = error "empty case"
    getType (Case _ ((_ :-> e):_)) = getType e
    getType NewRegion { expLam = _ :-> body } = getType body
    getType Alloc { expValue = v } = [TyPtr (getType v)]
    getType Let { expBody = body } = getType body
    getType MkCont { expLam = _ :-> rbody } = getType rbody
    getType Call { expType = ty } = ty
    getType MkClosure { expType = ty } = ty
    getType GcRoots { expBody = body } = getType body

instance CanType Val Ty where
    getType (Var _ t) = t
    getType (Lit _ t) = t
    getType (Index v _) = getType v
    getType Unit = TyUnit
    getType (Const t) = case (getType t) of
        TyNode -> TyINode
        t -> TyPtr t
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
--    freeVars (Store v) = freeVars v
    freeVars (BaseOp _ vs) = freeVars vs
    freeVars (Prim _ x _) = freeVars x
    freeVars Error {} = Set.empty
    freeVars Let { expDefs = fdefs, expBody = body } = mconcat (map (funcFreeVars . funcDefProps) fdefs) `mappend` freeVars body
    freeVars NewRegion { expLam = l } = freeVars l
    freeVars Alloc { expValue = v, expCount = c, expRegion = r } = freeVars (v,c,r)
    freeVars Call { expValue = v, expArgs = as } = freeVars (v:as)
    freeVars MkClosure { expValue = v, expArgs = as, expRegion = r } = freeVars (v,as,r)
    freeVars MkCont { expCont = v, expLam = as} = freeVars (v,as)
    freeVars GcRoots { expValues = v, expBody = b } = freeVars (v,b)

instance FreeVars Exp (Set.Set (Var,Ty)) where
    freeVars (a :>>= b) = freeVars (a,b)
    freeVars (App a vs _) =  freeVars vs
    freeVars (Case x xs) = freeVars (x,xs)
    freeVars (Return v) = freeVars v
--    freeVars (Store v) = freeVars v
    freeVars (BaseOp _ vs) = freeVars vs
    freeVars (Prim _ x _) = freeVars x
    freeVars Error {} = Set.empty
    freeVars Let { expDefs = fdefs, expBody = body } = mconcat (map (freeVars . funcDefBody) fdefs) `mappend` freeVars body
    freeVars NewRegion { expLam = l } = freeVars l
    freeVars Alloc { expValue = v, expCount = c, expRegion = r } = freeVars (v,c,r)
    freeVars Call { expValue = v, expArgs = as } = freeVars (v:as)
    freeVars MkClosure { expValue = v, expArgs = as, expRegion = r } = freeVars (v,as,r)
    freeVars MkCont { expCont = v, expLam = as} = freeVars (v,as)
    freeVars GcRoots { expValues = v, expBody = b } = freeVars (v,b)

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
--    freeVars (Store v) = freeVars v
    freeVars (BaseOp _ vs) = freeVars vs
    freeVars (Prim _ x _) = freeVars x
    freeVars Error {} = Set.empty
    freeVars Let { expDefs = fdefs, expBody = body } = mconcat (map (funcTags . funcDefProps) fdefs) `mappend` freeVars body
    freeVars NewRegion { expLam = l } = freeVars l
    freeVars Alloc { expValue = v, expCount = c, expRegion = r } = freeVars (v,c,r)
    freeVars Call { expValue = v, expArgs = as } = freeVars (v:as)
    freeVars MkClosure { expValue = v, expArgs = as, expRegion = r } = freeVars (v,as,r)
    freeVars MkCont { expCont = v, expLam = as} = freeVars (v,as)
    freeVars GcRoots { expValues = v, expBody = b } = freeVars (v,b)


lamExp (_ :-> e) = e
lamBind (b :-> _) = b

isVar Var {} = True
isVar _ = False




