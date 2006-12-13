module C.Prims where

import Data.Generics
import Data.Monoid

import Binary
import C.FFI(Requires(..))
import Doc.DocLike
import Doc.PPrint
import PackedString

data PrimTypeType = PrimTypeIntegral | PrimTypeFloating | PrimTypePointer | PrimTypeVoid
    deriving(Show,Eq,Ord)

data PrimType = PrimType {
    primTypeName :: ExtType,
    primTypeType :: PrimTypeType,
    primTypeAlignmentOf :: Int,
    primTypeIsSigned :: Bool,
    primTypeSizeOf :: Int
    } deriving(Show)

type ExtType = String

emptyExtType = ""

data Prim =
    PrimPrim String          -- Special primitive implemented in the compiler somehow.
    | CConst { primConst :: String, primRetType :: ExtType }  -- C code which evaluates to a constant
    | Operator { primOp :: String, primArgTypes ::  [ExtType], primRetType :: ExtType }   -- C operator
    | Func { funcIOLike :: Bool, funcName :: String, primArgTypes :: [ExtType], primRetType :: ExtType }   -- function call with C calling convention
    | IFunc { primArgTypes :: [ExtType], primRetType :: ExtType }-- indirect function call
    | AddrOf String                                              -- address of linker name
    | Peek { primArgType :: ExtType }                            -- read value from memory
    | Poke { primArgType :: ExtType }                            -- write value to memory
    | CCast { primArgType :: ExtType, primRetType :: ExtType }   -- Cast from one basic type to another, possibly lossy.
    | PrimTypeInfo { primArgType :: ExtType,  primRetType :: ExtType, primTypeInfo :: PrimTypeInfo }
    | PrimString PackedString                                 -- address of a raw string. encoded in utf8.
    deriving(Typeable, Data, Eq, Ord, Show)
    {-! derive: GhcBinary !-}

data PrimTypeInfo = PrimSizeOf | PrimMaxBound | PrimMinBound | PrimAlignmentOf | PrimTypeIsSigned
    deriving(Typeable, Data, Eq, Ord, Show)
    {-! derive: GhcBinary !-}

instance Data PackedString where

-- | These primitives may safely be duplicated without affecting performance or
-- correctness too adversly. either because they are cheap to begin with, or
-- will be recombined in a later pass.

primIsCheap :: Prim -> Bool
primIsCheap AddrOf {} = True
primIsCheap CCast {} = True
primIsCheap CConst {} = True
primIsCheap Operator {} = True
primIsCheap PrimTypeInfo {} = True
primIsCheap _ = False

aprimIsCheap (APrim p _) = primIsCheap p


-- | whether a primitive represents a constant expression (assuming all its arguments are constant)
-- TODO needs grin support
primIsConstant :: Prim -> Bool
primIsConstant CConst {} = True
primIsConstant AddrOf {} = True
primIsConstant PrimString {} = True
primIsConstant CCast {} = True
primIsConstant PrimTypeInfo {} = True
primIsConstant Operator { primOp = op } | op `elem` safeOps = True  where
    safeOps = ["+","-","*","==",">=","<=",">","<","&","|","^","~",">>","<<"]
primIsConstant _ = False

-- | whether a primitive can be eagarly evaluated.
-- TODO needs grin support
primEagerSafe :: Prim -> Bool
primEagerSafe CConst {} = True
primEagerSafe AddrOf {} = True
primEagerSafe PrimString {} = True
primEagerSafe CCast {} = True
primEagerSafe PrimTypeInfo {} = True
primEagerSafe Operator { primOp = op } | op `elem` safeOps = True  where
    safeOps = ["+","-","*","==",">=","<=",">","<","&","|","^","~",">>","<<"]
primEagerSafe _ = False



parsePrimString s = do
    ws@(_:_) <- return $ words s
    let v = case last ws of
            '&':s -> AddrOf s
            s -> Func False s [] emptyExtType
    let f opt@('-':'l':_) = Requires [] [opt]
        f s = Requires [s] []
    return (APrim v (mconcat (map f (init ws))))


primPrim s = APrim (PrimPrim s) mempty

data APrim = APrim Prim Requires
    deriving(Typeable, Data, Eq, Ord, Show)
    {-! derive: GhcBinary !-}

instance PPrint d Prim  => PPrint d APrim where
    pprint (APrim p _) = pprint p

instance DocLike d => PPrint d Prim where
    pprint (PrimPrim t) = text t
    pprint (CConst s t) = parens (text t) <> parens (text s)
    pprint (Operator s xs r) = parens (text r) <> text s <> tupled (map text xs)
    pprint (Func _ s xs r) = parens (text r) <> text s <> tupled (map text xs)
    pprint (IFunc xs r) = parens (text r) <> parens (char '*') <> tupled (map text xs)
    pprint (AddrOf s) = char '&' <> text s
    pprint (PrimString s) = tshow s <> char '#'
    pprint (Peek t) = char '*' <> text t
    pprint (Poke t) = char '=' <> text t
    pprint (CCast _ t) = parens (text t)
    pprint PrimTypeInfo { primArgType = at, primTypeInfo = PrimSizeOf } = text "sizeof" <> parens (text at)
    pprint PrimTypeInfo { primArgType = at, primTypeInfo = PrimMaxBound } = text "max" <> parens (text at)
    pprint PrimTypeInfo { primArgType = at, primTypeInfo = PrimMinBound } = text "min" <> parens (text at)

