module C.Prims where

import Data.Monoid
import Data.Typeable
import Data.Binary

import StringTable.Atom
import C.FFI(Requires(..))
import Doc.DocLike
import Doc.PPrint
import PackedString
import qualified Cmm.Op as Op

data PrimTypeType = PrimTypeIntegral | PrimTypeFloating | PrimTypePointer | PrimTypeVoid
    deriving(Show,Eq,Ord)

data PrimType = PrimType {
    primTypeName :: ExtType,
    primTypeType :: PrimTypeType,
    primTypeAlignmentOf :: !Int,
    primTypeIsSigned :: !Bool,
    primTypeSizeOf :: !Int
    } deriving(Show)

type ExtType = String


data DotNetPrim = DotNetField | DotNetCtor | DotNetMethod
    deriving(Typeable, Eq, Ord, Show)
    {-! derive: Binary !-}

data Prim =
    PrimPrim Atom          -- Special primitive implemented in the compiler somehow.
    | CConst { primConst :: String, primRetType :: ExtType }  -- C code which evaluates to a constant
    | Func {
        funcIOLike :: !Bool,
        funcName :: PackedString,
        primArgTypes :: [ExtType],
        primRetType :: ExtType
        }   -- function call with C calling convention
    | IFunc {
        funcIOLike :: !Bool,
        primArgTypes :: [ExtType],
        primRetType :: ExtType
        } -- indirect function call with C calling convention
    | AddrOf PackedString              -- address of linker name
    | Peek { primArgTy :: Op.Ty }  -- read value from memory
    | Poke { primArgTy :: Op.Ty }  -- write value to memory
    | PrimTypeInfo {
        primArgTy :: Op.Ty,
        primRetTy :: Op.Ty,
        primTypeInfo :: PrimTypeInfo
        }
    | PrimString PackedString                                 -- address of a raw string. encoded in utf8.
    | PrimDotNet {
        primStatic :: !Bool,
        primDotNet :: DotNetPrim,
        primIOLike :: !Bool,
        primAssembly :: PackedString,
        primDotNetName :: PackedString
        }
    | Op {
        primCOp :: Op.Op Op.Ty,
        primRetTy :: Op.Ty
        }
    deriving(Typeable, Eq, Ord, Show)
    {-! derive: Binary !-}

data PrimTypeInfo = PrimSizeOf | PrimMaxBound | PrimMinBound | PrimAlignmentOf | PrimUMaxBound
    deriving(Typeable, Eq, Ord, Show)
    {-! derive: Binary !-}

primStaticTypeInfo :: Op.Ty -> PrimTypeInfo -> Maybe Integer
primStaticTypeInfo (Op.TyBits (Op.Bits b) _) w = Just ans where
    bits = toInteger b
    ans = case w of
        PrimSizeOf -> bits `div` 8
        PrimAlignmentOf ->  bits `div` 8
        PrimMinBound -> negate $ 2^(bits - 1)
        PrimMaxBound -> 2^(bits - 1) - 1
        PrimUMaxBound -> 2^bits - 1
primStaticTypeInfo _ _ = Nothing

-- | These primitives may safely be duplicated without affecting performance or
-- correctness too adversly. either because they are cheap to begin with, or
-- will be recombined in a later pass.

primIsCheap :: Prim -> Bool
primIsCheap AddrOf {} = True
primIsCheap CConst {} = True
primIsCheap PrimString {} = True
primIsCheap PrimTypeInfo {} = True
primIsCheap Op { primCOp = op } = Op.isCheap op
primIsCheap _ = False

aprimIsCheap (APrim p _) = primIsCheap p


-- | whether a primitive represents a constant expression (assuming all its arguments are constant)
-- TODO needs grin support
primIsConstant :: Prim -> Bool
primIsConstant CConst {} = True
primIsConstant AddrOf {} = True
primIsConstant PrimString {} = True
primIsConstant PrimTypeInfo {} = True
primIsConstant Op { primCOp = op } = Op.isEagerSafe op
primIsConstant _ = False

-- | whether a primitive can be eagarly evaluated.
-- TODO needs grin support
primEagerSafe :: Prim -> Bool
primEagerSafe CConst {} = True
primEagerSafe PrimString {} = True
primEagerSafe AddrOf {} = True
primEagerSafe PrimTypeInfo {} = True
primEagerSafe Op { primCOp = op } = Op.isEagerSafe op
primEagerSafe _ = False



parsePrimString s = do
    ws@(_:_) <- return $ words s
    let v = case last ws of
            '&':s -> AddrOf (packString s)
            s -> Func False (packString s) [] ""
    let f opt@('-':'l':_) = Requires [] [opt]
        f s = Requires [s] []
    return (APrim v (mconcat (map f (init ws))))


primPrim s = APrim (PrimPrim $ toAtom s) mempty

data APrim = APrim Prim Requires
    deriving(Typeable,  Eq, Ord)
    {-! derive: Binary !-}

instance Show APrim where
    showsPrec n (APrim p r) | r == mempty = showsPrec n p
    showsPrec n (APrim p r) = showsPrec n p . shows r

instance PPrint d Prim  => PPrint d APrim where
    pprintAssoc a n (APrim p _) = pprintAssoc a n p

instance DocLike d => PPrint d Prim where
    pprint (PrimPrim t) = text (fromAtom t)
    pprint (CConst s t) = parens (text t) <> parens (text s)
    pprint (Func _ s xs r) = parens (text r) <> text (unpackPS s) <> tupled (map text xs)
    pprint (IFunc _ xs r) = parens (text r) <> parens (char '*') <> tupled (map text xs)
    pprint (AddrOf s) = char '&' <> text (unpackPS s)
    pprint (PrimString s) = tshow s <> char '#'
    pprint (Peek t) = char '*' <> tshow t
    pprint (Poke t) = char '=' <> tshow t
    pprint Op { primCOp = Op.BinOp bo ta tb, primRetTy = rt } | rt == ta && rt == tb = parens (pprint rt) <> tshow bo
    pprint Op { primCOp = Op.UnOp bo ta, primRetTy = rt } | rt == ta = parens (pprint rt) <> tshow bo
    pprint Op { primCOp = op, primRetTy = rt } = parens (pprint rt) <> pprint op
    pprint PrimDotNet { primDotNet = dn,  primDotNetName = nn} = parens (text (unpackPS nn))
    pprint PrimTypeInfo { primArgTy = at, primTypeInfo = PrimSizeOf } = text "sizeof" <> parens (tshow at)
    pprint PrimTypeInfo { primArgTy = at, primTypeInfo = PrimAlignmentOf } = text "alignmentof" <> parens (tshow at)
    pprint PrimTypeInfo { primArgTy = at, primTypeInfo = PrimMaxBound } = text "max" <> parens (tshow at)
    pprint PrimTypeInfo { primArgTy = at, primTypeInfo = PrimUMaxBound } = text "umax" <> parens (tshow at)
    pprint PrimTypeInfo { primArgTy = at, primTypeInfo = PrimMinBound } = text "min" <> parens (tshow at)

instance DocLike d => PPrint d Op.Ty where
    pprintAssoc _ n p = text (showsPrec n p "")
instance (DocLike d,Show v) => PPrint d (Op.Op v) where
    pprintAssoc _ n p = text (showsPrec n p "")

parseDotNetFFI :: Monad m => String -> m Prim
parseDotNetFFI s = ans where
    init = PrimDotNet { primIOLike = False, primStatic = False, primDotNet = DotNetField, primAssembly = packString "", primDotNetName = packString "" }
    ans = case words s of
        ("static":rs) -> f rs init { primStatic = True }
        rs -> f rs init
    f ("field":rs) dn = g dn { primDotNet = DotNetField } rs
    f ("ctor":rs) dn = g dn { primDotNet = DotNetCtor } rs
    f ("method":rs) dn = g dn { primDotNet = DotNetMethod } rs
    f _ _ = fail "invalid .NET ffi specification"
    g dn ['[':rs] | (as,']':nm) <- span (/= ']') rs = return dn { primAssembly = packString as, primDotNetName = packString nm }
    g dn [n] = return dn { primDotNetName = packString n }
    g _ _ = fail "invalid .NET ffi specification"


