{-# LANGUAGE OverloadedStrings #-}
module C.Prims where

import Data.Binary
import Data.Monoid(Monoid(..))
import Data.Typeable
import qualified Data.Set as Set

import Doc.DocLike
import Doc.PPrint
import PackedString
import StringTable.Atom
import qualified Cmm.Op as Op

import GHC.Exts

data CallConv = CCall | StdCall | CApi | Primitive | DotNet
    deriving(Eq,Ord,Show)
    {-! derive: Binary !-}

data Safety = Safe | Unsafe deriving(Eq,Ord,Show)
    {-! derive: Binary !-}

newtype ExtType = ExtType PackedString
    deriving(Binary,IsString,Eq,Ord)

instance Show ExtType where
    show (ExtType p) = unpackPS p

instance Show Requires where
    show (Requires s) = show (Set.toList s)

newtype Requires = Requires (Set.Set (CallConv,PackedString))
    deriving(Eq,Ord,Monoid,Binary)

data DotNetPrim = DotNetField | DotNetCtor | DotNetMethod
    deriving(Typeable, Eq, Ord, Show)
    {-! derive: Binary !-}

primReqs p = f p where
    f CConst {} = primRequires p
    f Func {} = primRequires p
    f IFunc {} = primRequires p
    f AddrOf {} = primRequires p
    f _ = mempty

data Prim =
    PrimPrim Atom -- Special primitive implemented in the compiler somehow.
    | CConst {
        primRequires :: Requires,
        primConst :: !PackedString
        }  -- C code which evaluates to a constant
    | Func {
        primRequires :: Requires,
        funcName :: !PackedString,
        primArgTypes :: [ExtType],
        primRetType :: ExtType,
	primRetArgs :: [ExtType],
        primSafety  :: Safety
        }   -- function call with C calling convention
    | IFunc {
        primRequires :: Requires,
        primArgTypes :: [ExtType],
        primRetType :: ExtType
        } -- indirect function call with C calling convention
    | AddrOf {
        primRequires :: Requires,
        primConst :: !PackedString -- address of linker name
        }
    | Peek { primArgTy :: Op.Ty }  -- read value from memory
    | Poke { primArgTy :: Op.Ty }  -- write value to memory
    | PrimTypeInfo {
        primArgTy :: Op.Ty,
        primRetTy :: Op.Ty,
        primTypeInfo :: {-# UNPACK #-} !PrimTypeInfo
        }
    | PrimString !PackedString  -- address of a raw string. encoded in utf8.
    | PrimDotNet {
        primStatic :: {-# UNPACK #-} !Bool,
        primDotNet :: !DotNetPrim,
        primIOLike :: {-# UNPACK #-} !Bool,
        primAssembly :: !PackedString,
        primDotNetName :: !PackedString
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

primPrim s = PrimPrim $ toAtom s

instance DocLike d => PPrint d ExtType where
    pprint t = tshow t
--instance DocLike d => PPrint d PackedString where
--    pprint t = text $ unpackPS t

instance DocLike d => PPrint d Prim where
    pprint (PrimPrim t) = text (fromAtom t)
    pprint (CConst _ s) = parens (text $ unpackPS s)
    pprint Func { .. } = parens (tshow primRetType) <> text (unpackPS funcName) <> tupled (map pprint primArgTypes)
    pprint IFunc { .. } = parens (tshow primRetType) <> parens (char '*') <> tupled (map pprint primArgTypes)
    pprint (AddrOf _ s) = char '&' <> text (unpackPS s)
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
