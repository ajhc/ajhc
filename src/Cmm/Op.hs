{-# OPTIONS -funbox-strict-fields #-}
module Cmm.Op where

import Data.Binary
import Util.Gen

{-

Basic operations. These are chosen to be roughly equivalent to c-- operations,
but can be effectively used to generate C or assembly code as well.

An operation consists of the operation itself, the type of the arguments and
return value, and a hint attached to each argument.

A condition is that the operation must be fully determined by the operation
name and the type of its arguments. this specifically does not include the
hint. For instance, since whether a number is signed or unsigned is in the
hint, so the operation itself must say whether it is signed or unsigned.

Also, distinct algorithms should be given different operations, for instance
floating point and integer comparison are so different that they should be
separate opcodes, even if it could be determined by the type they operate on.

-}

-- these take 2 arguments of the same type, and return one of the same type.
-- an exception are the mulx routines, which may return a type exactly
-- double in size of the original, and the shift and rotate routines, where the
-- second argument may be of any width and is interpreted as an unsigned
-- number.
--
-- the invarient is that the return type is always exactly determined by the
-- argument types

data BinOp
    = Add
    | Sub

    | Mul
    | Mulx
    | UMulx

    | Div   -- ^ round to -Infinity
    | Mod   -- ^ mod rounding to -Infinity

    | Quot  -- ^ round to 0
    | Rem   -- ^ rem rounding to 0

    | UDiv  -- ^ round to zero (unsigned)
    | UMod  -- ^ unsigned mod

    -- bitwise
    | And
    | Or
    | Xor
    | Shl
    | Shr    -- ^ shift right logical
    | Shra   -- ^ shift right arithmetic
    | Rotl
    | Rotr

    -- floating
    | FAdd
    | FSub
    | FDiv
    | FMul
    | FPwr
    | FAtan2

    -- These all compare two things of the same type, and return a boolean.
    | Eq
    | NEq
    | Gt
    | Gte
    | Lt
    | Lte
    -- unsigned versions
    | UGt
    | UGte
    | ULt
    | ULte

    -- floating point comparasons
    | FEq
    | FNEq
    | FGt
    | FGte
    | FLt
    | FLte
    -- whether two values can be compared at all.
    | FOrdered
    deriving(Eq,Show,Ord,Read,Enum,Bounded)
    {-! derive: Binary !-}

data UnOp
    = Neg   -- ^ 2s compliment negation
    | Com   -- ^ bitwise compliment
    -- floating
    | FAbs  -- ^ floating absolute value
    | FNeg  -- ^ floating point negation
    | Sin
    | Cos
    | Tan
    | Sinh
    | Cosh
    | Tanh
    | Asin
    | Acos
    | Atan
    | Log
    | Exp
    | Sqrt
    -- exotic bit operations
    | Bswap  -- ^ Switch the order of the bytes in a word
    | Ffs    -- ^ Returns one plus the index of the least
             --   significant 1-bit of x, 0 if x is zero.
    | Clz    -- ^ number of leading (from MSB) zeros, undefined if zero
    | Ctz    -- ^ number of trailing (from LSB) zeros, undefined if zero.
    | Popcount -- ^ number of bits set to 1 in word
    | Parity   -- ^ number of bits set to 1 mod 2
    deriving(Eq,Show,Ord,Read,Enum,Bounded)
    {-! derive: Binary !-}

-- conversion ops

data ConvOp
    = F2I         -- ^ convert a floating point to an integral value via truncation
    | F2U         -- ^ convert a floating point to an unsigned integral value via truncation, negative values become zero
    | U2F         -- ^ convert an unsigned integral value to a floating point number
    | I2F         -- ^ convert an integral value to a floating point number
    | F2F         -- ^ convert a float from one precision to another, preserving value as much as possible
    | Lobits      -- ^ extract the low order bits
    | Sx          -- ^ sign extend a value (signed)
    | Zx          -- ^ zero extend a value (unsigned)
    | I2I         -- ^ perform a 'Lobits' or a 'Sx' depending on the sizes of the arguments
    | U2U         -- ^ perform a 'Lobits' or a 'Zx' depending on the sizes of the arguments
    | B2B         -- ^ a nop, useful for coercing hints (bits 2 bits)
    deriving(Eq,Show,Ord,Read,Enum,Bounded)
    {-! derive: Binary !-}

data ValOp
    = NaN
    | PInf
    | NInf
    | PZero
    | NZero
    deriving(Eq,Show,Ord,Read,Bounded)
    {-! derive: Binary !-}

data ArchBits = BitsMax | BitsPtr | BitsUnknown
    deriving(Eq,Ord)
    {-! derive: Binary !-}

data TyBits = Bits {-# UNPACK #-} !Int | BitsArch !ArchBits |  BitsExt String
    deriving(Eq,Ord)
    {-! derive: Binary !-}

data TyHint
    = HintSigned
    | HintUnsigned
    | HintFloat        -- an IEEE floating point value
    | HintCharacter    -- a unicode character, implies unsigned
    | HintNone         -- no hint
    deriving(Eq,Ord)
    {-! derive: Binary !-}

data Ty
    = TyBits !TyBits !TyHint
    | TyBool
    deriving(Eq,Ord)
    {-! derive: Binary !-}

readTy :: Monad m => String -> m Ty
readTy "bool" = return TyBool
readTy "bits<ptr>" = return $ TyBits (BitsArch BitsPtr) HintNone
readTy "bits<max>" = return $ TyBits (BitsArch BitsMax) HintNone
readTy "bits<?>" = return $ TyBits (BitsArch BitsUnknown) HintNone
readTy ('b':'i':'t':'s':'<':rs) = return $ TyBits (BitsExt (takeWhile ('>' /=) rs)) HintNone
readTy ('b':'i':'t':'s':rs) = do n <- readM rs; return $ TyBits (Bits n) HintNone
readTy ('s':rs) = do TyBits x _ <- readTy rs; return $ TyBits x HintSigned
readTy ('u':rs) = do TyBits x _ <- readTy rs; return $ TyBits x HintUnsigned
readTy ('f':rs) = do TyBits x _ <- readTy rs; return $ TyBits x HintFloat
readTy ('c':rs) = do TyBits x _ <- readTy rs; return $ TyBits x HintCharacter
readTy _ = fail "readTy: not type"

stringToOpTy ::  String -> Ty
stringToOpTy s = case readTy s of
    Just t -> t
    _ -> error $ "stringToOpTy: " ++ show s

bool = TyBool
bits_ptr = TyBits (BitsArch BitsPtr) HintNone
bits_max = TyBits (BitsArch BitsMax) HintNone
bits8    = TyBits (Bits 8)  HintNone
bits16   = TyBits (Bits 16) HintNone
bits32   = TyBits (Bits 32) HintNone
bits64   = TyBits (Bits 64) HintNone

class ToCmmTy a where
    toCmmTy :: a -> Maybe Ty

instance ToCmmTy Ty where
    toCmmTy a = Just a

instance ToCmmTy String where
    toCmmTy s = readTy s

cmmTyBits :: ToCmmTy a => a -> Maybe Int
cmmTyBits x = do TyBits (Bits b) _ <- toCmmTy x; return b
cmmTyHint x = do TyBits _ hint <- toCmmTy x; return hint

instance Show TyHint where
    showsPrec _ HintSigned = ('s':)
    showsPrec _ HintUnsigned = ('u':)
    showsPrec _ HintFloat = ('f':)
    showsPrec _ HintCharacter = ('c':)
    showsPrec _ HintNone = id

instance Show Ty where
    showsPrec _ TyBool = showString "bool"
    showsPrec _ (TyBits b h) = shows h . showString "bits" . shows b

instance Show TyBits where
    showsPrec _ (Bits n) = shows n
    showsPrec _ (BitsExt s) = showChar '<' . showString s . showChar '>'
    showsPrec _ (BitsArch s) = showChar '<' . shows s . showChar '>'

instance Show ArchBits where
    show BitsMax = "max"
    show BitsPtr = "ptr"
    show BitsUnknown = "?"

data Op v
    = BinOp BinOp v v
    | UnOp UnOp v
    | ValOp ValOp
    | ConvOp ConvOp v
    deriving(Eq,Show,Ord)
    {-! derive: Binary !-}

binopType :: BinOp -> Ty -> Ty -> Ty
binopType Mulx  (TyBits (Bits i) h) _ = TyBits (Bits (i*2)) h
binopType UMulx (TyBits (Bits i) h) _ = TyBits (Bits (i*2)) h
binopType Eq  _ _ =  TyBool
binopType NEq _ _ =  TyBool
binopType Gt  _ _ =  TyBool
binopType Gte _ _ =  TyBool
binopType Lt  _ _ =  TyBool
binopType Lte _ _ =  TyBool
binopType UGt  _ _ =  TyBool
binopType UGte _ _ =  TyBool
binopType ULt  _ _ =  TyBool
binopType ULte _ _ =  TyBool
binopType FEq  _ _ =  TyBool
binopType FNEq _ _ =  TyBool
binopType FGt  _ _ =  TyBool
binopType FGte _ _ =  TyBool
binopType FLt  _ _ =  TyBool
binopType FLte _ _ =  TyBool
binopType FOrdered _ _ =  TyBool
binopType _ t1 _ = t1

isCommutable :: BinOp -> Bool
isCommutable x = f x where
    f Add = True
    f Mul = True
    f And = True
    f Or  = True
    f Xor = True
    f Eq  = True
    f NEq = True
    f FAdd = True
    f FMul = True
    f FEq  = True
    f FNEq = True
    f FOrdered = True
    f _ = False

commuteBinOp :: BinOp -> Maybe BinOp
commuteBinOp x | isCommutable x = return x
commuteBinOp Lt = return Gt
commuteBinOp Gt = return Lt
commuteBinOp Lte = return Gte
commuteBinOp Gte = return Lte
commuteBinOp ULt = return UGt
commuteBinOp UGt = return ULt
commuteBinOp ULte = return UGte
commuteBinOp UGte = return ULte
commuteBinOp FLt = return FGt
commuteBinOp FGt = return FLt
commuteBinOp FLte = return FGte
commuteBinOp FGte = return FLte
commuteBinOp _ = Nothing

isAssociative :: BinOp -> Bool
isAssociative x = f x where
    f Add = True
    f Mul = True
    f And = True
    f Or  = True
    f Xor = True
    f _ = False

unopFloat :: Ty -> UnOp -> Maybe String
unopFloat (TyBits b HintFloat) op = g b =<< f op where
    g (Bits 64) x = return x
    g (Bits 32) x = return $ x ++ "f"
    g _ _ = Nothing
    f FAbs = return "fabs"
    f Sin  = return "sin"
    f Cos  = return "cos"
    f Tan  = return "tan"
    f Sinh  = return "sinh"
    f Cosh  = return "cosh"
    f Tanh  = return "tanh"
    f Asin  = return "asin"
    f Acos  = return "acos"
    f Atan  = return "atan"
    f Sqrt = return "sqrt"
    f Log = return "log"
    f Exp = return "exp"

    f _ = Nothing
unopFloat _ _ = Nothing

binopFunc :: Ty -> Ty -> BinOp -> Maybe String
binopFunc (TyBits b _) _ bop = g b =<< f bop where
    g (Bits 64) x = return x
    g (Bits 32) x = return $ x ++ "f"
    g _ _ = Nothing
    f FPwr = Just "pow"
    f FAtan2 = Just "atan2"
    f _ = Nothing
binopFunc TyBool _ bop = Nothing where

binopInfix :: BinOp -> Maybe (String,Int)
binopInfix UDiv = Just ("/",8)
binopInfix Mul  = Just ("*",8)
binopInfix UMod = Just ("%",8)
binopInfix Sub  = Just ("-",7)
binopInfix Add  = Just ("+",7)
binopInfix Shr  = Just (">>",6)
binopInfix Shl  = Just ("<<",6)
binopInfix And  = Just ("&",5)
binopInfix Xor  = Just ("^",4)
binopInfix Or   = Just ("|",3)
binopInfix UGte = Just (">=",2)
binopInfix UGt  = Just (">",2)
binopInfix ULte = Just ("<=",2)
binopInfix ULt  = Just ("<",2)
binopInfix Eq   = Just ("==",2)
binopInfix NEq  = Just ("!=",2)
binopInfix _ = Nothing

class IsOperator o where
    isCheap :: o -> Bool
    isEagerSafe :: o -> Bool

instance IsOperator BinOp where
    isCheap FAtan2 = False
    isCheap _ = True

    isEagerSafe Div = False
    isEagerSafe Mod = False
    isEagerSafe Quot = False
    isEagerSafe Rem  = False
    isEagerSafe UDiv = False
    isEagerSafe UMod = False
    isEagerSafe _ = True

instance IsOperator UnOp where
    isCheap _ = True
    isEagerSafe _ = True

instance IsOperator ConvOp where
    isCheap _ = True
    isEagerSafe _ = True

instance IsOperator (Op v) where
    isCheap (BinOp o _ _) = isCheap o
    isCheap (UnOp o _) = isCheap o
    isCheap (ConvOp o _) = isCheap o
    isCheap _ = False
    isEagerSafe (BinOp o _ _) = isEagerSafe o
    isEagerSafe (UnOp o _) = isEagerSafe o
    isEagerSafe (ConvOp o _) = isEagerSafe o
    isEagerSafe _ = False
