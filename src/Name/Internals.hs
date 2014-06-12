module Name.Internals
    (NameType(..)
    ,Module(..)
    ,Name()
    ,QualifiedName
    ,UnqualifiedName
    ,ClassName
    ,FieldName
    ,QuotedName
    ,UnQuotedName
    ,forgeName
    ,nameParts
    ,nameCompare
    ,encodeNameType
    ,decodeNameType
    ,quoteName
    ,fromQuotedName
    ,unsafeWord32ToName
    ,unsafeNameToWord32
    ,unsafeWord8ToNameType
    ,unsafeNameTypeToWord8
    ,nameToBits
    ,bitsToName) where

import Data.Binary
import Data.Bits
import Data.Char
import Data.Data
import GHC.Base
import StringTable.Atom

{-
 - TODO:
  We cache pertinent information about a name in a single byte for easy access.
    [qocrrss0]             [q-xrr110]
    ss 0 term              q 0 not quoted
       1 type                1 quoted
       2 sort              x 0  class
    rr 0 unqualified         1  field
       1 qualified         c 0 not constructor
       2 prim                1 constructor
       3 composition       o 0 not operator
                             1 operator
-}

-------------
-- Name types
-------------
--
--newtype Name = Name Atom

newtype Name = Name Word32
    deriving(Ord,Eq,Typeable,Data)
{-# INLINE nameToBits #-}
nameToBits :: Name -> (Atom,NameType)
nameToBits (Name n) = (unsafeWord32ToAtom (n `unsafeShiftR` 8), toEnum $ fromIntegral (n .&. 0xff))

{-# INLINE bitsToName #-}
bitsToName ::  Atom -> NameType -> Name
bitsToName a w = Name $ (unsafeAtomToWord32 a `unsafeShiftL` 8) .|. fromIntegral (fromEnum w)

-- takes up 31 bits
-- 23 bits of atom, 8 bits of tag
-- only 4 bits of tag may be used
-- due to current binary implementation
-- in Id

{-# INLINE unsafeNameToWord32 #-}
unsafeNameToWord32 :: Name -> Word32
unsafeNameToWord32 (Name w) = w

{-# INLINE unsafeWord32ToName #-}
unsafeWord32ToName :: Word32 -> Name
unsafeWord32ToName w = Name w

--instance Show Name where
--    showsPrec p n = case nameToBits n of
--        (a,_) -> showsPrec p a

-- Used for documentation
type QualifiedName   = Name
type UnqualifiedName = Name
type ClassName       = Name
type FieldName       = Name
type QuotedName      = Name
type UnQuotedName    = Name

data NameType
    = TypeConstructor
    | DataConstructor
    | ClassName
    | TypeVal
    | Val
    | SortName
    | FieldLabel
    | RawType
    | UnknownType
    | QuotedName
    deriving(Ord,Eq,Enum,Read,Show,Bounded)

newtype Module = Module Atom
  deriving(Eq,Data,Typeable,ToAtom,FromAtom)

instance Ord Module where
    compare (Module x) (Module y) = x `atomCompare` y

instance Show Module where
    showsPrec _ (Module n) = shows n

forgeName :: NameType -> Maybe Module -> String -> Name
forgeName t Nothing  i = bitsToName (toAtom $ ';':i) t
forgeName t (Just (Module m)) i = bitsToName (toAtom $ show m ++ (';':i)) t

quoteName :: Name -> Name
quoteName name = bitsToName (toAtom $ encodeNameType nt:fromAtom atom) QuotedName where
    (atom,nt) = nameToBits name

fromQuotedName :: Name -> Maybe Name
fromQuotedName n = case nameToBits n of
    (a,QuotedName) -> case fromAtom a of
        nt:rest -> Just $ bitsToName (toAtom rest) (decodeNameType nt)
        [] -> error $ "invalid quoted name: " ++ show (a,QuotedName)
    _ -> Nothing

nameParts :: Name -> (NameType,Maybe Module,String)
nameParts (nameToBits -> (atom,nt)) = (nt,a,b) where
    ~(a,b) = f (fromAtom atom)
    f (';':xs) = (Nothing,xs)
    f xs = (Just $ Module (toAtom a),b) where
        (a,_:b) = span (/= ';') xs

encodeNameType :: NameType -> Char
encodeNameType t = unsafeChr $ ord '0' + fromEnum t

decodeNameType :: Char -> NameType
decodeNameType c = case ord c - ord '0' of
    n | n < 0 || n > fromEnum (maxBound::NameType) -> error $ "decodeNameType: out of range " ++ show (c,n)
    n -> toEnum n

unsafeNameTypeToWord8 :: NameType -> Word8
unsafeNameTypeToWord8 nt = fromIntegral $ fromEnum nt

unsafeWord8ToNameType :: Word8 -> NameType
unsafeWord8ToNameType w8 = toEnum $ fromIntegral w8

-- lexigraphic comparison
(nameToBits -> (a,ant)) `nameCompare` (nameToBits -> (b,bnt)) = case compare ant bnt of
    EQ -> a `atomCompare` b
    x -> x

instance Binary Name where
    put x = case nameToBits x of
        (a,nt) -> do
            putWord8 (unsafeNameTypeToWord8 nt)
            put a
    get = do
        x <- getWord8
        a <- get
        return $ bitsToName a (unsafeWord8ToNameType x)
