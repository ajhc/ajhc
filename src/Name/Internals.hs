module Name.Internals(NameType(..),Module(..),Name(..),QualifiedName,UnqualifiedName,ClassName,FieldName,QuotedName,UnQuotedName,forgeName,nameCompare,quoteName) where

import StringTable.Atom
import Data.Binary
import Data.Char
import Data.Data

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
newtype Name = Name Atom
    deriving(Ord,Eq,Typeable,Binary,Data,ToAtom,FromAtom)

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
    deriving(Ord,Eq,Enum,Read,Show)

newtype Module = Module Atom
  deriving(Eq,Data,Typeable,ToAtom,FromAtom)

instance Ord Module where
    compare (Module x) (Module y) = x `atomCompare` y

instance Show Module where
    showsPrec _ (Module n) = shows n

forgeName :: NameType -> Maybe Module -> String -> Name
forgeName t Nothing  i = Name $ toAtom $ (chr $ fromEnum t + ord '1'):";" ++ i
forgeName t (Just m) i = Name $ toAtom $ (chr $ ord '1' + fromEnum t):show m ++ ";" ++ i

quoteName :: Name -> Name
quoteName (Name n) = forgeName QuotedName Nothing (fromAtom n)

-- lexigraphic comparison
Name x `nameCompare` Name y = x `atomCompare` y
