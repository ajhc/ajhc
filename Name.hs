module Name(
    NameType(..),
    Name,
    nameName,
    nameType,
    nameValue,
    getModule,
    toUnqualified,
    qualifyName,
    toPlainAtom,
    nameTuple,
    ToName(..),
    fromTypishHsName,
    fromValishHsName,
    parseName,
    isConstructorLike,
    unboxedNameTuple,
    fromUnboxedNameTuple,
    toId,
    fromId,
    setModule
    ) where

import Char
import Data.Generics
import Monad(liftM)

import Atom
import Binary
import Doc.DocLike
import Doc.PPrint
import GenUtil
import HsSyn
import VConsts

data NameType =
    TypeConstructor
    | DataConstructor
    | ClassName
    | TypeVal
    | Val
    | SortName
    | FieldLabel
    | RawType
    deriving(Ord,Eq,Enum,Read,Show,Typeable,Data)


newtype Name = Name Atom
    deriving(Ord,Eq,Typeable,Data,Binary,ToAtom,FromAtom)

isTypeNamespace TypeConstructor = True
isTypeNamespace ClassName = True
isTypeNamespace TypeVal = True
isTypeNamespace _ = False

isValNamespace DataConstructor = True
isValNamespace Val = True
isValNamespace _ = False

isConstructorLike xs@(x:_) =  isUpper x || x `elem` ":("  || xs == "->"
isConstructorLike [] = error "isConstructorLike: empty"

instance ValName Name where
    hsValName (a,b) = toName Val (a,b)
    hsTypName (a,b) = toName TypeVal (a,b)
    hsUnqualTypName b = toName TypeVal b

instance ValName HsName where
    hsValName (a,b) = Qual (Module a) $ HsIdent b
    hsUnqualValName b = UnQual $ HsIdent b




fromTypishHsName, fromValishHsName :: HsName -> Name
fromTypishHsName name
    | isUpper x || x `elem` ":(" = toName TypeConstructor name
    | otherwise = toName TypeVal name
    where x = head (hsIdentString . hsNameIdent  $ name)
fromValishHsName name
    | isUpper x || x `elem` ":(" = toName DataConstructor name
    | otherwise = toName Val name
    where x = head (hsIdentString . hsNameIdent  $ name)

createName _ "" i = error $ "createName: empty module "  ++ i
createName _ m "" = error $ "createName: empty ident "   ++ m
createName t m i = Name $  toAtom $ (chr $ fromEnum t):m ++ "\NUL" ++ i
createUName _ "" = error $ "createUName: empty ident"
createUName t i =  Name $ toAtom $ (chr $ fromEnum t):"\NUL" ++ i

class ToName a where
    toName :: NameType -> a -> Name
    fromName :: Name -> (NameType, a)

instance ToName HsName where
    toName nt n = m where
        i = hsIdentString $ hsNameIdent n
        m | Qual (Module m) _ <- n = createName nt m i
          | otherwise = createUName nt i
    fromName n = (nameType n, nameName n)

instance ToName (String,String) where
    toName nt (m,i) = createName nt m i
    fromName n = (nameType n, mi ) where
        nn = nameName n
        mi  | Qual (Module m) (HsIdent i) <- nn = (m,i)
            | UnQual (HsIdent i) <- nn = ("",i)
            | otherwise = error "can't happen"

instance ToName String where
    toName nt i = createUName nt i
    fromName n = (nameType n, m ++ i ) where
        nn = nameName n
        (m,i)  | Qual (Module m) (HsIdent i) <- nn = (m ++ ".",i)
               | UnQual (HsIdent i) <- nn = ("",i)


getModule :: Monad m => Name -> m Module
getModule n = case nameName n of
    Qual m _ -> return m
    UnQual {} -> fail "Name is unqualified."

toUnqualified :: Name -> Name
toUnqualified n = case fromName n of
    (_,UnQual {}) -> n
    (t,Qual m n) -> toName t (UnQual n)

qualifyName :: Module -> Name -> Name
qualifyName m n = case fromName n of
    (t,UnQual n) -> toName t (Qual m n)
    (_,Qual {}) -> n

setModule :: Module -> Name -> Name
setModule m n = qualifyName m  $ toUnqualified n


parseName :: NameType -> String -> Name
--parseName t name = if not (null ms) then toName t (concatInter "." ms, concatInter "." (ns ++ [last sn])) else toName t (concatInter "." (ns ++ [last sn])) where
parseName t name = toName t (concatInter "." ms, concatInter "." (ns ++ [last sn])) where
    sn = (split (== '.') name)
    (ms,ns) = span validMod (init sn)
    validMod (c:cs) = isUpper c && all (\c -> isAlphaNum c || c `elem` "_'") cs
    validMod _ = False




nameType :: Name -> NameType
nameType (Name a) = toEnum (ord (head (toString a)))

nameName :: Name -> HsName
nameName (Name a) = f $ tail (toString a) where
    f ('\NUL':xs) = UnQual $ HsIdent xs
    f xs | (a,_:b) <- span (/= '\NUL') xs  = Qual (Module a) (HsIdent b)
    f _ = error "invalid Name"


instance Show Name where
    show a = show $ nameName a

toPlainAtom a = toAtom (show a)

hsname m n = (m,n) -- Qual (Module m) $ HsIdent n


nameTuple _ n | n < 2 = error "attempt to create tuple of length < 2"
nameTuple t n = toName t  $ (toTuple n:: (String,String)) -- Qual (HsIdent ("(" ++ replicate (n - 1) ',' ++ ")"))

unboxedNameTuple t n = toName t $ "(#" ++ show n ++ "#)"
--unboxedNameTuple t n = toName t $ "(#": replicate (n - 1) ',' ++ "#)"

fromUnboxedNameTuple n = case show n of
    '(':'#':xs | (ns@(_:_),"#)") <- span isDigit xs -> return (read ns::Int)
    _ -> fail $ "Not unboxed tuple: " ++ show n



instance TypeNames Name where
    tInt = toName TypeConstructor $ hsname "Prelude" "Int"
    tBool = toName TypeConstructor $ hsname "Prelude" "Bool"
    tInteger = toName TypeConstructor $ hsname "Prelude" "Integer"
    --tRational = toName TypeConstructor $ hsname "Ratio" "Rational"
    tChar = toName TypeConstructor $ hsname "Prelude" "Char"
    tStar = toName SortName $ hsname "Jhc@" "*"
    tHash = toName SortName $ hsname "Jhc@" "#"
    tUnit = toName TypeConstructor $ hsname "Prelude" "()"
    tIntzh = toName RawType "int"
    tCharzh = toName RawType "uint32_t"
    tIntegerzh = toName RawType "intmax_t"
    tWorld__ = toName TypeConstructor $ hsname "Jhc.IO" "World__"

instance ConNames Name where
    vTrue = toName DataConstructor $ hsname "Prelude" "True"
    vFalse = toName DataConstructor $ hsname "Prelude" "False"
    vEmptyList = toName DataConstructor $ hsname "Prelude" "[]"
    vUnit = toName DataConstructor $ hsname "Prelude" "()"
    vCons = toName DataConstructor $ hsname "Prelude" ":"

instance ToTuple Name where
    toTuple n = toName DataConstructor (toTuple n :: (String,String))


instance DocLike d => PPrint d Name  where
    pprint n = text (show n)

{-
instance ClassNames Name where
    classEq :: a
    classOrd :: a
    classEnum :: a
    classBounded :: a
    classShow :: a
    classRead :: a
    classIx :: a
    classFunctor :: a
    classMonad :: a
    classNum  :: a
    classReal :: a
    classIntegral :: a
    classFractional :: a
    classFloating :: a
    classRealFrac :: a
    classRealFloat :: a

-}

nameValue m n = atomIndex $ toAtom (toName Val (m,n))

toId :: Name -> Int
toId x = atomIndex (toAtom x)

fromId :: Monad m => Int -> m Name
fromId i = liftM Name (intToAtom i)



