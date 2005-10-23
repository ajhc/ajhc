module Name.Name(
    NameType(..),
    Name,
    nameName,
    nameType,
    getModule,
    toUnqualified,
    qualifyName,
    ToName(..),
    fromTypishHsName,
    fromValishHsName,
    parseName,
    isConstructorLike,
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

instance DocLike d => PPrint d Name  where
    pprint n = text (show n)

toId :: Name -> Int
toId x = atomIndex (toAtom x)

fromId :: Monad m => Int -> m Name
fromId i = liftM Name (intToAtom i)


