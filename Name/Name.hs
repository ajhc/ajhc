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
    ffiExportName,
    isConstructorLike,
    toId,
    fromId,
    Module,
    mainModule,
    nameParts,
    mapName,
    setModule
    ) where

import Char
import Monad(liftM)
import Data.Typeable

import StringTable.Atom
import Data.Binary
import C.FFI
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
    deriving(Ord,Eq,Enum,Read,Show)


newtype Name = Name Atom
    deriving(Ord,Eq,Typeable,Binary,ToAtom,FromAtom)

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
    where (x:_) = (hsIdentString . hsNameIdent  $ name)
fromValishHsName name
    | isUpper x || x `elem` ":(" = toName DataConstructor name
    | otherwise = toName Val name
    where (x:_) = (hsIdentString . hsNameIdent  $ name)

createName _ "" i = error $ "createName: empty module "  ++ i
createName _ m "" = error $ "createName: empty ident "   ++ m
createName t m i = Name $  toAtom $ (chr $ fromEnum t):m ++ "\NUL" ++ i
createUName :: NameType -> String -> Name
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
    fromName n = case nameParts n of
            (nt,Just m,i) -> (nt,(m,i))
            (nt,Nothing,i) -> (nt,("",i))

instance ToName (Maybe String,String) where
    toName nt (Just m,i) = createName nt m i
    toName nt (Nothing,i) = createUName nt i
    fromName n = case nameParts n of
        (nt,a,b) -> (nt,(a,b))

instance ToName (Maybe Module,String) where
    toName nt (Just (Module m),i) = createName nt m i
    toName nt (Nothing,i) = createUName nt i
    fromName n = case nameParts n of
        (nt,a,b) -> (nt,(fmap Module a,b))

instance ToName String where
    toName nt i = createUName nt i
    fromName n = (nameType n, mi ) where
        mi = case snd $ fromName n of
            (Just m,i) -> m ++ "." ++ i
            (Nothing,i) -> i


getModule :: Monad m => Name -> m Module
getModule n = case nameParts n of
    (_,Just m,_)  -> return (Module m)
    _ -> fail "Name is unqualified."

toUnqualified :: Name -> Name
toUnqualified n = case nameParts n of
    (_,Nothing,_) -> n
    (t,Just _,i) -> toName t i

qualifyName :: Module -> Name -> Name
qualifyName m n = case nameParts n of
    (t,Nothing,n) -> toName t (Just m, n)
    _ -> n

setModule :: Module -> Name -> Name
setModule m n = qualifyName m  $ toUnqualified n


parseName :: NameType -> String -> Name
parseName t name = toName t (intercalate "." ms, intercalate "." (ns ++ [last sn])) where
    sn = (split (== '.') name)
    (ms,ns) = span validMod (init sn)
    validMod (c:cs) = isUpper c && all (\c -> isAlphaNum c || c `elem` "_'") cs
    validMod _ = False


nameType :: Name -> NameType
nameType (Name a) = toEnum (ord (head (fromAtom a)))

nameName :: Name -> HsName
nameName (Name a) = f $ tail (fromAtom a) where
    f ('\NUL':xs) = UnQual $ HsIdent xs
    f xs | (a,_:b) <- span (/= '\NUL') xs  = Qual (Module a) (HsIdent b)
    f _ = error $ "invalid Name: " ++ (show $ (fromAtom a :: String))

nameParts :: Name -> (NameType,Maybe String,String)
nameParts n@(Name a) = f $ tail (fromAtom a) where
    f ('\NUL':xs) = (nameType n,Nothing,xs)
    f xs = (nameType n,Just a,b) where
        (a,_:b) = span (/= '\NUL') xs

instance Show Name where
    show a = show $ nameName a

instance DocLike d => PPrint d Name  where
    pprint n = text (show n)

toId :: Name -> Int
toId x = fromAtom (toAtom x)

fromId :: Monad m => Int -> m Name
--fromId i | even i || i < 0 = fail $ "Name.fromId: not a name " ++ show i
fromId i | not $ isValidAtom i = fail $ "Name.fromId: not a name " ++ show i
fromId i = return $ case intToAtom i of
    Just a -> Name a
    Nothing -> error $ "Name.fromId: not a name " ++ show i

mapName :: (String -> String,String -> String) -> Name -> Name
mapName (f,g) n = case nameParts n of
    (nt,Nothing,i) -> toName nt (g i)
    (nt,Just m,i) -> toName nt (Just (f m :: String),g i)

mainModule = Module "Main@"

ffiExportName :: FfiExport -> Name
ffiExportName (FfiExport cn _ cc) = toName Val ("FE@", show cc ++ "." ++ cn)

