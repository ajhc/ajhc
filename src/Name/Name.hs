{-# LANGUAGE OverloadedStrings #-}
module Name.Name(
    Module(),
    Name,
    Class,
    NameType(..),
    ToName(..),
    ffiExportName,
    fromTypishHsName,
    fromValishHsName,
    getIdent,
    getModule,
    isConstructorLike,
    isConstructor,
    isTypeNamespace,
    isValNamespace,
    isOpLike,
    mapName,
    mapName',
    nameParts,
    nameType,
    parseName,
    qualifyName,
    setModule,
    quoteName,
    fromQuotedName,
    toModule,
    FieldName,
    ClassName,
    combineName,
    toUnqualified,
    removeUniquifier,
    -- new interface
    unMkName,
    mkName,
    mkComplexName,
    emptyNameParts,
    mkNameType,
    NameParts(..),
    nameTyLevel_u,
    nameTyLevel_s,
    typeLevel,
    kindLevel,
    termLevel,
    originalUnqualifiedName,
    deconstructName
    ) where

import C.FFI
import Data.Char
import Doc.DocLike
import Doc.PPrint
import GenUtil
import Name.Internals
import Name.Prim
import StringTable.Atom
import Ty.Level
import Util.Std

isTypeNamespace TypeConstructor = True
isTypeNamespace ClassName = True
isTypeNamespace TypeVal = True
isTypeNamespace _ = False

isValNamespace DataConstructor = True
isValNamespace Val = True
isValNamespace _ = False

-----------------
-- name definiton
-----------------

isConstructorLike n =  isUpper x || x `elem` ":("  || xs == "->" || xs == "[]" where
    (_,_,xs@(x:_)) = nameParts n

isOpLike n  = x `elem` "!#$%&*+./<=>?@\\^-~:|" where
    (_,_,(x:_)) = nameParts n

fromTypishHsName, fromValishHsName :: Name -> Name
fromTypishHsName name
    | nameType name == QuotedName = name
    | isConstructorLike name = toName TypeConstructor name
    | otherwise = toName TypeVal name
fromValishHsName name
    | nameType name == QuotedName = name
    | isConstructorLike name = toName DataConstructor name
    | otherwise = toName Val name

createName :: NameType -> Module -> String -> Name
createName _ (Module "") i = error $ "createName: empty module " ++ i
createName _ m "" = error $ "createName: empty ident " ++ show m
createName t m i = forgeName t (Just m) i

createUName :: NameType -> String -> Name
createUName _ "" = error $ "createUName: empty ident"
createUName t i =  forgeName t Nothing i

class ToName a where
    toName :: NameType -> a -> Name
    fromName :: Name -> (NameType, a)

instance ToName (String,String) where
    toName nt (m,i) = createName nt (Module $ toAtom m) i
    fromName n = case nameParts n of
            (nt,Just (Module m),i) -> (nt,(show m,i))
            (nt,Nothing,i) -> (nt,("",i))

instance ToName (Module,String) where
    toName nt (m,i) = createName nt m i
    fromName n = case nameParts n of
            (nt,Just m,i) -> (nt,(m,i))
            (nt,Nothing,i) -> (nt,(Module "",i))

instance ToName (Maybe Module,String) where
    toName nt (Just m,i) = createName nt m i
    toName nt (Nothing,i) = createUName nt i
    fromName n = case nameParts n of
        (nt,a,b) -> (nt,(a,b))

instance ToName Name where
    toName nt i | nt == ct = i
                | otherwise = toName nt (x,y) where
        (ct,x,y) = nameParts i
    fromName n = (nameType n,n)

instance ToName String where
    toName nt i = createUName nt i
    fromName n = (nameType n, mi ) where
        mi = case snd $ fromName n of
            (Just (Module m),i) -> show m ++ "." ++ i
            (Nothing,i) -> i

getModule :: Monad m => Name -> m Module
getModule n = case nameParts n of
    (_,Just m,_) -> return m
    _ -> fail "Name is unqualified."

getIdent :: Name -> String
getIdent n = case nameParts n of
    (_,_,s)  -> s

toUnqualified :: Name -> Name
toUnqualified n = case nameParts n of
    (_,Nothing,_) -> n
    (t,Just _,i) -> toName t (Nothing :: Maybe Module,i)

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
nameType name = snd $ nameToBits name

instance Show Name where
    showsPrec _ n = f n where
        f (fromQuotedName -> Just n) = showChar '`' . f n
        f (nameType -> UnknownType)  = showChar '¿' . (g $ nameParts n)
        f n = g $ nameParts n
        g (_,Just a,b) = shows a . showChar '.' . showString b
        g (_,Nothing,b) = showString b

instance DocLike d => PPrint d Name  where
    pprint n = text (show n)

mapName :: (Module -> Module,String -> String) -> Name -> Name
mapName (f,g) n = case nameParts n of
    (nt,Nothing,i) -> toName nt (g i)
    (nt,Just m,i) -> toName nt (Just (f m :: Module),g i)
mapName' :: (Maybe Module -> Maybe Module) -> (String -> String) -> Name -> Name
mapName' f g n = case nameParts n of
    (nt,m,i) -> toName nt (f m,g i)

ffiExportName :: FfiExport -> Name
ffiExportName (FfiExport cn _ cc _ _) = toName Val (Module "FE@", show cc ++ "." ++ cn)
type Class = Name

----------
-- Modules
----------

toModule :: String -> Module
toModule s = Module $ toAtom s

--------------
--prefered api
--------------
mkName :: TyLevel -> Bool -> Maybe Module -> String -> Name
mkName l b mm s = toName (mkNameType l b) (mm,s)

-- the left name effectively becomes the module.
combineName :: Name -> Name -> Name
combineName (nameToBits -> (a1,nt1)) (nameToBits -> (a2,nt2)) = bitsToName  ((toAtom $ "{" ++ encodeNameType nt1:show a1 ++ "}") `mappend` a2) nt2

data NameParts = NameParts {
    nameLevel       :: TyLevel,
    nameQuoted      :: Bool,
    nameConstructor :: Bool,
    nameModule      :: Maybe Module,
    nameOuter       :: [Name],
    nameUniquifier  :: Maybe Int,
    nameIdent       :: String
    } deriving(Show)

emptyNameParts = NameParts {
    nameLevel       = termLevel,
    nameQuoted      = False,
    nameConstructor = False,
    nameModule      = Nothing,
    nameOuter       = [],
    nameUniquifier  = Nothing,
    nameIdent       = "(empty)"
    }

mkComplexName :: NameParts -> Name
mkComplexName NameParts { .. } = qn $ on nameOuter $ toName (mkNameType nameLevel nameConstructor) (nameModule,ident) where
    ident = maybe id (\c s -> show c ++ "_" ++ s) nameUniquifier nameIdent
    qn = if nameQuoted then quoteName else id
    on xs n = case xs of
        (x:xs) -> combineName x (on xs n)
        [] -> n

unMkName :: Name -> NameParts
unMkName nm@(nameToBits -> (a1,nt1)) = NameParts { .. } where
    (nameOuter,name) = h (show a1) where
        h xs@('{':_) = g xs []
        h _ = ([],nm)
        g ('{':nt:rs) zs = f (0::Int) rs (decodeNameType nt) [] where
            f 0 ('}':xs) nt rs = g xs (bitsToName (toAtom $ reverse rs) nt:zs)
            f n ('{':xs) nt rs = f (n + 1) xs nt ('{':rs)
            f n ('}':xs) nt rs = f (n - 1) xs nt ('}':rs)
            f n (x:xs) nt rs = f n xs nt (x:rs)
            f n _ _ rs = error $ "unMkName: invalid outer " ++ show a1
        g rs zs = (reverse zs,bitsToName (toAtom rs) nt1)
    (nameQuoted,nameModule,_,uqn,nameUniquifier) = deconstructName name
    (_,_,nameIdent) = nameParts uqn
    nameConstructor = isConstructor nm
    nameLevel = tyLevelOf nm

-- internal
mkNameType :: TyLevel -> Bool -> NameType
mkNameType l b = (f l b) where
    f l b   | l == termLevel = if b then DataConstructor else Val
            | l == typeLevel = if b then TypeConstructor else TypeVal
            | l == kindLevel && b = SortName
    f l b = error ("mkName: invalid " ++ show (l,b))

unRenameString :: String -> (Maybe Int,String)
unRenameString s = case span isDigit s of
    (ns@(_:_),'_':rs) -> (Just (read ns),rs)
    (_,_) -> (Nothing,s)

-- deconstruct name into all its possible parts
-- does not work on quoted names.
deconstructName :: Name -> (Bool,Maybe Module,Maybe UnqualifiedName,UnqualifiedName,Maybe Int)
deconstructName name | Just name <- fromQuotedName name = case deconstructName name of
    (_,a,b,c,d) -> (True,a,b,c,d)
deconstructName name = f nt where
    (nt,mod,id) = nameParts name
    (mi,id') = unRenameString id
    f _ = (False,mod,Nothing,toName nt id',mi)

originalUnqualifiedName :: Name -> Name
originalUnqualifiedName n = case unMkName n of
    NameParts { .. } -> toName (nameType n) nameIdent

--constructName :: Maybe Module -> Maybe UnqualifiedName -> UnqualifiedName -> Maybe Int -> Name
--constructName mm

----------------
-- Parameterized
----------------

-- Goal, get rid of hardcoded NameType, move pertinent info into cache byte

instance HasTyLevel Name where
    getTyLevel n = f (nameType n) where
        f DataConstructor = Just termLevel
        f Val             = Just termLevel
        f RawType         = Just typeLevel
        f TypeConstructor = Just typeLevel
        f TypeVal         = Just typeLevel
        f SortName
            | n == s_HashHash = Just $ succ kindLevel
            | n == s_StarStar = Just $ succ kindLevel
            | otherwise = Just kindLevel
        f QuotedName     = getTyLevel $ fromJust (fromQuotedName n)
        f _ = Nothing

isConstructor :: Name -> Bool
isConstructor n = f (nameType n) where
    f TypeConstructor = True
    f DataConstructor = True
    f SortName = True
    f QuotedName = isConstructor $ fromJust (fromQuotedName n)
    f _ = False

nameTyLevel_s tl n = nameTyLevel_u (const tl) n

nameTyLevel_u f (fromQuotedName -> Just qn) = quoteName $ nameTyLevel_u f qn
nameTyLevel_u f n = case getTyLevel n of
    Nothing -> n
    Just cl | cl == cl' -> n
            | otherwise -> toName (mkNameType cl' (isConstructor n)) n
        where cl' = f cl

removeUniquifier name = mkComplexName (unMkName name) { nameUniquifier = Nothing }
