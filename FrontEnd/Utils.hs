{-------------------------------------------------------------------------------

        Copyright:              The Hatchet Team (see file Contributors)

        Module:                 Utils

        Description:            Generic utilities that don't have a good home 
                                anywhere else.

        Primary Authors:        Bernie Pope

        Notes:                  See the file License for license information

-------------------------------------------------------------------------------}

module Utils where

import HsSyn   
import Char             
--import PPrint (PPrint (..),vcat)
import Control.Monad.Identity
import Doc.DocLike
import Doc.PPrint
import VConsts
import Name
import qualified Data.Map as Map





instance FromTupname HsName where 
    fromTupname (Qual (Module "Prelude") (HsIdent xs))  = fromTupname xs
    fromTupname _ = fail "fromTupname: not Prelude"

instance ToTuple HsName where
    toTuple n = (Qual (Module "Prelude") (HsIdent $ toTuple n))

--------------------------------------------------------------------------------

--getAModuleName :: HsModule -> Module
--getAModuleName (HsModule modName _ _ _) = modName

getDeclNames ::  HsDecl -> [HsName]
getDeclNames (HsTypeSig _ ns _ ) =  ns
getDeclNames d = maybeGetDeclName d

maybeGetDeclName :: Monad m => HsDecl -> m HsName
maybeGetDeclName (HsPatBind sloc (HsPVar name) rhs wheres) = return name
maybeGetDeclName (HsFunBind ((HsMatch _ name _ _ _):_)) = return name
maybeGetDeclName (HsDataDecl _ _ name  _ _ _) = return name
maybeGetDeclName (HsNewTypeDecl _ _ name  _ _ _) = return name
maybeGetDeclName (HsClassDecl _ qualType _)
   = case qualType of
        HsQualType _cntxt t
           -> return $ leftMostTyCon t
        HsUnQualType t
           -> return $ leftMostTyCon t 
maybeGetDeclName (HsForeignDecl _ _ _ n _) = return n
--maybeGetDeclName (HsTypeSig _ [n] _ ) = return n
maybeGetDeclName d = fail  $ "getDeclName: could not find name for a decl: " ++ show d 

getDeclName d =  runIdentity $ maybeGetDeclName d 

--getDeclName :: HsDecl -> HsName
--getDeclName (HsPatBind sloc (HsPVar name) rhs wheres) = name
--getDeclName (HsFunBind ((HsMatch _ name _ _ _):_)) = name
--getDeclName (HsDataDecl _ _ name  _ _ _) = name
--getDeclName (HsNewTypeDecl _ _ name  _ _ _) = name
--getDeclName (HsClassDecl _ qualType _)
--   = case qualType of
--        HsQualType _cntxt t
--           -> leftMostTyCon t
--        HsUnQualType t
--           -> leftMostTyCon t
--getDeclName (HsForeignDecl _ _ _ n _) = n
--getDeclName d = error $ "getDeclName: could not find name for a decl: " ++ show d 


-- gets the left most type constructor from a type

--leftMostTyCon (HsTyTuple ts) = error "leftMostTyCon: applied to a tuple"
leftMostTyCon (HsTyTuple ts) = toTuple (length ts)
leftMostTyCon (HsTyApp t1 _) = leftMostTyCon t1 
leftMostTyCon (HsTyVar _) = error "leftMostTyCon: applied to a variable"
leftMostTyCon (HsTyCon n) = n

hsNameToOrig :: HsName -> HsName 
hsNameToOrig n = hsNameIdent_u (hsIdentString_u dn) n where
    dn xs = case dropWhile isDigit xs of 
        ('_':xs) -> xs
        _ -> error $ "hsNameToOrig: " ++ show n



fromHsName :: HsName -> String
fromHsName (UnQual i) = hsIdentString i 
fromHsName (Qual (Module m) i) = m ++ "." ++ (hsIdentString i)

fromHsIdentifier :: HsIdentifier -> String
fromHsIdentifier = hsIdentString

isBindDecl :: HsDecl -> Bool
isBindDecl HsPatBind {} = True
isBindDecl HsFunBind {} = True 
isBindDecl _ = False


isSigDecl :: HsDecl -> Bool
isSigDecl HsTypeSig {} = True
isSigDecl _ = False

fst3 :: (a,b,c) -> a
fst3 (a,_,_) = a
snd3 :: (a,b,c) -> b
snd3 (_,b,_) = b
trd3 :: (a,b,c) -> c
trd3 (_,_,c) = c

-- takes a list of things and puts a seperator string after each elem
-- except the last, first arg is a function to convert the things into
-- strings
showListAndSep :: (a -> String) -> String -> [a] -> String
showListAndSep f sep [] = []
showListAndSep f sep [s] = f s
showListAndSep f sep (s:ss) = f s ++ sep ++ showListAndSep f sep ss

accLen :: Int -> [[a]] -> [(Int, [a])]
accLen width [] = []
accLen width (x:xs)
   = let newWidth
           = length x + width
     in (newWidth, x) : accLen newWidth xs

groupStringsToWidth :: Int -> [String] -> [String]
groupStringsToWidth width ss
   = groupStringsToWidth' width (accLen 0 ss)
   where
   groupStringsToWidth' :: Int -> [(Int,String)] -> [String]
   groupStringsToWidth' width [] = []
   groupStringsToWidth' width xs
      = headString : groupStringsToWidth' width (accLen 0 $ map snd rest)
      where
      (headSegments, rest)
         = case span ((<=width).fst) xs of
              ([], ss)     -> ([head ss], tail ss)
              anythingElse -> anythingElse
      headString = concatMap snd headSegments

showListAndSepInWidth :: (a -> String) -> Int -> String -> [a] -> String
showListAndSepInWidth _ _ _ [] = []
showListAndSepInWidth f width sep things
   = unlines $ groupStringsToWidth width newThings
   where
   newThings = (map ((\t -> t ++ sep).f) (init things)) ++ [f (last things)]

-- an infinite list of alphabetic strings in the usual order
nameSupply :: [String]
nameSupply
  = [ x++[y] | x <- []:nameSupply, y <- ['a'..'z'] ]

nameOfTyCon :: HsType -> HsName
nameOfTyCon (HsTyCon n) = n
nameOfTyCon (HsTyTuple xs) = toTuple (length xs)
nameOfTyCon (HsTyFun _ _) = hsTypName ("Prelude","->")
nameOfTyCon t = error $ "nameOfTyCon: " ++ show t

groupEquations :: [HsDecl] -> [(HsName, HsDecl)]
groupEquations [] = []
groupEquations (d:ds)
   = (getDeclName d, d) : groupEquations ds 

spacesToUnderscores :: String -> String
spacesToUnderscores 
   = map $ \c -> if (isSpace c) then '_' else c

rJustify :: Int -> String -> String
rJustify n s = replicate (n - length s) ' ' ++ s

lJustify :: Int -> String -> String
lJustify n s = take n $ s ++ repeat ' ' 






-- module qualifies a name if it isn't already qualified

qualifyName :: Module -> HsName -> HsName
qualifyName _ name@(Qual {}) = name
qualifyName mod (UnQual name) = Qual mod name

qualifyName' :: Module -> HsName -> HsName
qualifyName' mod (Qual _ name) = Qual mod name
qualifyName' mod (UnQual name) = Qual mod name

unqualifyName :: HsName -> HsName
unqualifyName (Qual _ n)  = UnQual n
unqualifyName n = n

-- -- The possible bindings for names 

data Binding
   = TopFun             -- function binding at the top level
   | ClassMethod        -- name of a method in a class
   | Instance           -- an instance decl lifted to a top-level binding
   | WhereFun           -- function binding in a where clause
   | LetFun             -- function binding in a let expression (used to include topbinds too)
   | LamPat             -- pattern binding in a lambda expression
   | CasePat            -- pattern binding in a case expression
   | GenPat             -- pattern binding in a generator statement
   | FunPat             -- pattern binding in a function declaration
   | Constr             -- name is a data constructor 
   deriving (Show, Eq, Enum)

-- pretty printing a HsName, Module and HsIdentifier

instance DocLike d => PPrint d HsName where
   pprint (Qual mod ident)
      -- don't print the Prelude module qualifier
      | mod == Module "Prelude" = pprint ident
      | otherwise               = pprint mod <> text "." <> pprint ident
   pprint (UnQual ident)
      = pprint ident

instance DocLike d => PPrint d Module where
   pprint (Module s) = text s

instance DocLike d => PPrint d HsIdentifier where
   pprint (HsIdent   s) = text s 
--   pprint (HsSymbol  s) = text s 
--   pprint (HsSpecial s) = text s 


pprintEnvMap m = vcat [ pprint x <+> text "::" <+> pprint y | (x,y) <- Map.toList m ]
