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
import Control.Monad.Identity
import Doc.DocLike
import Doc.PPrint
import Name.VConsts
import Name.Names
import Name.Name
import qualified Data.Map as Map





instance FromTupname HsName where
    fromTupname (Qual (Module "Prelude") (HsIdent xs))  = fromTupname xs
    fromTupname _ = fail "fromTupname: not Prelude"

instance ToTuple HsName where
    toTuple n = (Qual (Module "Prelude") (HsIdent $ toTuple n))

--------------------------------------------------------------------------------


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




--leftMostTyCon (HsTyTuple ts) = error "leftMostTyCon: applied to a tuple"
leftMostTyCon (HsTyTuple ts) = toTuple (length ts)
leftMostTyCon (HsTyApp t1 _) = leftMostTyCon t1
leftMostTyCon (HsTyVar _) = error "leftMostTyCon: applied to a variable"
leftMostTyCon (HsTyCon n) = n
leftMostTyCon x = error $ "leftMostTyCon: " ++ show x


-- | Convert name to what it was before renaming.

hsNameToOrig :: HsName -> HsName
hsNameToOrig n = hsNameIdent_u (hsIdentString_u dn) n where
    dn xs = case dropWhile isDigit xs of
        ('_':xs) -> xs
        _ -> error $ "hsNameToOrig: " ++ show n



isSigDecl :: HsDecl -> Bool
isSigDecl HsTypeSig {} = True
isSigDecl _ = False

fst3 :: (a,b,c) -> a
fst3 (a,_,_) = a
snd3 :: (a,b,c) -> b
snd3 (_,b,_) = b
trd3 :: (a,b,c) -> c
trd3 (_,_,c) = c

-- an infinite list of alphabetic strings in the usual order
nameSupply :: [String]
nameSupply
  = [ x++[y] | x <- []:nameSupply, y <- ['a'..'z'] ]

nameOfTyCon :: HsType -> HsName
nameOfTyCon (HsTyCon n) = n
nameOfTyCon (HsTyTuple xs) = toTuple (length xs)
nameOfTyCon (HsTyFun _ _) = nameName tc_Arrow
nameOfTyCon t = error $ "nameOfTyCon: " ++ show t

groupEquations :: [HsDecl] -> [(HsName, HsDecl)]
groupEquations [] = []
groupEquations (d:ds)
   = (getDeclName d, d) : groupEquations ds


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


pprintEnvMap m = vcat [ pprint x <+> text "::" <+> pprint y | (x,y) <- Map.toList m ]


