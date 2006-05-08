
module FrontEnd.Utils where

import Char
import Control.Monad.Identity
import qualified Data.Map as Map

import Doc.DocLike
import Doc.PPrint
import HsSyn
import Name.Name
import Name.Names
import Name.VConsts
import Representation()



--------------------------------------------------------------------------------


maybeGetDeclName :: Monad m => HsDecl -> m Name
maybeGetDeclName (HsPatBind sloc (HsPVar name) rhs wheres) = return (toName Val name)
maybeGetDeclName (HsFunBind ((HsMatch _ name _ _ _):_)) = return (toName Val name)
maybeGetDeclName (HsDataDecl _ _ name  _ _ _) = return (toName TypeConstructor name)
maybeGetDeclName (HsNewTypeDecl _ _ name  _ _ _) = return (toName TypeConstructor name)
maybeGetDeclName (HsClassDecl _ qualType _) = case qualType of
            HsQualType _cntxt t -> return $ leftMostTyCon t
        where
            leftMostTyCon (HsTyTuple ts) = error "lehtMostTyCon applied to tuple" -- toTuple (length ts)
            leftMostTyCon (HsTyApp t1 _) = leftMostTyCon t1
            leftMostTyCon (HsTyVar _) = error "leftMostTyCon: applied to a variable"
            leftMostTyCon (HsTyCon n) = (toName ClassName n)
            leftMostTyCon x = error $ "leftMostTyCon: " ++ show x
maybeGetDeclName x@HsForeignDecl {} = return $ toName Val $ hsDeclName x
maybeGetDeclName (HsForeignExport _ e _ _)   = return $ ffiExportName e
--maybeGetDeclName (HsTypeSig _ [n] _ ) = return n
maybeGetDeclName d = fail  $ "getDeclName: could not find name for a decl: " ++ show d

getDeclName d =  runIdentity $ maybeGetDeclName d



-- | Convert name to what it was before renaming.

hsNameToOrig :: HsName -> HsName
hsNameToOrig n = hsNameIdent_u (hsIdentString_u dn) n where
    dn xs = case dropWhile isDigit xs of
        ('_':xs) -> xs
        _ -> error $ "hsNameToOrig: " ++ show n

rJustify :: Int -> String -> String
rJustify n s = replicate (n - length s) ' ' ++ s

lJustify :: Int -> String -> String
lJustify n s = take n $ s ++ repeat ' '



pprintEnvMap m = vcat [ pprint x <+> text "::" <+> pprint y | (x,y) <- Map.toList m ]

type Context = [(Name,Name)]
hsContextToContext :: HsContext -> [(Name,Name)]
hsContextToContext xs = [ (toName ClassName c, toName TypeVal t) | (c,t) <- xs]

