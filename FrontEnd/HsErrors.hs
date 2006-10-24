-- |
-- Routines to check for several error and warning conditions which can be locally determined from syntax.
--

module FrontEnd.HsErrors(hsType,hsDecl,checkDeriving) where

import FrontEnd.Class
import FrontEnd.SrcLoc
import FrontEnd.Syn.Traverse
import HsSyn
import Monad
import Name.Name
import Name.Names
import Name.VConsts
import Warning




hsType :: MonadWarn m => HsType -> m ()
hsType x@HsTyForall {} = do
    err "h98-forall" "Explicit quantification is a non-haskell98 feature"
    hsQualType (hsTypeType x)
hsType x@HsTyExists {} = do
    err "h98-forall" "Explicit quantification is a non-haskell98 feature"
    hsQualType (hsTypeType x)
hsType x = traverseHsType (\x -> hsType x >> return x) x >> return ()

hsQualType x  = hsType (hsQualTypeType x)


data Context = InClass | InInstance | TopLevel
    deriving(Eq)

instance Show Context where
    show InClass = "in a class declaration"
    show InInstance = "in an instance declaration"
    show TopLevel = "at the top level"


hsDecl :: MonadWarn m => HsDecl -> m ()
hsDecl decl = f TopLevel decl where
    f TopLevel HsDataDecl { hsDeclSrcLoc = sl, hsDeclCons = cs, hsDeclDerives = ds' } = do
        let ds = map (toName ClassName) ds'
        when (null cs) $ warn sl "h98-emptydata" "data types with no constructors are a non-haskell98 feature"
        checkDeriving sl False ds
        let isEnum = all (\x ->  null (hsConDeclArgs x)) cs
        when (not isEnum && class_Enum `elem` ds) $ warn sl "derive-enum" "Cannot derive enum from non enumeration type"
        when (not isEnum && length cs /= 1 && class_Bounded `elem` ds) $ warn sl "derive-bounded" "Cannot derive bounded from non enumeration or unary type"
        return ()
    f TopLevel HsNewTypeDecl { hsDeclSrcLoc = sl, hsDeclDerives = ds' } = do
        let ds = map (toName ClassName) ds'
        checkDeriving sl True ds
        return ()
    f context decl@HsTypeDecl { hsDeclTArgs = as }
        | context `elem` [TopLevel, InClass], any (not . isHsTyVar) as = warn (srcLoc decl) "invalid-decl" $ "complex type arguments not allowed " ++ show context
    f context decl@HsDataDecl {} = warn (srcLoc decl) "invalid-decl" $ "data declaration not allowed " ++ show context
    f context decl@HsNewTypeDecl {} = warn (srcLoc decl) "invalid-decl" $ "newtype declaration not allowed " ++ show context
    f TopLevel decl@HsClassDecl {} = return ()
    f TopLevel decl@HsInstDecl {} = return ()
    f context decl@HsClassDecl {} = warn (srcLoc decl) "invalid-decl" $ "class declaration not allowed " ++ show context
    f context decl@HsInstDecl {} = warn (srcLoc decl) "invalid-decl" $ "instance declaration not allowed " ++ show context

    f _ _ = return ()


checkDeriving _ _ xs | all (`elem` derivableClasses) xs = return ()
checkDeriving sl True _ = warn sl "h98-newtypederiv" "arbitrary newtype derivations are a non-haskell98 feature"
checkDeriving sl False _ = warn sl "unknown-deriving" "attempt to derive from a non-derivable class"


