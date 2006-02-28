-- |
-- Routines to check for several error and warning conditions which can be locally determined from syntax.
--

module FrontEnd.HsErrors where

import Class
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
hsType x = mapHsTypeHsType (\x -> hsType x >> return x) x >> return ()

hsQualType x  = hsType (hsQualTypeType x)



hsDecl :: MonadWarn m => HsDecl -> m ()
hsDecl HsDataDecl { hsDeclSrcLoc = sl, hsDeclCons = cs, hsDeclDerives = ds' } = do
    let ds = map (toName ClassName) ds'
    when (null cs) $ warn sl "h98-emptydata" "data types with no constructors are a non-haskell98 feature"
    checkDeriving sl False ds
    let isEnum = all (\x ->  null (hsConDeclArgs x)) cs
    when (not isEnum && class_Enum `elem` ds) $ warn sl "derive-enum" "Cannot derive enum from non enumeration type"
    when (not isEnum && length cs /= 1 && class_Bounded `elem` ds) $ warn sl "derive-bounded" "Cannot derive bounded from non enumeration or unary type"
    return ()
hsDecl HsNewTypeDecl { hsDeclSrcLoc = sl, hsDeclDerives = ds' } = do
    let ds = map (toName ClassName) ds'
    checkDeriving sl True ds
    return ()
hsDecl _ = return ()



--derivable = [ "Eq","Ord","Enum","Bounded","Show","Read" ]

checkDeriving _ _ xs | all (`elem` derivableClasses) xs = return ()
checkDerining sl True _ = warn sl "h98-newtypederiv" "arbitrary newtype derivations are a non-haskell98 feature"
checkDerining sl False _ = warn sl "unknown-deriving" "attempt to derive from a non-derivable class"



mapHsTypeHsType f (HsTyFun a b) = do
    a <- f a
    b <- f b
    return $ HsTyFun a b
mapHsTypeHsType f (HsTyTuple xs) = do
    xs <- mapM f xs
    return $ HsTyTuple xs
mapHsTypeHsType f (HsTyApp a b) = do
    a <- f a
    b <- f b
    return $ HsTyApp a b
mapHsTypeHsType f (HsTyForall vs qt) = do
    x <- f $ hsQualTypeType qt
    return $ HsTyForall vs qt { hsQualTypeType = x }
mapHsTypeHsType f (HsTyExists vs qt) = do
    x <- f $ hsQualTypeType qt
    return $ HsTyExists vs qt { hsQualTypeType = x }
mapHsTypeHsType _ x = return x


