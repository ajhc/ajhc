-- Routines to check for several error and warning conditions which can be
-- locally determined from syntax.
module FrontEnd.HsErrors(
    hsType,
    hsDeclTopLevel,
    hsDeclLocal
    ) where

import FrontEnd.Class
import FrontEnd.HsSyn
import FrontEnd.SrcLoc
import FrontEnd.Syn.Traverse
import FrontEnd.Warning
import Name.Name

hsType :: (MonadSrcLoc m, MonadWarn m) => HsType -> m ()
--hsType x@HsTyForall {} = do
--    addWarn "h98-forall" "Explicit quantification is a non-haskell98 feature"
--    hsQualType (hsTypeType x)
--hsType x@HsTyExists {} = do
--    addWarn "h98-forall" "Explicit quantification is a non-haskell98 feature"
--    hsQualType (hsTypeType x)
hsType x = traverseHsType (\x -> hsType x >> return x) x >> return ()

hsQualType x  = hsType (hsQualTypeType x)

data Context = InClass [HsType] | InInstance [HsType] | TopLevel | Local
    deriving(Eq)

instance Show Context where
    show InClass {} = "in a class declaration"
    show InInstance {} = "in an instance declaration"
    show TopLevel = "at the top level"
    show Local = "in local declaration block"

hsDeclTopLevel,hsDeclLocal :: MonadWarn m => HsDecl -> m ()
hsDeclTopLevel = hsDecl TopLevel
hsDeclLocal = hsDecl Local

hsDecl :: MonadWarn m => Context -> HsDecl -> m ()
hsDecl cntx decl = f cntx decl where
    f _ d@HsTypeFamilyDecl { } = do
        warn (srcLoc d) UnsupportedFeature "Type families currently not supported"
    f l d@HsTypeDecl { } | l /= TopLevel= do
        warn (srcLoc d) UnsupportedFeature "Type families currently not supported"
    f TopLevel HsDataDecl { hsDeclSrcLoc = sl, hsDeclCons = cs, hsDeclDerives = ds' } = do
        let ds = map (toName ClassName) ds'
--        when (null cs) $ warn sl "h98-emptydata" "data types with no constructors are a non-haskell98 feature"
        checkDeriving sl False ds
        let isEnum = all (\x ->  null (hsConDeclArgs x)) cs
--        when (not isEnum && class_Enum `elem` ds) $ warn sl "derive-enum" "Cannot derive enum from non enumeration type"
--        when (not isEnum && length cs /= 1 && class_Bounded `elem` ds) $ warn sl "derive-bounded" "Cannot derive bounded from non enumeration or unary type"
        return ()
--    f TopLevel HsNewTypeDecl { hsDeclSrcLoc = sl, hsDeclDerives = ds' } = do
--        let ds = map (toName ClassName) ds'
--        checkDeriving sl True ds
--        return ()
    f context@TopLevel decl@HsTypeDecl { hsDeclTArgs = as } | any (not . isHsTyVar) as = warn (srcLoc decl) InvalidDecl $ "complex type arguments not allowed " ++ show context
--    f context@(InClass ts) decl@HsTypeDecl { hsDeclTArgs = as }
--        | any (not . isHsTyVar) as = warn (srcLoc decl) InvalidDecl $ "complex type arguments not allowed " ++ show context
    --    | length as < length ts || or (zipWith (/=) as ts) = warn (srcLoc decl) "invalid-assoc" $ "arguments to associated type must match class decl" ++ show (as,ts)
    f context@(InInstance ts) decl@HsTypeDecl { hsDeclTArgs = as }
    --    | length as < length ts || or (zipWith (==) as ts) = warn (srcLoc decl) "invalid-assoc" $ "arguments to associated type must match instance head"
        | any (not . isHsTyVar) (drop (length ts) as) = warn (srcLoc decl) InvalidDecl $ "extra complex type arguments not allowed " ++ show context
    f context decl@HsDataDecl {} = warn (srcLoc decl) InvalidDecl $ "data declaration not allowed " ++ show context
--    f context decl@HsNewTypeDecl {} = warn (srcLoc decl) InvalidDecl $ "newtype declaration not allowed " ++ show context
--    f TopLevel decl@HsClassDecl { hsDeclQualType = qt, hsDeclDecls = decls } = do args <- fetchQtArgs (srcLoc decl) qt; mapM_ (f (InClass args)) decls
    f TopLevel decl@HsClassDecl { hsDeclClassHead = ch, hsDeclDecls = decls } = do mapM_ (f (InClass (hsClassHeadArgs ch))) decls
    f TopLevel decl@HsInstDecl { hsDeclClassHead = ch, hsDeclDecls = decls } = do mapM_ (f (InInstance (hsClassHeadArgs ch))) decls
    --f TopLevel decl@HsInstDecl { hsDeclQualType = qt, hsDeclDecls = decls } = do args <- fetchQtArgs (srcLoc decl) qt; mapM_ (f (InInstance args)) decls
    f context decl@HsClassDecl {} = warn (srcLoc decl) InvalidDecl $ "class declaration not allowed " ++ show context
    f context decl@HsInstDecl {} = warn (srcLoc decl) InvalidDecl $ "instance declaration not allowed " ++ show context

    f _ _ = return ()

fetchQtArgs sl HsQualType { hsQualTypeType = t } | (HsTyCon {},args@(_:_)) <- fromHsTypeApp t = return args
fetchQtArgs sl _ = warn sl InvalidDecl "invalid head in class or instance decl" >> return []

checkDeriving _ _ xs | all (`elem` derivableClasses) xs = return ()
--checkDeriving sl True _ = warn sl "h98-newtypederiv" "arbitrary newtype derivations are a non-haskell98 feature"
checkDeriving sl False xs
  = let nonDerivable = filter (`notElem` derivableClasses) xs
    in warn sl (UnknownDeriving nonDerivable) ("attempt to derive from a non-derivable class: " ++ unwords (map show nonDerivable))

fromHsTypeApp t = f t [] where
    f (HsTyApp a b) rs = f a (b:rs)
    f t rs = (t,rs)
