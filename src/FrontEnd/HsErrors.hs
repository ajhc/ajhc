-- Routines to check for several error and warning conditions which can be
-- locally determined from syntax.
module FrontEnd.HsErrors(
    hsType,
    hsDeclTopLevel,
    hsDeclLocal
    ) where

import Control.Applicative(Applicative)
import Control.Monad
import Control.Monad.Writer
import Data.List
import FrontEnd.Class
import FrontEnd.HsSyn
import FrontEnd.SrcLoc
import FrontEnd.Syn.Traverse
import FrontEnd.Warning
import Name.Name

hsType :: (Applicative m,MonadSrcLoc m, MonadWarn m) => HsType -> m ()
hsType x = traverseHsType (\x -> hsType x >> return x) x >> return ()

data Context = InClass [HsType] | InInstance [HsType] | TopLevel | Local
    deriving(Eq)

instance Show Context where
    show InClass {} = "in a class declaration"
    show InInstance {} = "in an instance declaration"
    show TopLevel = "at the top level"
    show Local = "in local declaration block"

hsDeclTopLevel,hsDeclLocal :: (MonadSetSrcLoc m, MonadSrcLoc m,MonadWarn m) => HsDecl -> m ()
hsDeclTopLevel d = withSrcLoc (srcLoc d) $ hsDecl TopLevel d
hsDeclLocal d = withSrcLoc (srcLoc d) $ hsDecl Local d

typeVars t = execWriter $ f t where
    f (HsTyVar v) = tell [v]
    f t = traverseHsType_ f t

hsDecl :: (MonadSetSrcLoc m,MonadSrcLoc m, MonadWarn m) => Context -> HsDecl -> m ()
hsDecl cntx decl = f cntx decl where
    f _ d@HsTypeFamilyDecl { } = do
        warn (srcLoc d) UnsupportedFeature "Type families currently not supported"
    f l d@HsTypeDecl { } | l /= TopLevel= do
        warn (srcLoc d) UnsupportedFeature "Type families currently not supported"
    f TopLevel HsDataDecl { .. } = do
        let ds = map (toName ClassName) hsDeclDerives
        checkDeriving False ds
--        let isEnum = all (\x ->  null (hsConDeclArgs x)) cs
--        when (not isEnum && class_Enum `elem` ds) $ warn sl "derive-enum" "Cannot derive enum from non enumeration type"
--        when (not isEnum && length cs /= 1 && class_Bounded `elem` ds) $ warn sl "derive-bounded" "Cannot derive bounded from non enumeration or unary type"
        return ()
    f TopLevel HsTypeDecl { hsDeclTArgs = as } = do
        when (any (not . isHsTyVar) as) $ do
            wDecl $ "complex type arguments not allowed for type synonym"
    f (InInstance ts) decl@HsTypeDecl { hsDeclTArgs = as }
        | length as < length ts || or (zipWith (/=) as ts) =
            wDecl "arguments to associated type must match instance head"
        | any (not . isHsTyVar) (drop (length ts) as) =
            wDecl $ "extra complex type arguments not allowed to type family"
    f (InClass ts) HsTypeSig { .. } = do
        let HsQualType { .. } = hsDeclQualType
--        when (null $ intersect (typeVars hsQualTypeType) [ v | HsTyVar v <- ts]) $ do
 --           wDecl "types of class methods must have class variables"
        return ()

    f TopLevel HsClassDecl { .. } = do
        let HsClassHead { .. } = hsDeclClassHead
        mapM_ (f (InClass hsClassHeadArgs)) hsDeclDecls
        when (any (not . isHsTyVar) hsClassHeadArgs) $ do
            wDecl $ "Class arguments must be plain variables"

    f TopLevel HsInstDecl { .. } = do
        let HsClassHead { .. } = hsDeclClassHead
        mapM_ (f (InInstance hsClassHeadArgs)) hsDeclDecls
        let vs = concatMap typeVars hsClassHeadArgs
        when (hasDuplicates vs) $ do
            wDecl "type variables cannot be duplicated in instance head"

    f context HsClassDecl {} = wDecl $ "class declaration not allowed " ++ show context
    f context HsInstDecl {} = wDecl $ "instance declaration not allowed " ++ show context
    f context HsDataDecl {} = wDecl $ "data declaration not allowed " ++ show context
    f _ d = applyHsOps ops d >> return ()
    wDecl = addWarn InvalidDecl
    wExp = addWarn InvalidExp
    ops = (hsOpsDefault ops) { opHsExp, opHsPat } where
            opHsExp e = traverseHsOps ops e
            opHsPat p@(HsPIrrPat (Located sl HsPUnboxedTuple {})) = withSrcSpan sl $ do
                wExp "unboxed tuples may not be irrefutably matched"
                traverseHsOps ops p
            opHsPat p = traverseHsOps ops p

--    f context@(InClass ts) decl@HsTypeDecl { hsDeclTArgs = as }

hasDuplicates xs = any ((> 1) . length) $ group (sort xs)

--        | any (not . isHsTyVar) as = warn (srcLoc decl) InvalidDecl $ "complex type arguments not allowed " ++ show context
    --    | length as < length ts || or (zipWith (/=) as ts) = warn (srcLoc decl) "invalid-assoc" $ "arguments to associated type must match class decl" ++ show (as,ts)
--    f TopLevel HsNewTypeDecl { hsDeclSrcLoc = sl, hsDeclDerives = ds' } = do
--        let ds = map (toName ClassName) ds'
--        checkDeriving sl True ds
--        return ()

--    f context decl@HsNewTypeDecl {} = warn (srcLoc decl) InvalidDecl $ "newtype declaration not allowed " ++ show context
--    f TopLevel decl@HsClassDecl { hsDeclQualType = qt, hsDeclDecls = decls } = do args <- fetchQtArgs (srcLoc decl) qt; mapM_ (f (InClass args)) decls

--fetchQtArgs sl HsQualType { hsQualTypeType = t } | (HsTyCon {},args@(_:_)) <- fromHsTypeApp t = return args
--fetchQtArgs sl _ = warn sl InvalidDecl "invalid head in class or instance decl" >> return []

checkDeriving _ xs | all (`elem` derivableClasses) xs = return ()
checkDeriving False xs
  = let nonDerivable = filter (`notElem` derivableClasses) xs
    in addWarn (UnknownDeriving nonDerivable) ("attempt to derive from a non-derivable class: " ++ unwords (map show nonDerivable))
checkDeriving True _ = addWarn InvalidDecl "generalized newtype deriving not implemented."

--fromHsTypeApp t = f t [] where
--    f (HsTyApp a b) rs = f a (b:rs)
--    f t rs = (t,rs)
