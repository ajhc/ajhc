-- Routines to check for several error and warning conditions which can be
-- locally determined from syntax.
module FrontEnd.HsErrors(
    hsType,
    hsDeclTopLevel,
    preTypecheckChecks,
    hsDeclLocal
    ) where

import Control.Monad.Writer
import FrontEnd.Class
import FrontEnd.HsSyn
import FrontEnd.Syn.Traverse
import FrontEnd.Warning
import GenUtil(hasRepeatUnder)
import Name.Names
import Util.Std

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
hsDecl cntx decl = withSrcLoc (srcLoc decl) $ f cntx decl where
    f _ d@HsTypeFamilyDecl { hsDeclFamily = True } = do
        warn (srcLoc d) UnsupportedFeature "Type families currently not supported"
    f TopLevel d@HsTypeFamilyDecl { hsDeclFamily = False, hsDeclHasKind = Nothing } = do
        warn (srcLoc d) UnsupportedFeature "Type families currently not supported"
    f l d@HsTypeDecl { } | l /= TopLevel= do
        warn (srcLoc d) UnsupportedFeature "Type families currently not supported"
    f TopLevel HsForeignDecl {} = return ()
    f TopLevel HsActionDecl {} = do
        wDecl "top level actions not supported"
    f TopLevel HsDataDecl { .. } = do
        --checkDeriving False hsDeclDerives
        when (hasRepeatUnder hsConDeclName hsDeclCons) $ do
            wDecl "repeated constructor name is not allowed"

--        let isEnum = all (\x ->  null (hsConDeclArgs x)) cs
--        when (not isEnum && class_Enum `elem` ds) $ warn sl "derive-enum" "Cannot derive enum from non enumeration type"
--        when (not isEnum && length cs /= 1 && class_Bounded `elem` ds) $ warn sl "derive-bounded" "Cannot derive bounded from non enumeration or unary type"
        return ()
    f TopLevel HsTypeDecl { hsDeclTArgs = as } = do
        when (any (not . isHsTyVar) as) $ do
            wDecl "complex type arguments not allowed for type synonym"
    f (InInstance ts) decl@HsTypeDecl { hsDeclTArgs = as }
        | length as < length ts || or (zipWith (/=) as ts) =
            wDecl "arguments to associated type must match instance head"
        | any (not . isHsTyVar) (drop (length ts) as) =
            wDecl "extra complex type arguments not allowed to type family"
    f InInstance {} HsTypeSig {} = wDecl "type signatures not allowed in instance declaration"
    f (InClass ts) HsTypeSig { .. } = do
        let HsQualType { .. } = hsDeclQualType
        when (null $ intersect (map removeUniquifier $ typeVars hsQualTypeType) [ removeUniquifier v | HsTyVar v <- ts]) $ do
            wDecl "types of class methods must have class variables"
    f TopLevel HsClassDecl { .. } = do
        let HsClassHead { .. } = hsDeclClassHead
        mapM_ (f (InClass hsClassHeadArgs)) hsDeclDecls
        when (any (not . isHsTyVar) hsClassHeadArgs) $ do
            wDecl $ "Class arguments must be plain variables"

    f TopLevel HsInstDecl { .. } = do
        let HsClassHead { .. } = hsDeclClassHead
        mapM_ (f (InInstance hsClassHeadArgs)) hsDeclDecls
        when (hasDuplicates . catMaybes $ map maybeGetDeclName hsDeclDecls) $ do
            wDecl "multiple definitions for the same method"
        let vs = concatMap typeVars hsClassHeadArgs
        when (hasDuplicates vs) $ do
            wDecl "type variables cannot be duplicated in instance head"

    f context HsClassDecl {} = wDecl $ "class declaration not allowed " ++ show context
    f context HsInstDecl {} = wDecl $ "instance declaration not allowed " ++ show context
    f context HsDataDecl {} = wDecl $ "data declaration not allowed " ++ show context
    f context HsForeignDecl {} = wDecl $ "foreign declaration not allowed " ++ show context
    f context HsActionDecl {} = wDecl $ "action declaration not allowed " ++ show context
    f _ d = return ()
    wDecl = addWarn InvalidDecl

preTypecheckChecks :: (MonadWarn m,MonadSrcLoc m,MonadSetSrcLoc m,Applicative m,TraverseHsOps a) => a -> m a
preTypecheckChecks x = traverseHsOps ops x where
    ops = (hsOpsDefault ops) { opHsExp, opHsPat } where
            opHsExp e = traverseHsOps ops e
            opHsPat p@(HsPIrrPat (Located sl HsPUnboxedTuple {})) = withSrcSpan sl $ do
                wExp "unboxed tuples may not be irrefutably matched"
                traverseHsOps ops p
            opHsPat p = traverseHsOps ops p
    wExp = addWarn InvalidExp

--    f context@(InClass ts) decl@HsTypeDecl { hsDeclTArgs = as }

hasDuplicates xs = any ((> 1) . length) $ group (sort xs)

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
