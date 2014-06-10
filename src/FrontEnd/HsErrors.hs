-- Routines to check for several error and warning conditions which can be
-- locally determined from syntax.
module FrontEnd.HsErrors(
    hsType,
    hsDeclTopLevel,
    preTypecheckChecks,
    hsDeclLocal
    ) where

import Control.Monad.Writer
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
    f (InInstance ts) decl = g decl where
        g decl@HsTypeDecl { hsDeclTArgs = as }
            | length as < length ts || or (zipWith (/=) as ts) =
                wDecl "arguments to associated type must match instance head"
            | any (not . isHsTyVar) (drop (length ts) as) =
                wDecl "extra complex type arguments not allowed to type family"

        g decl@HsPatBind { hsDeclPat = p } | not $ isHsPVar p  =
                wDecl "Instance methods may not be bound by a pattern match."
        g HsTypeSig {} = wDecl "type signatures not allowed in instance declaration"
        g decl = do simpleDecl decl
    f TopLevel decl = g decl where
        g  d@HsTypeFamilyDecl { hsDeclFamily = False, hsDeclHasKind = Nothing } = do
            warn (srcLoc d) UnsupportedFeature "Type families currently not supported"
        g  HsForeignDecl {} = return ()
        g  HsActionDecl {} = do
            wDecl "top level actions not supported"
        g  HsDataDecl { .. } = do
            when (hasRepeatUnder hsConDeclName hsDeclCons) $ do
                wDecl "repeated constructor name is not allowed"
            when (hsDeclDeclType == DeclTypeNewtype && any (not . null. hsConDeclExists) hsDeclCons) $ do
                wDecl "existential constructors not allowed on newtypes."
            return ()
        g  HsTypeDecl { hsDeclTArgs = as } = do
            when (any (not . isHsTyVar) as) $ do
                wDecl "complex type arguments not allowed for type synonym"
        g HsClassDecl { .. } = do
            let HsClassHead { .. } = hsDeclClassHead
            mapM_ (f (InClass hsClassHeadArgs)) hsDeclDecls
            when (any (not . isHsTyVar) hsClassHeadArgs) $ do
                wDecl $ "Class arguments must be plain variables"
        g HsInstDecl { .. } = do
            let HsClassHead { .. } = hsDeclClassHead
            mapM_ (f (InInstance hsClassHeadArgs)) hsDeclDecls
            when (hasDuplicates . catMaybes $ map maybeGetDeclName hsDeclDecls) $ do
                wDecl "multiple definitions for the same method"
            let vs = concatMap typeVars hsClassHeadArgs
            when (hasDuplicates vs) $ do
                wDecl "type variables cannot be duplicated in instance head"
        g decl = allDecls decl
    f (InClass ts) HsTypeSig { .. } = do
        let HsQualType { .. } = hsDeclQualType
        when (null $ intersect (map removeUniquifier $ typeVars hsQualTypeType) [ removeUniquifier v | HsTyVar v <- ts]) $ do
            wDecl "types of class methods must have class variables"
    f InClass {} decl = simpleDecl decl
    f _ decl = simpleDecl decl
    simpleDecl  HsClassDecl {} = wDecl $ "class declaration not allowed " ++ show cntx
    simpleDecl  HsInstDecl {} = wDecl $ "instance declaration not allowed " ++ show cntx
    simpleDecl  HsDataDecl {} = wDecl $ "data declaration not allowed " ++ show cntx
    simpleDecl  HsForeignDecl {} = wDecl $ "foreign declaration not allowed " ++ show cntx
    simpleDecl  HsActionDecl {} = wDecl $ "action declaration not allowed " ++ show cntx
    simpleDecl d@HsTypeDecl {} = warn (srcLoc d) UnsupportedFeature "Type families currently not supported"
    simpleDecl decl = allDecls decl

    allDecls d@HsTypeFamilyDecl { hsDeclFamily = True } = do
        warn (srcLoc d) UnsupportedFeature "Type families currently not supported"
    allDecls _ = return ()
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

hasDuplicates xs = any ((> 1) . length) $ group (sort xs)
