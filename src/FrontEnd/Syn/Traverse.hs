{-# OPTIONS_GHC -fwarn-unused-matches  -fwarn-incomplete-patterns -fwarn-type-defaults #-}
module FrontEnd.Syn.Traverse where

import Control.Applicative
import Control.Monad.Writer
import qualified Data.Set as Set
import qualified Data.Traversable as T

import FrontEnd.HsSyn
import FrontEnd.SrcLoc
import Name.Name
import Support.FreeVars
import Util.Inst()

--instance FreeVars HsType (Set.Set HsName) where
--    freeVars t = execWriter (f t) where
--        f (HsTyVar v) = tell (Set.singleton v)
--        f t = traverseHsType_ f t

instance FreeVars HsType (Set.Set Name) where
    freeVars t = execWriter (f t) where
        f (HsTyVar v) = tell (Set.singleton $ toName TypeVal v)
        f (HsTyCon v) = tell (Set.singleton $ toName TypeConstructor v)
        f t = traverseHsType_ f t

traverse_ :: Applicative m => (a -> m b) -> a -> m a
traverse_ fn x = fn x *> pure x

traverseHsExp_ :: (Monad m,Applicative m,MonadSetSrcLoc m) => (HsExp -> m ()) -> HsExp -> m ()
traverseHsExp_ fn e = traverseHsExp (traverse_ fn) e *> pure ()

traverseHsExp :: (Monad m,MonadSetSrcLoc m) => (HsExp -> m HsExp) -> HsExp -> m HsExp
traverseHsExp fn e = f e where
    fns = mapM fn
    f e@HsVar {} = return e
    f e@HsCon {} = return e
    f e@HsLit {} = return e
    f e@HsError {} = return e
    f (HsInfixApp hsExp1 hsExp2 hsExp3) = do
        hsExp1' <- fn hsExp1
        hsExp2' <- fn hsExp2
        hsExp3' <- fn hsExp3
        return (HsInfixApp hsExp1' hsExp2' hsExp3')
    f (HsApp hsExp1 hsExp2)  = do
        hsExp1' <- fn hsExp1
        hsExp2' <- fn hsExp2
        return (HsApp hsExp1' hsExp2')
    f (HsNegApp hsExp)  = do
        hsExp' <- fn hsExp
        return (HsNegApp hsExp')
    f (HsLambda srcLoc hsPats hsExp) = withSrcLoc srcLoc $ do
        hsExp' <- fn hsExp
        return (HsLambda srcLoc hsPats hsExp')
    f (HsIf hsExp1 hsExp2 hsExp3)  = do
        hsExp1' <- fn hsExp1
        hsExp2' <- fn hsExp2
        hsExp3' <- fn hsExp3
        return (HsIf hsExp1' hsExp2' hsExp3')
    f (HsTuple hsExps)  = do
        hsExps' <- fns hsExps
        return (HsTuple hsExps')
    f (HsUnboxedTuple hsExps)  = do
        hsExps' <- fns hsExps
        return (HsUnboxedTuple hsExps')
    f (HsList hsExps)  = do
        hsExps' <- fns hsExps
        return (HsList hsExps')
    f (HsParen hsExp)  = do
        hsExp' <- fn hsExp
        return (HsParen hsExp')
    f (HsLeftSection hsExp1 hsExp2)  = do
        hsExp1' <- fn hsExp1
        hsExp2' <- fn hsExp2
        return (HsLeftSection hsExp1' hsExp2')
    f (HsRightSection hsExp1 hsExp2)  = do
        hsExp1' <- fn hsExp1
        hsExp2' <- fn hsExp2
        return (HsRightSection hsExp1' hsExp2')
    f (HsEnumFrom hsExp)  = do
        hsExp' <- fn hsExp
        return (HsEnumFrom hsExp')
    f (HsEnumFromTo hsExp1 hsExp2)  = do
        hsExp1' <- fn hsExp1
        hsExp2' <- fn hsExp2
        return (HsEnumFromTo hsExp1' hsExp2')
    f (HsEnumFromThen hsExp1 hsExp2)  = do
        hsExp1' <- fn hsExp1
        hsExp2' <- fn hsExp2
        return (HsEnumFromThen hsExp1' hsExp2')
    f (HsEnumFromThenTo hsExp1 hsExp2 hsExp3)  = do
        hsExp1' <- fn hsExp1
        hsExp2' <- fn hsExp2
        hsExp3' <- fn hsExp3
        return (HsEnumFromThenTo hsExp1' hsExp2' hsExp3')
    f (HsExpTypeSig srcLoc hsExp hsQualType)  = withSrcLoc srcLoc $ do
        hsExp' <- fn hsExp
        return (HsExpTypeSig srcLoc hsExp' hsQualType)
    f (HsAsPat hsName hsExp)  = do
        hsExp' <- fn hsExp
        return (HsAsPat hsName hsExp')
    f (HsWildCard x) = do return (HsWildCard x)
    f (HsIrrPat hsExp)  = do
        hsExp' <- fnl hsExp
        return (HsIrrPat hsExp')
    f (HsBangPat hsExp)  = do
        hsExp' <- fnl hsExp
        return (HsBangPat hsExp')
    f (HsRecConstr n fus) = do
        fus' <- mapM (T.mapM fn) fus
        return $ HsRecConstr n fus'
    f (HsRecUpdate e fus) = do
        fus' <- mapM (T.mapM fn) fus
        e' <- fn e
        return $ HsRecUpdate e' fus'
    f (HsLocatedExp le) = HsLocatedExp `liftM` fnl le
    f (HsLet hsDecls hsExp)  = do
        ds <- mapM (traverseHsDeclHsExp fn) hsDecls
        e <- fn hsExp
        return $ HsLet ds e
    f (HsDo hsStmts)  = HsDo `liftM` mapM (traverseHsStmtHsExp fn) hsStmts
    f (HsWords ws) = HsWords <$> T.traverse fn ws
    f (HsBackTick e) = HsBackTick <$> fn e
    f h = error $ "FrontEnd.Syn.Traverse.traverseHsExp f unrecognized construct: " ++ show h
    fnl (Located l e) = withSrcSpan l $ Located l `liftM` fn e

    {-
-- not done
    f (HsRecUpdate hsExp hsFieldUpdates)  = do
        hsExp' <- fn hsExp
        hsFieldUpdates' <- renameHsFieldUpdates hsFieldUpdates
        return (HsRecUpdate hsExp' hsFieldUpdates')
    fn (HsRecConstr hsName hsFieldUpdates)  = do
        hsName' <- renameHsName hsName   -- do I need to change this name?
        hsFieldUpdates' <- renameHsFieldUpdates hsFieldUpdates
        return (HsRecConstr hsName' hsFieldUpdates')
--    fn (HsCase hsExp hsAlts)  = do
--        hsExp' <- fn hsExp
--        hsAlts' <- renameHsAlts hsAlts
--        return (HsCase hsExp' hsAlts')
--    fn (HsDo hsStmts)  = do
--        let e = doToExp hsStmts
--        fn e
        --(hsStmts',_) <- renameHsStmts hsStmts
        --return (doToExp hsStmts')
    fn (HsListComp hsExp hsStmts)  = do
        (hsStmts',') <- renameHsStmts hsStmts
        hsExp' <- fn hsExp '
        return (HsListComp hsExp' hsStmts')
    fn (HsLet hsDecls hsExp)  = do
        ' <- updateSubTableWithHsDecls  hsDecls LetFun
        hsDecls' <- renameHsDecls hsDecls '
        hsExp' <- fn hsExp '
        return (HsLet hsDecls' hsExp')

-}

traverseHsType_ :: Applicative m => (HsType -> m b) -> HsType -> m ()
traverseHsType_ fn p = traverseHsType (traverse_ fn) p *> pure ()

traverseHsType :: Applicative m => (HsType -> m HsType) -> HsType -> m HsType
traverseHsType fn t = f t where
    f (HsTyFun a1 a2) = HsTyFun <$> fn a1 <*> fn a2
    f (HsTyTuple a1) = HsTyTuple <$> T.traverse fn a1
    f (HsTyUnboxedTuple a1) = HsTyUnboxedTuple <$> T.traverse fn a1
    f (HsTyApp a1 a2) = HsTyApp <$> fn a1 <*> fn a2
    f (HsTyForall vs qt) = doQual HsTyForall f vs qt
    f (HsTyExists vs qt) = doQual HsTyExists f vs qt
    f x@HsTyVar {} = pure x
    f x@HsTyCon {} = pure x
    f HsTyExpKind { .. } = h <$> T.traverse fn hsTyLType
        where h hsTyLType = HsTyExpKind { .. }
--    f HsTyExpKind { .. } = do
 --   hsTyLType <- T.mapM f hsTyLType
  --  return HsTyExpKind { .. }
    f (HsTyEq a1 a2) = HsTyEq <$> fn a1 <*> fn a2
    --f (HsTyEq a b) = return HsTyEq `ap` f a `ap` f b
    f (HsTyStrictType a1 a2) = HsTyStrictType <$> pure a1 <*> T.traverse fn a2
--    f (HsTyStrictType a b ) = return HsTyStrictType `ap` return a `ap` T.mapM f b
    f HsTyAssoc = pure HsTyAssoc

doQual :: Applicative m => (a -> HsQualType -> b) -> (HsType -> m HsType) -> a -> HsQualType -> m b
doQual hsTyForall f vs qt = cr <$> cntx <*> f (hsQualTypeType qt) where
    cr cntx x = hsTyForall vs qt { hsQualTypeContext = cntx, hsQualTypeType = x }
    cntx = flip T.traverse (hsQualTypeContext qt) $ \v -> case v of
         x@HsAsst {} -> pure x
         HsAsstEq a b -> HsAsstEq <$> f a <*> f b
--    return $ hsTyForall vs qt { hsQualTypeContext = cntx, hsQualTypeType = x }

traverseHsPat_ :: (Monad m,Applicative m,MonadSetSrcLoc m) => (HsPat -> m b) -> HsPat -> m ()
traverseHsPat_ fn p = traverseHsPat (traverse_ fn) p *> pure ()

traverseHsPat :: (Monad m,MonadSetSrcLoc m) => (HsPat -> m HsPat) -> HsPat -> m HsPat
traverseHsPat fn p = f p where
    f p@HsPVar {} = return p
    f p@HsPLit {} = return p
    f (HsPNeg hsPat)  = do
          hsPat' <- fn hsPat
          return (HsPNeg hsPat')
    f (HsPInfixApp hsPat1 hsName hsPat2)  = do
          hsPat1' <- fn hsPat1
          hsPat2' <- fn hsPat2
          return (HsPInfixApp hsPat1' hsName hsPat2')
    f (HsPApp hsName hsPats)  = do
          hsPats' <- mapM fn hsPats
          return (HsPApp hsName hsPats')
    f (HsPTuple hsPats)  = do
          hsPats' <- mapM fn hsPats
          return (HsPTuple hsPats')
    f (HsPUnboxedTuple hsPats)  = do
          hsPats' <- mapM fn hsPats
          return (HsPUnboxedTuple hsPats')
    f (HsPList hsPats)  = do
          hsPats' <- mapM fn hsPats
          return (HsPList hsPats')
    f (HsPParen hsPat)  = do
          hsPat' <- fn hsPat
          return (HsPParen hsPat')
    f (HsPAsPat hsName hsPat)  = do
          hsPat' <- fn hsPat
          return (HsPAsPat hsName hsPat')
    f HsPWildCard  = do return HsPWildCard
    f (HsPIrrPat hsPat)  = do
          hsPat' <- fnl hsPat
          return (HsPIrrPat hsPat')
    f (HsPBangPat hsPat)  = do
          hsPat' <- fnl hsPat
          return (HsPBangPat hsPat')
    f (HsPTypeSig srcLoc hsPat qt) = withSrcLoc srcLoc $ do
          hsPat' <- fn hsPat
          return (HsPTypeSig srcLoc hsPat' qt)
    f (HsPRec hsName hsPatFields)  = do
          hsPatFields' <- mapM (T.mapM fn) hsPatFields
          return (HsPRec hsName hsPatFields')
    f p@HsPatExp {} = return p
    f (HsPatWords ws) = HsPatWords <$> mapM fn ws
    f (HsPatBackTick ws) = HsPatBackTick <$> fn ws
    fnl (Located l e) = withSrcSpan l (Located l `liftM` fn e)

traverseHsRhsHsExp :: (Monad m,MonadSetSrcLoc m) => (HsExp -> m HsExp) -> HsRhs -> m HsRhs
traverseHsRhsHsExp fn d = f d where
    f (HsUnGuardedRhs a1) = HsUnGuardedRhs `liftM` fn a1
    f (HsGuardedRhss a1) = HsGuardedRhss `liftM` mapM g a1
    g (HsGuardedRhs sl a1 a2) = HsGuardedRhs sl `liftM` fn a1 `ap` fn a2

traverseHsStmtHsExp :: (Monad m,MonadSetSrcLoc m) => (HsExp -> m HsExp) -> HsStmt -> m HsStmt
traverseHsStmtHsExp fn d = f d where
    f (HsGenerator sl p e) = withSrcLoc sl $ HsGenerator sl p `liftM` fn e
    f (HsQualifier e) = HsQualifier `liftM` fn e
    f (HsLetStmt ds) = HsLetStmt `liftM` mapM (traverseHsDeclHsExp fn) ds

traverseHsDeclHsExp :: (Monad m,MonadSetSrcLoc m) => (HsExp -> m HsExp) -> HsDecl -> m HsDecl
traverseHsDeclHsExp fn d = f d where
    f (HsPatBind srcLoc hsPat hsRhs {-where-} hsDecls) = withSrcLoc srcLoc $ do
        hsDecls'  <- mapM (traverseHsDeclHsExp fn) hsDecls
        hsRhs'    <- traverseHsRhsHsExp fn hsRhs
        return (HsPatBind srcLoc hsPat hsRhs' {-where-} hsDecls')
    f (HsActionDecl sl p e) = withSrcLoc sl $ do
        e <- fn e
        return $ HsActionDecl sl p e
--    f (HsFunBind hsMatches)  = do
--        hsMatches'     <- mapM (traverseHsMatchHsExp fn) hsMatches
--        return (HsFunBind hsMatches')
    f (HsClassDecl srcLoc hsQualType hsDecls)  = withSrcLoc srcLoc $ do
        hsDecls'  <- mapM (traverseHsDeclHsExp fn) hsDecls
        return (HsClassDecl srcLoc hsQualType hsDecls')
    f decl@(HsClassAliasDecl { hsDeclSrcLoc = sl})  = withSrcLoc sl $ do
        hsDecls'  <- mapM (traverseHsDeclHsExp fn) (hsDeclDecls decl)
        return (decl { hsDeclDecls = hsDecls' })
    f (HsInstDecl srcLoc hsQualType hsDecls)  = withSrcLoc srcLoc $ do
        hsDecls'  <- mapM (traverseHsDeclHsExp fn) hsDecls
        return (HsInstDecl srcLoc hsQualType hsDecls')
--    f prules@HsPragmaRules { hsDeclSrcLoc = srcLoc, hsDeclFreeVars = fvs, hsDeclLeftExpr = e1, hsDeclRightExpr = e2 }  = withSrcLoc srcLoc $ do
--        fvs' <- sequence [ fmapM (`renameHsType` ) t  >>= return . (,) n | (n,t) <- fvs]
--        e1' <- renameHsExp e1
--        e2' <- renameHsExp e2
--        return prules {  hsDeclFreeVars = fvs', hsDeclLeftExpr = e1', hsDeclRightExpr = e2' }
    f otherHsDecl = return otherHsDecl

getNamesFromHsPat :: HsPat -> [HsName]
getNamesFromHsPat p = execWriter (getNamesFromPat p) where
    getNamesFromPat (HsPVar hsName) = tell [toName Val hsName]
    getNamesFromPat (HsPAsPat hsName hsPat) = do
        tell [toName Val hsName]
        getNamesFromPat hsPat
    getNamesFromPat (HsPatExp e) = f e where
        f (HsVar v) = do
            tell [toName Val v]
        f (HsAsPat v e) = do
            tell [toName Val v]
            f e
        f e = traverseHsExp_ f e

    getNamesFromPat p = traverseHsPat_ getNamesFromPat p

data HsOps m = HsOps {
    opHsDecl  :: HsDecl -> m HsDecl,
    opHsExp   :: HsExp  -> m HsExp,
    opHsPat   :: HsPat  -> m HsPat,
    opHsType  :: HsType -> m HsType,
    opHsStmt  :: HsStmt -> m HsStmt,
    opHsList  :: HsOps' [] m,
    opHsMaybe :: HsOps' Maybe m
    }

-- allow specialization on traversables via the same trick used for showing
-- lists of chars different than other lists.
data HsOps' f m = HsOps' {
    opHsDecl' :: f HsDecl -> m (f HsDecl),
    opHsExp'  :: f HsExp  -> m (f HsExp),
    opHsPat'  :: f HsPat  -> m (f HsPat),
    opHsType' :: f HsType -> m (f HsType),
    opHsStmt' :: f HsStmt -> m (f HsStmt),
    opHsOps'  :: HsOps m
    }

-- | provides a default hsOps that recurses further down the tree for undeclared
-- operations. In order to tie the knot properly, you need to pass its return
-- value into itself, as in.
--
-- let ops = (hsOpsDefault ops) { opHsType = custom type handler }
--
-- NOTE: if you forget the parentheses around hsopsdefault ops, your program
-- will still typecheck and compile, but it will behave incorrectly.

hsOpsDefault :: (Applicative m, MonadSetSrcLoc m) => HsOps m -> HsOps m
hsOpsDefault hops = HsOps { .. } where
    f x = traverseHsOps hops x
    opHsDecl = f
    opHsExp  = f
    opHsPat  = f
    opHsStmt = f
    opHsType = f
    opHsList = HsOps' {
        opHsDecl' = T.traverse opHsDecl,
        opHsExp'  = T.traverse opHsExp,
        opHsPat'  = T.traverse opHsPat,
        opHsStmt' = T.traverse opHsStmt,
        opHsType' = T.traverse opHsType,
        opHsOps'  = hops
        }
    opHsMaybe = HsOps' {
        opHsDecl' = T.traverse opHsDecl,
        opHsExp'  = T.traverse opHsExp,
        opHsPat'  = T.traverse opHsPat,
        opHsStmt' = T.traverse opHsStmt,
        opHsType' = T.traverse opHsType,
        opHsOps'  = hops
        }

-- hsOpsSetList :: (Applicative m, MonadSetSrcLoc m) => HsOps' [] m -> HsOps' [] m
-- hsOpsSetList hops = HsOps { .. } where
--     f x = traverseHsOps hops x
--     opHsDecl = f
--     opHsExp  = f
--     opHsPat  = f
--     opHsStmt = f
--     opHsType = f
--     opHsList = HsOps' {
--         opHsDecl' = T.traverse opHsDecl,
--         opHsExp'  = T.traverse opHsExp,
--         opHsPat'  = T.traverse opHsPat,
--         opHsStmt' = T.traverse opHsStmt,
--         opHsType' = T.traverse opHsType,
--         opHsOps'  = hops
--         }
--     opHsMaybe = HsOps' {
--         opHsDecl' = T.traverse opHsDecl,
--         opHsExp'  = T.traverse opHsExp,
--         opHsPat'  = T.traverse opHsPat,
--         opHsStmt' = T.traverse opHsStmt,
--         opHsType' = T.traverse opHsType,
--         opHsOps'  = hops
--         }

class TraverseHsOps a where
    -- experimental traversable overloading.
    applyHsOps'  :: (MonadSetSrcLoc m,Applicative m,T.Traversable f) =>
        HsOps' f m -> f a -> m (f a)
    applyHsOps' HsOps' { .. } fa = T.traverse (applyHsOps opHsOps') fa

    -- act on the direct children of the argument.
    traverseHsOps :: (Applicative m,MonadSetSrcLoc m) => HsOps m -> a -> m a
    -- act on the argument itself or its children.
    applyHsOps   :: (Applicative m,MonadSetSrcLoc m) => HsOps m -> a -> m a
    applyHsOps os x = traverseHsOps os x

instance TraverseHsOps HsAlt where
    traverseHsOps HsOps { .. } (HsAlt sl p rhs ds) =
        HsAlt sl <$> opHsPat p <*> T.traverse opHsExp rhs <*> T.traverse opHsDecl ds

instance TraverseHsOps HsModule where
    traverseHsOps  HsOps { .. } HsModule { .. } = cr <$> T.traverse opHsDecl hsModuleDecls
        where cr hsModuleDecls = HsModule { .. }

instance TraverseHsOps HsType where
    applyHsOps = opHsType
    traverseHsOps HsOps { .. } = traverseHsType opHsType

instance TraverseHsOps HsDecl where
    applyHsOps = opHsDecl
    traverseHsOps hops@HsOps { .. } x = g x where
        thops x = applyHsOps hops x
        g x = withSrcLoc (srcLoc x) $ f x
        f HsTypeFamilyDecl { .. } = h <$> thops hsDeclTArgs
            where h hsDeclTArgs = HsTypeFamilyDecl { .. }
        f HsTypeDecl { .. } = h <$> thops hsDeclTArgs <*> thops hsDeclType
            where h hsDeclTArgs hsDeclType = HsTypeDecl { .. }
        f HsDefaultDecl { .. } = h <$> thops hsDeclType
            where h hsDeclType = HsDefaultDecl { .. }
        f HsDataDecl { .. } = h <$> thops hsDeclContext <*> thops hsDeclCons
            where h hsDeclContext hsDeclCons = HsDataDecl { .. }
        f HsClassDecl { .. } = h <$> thops hsDeclClassHead <*> thops hsDeclDecls
            where h hsDeclClassHead hsDeclDecls = HsClassDecl { .. }
        f HsClassAliasDecl { .. } = h <$> thops hsDeclTypeArgs <*> thops hsDeclContext <*> thops hsDeclClasses <*> thops hsDeclDecls
            where h hsDeclTypeArgs hsDeclContext hsDeclClasses hsDeclDecls = HsClassAliasDecl { .. }
        f HsInstDecl { .. } = h <$> thops hsDeclClassHead <*> thops hsDeclDecls
            where h hsDeclClassHead hsDeclDecls = HsInstDecl { .. }
        f HsTypeSig { .. } = h <$> thops hsDeclQualType
            where h hsDeclQualType = HsTypeSig { .. }
        f HsActionDecl { .. } = h <$> thops hsDeclPat <*> thops hsDeclExp
            where h hsDeclPat hsDeclExp = HsActionDecl { .. }
        f (HsFunBind ms) = HsFunBind <$> thops ms
        f HsPatBind { .. } = h <$> thops hsDeclPat <*> thops hsDeclRhs <*> thops hsDeclDecls
            where h hsDeclPat hsDeclRhs hsDeclDecls = HsPatBind { .. }
        f HsSpaceDecl { .. } = dr <$> opHsExp hsDeclExp <*> thops hsDeclQualType
            where dr hsDeclExp hsDeclQualType =  HsSpaceDecl { .. }
        f HsForeignDecl { .. } = dr <$> thops hsDeclQualType
            where dr hsDeclQualType =  HsForeignDecl { .. }
        f HsForeignExport { .. } = dr <$> thops hsDeclQualType
            where dr hsDeclQualType =  HsForeignExport { .. }
        f HsDeclDeriving { .. } = dr <$> thops hsDeclClassHead
            where dr hsDeclClassHead =  HsDeclDeriving { .. }
        f x@HsInfixDecl {} = pure x
        f x@HsPragmaProps {} = pure x
        f (HsPragmaRules rs) = HsPragmaRules <$> thops rs
        f HsPragmaSpecialize { .. } = dr <$> thops hsDeclType
            where dr hsDeclType =  HsPragmaSpecialize { .. }

instance TraverseHsOps HsRule where
    traverseHsOps hops HsRule { .. } = hr <$>
            ah hsRuleLeftExpr <*> ah hsRuleRightExpr <*> f hsRuleFreeVars where
        --f xs = T.traverse (T.traverse (T.traverse ah)) xs
        f xs = applyHsOps hops xs
        hr hsRuleLeftExpr hsRuleRightExpr hsRuleFreeVars = HsRule { .. }
        ah x = applyHsOps hops x

instance TraverseHsOps HsClassHead where
    traverseHsOps hops HsClassHead { .. } =
        mch <$> applyHsOps hops hsClassHeadContext <*> applyHsOps hops hsClassHeadArgs where
            mch hsClassHeadContext hsClassHeadArgs = HsClassHead { .. }

instance TraverseHsOps HsMatch where
    traverseHsOps hops m = withSrcLoc (hsMatchSrcLoc m) $ f m where
        f HsMatch { .. } = h <$> thops hsMatchPats <*> thops hsMatchRhs <*> thops hsMatchDecls
            where h hsMatchPats hsMatchRhs hsMatchDecls = HsMatch { .. }
        thops x = applyHsOps hops x

instance TraverseHsOps HsConDecl where
    traverseHsOps hops d = withSrcLoc (srcLoc d) $ f d where
        thops x = applyHsOps hops x
        f HsConDecl { .. } = h <$> thops hsConDeclConArg
            where h hsConDeclConArg = HsConDecl { .. }
        f HsRecDecl { .. } = h <$> thops hsConDeclRecArg
            where h hsConDeclRecArg = HsRecDecl { .. }

instance TraverseHsOps HsPat where
    applyHsOps ho x = opHsPat ho x
    traverseHsOps hops@HsOps { .. } x = f x where
        fn x = applyHsOps hops x
        f (HsPTypeSig sl p qt)   = HsPTypeSig sl <$> fn p <*> fn qt
        f p@HsPVar {}            = pure p
        f p@HsPLit {}            = pure p
        f (HsPNeg a1)            = HsPNeg <$> fn a1
        f (HsPInfixApp a1 a2 a3) = HsPInfixApp <$> fn a1 <*> pure a2 <*> fn a3
        f (HsPApp d1 a1)         = HsPApp d1 <$> fn a1
        f (HsPTuple a1)          = HsPTuple <$> fn a1
        f (HsPUnboxedTuple a1)   = HsPUnboxedTuple <$> fn a1
        f (HsPList a1)           = HsPList <$> fn a1
        f (HsPParen a1)          = HsPParen <$> fn a1
        f (HsPAsPat d1 a1)       = HsPAsPat d1 <$> fn a1
        f HsPWildCard            = pure HsPWildCard
        f (HsPIrrPat a1)         = HsPIrrPat <$> fn a1
        f (HsPBangPat a1)        = HsPBangPat <$> fn a1
        f (HsPRec d1 a1)         = HsPRec d1 <$> fn a1
        f (HsPatExp e)           = HsPatExp <$> fn e
        f (HsPatWords ws)        = HsPatWords <$> fn ws
        f (HsPatBackTick ws)     = HsPatBackTick <$> fn ws

instance TraverseHsOps HsQualType where
    traverseHsOps hops HsQualType { .. } = h <$> applyHsOps hops hsQualTypeContext <*> applyHsOps hops hsQualTypeType
        where h hsQualTypeContext hsQualTypeType = HsQualType { .. }
    -- traverseHsOps hops HsQualType { .. } = do
    --     hsQualTypeContext <- applyHsOps hops hsQualTypeContext
    --     hsQualTypeType <- opHsType hops hsQualTypeType
    --     return HsQualType { .. }

instance TraverseHsOps HsAsst where
    traverseHsOps HsOps { .. } (HsAsstEq a b) = HsAsstEq <$> opHsType a <*> opHsType b
    traverseHsOps _ x = pure x

instance TraverseHsOps HsStmt where
    applyHsOps = opHsStmt
    traverseHsOps hops@HsOps { .. } x = f x where
        f (HsGenerator sl p e) = withSrcLoc sl $ HsGenerator sl <$> opHsPat p <*> opHsExp e
        f (HsQualifier e) = HsQualifier <$> opHsExp e
        f (HsLetStmt dl) = HsLetStmt <$> applyHsOps hops dl

instance TraverseHsOps HsExp where
    applyHsOps = opHsExp
    traverseHsOps hops@HsOps { .. } e = g e where
        fn x = applyHsOps hops x
        g e = withSrcLoc (srcLoc e) $ f e
        f (HsCase e as)                      = HsCase <$> fn e <*> fn as
        f (HsDo hsStmts)                     = HsDo <$> fn hsStmts
        f (HsExpTypeSig srcLoc e hsQualType) = HsExpTypeSig srcLoc <$> fn e <*> fn hsQualType
        f (HsLambda srcLoc hsPats e)         = HsLambda srcLoc <$> fn hsPats <*> fn e
        f (HsLet hsDecls e)                  = HsLet <$> fn hsDecls <*> fn e
        f (HsListComp e ss)                  = HsListComp <$> fn e <*> fn ss
        f (HsRecConstr n fus)                = HsRecConstr n <$> fn fus
        f (HsRecUpdate e fus)                = HsRecUpdate <$> fn e <*> fn fus
        -- only exp
        f e@HsCon {}                  = pure e
        f e@HsError {}                = pure e
        f e@HsLit {}                  = pure e
        f e@HsVar {}                  = pure e
        f (HsApp a1 a2)               = HsApp <$> fn a1 <*> fn a2
        f (HsAsPat hsName e)          = HsAsPat hsName <$> fn e
        f (HsBackTick e)              = HsBackTick <$> fn e
        f (HsBangPat e)               = HsBangPat <$> fn e
        f (HsEnumFrom e)              = HsEnumFrom <$> fn e
        f (HsEnumFromThen e1 e2)      = HsEnumFromThen <$> fn e1 <*> fn e2
        f (HsEnumFromThenTo a1 a2 a3) = HsEnumFromThenTo <$> fn a1 <*> fn a2 <*> fn a3
        f (HsEnumFromTo e1 e2)        = HsEnumFromTo <$> fn e1 <*> fn e2
        f (HsIf e1 e2 e3)             = liftA3 HsIf (fn e1) (fn e2) (fn e3)
        f (HsInfixApp a1 a2 a3)       = HsInfixApp <$> fn a1 <*> fn a2 <*> fn a3
        f (HsIrrPat hsExp)            = HsIrrPat <$> fn hsExp
        f (HsLeftSection e1 e2)       = HsLeftSection <$> fn e1 <*> fn e2
        f (HsList hsExps)             = HsList <$> fn hsExps
        f (HsLocatedExp le)           = HsLocatedExp <$> fn le
        f (HsNegApp a1)               = HsNegApp <$> fn a1
        f (HsParen e)                 = HsParen <$> fn e
        f (HsRightSection e1 e2)      = HsRightSection <$> fn e1 <*> fn e2
        f (HsTuple es)                = HsTuple <$> fn es
        f (HsUnboxedTuple es)         = HsUnboxedTuple <$> fn es
        f (HsWildCard x)              = pure (HsWildCard x)
        f (HsWords ws)                = HsWords <$> fn ws
        --f h = error $ "FrontEnd.Syn.Traverse.traverseHsExp f unrecognized construct: " ++ show h

instance TraverseHsOps e => TraverseHsOps (Located e) where
    traverseHsOps hops (Located l e) = withSrcSpan l (Located l <$> applyHsOps hops e)

--        f e = traverseHsExp opHsExp e

--instance TraverseHsOps a => TraverseHsOps [a] where
--    applyHsOps hops xs = applyHsOps' (opHsList hops) xs
--    traverseHsOps hops xs = T.traverse (applyHsOps hops) xs
--instance TraverseHsOps a => TraverseHsOps (Maybe a) where
--    applyHsOps hops xs = applyHsOps' (opHsMaybe hops) xs
--    traverseHsOps hops xs = T.traverse (applyHsOps hops) xs
instance (TraverseHsOps a,T.Traversable f) => TraverseHsOps (f a) where
    traverseHsOps hops xs = T.traverse (applyHsOps hops) xs

--instance TraverseHsOps a => TraverseHsOps (HsField a) where
--    traverseHsOps hops x = T.traverse (traverseHsOps hops) x
