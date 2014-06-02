{-# OPTIONS_GHC -fwarn-unused-matches  -fwarn-incomplete-patterns -fwarn-type-defaults #-}
module FrontEnd.Syn.Traverse where

import Control.Monad.Writer
import Util.Std
import qualified Data.Set as Set
import qualified Data.Traversable as T

import FrontEnd.HsSyn
import FrontEnd.SrcLoc
import Name.Name
import Support.FreeVars
import Util.Inst()

instance FreeVars HsType (Set.Set Name) where
    freeVars t = execWriter (f t) where
        f (HsTyVar v) = tell (Set.singleton $ toName TypeVal v)
        f (HsTyCon v) = tell (Set.singleton $ toName TypeConstructor v)
        f t = traverseHsType_ f t

traverse_ :: Applicative m => (a -> m b) -> a -> m a
traverse_ fn x = fn x *> pure x

traverseHsExp_ :: (Monad m,Applicative m,MonadSetSrcLoc m) => (HsExp -> m ()) -> HsExp -> m ()
traverseHsExp_ fn e = traverseHsExp (traverse_ fn) e *> pure ()

traverseHsExp :: (Monad m,MonadSetSrcLoc m,TraverseHsOps e) => (HsExp -> m HsExp) -> e -> m e
traverseHsExp fn e = traverseHsOps ops e where
    ops = (hsOpsDefault ops) { opHsExp, opHsPat, opHsType } where
            opHsExp e = fn e
            opHsPat p = return p
            opHsType t = return t

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

traverseHsPat :: (Monad m,MonadSetSrcLoc m,TraverseHsOps e) => (HsPat -> m HsPat) -> e -> m e
traverseHsPat fn e = traverseHsOps ops e where
    ops = (hsOpsDefault ops) { opHsPat, opHsType } where
        opHsPat p = fn p
        opHsType t = return t

traverseHsDeclHsExp :: (Monad m,MonadSetSrcLoc m) => (HsExp -> m HsExp) -> HsDecl -> m HsDecl
traverseHsDeclHsExp fn d = traverseHsExp fn d

getNamesFromHsPat :: HsPat -> [Name]
getNamesFromHsPat p = execWriter (getNamesFromPat p) where
    getNamesFromPat (HsPVar hsName) = tell [hsName]
    getNamesFromPat (HsPAsPat hsName hsPat) = do
        tell [hsName]
        getNamesFromPat hsPat
    getNamesFromPat (HsPatExp e) = f e where
        f (HsVar v) = do
            tell [v]
        f (HsAsPat v e) = do
            tell [v]
            f e
        f e = traverseHsExp_ f e

    getNamesFromPat p = traverseHsPat_ getNamesFromPat p

data HsOps m = HsOps {
    opHsDecl  :: HsDecl -> m HsDecl,
    opHsExp   :: HsExp  -> m HsExp,
    opHsPat   :: HsPat  -> m HsPat,
    opHsType  :: HsType -> m HsType,
    opHsStmt  :: HsStmt -> m HsStmt
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

class TraverseHsOps a where
    -- act on the direct children of the argument.
    traverseHsOps :: (Applicative m,MonadSetSrcLoc m) => HsOps m -> a -> m a
    -- act on the argument itself or its children.
    applyHsOps   :: (Applicative m,MonadSetSrcLoc m) => HsOps m -> a -> m a
    applyHsOps os x = traverseHsOps os x

instance TraverseHsOps HsAlt where
    traverseHsOps ops@HsOps { .. } (HsAlt sl p rhs ds) =
        HsAlt sl <$> opHsPat p <*> applyHsOps ops rhs <*> T.traverse opHsDecl ds

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

instance TraverseHsOps HsComp where
    traverseHsOps ops HsComp { .. } = h <$> applyHsOps ops hsCompStmts <*> applyHsOps ops hsCompBody where
        h hsCompStmts hsCompBody = HsComp { .. }
instance TraverseHsOps HsRhs where
    traverseHsOps ops (HsUnGuardedRhs rhs) = HsUnGuardedRhs <$> applyHsOps ops rhs
    traverseHsOps ops (HsGuardedRhss rhss) = HsGuardedRhss <$> applyHsOps ops rhss

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
        f (HsListComp c)                     = HsListComp <$> fn c
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

instance (TraverseHsOps a,T.Traversable f) => TraverseHsOps (f a) where
    traverseHsOps hops xs = T.traverse (applyHsOps hops) xs

maybeGetDeclName :: Monad m => HsDecl -> m Name
maybeGetDeclName (HsPatBind sloc (HsPVar name) rhs wheres) = return (toName Val name)
maybeGetDeclName (HsActionDecl sloc (HsPVar name) _) = return (toName Val name)
maybeGetDeclName (HsFunBind ((HsMatch _ name _ _ _):_)) = return (toName Val name)
maybeGetDeclName HsDataDecl { hsDeclDeclType = DeclTypeKind, hsDeclName } = return (toName SortName hsDeclName)
maybeGetDeclName HsDataDecl { hsDeclName = name } = return (toName TypeConstructor name)
maybeGetDeclName HsClassDecl { hsDeclClassHead = h } = return $ toName ClassName $ hsClassHead h
maybeGetDeclName x@HsForeignDecl {} = return $ toName Val $ hsDeclName x
maybeGetDeclName (HsForeignExport _ e _ _)   = return $ ffiExportName e
--maybeGetDeclName (HsTypeSig _ [n] _ ) = return n
maybeGetDeclName d = fail  $ "getDeclName: could not find name for a decl: " ++ show d

getDeclName :: HsDecl -> Name
getDeclName d =  runIdentity $ maybeGetDeclName d
