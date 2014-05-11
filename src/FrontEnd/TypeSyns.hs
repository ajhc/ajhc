module FrontEnd.TypeSyns(expandTypeSyns) where

import Control.Applicative
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Traversable as T

import FrontEnd.HsSyn
import FrontEnd.SrcLoc hiding(srcLoc)
import FrontEnd.Syn.Traverse
import FrontEnd.TypeSynonyms
import FrontEnd.Warning

type SubTable = ()

-- the monadic state

data ScopeState = ScopeState {
    errors         :: [Warning],
    synonyms       :: TypeSynonyms,
    srcLoc         :: !SrcLoc
    }

-- The monadic type
type ScopeSM = State ScopeState

instance MonadWarn ScopeSM where
    addWarning w = modify (\s -> s { errors = w: errors s})

instance MonadSrcLoc ScopeSM where
    getSrcLoc = gets srcLoc
instance MonadSetSrcLoc ScopeSM where
    withSrcLoc sl a = modify (\s -> s { srcLoc = sl `mappend` srcLoc s}) >> a

expandTypeSyns :: (Expand x,MonadWarn m) => TypeSynonyms -> x -> m x
expandTypeSyns syns m = ans where
    startState = ScopeState {
        errors         = [],
        synonyms       =  syns,
        srcLoc         = bogusASrcLoc
        }
    (rm, fs) = runState (expand m) startState
    ans = do
        mapM_ addWarning (errors fs)
        return rm

{-
expandTypeSyns :: MonadWarn m => TypeSynonyms -> HsModule -> m HsModule
expandTypeSyns syns m = ans where
    startState = ScopeState {
        errors         = [],
        synonyms       =  syns,
        srcLoc         = bogusASrcLoc
        }
    (rm, fs) = runState (expand m) startState
    ans = do
        mapM_ addWarning (errors fs)
        return rm

expandTypeSynsStmt :: MonadWarn m => TypeSynonyms -> Module -> HsStmt -> m HsStmt
expandTypeSynsStmt syns _ m = ans where
    startState = ScopeState {
        errors         = [],
        synonyms       =  syns,
        srcLoc         = bogusASrcLoc
        }
    (rm, fs) = runState (renameHsStmt m ()) startState
    ans = do
        mapM_ addWarning (errors fs)
        return rm
        -}

class Expand a where
    expand :: a -> ScopeSM a
    expandList :: [a] -> ScopeSM [a]
    expandList = mapM expand

instance Expand a => Expand [a] where
    expand xs = expandList xs

instance Expand HsModule where
    expand tidy = do
        decls' <- expand (hsModuleDecls tidy)
        return tidy { hsModuleDecls = decls' }

instance Expand HsDecl where
    expand x = renameHsDecl x ()
    expandList decls = do
        let expandTypeSigs ds =  (concatMap f ds) where
            f (HsTypeSig sl ns qt) =  [ HsTypeSig sl [n] qt | n <- ns]
            f d = [d]
        mapM expand (expandTypeSigs decls)

renameHsDecl :: HsDecl -> SubTable -> ScopeSM (HsDecl)
renameHsDecl (HsPatBind srcLoc hsPat hsRhs {-where-} hsDecls) subTable = withSrcLoc srcLoc $ do
    hsPat'    <- expand hsPat
    hsDecls'  <- expand hsDecls
    hsRhs'    <- expand hsRhs
    let patbind' = (HsPatBind srcLoc hsPat' hsRhs' {-where-} hsDecls')
    return patbind'

renameHsDecl (HsForeignDecl a b n t) subTable = withSrcLoc a $ do
    t <- renameHsQualType t subTable
    return  (HsForeignDecl a b n t)

renameHsDecl (HsForeignExport a b n t) subTable = withSrcLoc a $ do
    t <- renameHsQualType t subTable
    return  (HsForeignExport a b n t)

renameHsDecl (HsFunBind hsMatches) subTable = do
    hsMatches' <- expand hsMatches
    return (HsFunBind hsMatches')

renameHsDecl (HsTypeSig srcLoc hsNames hsQualType) subTable = withSrcLoc srcLoc $ do
    hsQualType' <- expand hsQualType
    return (HsTypeSig srcLoc hsNames hsQualType')

renameHsDecl dl@HsDataDecl { hsDeclContext = hsContext, hsDeclCons = hsConDecls  } subTable = do
    hsContext' <- expand hsContext
    hsConDecls' <- expand hsConDecls
    return dl { hsDeclContext = hsContext', hsDeclCons = hsConDecls' }
renameHsDecl (HsTypeDecl srcLoc name hsNames t) subTable = withSrcLoc srcLoc $ do
    t' <- renameHsType' False t undefined
    return (HsTypeDecl srcLoc name hsNames t')
renameHsDecl decl@HsActionDecl { hsDeclSrcLoc = srcLoc, hsDeclExp = e }  subTable = withSrcLoc srcLoc $ do
    e <- renameHsExp e subTable
    return decl { hsDeclExp = e }
renameHsDecl (HsClassDecl srcLoc hsQualType hsDecls) subTable = withSrcLoc srcLoc $ do
    hsQualType' <- expand hsQualType
    hsDecls' <- expand hsDecls
    return (HsClassDecl srcLoc hsQualType' hsDecls')
renameHsDecl (HsInstDecl srcLoc hsQualType hsDecls) subTable = withSrcLoc srcLoc $ do
    hsQualType' <- expand hsQualType
    hsDecls' <- expand hsDecls
    return (HsInstDecl srcLoc hsQualType' hsDecls')
renameHsDecl (HsPragmaRules rs) subTable = do
    rs' <- mapM (`renameHsRule` subTable) rs
    return $ HsPragmaRules rs'
renameHsDecl prules@HsPragmaSpecialize { hsDeclSrcLoc = srcLoc, hsDeclName = n, hsDeclType = t } subTable = withSrcLoc srcLoc $ do
    t <- renameHsType t subTable
    return prules {  hsDeclType = t }
renameHsDecl otherHsDecl _ = return otherHsDecl

renameHsRule prules@HsRule { hsRuleSrcLoc = srcLoc, hsRuleFreeVars = fvs, hsRuleLeftExpr = e1, hsRuleRightExpr = e2 } subTable = withSrcLoc srcLoc $ do
    fvs' <- sequence [ T.mapM (`renameHsType` subTable) t  >>= return . (,) n | (n,t) <- fvs]
    e1' <- renameHsExp e1 subTable
    e2' <- renameHsExp e2 subTable
    return prules {  hsRuleFreeVars = fvs', hsRuleLeftExpr = e1', hsRuleRightExpr = e2' }

renameHsQualType :: HsQualType -> SubTable -> ScopeSM (HsQualType)
renameHsQualType (HsQualType hsContext hsType) subTable = do
      hsContext' <- expand hsContext
      hsType' <- expand hsType
      return (HsQualType hsContext' hsType')

instance Expand HsAsst where
    expand a = renameHsAsst a ()
instance Expand HsQualType where
    expand a = renameHsQualType a ()
instance Expand HsConDecl where
    expand a = renameHsConDecl a ()

instance Expand HsClassHead where
    expand HsClassHead { .. }  = do
        hsClassHeadContext <- expand hsClassHeadContext
        hsClassHeadArgs <- mapM (flip renameHsType ()) hsClassHeadArgs
        return HsClassHead { .. }

renameHsAsst :: HsAsst -> SubTable -> ScopeSM (HsAsst)
renameHsAsst (HsAsst hsName1  hsName2s) subTable = do
      return (HsAsst hsName1 hsName2s)
renameHsAsst (HsAsstEq t1 t2) subTable = do
      t1' <- renameHsType t1 subTable  -- for class names
      t2' <- renameHsType t2 subTable  -- for class names
      return (HsAsstEq t1' t2')

renameHsConDecl :: HsConDecl -> SubTable -> ScopeSM (HsConDecl)
renameHsConDecl cd@(HsConDecl { hsConDeclSrcLoc = srcLoc, hsConDeclName = hsName, hsConDeclConArg = hsBangTypes }) subTable = withSrcLoc srcLoc $ do
    hsBangTypes' <- expand hsBangTypes
    return cd { hsConDeclName = hsName, hsConDeclConArg = hsBangTypes' }
renameHsConDecl cd@HsRecDecl { hsConDeclSrcLoc = srcLoc, hsConDeclName = hsName, hsConDeclRecArg = stuff} subTable = withSrcLoc srcLoc $ do
    stuff' <- sequence [ do t' <- expand t ; return (ns,t') |  (ns,t) <- stuff]
    return cd { hsConDeclName = hsName, hsConDeclRecArg = stuff' }

instance Expand a => Expand (HsBangType' a) where
    expand x = T.mapM expand x

instance Expand HsType where
    expand x = renameHsType x ()
instance Expand HsMatch where
    expand x = renameHsMatch x ()

renameHsType = renameHsType' True
renameHsType' dovar t st = pp (rt t) where
    rt :: HsType -> ScopeSM HsType
    rt (HsTyFun hsType1 hsType2) = do
        hsType1' <- rt hsType1
        hsType2' <- rt hsType2
        return (HsTyFun hsType1' hsType2')
    rt (HsTyTuple hsTypes) = do
        hsTypes' <- mapM rt hsTypes
        return (HsTyTuple hsTypes')
    rt (HsTyUnboxedTuple hsTypes) = do
        hsTypes' <- mapM rt hsTypes
        return (HsTyUnboxedTuple hsTypes')
    rt (HsTyApp hsType1 hsType2) = do
        hsType1' <- rt hsType1
        hsType2' <- rt hsType2
        return (HsTyApp hsType1' hsType2')
    rt (HsTyForall ts v) = do
        v <- renameHsQualType v  ()
        return $ HsTyForall ts v
    rt (HsTyExists ts v) = do
        v <- renameHsQualType v  ()
        return $ HsTyExists ts v
 --   rt (HsTyAssoc) = return HsTyAssoc
--    rt (HsTyEq a b) = return HsTyEq `ap` (flip rt a) `ap` (flip rt b)
 --   rt HsTyExpKind {} _subTable = error "cannot rename HsTyExpKind TypeSyns"
    rt ty = traverseHsType rt ty
    pp t | not dovar = t
    pp t = do
        t' <- t
        syns <- gets synonyms
        removeSynonymsFromType syns t'

-- note that for renameHsMatch, the 'wheres' dominate the 'pats'

renameHsMatch :: HsMatch -> SubTable -> ScopeSM HsMatch
renameHsMatch (HsMatch srcLoc hsName hsPats hsRhs {-where-} hsDecls) subTable = withSrcLoc srcLoc $ do
    hsPats' <- expand hsPats
    hsDecls' <- expand hsDecls
    hsRhs' <- expand hsRhs
    return (HsMatch srcLoc hsName hsPats' hsRhs' {-where-} hsDecls')

instance Expand HsPat where
    expand x = renameHsPat x ()
instance Expand HsRhs where
    expand x = renameHsRhs x ()

renameHsPat :: HsPat -> SubTable -> ScopeSM (HsPat)
renameHsPat (HsPTypeSig srcLoc hsPat qt) subTable = withSrcLoc srcLoc $ do
    hsQualType' <- renameHsQualType qt subTable
    hsPat' <- expand hsPat
    return (HsPTypeSig srcLoc hsPat' hsQualType')
renameHsPat p subTable = traverseHsPat (flip renameHsPat subTable) p

renameHsRhs :: HsRhs -> SubTable -> ScopeSM HsRhs
renameHsRhs (HsUnGuardedRhs hsExp) subTable = do
      hsExp' <- expand hsExp
      return (HsUnGuardedRhs hsExp')
renameHsRhs (HsGuardedRhss hsGuardedRhss) subTable = do
      hsGuardedRhss' <- expand hsGuardedRhss
      return (HsGuardedRhss hsGuardedRhss')

instance Expand HsExp where
    expand x = renameHsExp x ()

renameHsExp :: HsExp -> SubTable -> ScopeSM HsExp
renameHsExp (HsLambda srcLoc hsPats hsExp) subTable = withSrcLoc srcLoc $ do
    hsPats' <- expand hsPats
    hsExp' <- expand hsExp
    return (HsLambda srcLoc hsPats' hsExp')
renameHsExp (HsLet hsDecls hsExp) subTable = do
    hsDecls' <- expand hsDecls
    hsExp' <- expand hsExp
    return (HsLet hsDecls' hsExp')
renameHsExp (HsCase hsExp hsAlts) subTable = do
    hsExp' <- expand hsExp
    hsAlts' <- expand hsAlts
    return (HsCase hsExp' hsAlts')
renameHsExp (HsRecConstr hsName hsFieldUpdates) subTable = do
    hsFieldUpdates' <- expand hsFieldUpdates
    return (HsRecConstr hsName hsFieldUpdates')
renameHsExp (HsRecUpdate hsExp hsFieldUpdates) subTable = do
    hsExp' <- expand hsExp
    hsFieldUpdates' <- expand hsFieldUpdates
    return (HsRecUpdate hsExp' hsFieldUpdates')
renameHsExp (HsListComp hsExp hsStmts) _
    = HsListComp <$> expand hsExp <*> expand hsStmts
renameHsExp (HsExpTypeSig srcLoc hsExp hsQualType) subTable = do
    hsExp' <- expand hsExp
    hsQualType' <- expand hsQualType
    return (HsExpTypeSig srcLoc hsExp' hsQualType')
renameHsExp e _ = traverseHsExp expand e

instance Expand HsAlt where
    expand (HsAlt srcLoc hsPat hsGuardedAlts {-where-} hsDecls) = withSrcLoc srcLoc $ do
        hsPat' <- expand hsPat
        hsDecls' <- expand hsDecls
        hsGuardedAlts' <- expand hsGuardedAlts
        return (HsAlt srcLoc hsPat' hsGuardedAlts' hsDecls')

instance Expand HsGuardedRhs where
    expand (HsGuardedRhs srcLoc hsExp1 hsExp2) = withSrcLoc srcLoc $ do
        HsGuardedRhs srcLoc <$> expand hsExp1 <*> expand hsExp2

instance Expand HsStmt where
    expand (HsGenerator sl a b) = HsGenerator sl <$> expand a <*> expand b
    expand (HsQualifier e) = HsQualifier <$> expand e
    expand (HsLetStmt ds) = HsLetStmt <$> expand ds

instance Expand a => Expand (HsField a) where
    expand (HsField a b) = HsField a <$> expand b
