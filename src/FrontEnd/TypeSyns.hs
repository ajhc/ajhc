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

-- the monadic state

data ScopeState = ScopeState {
    errors   :: [Warning],
    synonyms :: TypeSynonyms,
    srcLoc   :: !SrcLoc
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
        errors   = [],
        synonyms = syns,
        srcLoc   = bogusASrcLoc }
    (rm, fs) = runState (expand m) startState
    ans = mapM_ addWarning (errors fs) >> return rm

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
    expandList decls = do
        let expandTypeSigs ds =  (concatMap f ds) where
            f (HsTypeSig sl ns qt) =  [ HsTypeSig sl [n] qt | n <- ns]
            f d = [d]
        mapM expand (expandTypeSigs decls)
    expand x = f x where
        f (HsPatBind srcLoc hsPat hsRhs {-where-} hsDecls) = withSrcLoc srcLoc $ do
            hsPat'    <- expand hsPat
            hsDecls'  <- expand hsDecls
            hsRhs'    <- expand hsRhs
            return (HsPatBind srcLoc hsPat' hsRhs' {-where-} hsDecls')
        f (HsForeignDecl a b n t) = withSrcLoc a $ do
            t <- expand t
            return  (HsForeignDecl a b n t)
        f (HsForeignExport a b n t) = withSrcLoc a $ do
            t <- expand t
            return  (HsForeignExport a b n t)
        f (HsFunBind hsMatches) = HsFunBind <$> expand hsMatches
        f (HsTypeSig srcLoc hsNames hsQualType) = withSrcLoc srcLoc $ do
            hsQualType' <- expand hsQualType
            return (HsTypeSig srcLoc hsNames hsQualType')
        f dl@HsDataDecl { hsDeclContext = hsContext, hsDeclCons = hsConDecls  } = do
            hsContext' <- expand hsContext
            hsConDecls' <- expand hsConDecls
            return dl { hsDeclContext = hsContext', hsDeclCons = hsConDecls' }
        f (HsTypeDecl srcLoc name hsNames t) = withSrcLoc srcLoc $ do
            t' <- renameHsType' False t
            return (HsTypeDecl srcLoc name hsNames t')
        f decl@HsActionDecl { hsDeclSrcLoc = srcLoc, hsDeclExp = e }  = withSrcLoc srcLoc $ do
            e <- expand e
            return decl { hsDeclExp = e }
        f (HsClassDecl srcLoc hsQualType hsDecls) = withSrcLoc srcLoc $ do
            hsQualType' <- expand hsQualType
            hsDecls' <- expand hsDecls
            return (HsClassDecl srcLoc hsQualType' hsDecls')
        f (HsInstDecl srcLoc hsQualType hsDecls) = withSrcLoc srcLoc $ do
            hsQualType' <- expand hsQualType
            hsDecls' <- expand hsDecls
            return (HsInstDecl srcLoc hsQualType' hsDecls')
        f (HsPragmaRules rs) = HsPragmaRules <$> mapM renameHsRule rs
        f prules@HsPragmaSpecialize { hsDeclSrcLoc = srcLoc, hsDeclName = n, hsDeclType = t } = withSrcLoc srcLoc $ do
            t <- expand t
            return prules {  hsDeclType = t }
        f otherHsDecl = return otherHsDecl

renameHsRule prules@HsRule { hsRuleSrcLoc = srcLoc, hsRuleFreeVars = fvs, hsRuleLeftExpr = e1, hsRuleRightExpr = e2 } = withSrcLoc srcLoc $ do
    fvs' <- sequence [ T.mapM renameHsType t  >>= return . (,) n | (n,t) <- fvs]
    e1' <- expand e1
    e2' <- expand e2
    return prules {  hsRuleFreeVars = fvs', hsRuleLeftExpr = e1', hsRuleRightExpr = e2' }

instance Expand HsQualType where
    expand (HsQualType hsContext hsType) = do
        hsContext' <- expand hsContext
        hsType' <- expand hsType
        return (HsQualType hsContext' hsType')

instance Expand HsClassHead where
    expand HsClassHead { .. }  = do
        hsClassHeadContext <- expand hsClassHeadContext
        hsClassHeadArgs <- mapM renameHsType hsClassHeadArgs
        return HsClassHead { .. }

instance Expand HsAsst where
    expand (HsAsstEq t1 t2) = HsAsstEq <$> expand t1 <*> expand t2
    expand x = return x

instance Expand HsConDecl where
    expand cd@(HsConDecl { hsConDeclSrcLoc = srcLoc, hsConDeclName = hsName, hsConDeclConArg = hsBangTypes }) = withSrcLoc srcLoc $ do
        hsBangTypes' <- expand hsBangTypes
        return cd { hsConDeclName = hsName, hsConDeclConArg = hsBangTypes' }
    expand cd@HsRecDecl { hsConDeclSrcLoc = srcLoc, hsConDeclName = hsName, hsConDeclRecArg = stuff} = withSrcLoc srcLoc $ do
        stuff' <- sequence [ do t' <- expand t ; return (ns,t') |  (ns,t) <- stuff]
        return cd { hsConDeclName = hsName, hsConDeclRecArg = stuff' }

instance Expand a => Expand (HsBangType' a) where
    expand x = T.mapM expand x

instance Expand HsType where
    expand x = renameHsType x

renameHsType = renameHsType' True
renameHsType' dovar t = pp t where
    pp t | not dovar = return t
    pp t = do
        syns <- gets synonyms
        removeSynonymsFromType syns t

-- note that for renameHsMatch, the 'wheres' dominate the 'pats'

instance Expand HsMatch where
    expand (HsMatch srcLoc hsName hsPats hsRhs {-where-} hsDecls) = withSrcLoc srcLoc $ do
        hsPats' <- expand hsPats
        hsDecls' <- expand hsDecls
        hsRhs' <- expand hsRhs
        return (HsMatch srcLoc hsName hsPats' hsRhs' {-where-} hsDecls')

instance Expand a => Expand (HsRhs' a) where
    expand = T.mapM expand

instance Expand HsPat where
    expand (HsPTypeSig srcLoc hsPat qt) = withSrcLoc srcLoc $ do
        hsQualType' <- expand qt
        hsPat' <- expand hsPat
        return (HsPTypeSig srcLoc hsPat' hsQualType')
    expand p = traverseHsPat expand p

instance Expand HsExp where
    expand x = f x where
        f (HsLambda srcLoc hsPats hsExp) = withSrcLoc srcLoc $ do
            hsPats' <- expand hsPats
            hsExp' <- expand hsExp
            return (HsLambda srcLoc hsPats' hsExp')
        f (HsLet hsDecls hsExp) = do
            hsDecls' <- expand hsDecls
            hsExp' <- expand hsExp
            return (HsLet hsDecls' hsExp')
        f (HsCase hsExp hsAlts) = do
            hsExp' <- expand hsExp
            hsAlts' <- expand hsAlts
            return (HsCase hsExp' hsAlts')
        f (HsRecConstr hsName hsFieldUpdates) = do
            hsFieldUpdates' <- expand hsFieldUpdates
            return (HsRecConstr hsName hsFieldUpdates')
        f (HsRecUpdate hsExp hsFieldUpdates) = do
            hsExp' <- expand hsExp
            hsFieldUpdates' <- expand hsFieldUpdates
            return (HsRecUpdate hsExp' hsFieldUpdates')
        f (HsListComp a b) = HsListComp <$> expand a <*> expand b
        f (HsExpTypeSig srcLoc a b) = HsExpTypeSig srcLoc <$> expand a <*> expand b
        f e = traverseHsExp expand e

instance Expand HsAlt where
    expand (HsAlt srcLoc hsPat hsGuardedAlts {-where-} hsDecls) = withSrcLoc srcLoc $ do
        hsPat' <- expand hsPat
        hsDecls' <- expand hsDecls
        hsGuardedAlts' <- expand hsGuardedAlts
        return (HsAlt srcLoc hsPat' hsGuardedAlts' hsDecls')

instance Expand a => Expand (HsGuardedRhs' a) where
    expand = T.mapM expand

instance Expand HsStmt where
    expand (HsGenerator sl a b) = HsGenerator sl <$> expand a <*> expand b
    expand (HsQualifier e) = HsQualifier <$> expand e
    expand (HsLetStmt ds) = HsLetStmt <$> expand ds

instance Expand a => Expand (HsField a) where
    expand (HsField a b) = HsField a <$> expand b
