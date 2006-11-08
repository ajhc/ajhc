module TypeSyns( expandTypeSyns, expandTypeSynsStmt ) where

import Control.Monad.State
import Control.Monad.Writer
import Data.FunctorM
import List

import FrontEnd.Desugar (doToExp)
import FrontEnd.SrcLoc hiding(srcLoc)
import HsSyn
import TypeSynonyms
import Warning
import FrontEnd.Syn.Traverse


type SubTable = ()

-- the monadic state

data ScopeState = ScopeState {
    currentModule  :: Module,
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


expandTypeSyns :: MonadWarn m => TypeSynonyms -> HsModule -> m HsModule
expandTypeSyns syns m = ans where
    startState = ScopeState {
        errors         = [],
        synonyms       =  syns,
        srcLoc         = bogusASrcLoc,
        currentModule  = hsModuleName m
        }

    (rm, fs) = runState (renameDecls m) startState
    ans = do
        mapM_ addWarning (errors fs)
        return rm

expandTypeSynsStmt :: MonadWarn m => TypeSynonyms -> Module -> HsStmt -> m HsStmt
expandTypeSynsStmt syns mod m = ans where
    startState = ScopeState {
        errors         = [],
        synonyms       =  syns,
        srcLoc         = bogusASrcLoc,
        currentModule  = mod
        }

    (rm, fs) = runState (renameHsStmt m ()) startState
    ans = do
        mapM_ addWarning (errors fs)
        return rm


-- This is Bryn's modification to make the code a bit easier to understand for
-- functions like renameHsNames, renameHsFileUpdates
mapRename :: (a -> SubTable -> ScopeSM a) -> [a] -> SubTable -> ScopeSM [a]
mapRename renameIndividual individuals subTable
    = mapM (`renameIndividual` subTable) individuals



renameDecls :: HsModule -> ScopeSM HsModule
renameDecls tidy = do
        decls' <- renameHsDecls (hsModuleDecls tidy) undefined
        return tidy { hsModuleDecls = decls' }



renameHsDecls :: [HsDecl] -> SubTable -> ScopeSM ([HsDecl])
renameHsDecls decls subtable = do
    ans <- mapRename renameHsDecl (expandTypeSigs decls) subtable
    return ans


expandTypeSigs :: [HsDecl] -> [HsDecl]
expandTypeSigs ds =  (concatMap f ds) where
    f (HsTypeSig sl ns qt) =  [ HsTypeSig sl [n] qt | n <- ns]
    f d = return d

renameHsDecl :: HsDecl -> SubTable -> ScopeSM (HsDecl)
renameHsDecl (HsPatBind srcLoc hsPat hsRhs {-where-} hsDecls) subTable = withSrcLoc srcLoc $ do
    hsPat'    <- renameHsPat hsPat subTable
    hsDecls'  <- renameHsDecls hsDecls subTable
    hsRhs'    <- renameHsRhs hsRhs subTable
    let patbind' = (HsPatBind srcLoc hsPat' hsRhs' {-where-} hsDecls')
    return patbind'

renameHsDecl (HsForeignDecl a b n t) subTable = withSrcLoc a $ do
    n <- renameHsName n subTable
    t <- renameHsQualType t subTable
    return  (HsForeignDecl a b n t)

renameHsDecl (HsForeignExport a b n t) subTable = withSrcLoc a $ do
    n <- renameHsName n subTable
    t <- renameHsQualType t subTable
    return  (HsForeignExport a b n t)

renameHsDecl (HsFunBind hsMatches) subTable = do
    hsMatches' <- renameHsMatches hsMatches subTable
    return (HsFunBind hsMatches')

renameHsDecl (HsTypeSig srcLoc hsNames hsQualType) subTable = withSrcLoc srcLoc $ do
    hsNames' <- renameHsNames hsNames subTable
    hsQualType' <- renameHsQualType hsQualType subTable
    return (HsTypeSig srcLoc hsNames' hsQualType')

renameHsDecl (HsDataDecl srcLoc hsContext hsName hsNames1 hsConDecls hsNames2) subTable = do
    hsName' <- renameTypeHsName hsName subTable
    hsContext' <- renameHsContext hsContext subTable
    hsNames1' <- renameHsNames hsNames1 subTable
    hsConDecls' <- renameHsConDecls hsConDecls subTable
    -- don't need to rename the hsNames2 as it is just a list of TypeClasses
    return (HsDataDecl srcLoc hsContext' hsName' hsNames1' hsConDecls' hsNames2)
renameHsDecl (HsTypeDecl srcLoc name hsNames t) subTable = withSrcLoc srcLoc $ do
    hsName' <- renameTypeHsName name subTable
    t' <- renameHsType' False t undefined
    return (HsTypeDecl srcLoc  hsName' hsNames t')

renameHsDecl (HsNewTypeDecl srcLoc hsContext hsName hsNames1 hsConDecl hsNames2) subTable = withSrcLoc srcLoc $ do
    hsContext' <- renameHsContext hsContext subTable
    hsNames1' <- renameHsNames hsNames1 subTable
    hsConDecl' <- renameHsConDecl hsConDecl subTable
    return (HsNewTypeDecl srcLoc hsContext' hsName hsNames1' hsConDecl' hsNames2)

renameHsDecl (HsClassDecl srcLoc hsQualType hsDecls) subTable = withSrcLoc srcLoc $ do
    hsQualType' <- renameHsQualType hsQualType undefined
    hsDecls' <- renameHsDecls hsDecls subTable
    return (HsClassDecl srcLoc hsQualType' hsDecls')
renameHsDecl (HsInstDecl srcLoc hsQualType hsDecls) subTable = withSrcLoc srcLoc $ do
    hsQualType' <- renameHsQualType hsQualType subTable
    hsDecls' <- renameHsDecls hsDecls subTable
    return (HsInstDecl srcLoc hsQualType' hsDecls')
renameHsDecl (HsInfixDecl srcLoc assoc int hsNames) subTable = withSrcLoc srcLoc $ do
    hsNames' <- renameHsNames hsNames subTable
    return $ HsInfixDecl srcLoc assoc int hsNames'
renameHsDecl (HsPragmaRules rs) subTable = do
    rs' <- mapM (`renameHsRule` subTable) rs
    return $ HsPragmaRules rs'
renameHsDecl prules@HsPragmaSpecialize { hsDeclSrcLoc = srcLoc, hsDeclName = n, hsDeclType = t } subTable = withSrcLoc srcLoc $ do
    t <- renameHsType t subTable
    return prules {  hsDeclType = t }
renameHsDecl otherHsDecl _ = return otherHsDecl


renameHsRule prules@HsRule { hsRuleSrcLoc = srcLoc, hsRuleFreeVars = fvs, hsRuleLeftExpr = e1, hsRuleRightExpr = e2 } subTable = withSrcLoc srcLoc $ do
    fvs' <- sequence [ fmapM (`renameHsType` subTable) t  >>= return . (,) n | (n,t) <- fvs]
    e1' <- renameHsExp e1 subTable
    e2' <- renameHsExp e2 subTable
    return prules {  hsRuleFreeVars = fvs', hsRuleLeftExpr = e1', hsRuleRightExpr = e2' }



renameHsQualType :: HsQualType -> SubTable -> ScopeSM (HsQualType)
renameHsQualType (HsQualType hsContext hsType) subTable = do
      hsContext' <- renameHsContext hsContext subTable
      hsType' <- renameHsType hsType subTable
      return (HsQualType hsContext' hsType')

renameHsContext :: HsContext -> SubTable -> ScopeSM (HsContext)
renameHsContext = mapRename renameHsAsst

renameHsAsst :: HsAsst -> SubTable -> ScopeSM (HsAsst)
renameHsAsst (HsAsst hsName1  hsName2s) subTable = do
      hsName1' <- renameTypeHsName hsName1 subTable  -- for class names
      hsName2s' <- mapRename renameTypeHsName hsName2s subTable
      return (HsAsst hsName1' hsName2s')
renameHsAsst (HsAsstEq t1 t2) subTable = do
      t1' <- renameHsType t1 subTable  -- for class names
      t2' <- renameHsType t2 subTable  -- for class names
      return (HsAsstEq t1' t2')

renameHsConDecls :: [HsConDecl] -> SubTable -> ScopeSM ([HsConDecl])
renameHsConDecls = mapRename renameHsConDecl

renameHsConDecl :: HsConDecl -> SubTable -> ScopeSM (HsConDecl)
renameHsConDecl cd@(HsConDecl { hsConDeclSrcLoc = srcLoc, hsConDeclName = hsName, hsConDeclConArg = hsBangTypes }) subTable = withSrcLoc srcLoc $ do
    hsName' <- renameHsName hsName subTable
    hsBangTypes' <- renameHsBangTypes hsBangTypes subTable
    return cd { hsConDeclName = hsName', hsConDeclConArg = hsBangTypes' }
renameHsConDecl cd@HsRecDecl { hsConDeclSrcLoc = srcLoc, hsConDeclName = hsName, hsConDeclRecArg = stuff} subTable = withSrcLoc srcLoc $ do
    hsName' <- renameHsName hsName subTable
    stuff' <- sequence [ do ns' <- mapRename renameHsName ns subTable; t' <- renameHsBangType t subTable; return (ns',t')  |  (ns,t) <- stuff]
    return cd { hsConDeclName = hsName', hsConDeclRecArg = stuff' }

renameHsBangTypes :: [HsBangType] -> SubTable -> ScopeSM ([HsBangType])
renameHsBangTypes = mapRename renameHsBangType

renameHsBangType :: HsBangType -> SubTable -> ScopeSM (HsBangType)
renameHsBangType (HsBangedTy hsType) subTable = do
    hsType' <- renameHsType hsType subTable
    return (HsBangedTy hsType')
renameHsBangType (HsUnBangedTy hsType) subTable = do
    hsType' <- renameHsType hsType subTable
    return (HsUnBangedTy hsType')

renameHsType = renameHsType' True

renameHsType' dovar t st = pp (rt t st) where
    rt :: HsType -> SubTable -> ScopeSM (HsType)
    rt (HsTyFun hsType1 hsType2) subTable = do
        hsType1' <- rt hsType1 subTable
        hsType2' <- rt hsType2 subTable
        return (HsTyFun hsType1' hsType2')
    rt (HsTyTuple hsTypes) subTable = do
        hsTypes' <- mapRename rt hsTypes subTable
        return (HsTyTuple hsTypes')
    rt (HsTyUnboxedTuple hsTypes) subTable = do
        hsTypes' <- mapRename rt hsTypes subTable
        return (HsTyUnboxedTuple hsTypes')
    rt (HsTyApp hsType1 hsType2) subTable = do
        hsType1' <- rt hsType1 subTable
        hsType2' <- rt hsType2 subTable
        return (HsTyApp hsType1' hsType2')
    rt (HsTyVar hsName) subTable | dovar = do
        hsName' <- renameTypeHsName hsName subTable
        return (HsTyVar hsName')
    rt v@(HsTyVar _) _   = return v
    rt (HsTyCon hsName) subTable = do
        hsName' <- renameTypeHsName hsName subTable
        return (HsTyCon hsName')
    rt (HsTyForall ts v) subTable  = do
        v <- renameHsQualType v subTable
        return $ HsTyForall ts v
    rt (HsTyExists ts v) subTable  = do
        v <- renameHsQualType v subTable
        return $ HsTyExists ts v
    rt (HsTyAssoc) subTable = return HsTyAssoc
    rt (HsTyEq a b) subTable = return HsTyEq `ap` (flip rt subTable a) `ap` (flip rt subTable b)
    pp t | not dovar = t
    pp t = do
        t' <- t
        syns <- gets synonyms
        removeSynonymsFromType syns t'

renameHsMatches :: [HsMatch] -> SubTable -> ScopeSM [HsMatch]
renameHsMatches = mapRename renameHsMatch

-- note that for renameHsMatch, the 'wheres' dominate the 'pats'

renameHsMatch :: HsMatch -> SubTable -> ScopeSM HsMatch
renameHsMatch (HsMatch srcLoc hsName hsPats hsRhs {-where-} hsDecls) subTable = withSrcLoc srcLoc $ do
    hsName' <- renameHsName hsName subTable
    subTable' <- updateSubTableWithHsPats subTable hsPats srcLoc FunPat
    hsPats' <- renameHsPats hsPats subTable'
    subTable'' <- updateSubTableWithHsDecls subTable' hsDecls WhereFun
    hsDecls' <- renameHsDecls hsDecls subTable''
    hsRhs' <- renameHsRhs hsRhs subTable''
    return (HsMatch srcLoc hsName' hsPats' hsRhs' {-where-} hsDecls')


renameHsPats :: [HsPat] -> SubTable -> ScopeSM ([HsPat])
renameHsPats = mapRename renameHsPat

renameHsPat :: HsPat -> SubTable -> ScopeSM (HsPat)
renameHsPat (HsPTypeSig srcLoc hsPat qt) subTable = withSrcLoc srcLoc $ do
    hsQualType' <- renameHsQualType qt subTable
    hsPat' <- renameHsPat hsPat subTable
    return (HsPTypeSig srcLoc hsPat' hsQualType')
renameHsPat p subTable = traverseHsPat (flip renameHsPat subTable) p

renameHsRhs :: HsRhs -> SubTable -> ScopeSM HsRhs
renameHsRhs (HsUnGuardedRhs hsExp) subTable = do
      hsExp' <- renameHsExp hsExp subTable
      return (HsUnGuardedRhs hsExp')
renameHsRhs (HsGuardedRhss hsGuardedRhss) subTable = do
      hsGuardedRhss' <- renameHsGuardedRhsList hsGuardedRhss subTable
      return (HsGuardedRhss hsGuardedRhss')


renameHsExp :: HsExp -> SubTable -> ScopeSM HsExp
renameHsExp (HsLambda srcLoc hsPats hsExp) subTable = withSrcLoc srcLoc $ do
    subTable' <- updateSubTableWithHsPats subTable hsPats srcLoc LamPat
    hsPats' <- renameHsPats hsPats subTable'
    hsExp' <- renameHsExp hsExp subTable'
    return (HsLambda srcLoc hsPats' hsExp')
renameHsExp (HsLet hsDecls hsExp) subTable = do
    subTable' <- updateSubTableWithHsDecls subTable hsDecls LetFun
    hsDecls' <- renameHsDecls hsDecls subTable'
    hsExp' <- renameHsExp hsExp subTable'
    return (HsLet hsDecls' hsExp')
renameHsExp (HsCase hsExp hsAlts) subTable = do
    hsExp' <- renameHsExp hsExp subTable
    hsAlts' <- renameHsAlts hsAlts subTable
    return (HsCase hsExp' hsAlts')
renameHsExp (HsDo hsStmts) subTable = do
    e <- doToExp hsStmts
    renameHsExp e subTable
renameHsExp (HsRecConstr hsName hsFieldUpdates) subTable = do
    hsName' <- renameHsName hsName subTable  -- do I need to change this name?
    hsFieldUpdates' <- renameHsFieldUpdates hsFieldUpdates subTable
    return (HsRecConstr hsName' hsFieldUpdates')
renameHsExp (HsRecUpdate hsExp hsFieldUpdates) subTable = do
    hsExp' <- renameHsExp hsExp subTable
    hsFieldUpdates' <- renameHsFieldUpdates hsFieldUpdates subTable
    return (HsRecUpdate hsExp' hsFieldUpdates')
renameHsExp (HsListComp hsExp hsStmts) subTable = do
    (hsStmts',subTable') <- renameHsStmts hsStmts subTable
    hsExp' <- renameHsExp hsExp subTable'
    return (HsListComp hsExp' hsStmts')
renameHsExp (HsExpTypeSig srcLoc hsExp hsQualType) subTable = do
    hsExp' <- renameHsExp hsExp subTable
    subTable' <- updateSubTableWithHsQualType subTable hsQualType
    hsQualType' <- renameHsQualType hsQualType subTable'
    return (HsExpTypeSig srcLoc hsExp' hsQualType')
renameHsExp e subTable = traverseHsExp (flip renameHsExp subTable) e

renameHsAlts :: [HsAlt] -> SubTable -> ScopeSM [HsAlt]
renameHsAlts = mapRename renameHsAlt

-- note for renameHsAlt, the 'wheres' dominate the 'pats'

renameHsAlt :: HsAlt -> SubTable -> ScopeSM (HsAlt)
renameHsAlt (HsAlt srcLoc hsPat hsGuardedAlts {-where-} hsDecls) subTable = withSrcLoc srcLoc $ do
    subTable' <- updateSubTableWithHsPats subTable [hsPat] srcLoc CasePat
    hsPat' <- renameHsPat hsPat subTable'
    subTable'' <- updateSubTableWithHsDecls subTable' hsDecls WhereFun
    hsDecls' <- renameHsDecls hsDecls subTable''
    hsGuardedAlts' <- renameHsRhs hsGuardedAlts subTable''
    return (HsAlt srcLoc hsPat' hsGuardedAlts' hsDecls')

renameHsGuardedRhss :: HsRhs -> SubTable -> ScopeSM (HsRhs)
renameHsGuardedRhss (HsUnGuardedRhs hsExp) subTable = do
      hsExp' <- renameHsExp hsExp subTable
      return (HsUnGuardedRhs hsExp')
renameHsGuardedRhss (HsGuardedRhss hsGuardedAltList) subTable = do
      hsGuardedAltList' <- renameHsGuardedRhsList hsGuardedAltList subTable
      return (HsGuardedRhss hsGuardedAltList')

renameHsGuardedRhsList :: [HsGuardedRhs] -> SubTable -> ScopeSM [HsGuardedRhs]
renameHsGuardedRhsList = mapRename renameHsGuardedRhs

renameHsGuardedRhs :: HsGuardedRhs -> SubTable -> ScopeSM HsGuardedRhs
renameHsGuardedRhs (HsGuardedRhs srcLoc hsExp1 hsExp2) subTable = withSrcLoc srcLoc $ do
    hsExp1' <- renameHsExp hsExp1 subTable
    hsExp2' <- renameHsExp hsExp2 subTable
    return (HsGuardedRhs srcLoc hsExp1' hsExp2')

-- renameHsStmts is trickier than you would expect because
-- the statements are only in scope after they have been declared
-- and thus the subTable must be more carefully threaded through

-- the updated subTable is returned at the end because it is needed by
-- the first section of a list comprehension.

renameHsStmts :: [HsStmt] -> SubTable -> ScopeSM (([HsStmt],SubTable))
renameHsStmts (hsStmt:hsStmts) subTable = do
      subTable' <- updateSubTableWithHsStmt subTable hsStmt
      hsStmt' <- renameHsStmt hsStmt subTable'
      (hsStmts',subTable'') <- renameHsStmts hsStmts subTable'
      return ((hsStmt':hsStmts'),subTable'')
renameHsStmts [] subTable = do
      return ([],subTable)

renameHsStmt :: HsStmt -> SubTable -> ScopeSM (HsStmt)
renameHsStmt (HsGenerator srcLoc hsPat hsExp) subTable = do
      hsExp' <- renameHsExp hsExp subTable
      hsPat' <- renameHsPat hsPat subTable
      return (HsGenerator srcLoc hsPat' hsExp')
renameHsStmt (HsQualifier hsExp) subTable = do
      hsExp' <- renameHsExp hsExp subTable
      return (HsQualifier hsExp')
renameHsStmt (HsLetStmt hsDecls) subTable = do
      hsDecls' <- renameHsDecls hsDecls subTable
      return (HsLetStmt hsDecls')


renameHsFieldUpdates :: [HsFieldUpdate] -> SubTable -> ScopeSM ([HsFieldUpdate])
renameHsFieldUpdates = mapRename renameHsFieldUpdate

renameHsFieldUpdate :: HsFieldUpdate -> SubTable -> ScopeSM (HsFieldUpdate)
-- XXX I'm not 100% sure that this works
{-
renameHsFieldUpdate (HsFieldBind hsName) subTable
  = do
      hsName' <- renameHsName hsName subTable  -- do i need to rename this name?
      return (HsFieldBind hsName')
-}
renameHsFieldUpdate (HsFieldUpdate hsName hsExp) subTable = do
    hsName' <- renameHsName hsName undefined
    hsExp' <- renameHsExp hsExp subTable
    return (HsFieldUpdate hsName' hsExp')


renameHsNames :: [HsName] -> SubTable -> ScopeSM ([HsName])
renameHsNames ns _ = return ns

-- This looks up a replacement name in the subtable.
-- Regardless of whether the name is found, if it's not qualified
-- it will be qualified with the current module's prefix.
renameHsName :: HsName -> SubTable -> ScopeSM (HsName)
renameHsName hsName _ = return hsName



renameTypeHsName hsName subTable  =  return hsName

---------------------------------------
-- utility functions

-- clobberHsName(s) is called by the updateSubTableWith* functions to
-- deal with newly declared identifiers

-- clobberHsName(s) adds new mappings to the SubTable.
-- If a name already appeared, it's mapping is altered to the new one.

-- clobberHsNamesAndUpdateIdentTable also adds a mapping from this
-- renamed name to its source location and binding type

clobberHsNamesAndUpdateIdentTable :: [(HsName,SrcLoc)] -> SubTable -> Binding -> ScopeSM (SubTable)
clobberHsNamesAndUpdateIdentTable ((hsName,srcLoc):hsNamesAndASrcLocs) subTable binding = do
      subTable'  <- clobberHsName hsName subTable
      subTable'' <- clobberHsNamesAndUpdateIdentTable hsNamesAndASrcLocs subTable' binding
      return (subTable'')
clobberHsNamesAndUpdateIdentTable [] subTable _binding = return (subTable)

{-
clobberHsNameAndUpdateIdentTable :: HsName -> SrcLoc -> SubTable -> Binding -> ScopeSM (SubTable)
clobberHsNameAndUpdateIdentTable hsName srcLoc subTable binding
  = do
      unique <- getUnique
      currModule <- getCurrentModule
      let
        hsName'     = renameAndQualify hsName unique currModule
        subTable'   = addToFM (addToFM subTable hsName hsName') hsName' hsName'
      addToIdentTable hsName' (srcLoc, binding)
      incUnique
      return (subTable')
-}

-- takes a list of names and a subtable. adds the associations
-- [name -> renamedName] to the table and returns it.
clobberHsNames :: [HsName] -> SubTable -> ScopeSM (SubTable)
clobberHsNames (hsName:hsNames) subTable
  = do
      subTable'  <- clobberHsName  hsName  subTable
      subTable'' <- clobberHsNames hsNames subTable'
      return (subTable'')
clobberHsNames [] subTable
  = return subTable

clobberHsName :: HsName -> SubTable -> ScopeSM (SubTable)
clobberHsName hsName subTable = return subTable


--------------------------------------------------------
----This section of code updates the current SubTable to reflect the present scope


updateSubTableWithHsDecls :: SubTable -> [HsDecl] -> Binding -> ScopeSM (SubTable)
updateSubTableWithHsDecls subTable [] _binding = return subTable
updateSubTableWithHsDecls subTable (hsDecl:hsDecls) binding = do
    let hsNamesAndASrcLocs = getHsNamesAndASrcLocsFromHsDecl hsDecl
    subTable'  <- clobberHsNamesAndUpdateIdentTable hsNamesAndASrcLocs subTable binding
    subTable'' <- updateSubTableWithHsDecls subTable' hsDecls binding
    return (subTable'')

updateSubTableWithHsPats :: SubTable -> [HsPat] -> SrcLoc -> Binding -> ScopeSM (SubTable)
updateSubTableWithHsPats subTable (hsPat:hsPats) srcLoc binding = do
    let hsNamesAndASrcLocs = zip (getHsNamesFromHsPat hsPat) (repeat srcLoc)
    subTable'  <- clobberHsNamesAndUpdateIdentTable hsNamesAndASrcLocs subTable binding
    subTable'' <- updateSubTableWithHsPats subTable' hsPats srcLoc binding
    return subTable''
updateSubTableWithHsPats subTable [] _srcLoc _binding = do return (subTable)

-- Only one HsStmt should be added at a time because each new identifier is only valid
-- below the point at which it is defined

updateSubTableWithHsStmt :: SubTable -> HsStmt -> ScopeSM (SubTable)
updateSubTableWithHsStmt subTable hsStmt = do
    let hsNamesAndASrcLocs = getHsNamesAndASrcLocsFromHsStmt hsStmt
    subTable' <- clobberHsNamesAndUpdateIdentTable hsNamesAndASrcLocs subTable GenPat
    return (subTable')

----------------------------------------------------------
-- the following updateSubTableWith* functions do not need to alter the identTable aswell
--


-- takes an HsQualType (a type signature) and adds the names of its variables
-- to the current subTable

updateSubTableWithHsQualType :: SubTable -> HsQualType -> ScopeSM (SubTable)
updateSubTableWithHsQualType subTable hsQualType = do
      let hsNames = nub $ getHsNamesFromHsQualType hsQualType
      subTable' <- clobberHsNames hsNames subTable
      return (subTable')



getHsNamesAndASrcLocsFromHsDecl :: HsDecl -> [(HsName, SrcLoc)]
getHsNamesAndASrcLocsFromHsDecl (HsPatBind srcLoc (HsPVar hsName) _ _) = [(hsName, srcLoc)]
-- This will cause errors on code with PatBinds of the form (x,y) = blah...
-- and should be changed for a more general renamer (but is fine for thih)
getHsNamesAndASrcLocsFromHsDecl (HsPatBind sloc _ _ _)
  = error $ "non simple pattern binding found (sloc): " ++ show sloc
-- getHsNamesAndASrcLocsFromHsDecl (HsFunBind _ hsMatches)
getHsNamesAndASrcLocsFromHsDecl (HsFunBind hsMatches) = getHsNamesAndASrcLocsFromHsMatches hsMatches
getHsNamesAndASrcLocsFromHsDecl (HsForeignDecl a _ n _) = [(n,a)]
getHsNamesAndASrcLocsFromHsDecl _otherHsDecl = []

getHsNamesAndASrcLocsFromHsMatches :: [HsMatch] -> [(HsName, SrcLoc)]
getHsNamesAndASrcLocsFromHsMatches [] = []
getHsNamesAndASrcLocsFromHsMatches (hsMatch:_hsMatches) = getHsNamesAndASrcLocsFromHsMatch hsMatch

getHsNamesAndASrcLocsFromHsMatch :: HsMatch -> [(HsName, SrcLoc)]
getHsNamesAndASrcLocsFromHsMatch (HsMatch srcLoc hsName _ _ _)
  = [(hsName, srcLoc)]




getHsNamesFromHsPat :: HsPat -> [HsName]
getHsNamesFromHsPat (HsPVar hsName) = [hsName]
getHsNamesFromHsPat (HsPLit _hsName) = []
getHsNamesFromHsPat (HsPNeg hsPat) = getHsNamesFromHsPat hsPat
-- _hsName can be ignored as it is a Constructor (e.g. in (x:xs) we only want to know what's in scope; that is x and xs)
getHsNamesFromHsPat (HsPInfixApp hsPat1 _hsName hsPat2) = getHsNamesFromHsPat hsPat1 ++ getHsNamesFromHsPat hsPat2
getHsNamesFromHsPat (HsPApp _hsName hsPats) = concat (map getHsNamesFromHsPat hsPats)
getHsNamesFromHsPat (HsPTuple hsPats) = concat (map getHsNamesFromHsPat hsPats)
getHsNamesFromHsPat (HsPUnboxedTuple hsPats) = concat (map getHsNamesFromHsPat hsPats)
getHsNamesFromHsPat (HsPList hsPats) = concat (map getHsNamesFromHsPat hsPats)
getHsNamesFromHsPat (HsPParen hsPat) = getHsNamesFromHsPat hsPat
getHsNamesFromHsPat (HsPRec _hsName hsPatFields) = concat $ map getHsNamesFromHsPatField hsPatFields -- hsName can be ignored as it is a Constructor
getHsNamesFromHsPat (HsPAsPat hsName hsPat) = hsName:(getHsNamesFromHsPat hsPat)
getHsNamesFromHsPat (HsPWildCard) = []
getHsNamesFromHsPat (HsPIrrPat hsPat) = getHsNamesFromHsPat hsPat

-- the hsName can be ignored as it is the field name and must already be in scope
getHsNamesFromHsPatField :: HsPatField -> [HsName]
{-
getHsNamesFromHsPatField (HsPFieldPun _hsName)
  = []
  -}
getHsNamesFromHsPatField (HsPFieldPat _hsName hsPat) = getHsNamesFromHsPat hsPat

getHsNamesAndASrcLocsFromHsStmt :: HsStmt -> [(HsName, SrcLoc)]
getHsNamesAndASrcLocsFromHsStmt (HsGenerator srcLoc hsPat _hsExp) = zip (getHsNamesFromHsPat hsPat) (repeat srcLoc)
getHsNamesAndASrcLocsFromHsStmt (HsQualifier _hsExp) = []
getHsNamesAndASrcLocsFromHsStmt (HsLetStmt hsDecls) = concat $ map getHsNamesAndASrcLocsFromHsDecl hsDecls


-- the getNew... functions are used only inside class declarations to avoid _re_ renaming things
-- that should be left as is.


getHsNamesFromHsQualType :: HsQualType -> [HsName]
getHsNamesFromHsQualType (HsQualType _hsContext hsType) = getHsNamesFromHsType hsType

getHsNamesFromHsType :: HsType -> [HsName]
getHsNamesFromHsType (HsTyFun hsType1 hsType2) = (getHsNamesFromHsType hsType1) ++ (getHsNamesFromHsType hsType2)
getHsNamesFromHsType (HsTyTuple hsTypes) = concat $ map getHsNamesFromHsType hsTypes
getHsNamesFromHsType (HsTyUnboxedTuple hsTypes) = concat $ map getHsNamesFromHsType hsTypes
getHsNamesFromHsType (HsTyApp hsType1 hsType2) = (getHsNamesFromHsType hsType1) ++ (getHsNamesFromHsType hsType2)
getHsNamesFromHsType (HsTyVar hsName) = [hsName]
getHsNamesFromHsType (HsTyForall vs qt) = getHsNamesFromHsQualType qt List.\\ map hsTyVarBindName vs
getHsNamesFromHsType (HsTyExists vs qt) = getHsNamesFromHsQualType qt List.\\ map hsTyVarBindName vs
getHsNamesFromHsType (HsTyCon _hsName) = [] -- don't rename the Constructors


-- gets the names of the functions declared in a class declaration

--------------------------------------------------------------------------------

-- the Renameable class


-- stores the instance Renameable for all of HsSyn

class Renameable a where
    replaceName :: (HsName -> HsName) -> a -> a

instance Renameable SrcLoc where
    replaceName f = id

instance Renameable HsExportSpec where
    replaceName f hsexportspec
      = let a # b = a $ (replaceName f b)
        in case hsexportspec of
            HsEVar  name               ->
                HsEVar  # name
            HsEAbs  name               ->
                HsEAbs  # name
            HsEThingAll  name		 ->
                HsEThingAll  # name
            HsEThingWith  name names	 ->
                HsEThingWith  # name # names
            HsEModuleContents mod	 ->
                HsEModuleContents mod


instance Renameable HsImportDecl where
    replaceName f object
      = let a # b = a $ (replaceName f b)
            a $$ b = a b
            infixl 0 $$
        in case object of
            HsImportDecl  srcloc mod bool maybe1 maybe2 ->
                HsImportDecl # srcloc $$ mod $$ bool $$ maybe1 $$ maybe2'
                where maybe2' = fmap (\(b,importSpec) -> (b, replaceName f importSpec)) maybe2


instance Renameable HsImportSpec where
    replaceName f object
      = let a # b = a $ (replaceName f b)
        in case object of
            HsIVar  name			 ->
                HsIVar  # name
            HsIAbs  name			 ->
                HsIAbs  # name
            HsIThingAll  name		 ->
                HsIThingAll  # name
            HsIThingWith  name names	 ->
                HsIThingWith  # name # names


{-
instance Renameable HsInfixDecl where
    replaceName f object
      = let a # b = a $ (replaceName f b)
        in case object of
            HsInfixDecl  srcloc fixity names ->
                HsInfixDecl  # srcloc # fixity # names
-}


{-
instance Renameable HsFixity where
    replaceName f = id
-}

instance Renameable HsAssoc where
    replaceName _ object = object

instance Renameable HsTyVarBind where
    replaceName f = hsTyVarBindName_u (replaceName f)

instance Renameable (HsDecl) where
    replaceName f object
      = let a # b = a $ (replaceName f b)
        in case object of
            HsTypeDecl 	srcloc name names typ ->
                HsTypeDecl 	srcloc # name # names # typ
            HsDataDecl 	srcloc context name names condecls names' ->
                HsDataDecl 	srcloc # context # name # names # condecls # names'
            HsNewTypeDecl 	srcloc context name names condecl names' ->
                HsNewTypeDecl 	srcloc # context # name # names # condecl # names'
            HsClassDecl 	srcloc qualtyp objects ->
                HsClassDecl 	srcloc # qualtyp # objects
            HsInstDecl 	srcloc qualtyp objects ->
                HsInstDecl 	srcloc # qualtyp # objects
            HsDefaultDecl 	srcloc typ ->
                HsDefaultDecl 	srcloc # typ
            HsTypeSig 	srcloc names qualtyp ->
                HsTypeSig 	srcloc # names # qualtyp
            -- HsFunBind       srcloc matc ->
            HsFunBind          matc ->
                -- HsFunBind  # srcloc # matc
                HsFunBind  # matc
            HsPatBind 	srcloc pat r {-where-} objects ->
                HsPatBind 	srcloc # pat # r # objects
            od -> od


instance Renameable (HsMatch) where
    replaceName f object
      = let a # b = a $ (replaceName f b)
        in case object of
            HsMatch  srcloc name pats r {-where-} objects ->
                HsMatch  # srcloc # name # pats # r # objects


instance Renameable HsConDecl where
    replaceName f = hsConDeclExists_u (replaceName f) . hsConDeclName_u (replaceName f) . hsConDeclRecArg_u (replaceName f) . hsConDeclConArg_u (replaceName f)
--      let a # b = a $ (replaceName f b)
--        in case object of
--            HsConDecl  srcloc name bangtyps ->
--                HsConDecl  # srcloc # name # bangtyps
--            HsRecDecl  srcloc name names_and_bangtyp ->
--                HsRecDecl  # srcloc # name # names_and_bangtyp




instance Renameable HsBangType where
    replaceName f object
      = let a # b = a $ (replaceName f b)
        in case object of
            HsBangedTy    typ ->
                HsBangedTy  # typ
            HsUnBangedTy  typ ->
                HsUnBangedTy  # typ


instance Renameable (HsRhs) where
    replaceName f object
      = let a # b = a $ (replaceName f b)
        in case object of
            HsUnGuardedRhs  exp ->
                HsUnGuardedRhs  # exp
            HsGuardedRhss   guardedrs ->
                HsGuardedRhss  # guardedrs


instance Renameable (HsGuardedRhs) where
    replaceName f object
      = let a # b = a $ (replaceName f b)
        in case object of
            HsGuardedRhs  srcloc exp exp' ->
                HsGuardedRhs  # srcloc # exp # exp'


instance Renameable HsQualType where
    replaceName f object
      = let a # b = a $ (replaceName f b)
        in case object of
            HsQualType    context typ ->
                HsQualType  # context # typ


instance Renameable HsType where
    replaceName f object
      = let a # b = a $ (replaceName f b)
        in case object of
            HsTyFun    typ typ' ->
                HsTyFun  # typ # typ'
            HsTyTuple  typs ->
                HsTyTuple  # typs
            HsTyUnboxedTuple  typs ->
                HsTyUnboxedTuple  # typs
            HsTyApp    typ typ' ->
                HsTyApp  # typ # typ'
            HsTyVar    name ->
                HsTyVar  # name
            HsTyCon    name ->
                HsTyCon  # name

instance Renameable HsLiteral where
    replaceName f = id

instance Renameable (HsExp) where
    replaceName f object
      = let a # b = a $ (replaceName f b)
        in case object of
            -- HsVar  name ann -> HsVar (replaceName f name) ann
            HsVar  name -> HsVar (replaceName f name)
            HsCon  name ->
                HsCon  # name
            HsLit  literal ->
                HsLit  # literal
            HsInfixApp  exp exp' exp'' ->
                HsInfixApp  # exp # exp' # exp''
            HsApp  exp exp' ->
                HsApp  # exp # exp'
            HsNegApp  exp ->
                HsNegApp  # exp
            HsLambda  srcloc pats exp ->
                HsLambda  # srcloc # pats # exp
            HsLet  objects exp ->
                HsLet  # objects # exp
            HsIf  exp exp' exp'' ->
                HsIf  # exp # exp' # exp''
            HsCase  exp alts ->
                HsCase  # exp # alts
            HsDo  stmts ->
                HsDo  # stmts
            HsTuple  exps ->
                HsTuple  # exps
            HsUnboxedTuple  exps ->
                HsUnboxedTuple  # exps
            HsList  exps ->
                HsList  # exps
            HsParen  exp ->
                HsParen  # exp
            HsLeftSection  exp exp' ->
                HsLeftSection  # exp # exp'
            HsRightSection  exp exp' ->
                HsRightSection  # exp # exp'
            HsRecConstr  name fieldupdates ->
                HsRecConstr  # name # fieldupdates
            HsRecUpdate  exp fieldupdates ->
                HsRecUpdate  # exp # fieldupdates
            HsEnumFrom  exp ->
                HsEnumFrom  # exp
            HsEnumFromTo  exp exp' ->
                HsEnumFromTo  # exp # exp'
            HsEnumFromThen  exp exp' ->
                HsEnumFromThen  # exp # exp'
            HsEnumFromThenTo  exp exp' exp'' ->
                HsEnumFromThenTo  # exp # exp' # exp''
            HsListComp  exp stmts ->
                HsListComp  # exp # stmts
            HsExpTypeSig  srcloc exp qualtyp ->
                HsExpTypeSig  # srcloc # exp # qualtyp
            HsAsPat  name exp		 ->
                HsAsPat  # name # exp
            HsWildCard x 	      	 ->
                HsWildCard x
            HsIrrPat  exp		 ->
                HsIrrPat  # exp

instance Renameable HsPat where
    replaceName f object
      = let a # b = a $ (replaceName f b)
        in case object of
            HsPVar  name ->
                HsPVar  # name
            HsPLit  literal ->
                HsPLit  # literal
            HsPNeg  pat ->
                HsPNeg  # pat
            HsPInfixApp  pat name pat' ->
                HsPInfixApp  # pat # name # pat'
            HsPApp  name pats ->
                HsPApp  # name # pats
            HsPTuple  pats ->
                HsPTuple  # pats
            HsPUnboxedTuple  pats ->
                HsPUnboxedTuple  # pats
            HsPList  pats ->
                HsPList  # pats
            HsPParen  pat ->
                HsPParen  # pat
            HsPRec  name patfields ->
                HsPRec  # name # patfields
            HsPAsPat  name pat ->
                HsPAsPat  # name # pat
            HsPWildCard  ->
                HsPWildCard
            HsPIrrPat  pat ->
                HsPIrrPat  # pat


instance Renameable HsPatField where
    replaceName f object
      = let a # b = a $ (replaceName f b)
        in case object of
{-
            HsPFieldPun  name ->
                HsPFieldPun  # name
-}
            HsPFieldPat  name pat ->
                HsPFieldPat  # name # pat


instance Renameable (HsStmt) where
    replaceName f object
      = let a # b = a $ (replaceName f b)
        in case object of
            HsGenerator  srcloc pat exp ->
                HsGenerator  # srcloc # pat # exp
            HsQualifier  exp ->
                HsQualifier  # exp
            HsLetStmt  objects ->
                HsLetStmt  # objects


instance Renameable (HsFieldUpdate) where
    replaceName f object
      = let a # b = a $ (replaceName f b)
        in case object of
{-
            HsFieldBind  name ->
                HsFieldBind  # name
-}
            HsFieldUpdate  name exp ->
                HsFieldUpdate  # name # exp


instance Renameable (HsAlt) where
    replaceName f object
      = let a # b = a $ (replaceName f b)
        in case object of
            HsAlt  srcloc pat guardedalts objects ->
                HsAlt  # srcloc # pat # guardedalts # objects



instance Renameable HsName where
    replaceName f name = f name

instance Renameable HsAsst where
    replaceName f (HsAsst x xs) = HsAsst (replaceName f x) (replaceName f xs)
    replaceName f (HsAsstEq x y) = HsAsstEq (replaceName f x) (replaceName f y)

instance (Renameable a, Renameable b) => Renameable (a,b) where
    replaceName f (x,y) = (replaceName f x, replaceName f y)
instance Renameable a => Renameable [a] where
    replaceName f xs = map (replaceName f) xs


-- Ident table stuff
--type IdentTable = FiniteMap HsName (SrcLoc, Binding)
--addToIdentTable _ _ = return ()

data Binding
   = WhereFun           -- function binding in a where clause
   | LetFun             -- function binding in a let expression (used to include topbinds too)
   | LamPat             -- pattern binding in a lambda expression
   | CasePat            -- pattern binding in a case expression
   | GenPat             -- pattern binding in a generator statement
   | FunPat             -- pattern binding in a function declaration

{-
printIdentTable :: IdentTable -> IO ()
printIdentTable idt
   = putStr $ unlines $ map showIdentTabEntry $ toListFM idt
   where
   showIdentTabEntry :: (HsName, (SrcLoc, Binding)) -> String
   showIdentTabEntry (name, (SrcLoc fn row col, bind))
      = lJustify 40 (fromHsName name) ++
        fn ++ ":" ++ showPos (row, col) ++
        rJustify 10 (show bind)
   showPos pos@(row, col)
      | row < 0 || col < 0 = rJustify 10 "none"
      | otherwise          = rJustify 10 $ show pos

-- returns the binding type of a given identifier

bindOfId :: IdentTable -> HsName -> Binding
bindOfId idtab i
   = case lookupFM idtab i of
        Nothing -> error $ "bindOfId: could not find binding for this identifier: " ++ show i
        Just (_sloc, bind) -> bind
addToIdentTable :: HsName -> (SrcLoc,Binding) -> ScopeSM ()
addToIdentTable hsName srcLocAndBinding
   = modify (\state -> state {identTable = addToFM (identTable state) hsName srcLocAndBinding })
-}
