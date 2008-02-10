{-------------------------------------------------------------------------------

        Copyright:              The Hatchet Team (see file Contributors)

        Module:                 Rename

        Description:            Renames variables apart in a Module

        Primary Authors:        Toby Ord, Bryn Humberstone, Bernie Pope

        Notes:                  See the file License for license information

-------------------------------------------------------------------------------}

{-------------------------------------------------------------------------------

  Major changes by John, many comments probably invalid.
  This also desugars records

  Implementation:


     The algorithm then proceeds through the syntax tree, from outermost scope
     to innermost in a depth first manner.

     On entering a new scope, updateSubTableWith* is used to get the new names
     in this scope, putting them into the current subTable, clobbering any
     identifiers with the same name in outer scopes. It also creates their new
     names (although no renaming is performed yet)

     Now, all identifiers the algorithm finds before entering the next nested
     scope will have a mapping to a new name in the subTable and their old
     names get replaced by these


  * bugs

     It should work for records but it doesn't rename them because it never adds
     them to the scope

     It also needs more testing for records

     It assumes that all PatBinds have only one identifier to the left of the equals
     ie. x     = a b c d   is OK
         (x,y) = a b c d   is not
     this should not be too hard to change

     It doesn't add information about the identifiers in class and instance
     definitions to the identTable.

        - The correct behaviour here is not obvious as these are the only
          identifiers that are declared multiple times, so there is no unique
          source location.
        - The method of declaration is also different to normal as identifiers
          are normally added to the identTable when they are first added to the
          scope, but these are added in the class
          pass and can't be re-added.

     It doesn't rename type signatures that do not have an associated PatBind
     or FunBind

-------------------------------------------------------------------------------}

module FrontEnd.Rename(unRename, collectDefsHsModule, renameModule, FieldMap, renameStatement ) where

import Char
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Identity
import qualified Data.Traversable as T
import Data.Monoid
import List
import Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set


import Doc.DocLike(tupled)
import FrontEnd.Desugar (doToExp)
import FrontEnd.SrcLoc hiding(srcLoc)
import FrontEnd.Syn.Traverse
import FrontEnd.Utils
import HsSyn
import Name.Name as Name hiding(qualifyName)
import Name.Names
import Support.FreeVars
import Util.Gen
import Util.Inst()
import FrontEnd.Warning
import qualified FrontEnd.HsErrors as HsErrors
import qualified Name.VConsts as V

type FieldMap =  (Map.Map Name Int,Map.Map Name [(Name,Int)])

--------------------------------------------------------------------------------

--instance (Show a, Show b) => Show (FiniteMap a b) where
--    show fm = show (fmToList fm)


-- a 'Substitution Table' which is a map from old names to new names
-- All names in the current scope are stored in here, with their renamings

type SubTable = Map.Map HsName HsName

-- an Identifier Table is a map from renamed names to that identifier's source
-- location and binding type


-- the monadic state

data ScopeState = ScopeState {
    currentModule  :: Module,
    unique         :: !Int,
    globalSubTable :: Map.Map HsName HsName,  -- Current substition
    typeSubTable   :: Map.Map HsName HsName,  -- type substition table
    errorTable     :: Map.Map HsName String,  -- special error message. else it's just unknown.
    nameMap        :: Map.Map Name (Either String Name),
    fieldLabels    :: FieldMap,
    errors         :: [Warning],
    srcLoc         :: !SrcLoc
    }


-- The monadic type
type ScopeSM = State ScopeState
runScopeSM s0 a = runState a s0

instance MonadWarn ScopeSM where
    addWarning w = modify (\s -> s { errors = w: errors s})

instance UniqueProducer ScopeSM where
    newUniq = do
        u <- gets unique
        modify (\state -> state {unique = (unique state) + 1})
        return u


getCurrentModule :: ScopeSM Module
getCurrentModule = gets currentModule


setSrcLoc e = modify (\s -> s { srcLoc =  e `mappend` srcLoc s })

instance MonadSrcLoc ScopeSM where
    getSrcLoc = gets srcLoc
instance MonadSetSrcLoc ScopeSM where
    withSrcLoc sl a = modify (\s -> s { srcLoc = sl `mappend` srcLoc s}) >> a

-- functions to modify the ScopeSM



-----------------------------------------------------------
-- The renaming code:
--


addTopLevels ::  [HsDecl]  -> ScopeSM ()
addTopLevels  []  = return ()
addTopLevels  hsDecls = do
    mod <- getCurrentModule
    let (ns,ts) = mconcat (map namesHsDecl hsDecls)
        nm = Map.fromList $ foldl f [] (fsts ns)
        tm = Map.fromList $ foldl f [] (fsts ts)
        f r hsName@Qual {}
            | Just _ <- V.fromTupname hsName, Module "Jhc.Basics" <- mod
                = let nn = hsName in (nn,nn):r
            | otherwise = error $ "strong bad: " ++ show hsName
        f r z@(UnQual n) = let nn = Qual mod n in (z,nn):(nn,nn):r
        z ns = mapM mult (filter (\x -> length x > 1) $ groupBy (\a b -> fst a == fst b) (sort ns))
        mult xs@((n,sl):_) = warn sl "multiply-defined" (show n ++ " is defined multiple times: " ++ show xs )
    z ns >> z ts
    modify (\s -> s { globalSubTable = nm `Map.union` globalSubTable s })
    modify (\s -> s { typeSubTable = tm `Map.union` typeSubTable s })
    return ()


ambig x ys = "Ambiguous Name: " ++ show x ++ "\nCould refer to: " ++ tupled (map show ys)

-- | Main entry point.

{-# NOINLINE renameModule #-}
renameModule :: MonadWarn m => FieldMap -> [(Name,[Name])] -> HsModule -> m HsModule
renameModule fls ns m = mapM_ addWarning (errors finalState) >> return renamedMod where
    initialGlobalSubTable = Map.fromList [ (x,y) | ((typ,x),[y]) <- ns', typ == Val || typ == DataConstructor ]
    initialTypeSubTable = Map.fromList [ (x,y) | ((typ,x),[y]) <- ns', typ == TypeConstructor || typ == ClassName ]
    ns' = map fn ns
    fn (n,ns) = (fromName n, map nameName ns)

    errorTab =  Map.fromList [ (x,ambig x ys) | ((typ,x),ys@(_:_:_)) <- ns' ]

    startState = ScopeState {
        typeSubTable   = initialTypeSubTable,
        errorTable     = errorTab,
        nameMap        = Map.empty,
        errors         = [],
        srcLoc         = mempty,
        unique         = 1,   -- start the counting at 1
        globalSubTable = initialGlobalSubTable,
        fieldLabels    = fls,
        currentModule  = hsModuleName m
        }

    (renamedMod, finalState) = runScopeSM startState (renameDecls m initialGlobalSubTable)

{-# NOINLINE renameStatement #-}
renameStatement :: MonadWarn m => FieldMap -> [(Name,[Name])] -> Module -> HsStmt -> m HsStmt
renameStatement fls ns modName stmt = mapM_ addWarning (errors finalState) >> return renamedStmt where
    initialGlobalSubTable = Map.fromList [ (x,y) | ((typ,x),[y]) <- ns', typ == Val || typ == DataConstructor ]
    initialTypeSubTable = Map.fromList [ (x,y) | ((typ,x),[y]) <- ns', typ == TypeConstructor || typ == ClassName ]
    ns' = map fn ns
    fn (n,ns) = (fromName n, map nameName ns)

    errorTab =  Map.fromList [ (x,ambig x ys) | ((typ,x),ys@(_:_:_)) <- ns' ]

    startState = ScopeState {
        typeSubTable   = initialTypeSubTable,
        errorTable     = errorTab,
        nameMap        = Map.empty,
        errors         = [],
        srcLoc         = mempty,
        unique         = 1,   -- start the counting at 1
        globalSubTable = initialGlobalSubTable,
        fieldLabels    = fls,
        currentModule  = modName
        }

    (renamedStmt, finalState) = runScopeSM startState (renameHsStmt stmt initialGlobalSubTable)

-- This is Bryn's modification to make the code a bit easier to understand for
-- functions like renameHsNames, renameHsFileUpdates
mapRename :: (a -> SubTable -> ScopeSM a) -> [a] -> SubTable -> ScopeSM [a]
mapRename renameIndividual individuals subTable
    = mapM (`renameIndividual` subTable) individuals



renameDecls :: HsModule -> SubTable -> ScopeSM HsModule
renameDecls tidy subTable = do
        addTopLevels $ hsModuleDecls tidy
        subTable'a <- gets globalSubTable
        let subTable' = subTable `Map.union` subTable'a
        --addError (show $ fmToList subTable')
        decls' <-  renameHsDeclsTL (hsModuleDecls tidy) subTable' ; return decls'
        --addDiag (show syns)
        --sta <- gets globalSubTable
        --stb <- gets typeSubTable
        --addDiag (show (sta, stb))
        return tidy { hsModuleDecls = decls' }


-- The following functions all take a piece of the Haskell syntax tree
-- (as outlined in HsSyn) and uses the provided SubTable to rename it

-- Some of the functions have to create a new nested scope which they do
-- by creating a new SubTable using updateSubTableWith* and passing that
-- new table down to its children on the syntax tree.

renameHsDecls :: [HsDecl] -> SubTable -> ScopeSM ([HsDecl])
renameHsDecls  decls subtable = do
    ans <- mapRename renameHsDecl (expandTypeSigs decls) subtable
    mapM_ HsErrors.hsDeclLocal ans
    return ans

renameHsDeclsTL :: [HsDecl] -> SubTable -> ScopeSM ([HsDecl])
renameHsDeclsTL  decls subtable = do
    ans <- mapRename renameHsDecl (expandTypeSigs decls) subtable
    mapM_ HsErrors.hsDeclTopLevel ans
    return ans

renameHsDeclsN :: [HsDecl] -> SubTable -> ScopeSM ([HsDecl])
renameHsDeclsN  decls subtable = do
    ans <- mapRename renameHsDecl (expandTypeSigs decls) subtable
    return ans


expandTypeSigs :: [HsDecl] -> [HsDecl]
expandTypeSigs ds =  (concatMap f ds) where
    f (HsTypeSig sl ns qt) =  [ HsTypeSig sl [n] qt | n <- ns]
    f d = return d

renameHsDecl :: HsDecl -> SubTable -> ScopeSM (HsDecl)
renameHsDecl (HsPatBind srcLoc hsPat hsRhs {-where-} hsDecls) subTable = do
    setSrcLoc srcLoc
    hsPat'    <- renameHsPat hsPat subTable
    subTable' <- updateSubTableWithHsDecls subTable hsDecls
    hsDecls'  <- renameHsDecls hsDecls subTable'
    hsRhs'    <- renameHsRhs hsRhs subTable'
    let patbind' = (HsPatBind srcLoc hsPat' hsRhs' {-where-} hsDecls')
    return patbind'

renameHsDecl (HsForeignExport a b n t) subTable = do
    setSrcLoc a
    n <- renameHsName n subTable
    subTable' <- updateSubTableWithHsQualType subTable t
    t <- renameHsQualType t subTable'
    return (HsForeignExport a b n t)

renameHsDecl (HsForeignDecl a b n t) subTable = do
    setSrcLoc a
    n <- renameHsName n subTable
    subTable' <- updateSubTableWithHsQualType subTable t
    t <- renameHsQualType t subTable'
    return (HsForeignDecl a b n t)

--renameHsDecl (HsFunBind srcLoc hsMatches) subTable
renameHsDecl (HsFunBind hsMatches) subTable = do
    hsMatches' <- renameAny hsMatches subTable
    -- return (HsFunBind srcLoc hsMatches')
    return (HsFunBind hsMatches')

renameHsDecl (HsTypeSig srcLoc hsNames hsQualType) subTable = do
    setSrcLoc srcLoc
    hsNames' <- renameHsNames hsNames subTable
    subTable' <- updateSubTableWithHsQualType subTable hsQualType
    hsQualType' <- renameHsQualType hsQualType subTable'
    return (HsTypeSig srcLoc hsNames' hsQualType')
renameHsDecl dl@HsDataDecl { hsDeclSrcLoc = srcLoc, hsDeclContext = hsContext, hsDeclName = hsName, hsDeclArgs = hsNames1, hsDeclCons = hsConDecls, hsDeclDerives = hsNames2 } subTable = do
    setSrcLoc srcLoc
    hsName' <- renameTypeHsName hsName subTable
    subTable' <- updateSubTableWithHsNames subTable hsNames1
    hsContext' <- renameHsContext hsContext subTable'
    hsNames1' <- renameHsNames hsNames1 subTable'
    hsConDecls' <- renameHsConDecls hsConDecls subTable'
    -- don't need to rename the hsNames2 as it is just a list of TypeClasses
    hsNames2' <- mapM (`renameTypeHsName` subTable') hsNames2
    return dl { hsDeclContext = hsContext', hsDeclName = hsName', hsDeclArgs = hsNames1', hsDeclCons = hsConDecls', hsDeclDerives = hsNames2' }
renameHsDecl (HsTypeDecl srcLoc name hsNames t) subTable = do
    setSrcLoc srcLoc
    hsName' <- renameTypeHsName name subTable
    subTable' <- updateSubTableWithHsNames subTable (Set.toList $ freeVars hsNames)
    --subTable' <- updateSubTableWithHsNames subTable hsNames
    hsNames' <- renameAny hsNames subTable'
    t' <- renameHsType t subTable'
    return (HsTypeDecl srcLoc  hsName' hsNames' t')

renameHsDecl (HsNewTypeDecl srcLoc hsContext hsName hsNames1 hsConDecl hsNames2) subTable = do
    setSrcLoc srcLoc
    hsName' <- renameTypeHsName hsName subTable
    subTable' <- updateSubTableWithHsNames subTable hsNames1
    hsContext' <- renameHsContext hsContext subTable'
    hsNames1' <- renameHsNames hsNames1 subTable'
    hsConDecl' <- renameHsConDecl hsConDecl subTable'
    -- don't need to rename the hsNames2 as it is just a list of TypeClasses
    hsNames2' <- mapM (`renameTypeHsName` subTable') hsNames2
    return (HsNewTypeDecl srcLoc hsContext' hsName' hsNames1' hsConDecl' hsNames2')
renameHsDecl (HsClassDecl srcLoc hsQualType hsDecls) subTable = do
    setSrcLoc srcLoc
    startingSubTable <- return subTable
    {- WAS: typeSigSubTable <- updateSubTableWithHsQualType initialSubTable hsQualType -}
    typeSigSubTable <- updateSubTableWithHsQualType startingSubTable hsQualType
    hsQualType' <- renameHsQualType hsQualType typeSigSubTable
    doesClassMakeSense hsQualType'
    hsDecls' <- renameHsDeclsN hsDecls subTable
    return (HsClassDecl srcLoc hsQualType' hsDecls')
renameHsDecl (HsInstDecl srcLoc hsQualType hsDecls) subTable = do
    setSrcLoc srcLoc
    subTable' <- updateSubTableWithHsQualType subTable hsQualType
    hsQualType' <- renameHsQualType hsQualType subTable'
    hsDecls' <- renameHsDeclsN hsDecls subTable'
    return (HsInstDecl srcLoc hsQualType' hsDecls')
renameHsDecl (HsInfixDecl srcLoc assoc int hsNames) subTable = do
    setSrcLoc srcLoc
    hsNames' <- renameHsNames hsNames subTable
    return $ HsInfixDecl srcLoc assoc int hsNames'
renameHsDecl (HsActionDecl srcLoc pat e) subTable = do
    setSrcLoc srcLoc
    pat <- renameAny pat subTable
    e <- renameAny e subTable
    return (HsActionDecl srcLoc pat e)
renameHsDecl (HsPragmaProps srcLoc prop hsNames) subTable = do
    setSrcLoc srcLoc
    hsNames' <- renameHsNames hsNames subTable
    return (HsPragmaProps  srcLoc prop hsNames')
renameHsDecl (HsPragmaRules rs) subTable = do
    rs' <- mapM (`renameHsRule` subTable) rs
    return $ HsPragmaRules rs'
renameHsDecl prules@HsPragmaSpecialize { hsDeclSrcLoc = srcLoc, hsDeclName = n, hsDeclType = t } subTable = do
    setSrcLoc srcLoc
    n <- renameAny n subTable
    t <- renameAny t subTable
    m <- getCurrentModule
    i <- newUniq
    return prules { hsDeclUniq = (m,i), hsDeclName = n, hsDeclType = t }

renameHsDecl otherHsDecl _ = return otherHsDecl

renameHsRule prules@HsRule { hsRuleSrcLoc = srcLoc, hsRuleFreeVars = fvs, hsRuleLeftExpr = e1, hsRuleRightExpr = e2 } subTable = do
    setSrcLoc srcLoc
    subTable' <- updateSubTableWithHsNames subTable (fsts fvs)
    subTable'' <- updateSubTableWithHsTypes subTable (catMaybes $ snds fvs)
    fvs' <- sequence [ liftM2 (,) (renameAny x subTable') (renameAny y subTable'')| (x,y) <- fvs]
    e1' <- renameHsExp e1 subTable'
    e2' <- renameHsExp e2 subTable'
    m <- getCurrentModule
    i <- newUniq
    return prules {  hsRuleUniq = (m,i), hsRuleFreeVars = fvs', hsRuleLeftExpr = e1', hsRuleRightExpr = e2' }

doesClassMakeSense :: HsQualType -> ScopeSM ()
doesClassMakeSense (HsQualType _ type_) =
 case type_ of
  (HsTyApp (HsTyCon _) (HsTyVar _)) -> return ()
  (HsTyApp (HsTyApp _ _) _)         -> failRename "Multiparameter typeclasses not supported"
  (HsTyCon _)                       -> failRename "Typeclass with no parameters"
  _                                 -> failRename $ "Invalid type in class declaration: "++show type_

renameHsQualType :: HsQualType -> SubTable -> ScopeSM (HsQualType)
renameHsQualType (HsQualType hsContext hsType) subTable = do
      hsContext' <- renameHsContext hsContext subTable
      hsType' <- renameHsType hsType subTable
      return (HsQualType hsContext' hsType')

renameHsContext :: HsContext -> SubTable -> ScopeSM (HsContext)
renameHsContext = mapRename renameHsAsst

renameHsAsst :: HsAsst -> SubTable -> ScopeSM HsAsst
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
renameHsConDecl cd@(HsConDecl { hsConDeclSrcLoc = srcLoc, hsConDeclName = hsName, hsConDeclConArg = hsBangTypes }) subTable = do
    setSrcLoc srcLoc
    hsName' <- renameHsName hsName subTable
    subTable' <- updateSubTableWithHsNames subTable (map hsTyVarBindName (hsConDeclExists cd))
    es <- renameAny (hsConDeclExists cd) subTable'
    hsBangTypes' <- renameHsBangTypes hsBangTypes subTable'
    return cd { hsConDeclName = hsName', hsConDeclConArg = hsBangTypes', hsConDeclExists = es }
renameHsConDecl cd@HsRecDecl { hsConDeclSrcLoc = srcLoc, hsConDeclName = hsName, hsConDeclRecArg = stuff} subTable = do
    setSrcLoc srcLoc
    hsName' <- renameHsName hsName subTable
    subTable' <- updateSubTableWithHsNames subTable (map hsTyVarBindName (hsConDeclExists cd))
    es <- renameAny (hsConDeclExists cd) subTable'
    stuff' <- sequence [ do ns' <- mapRename renameHsName ns subTable'; t' <- renameHsBangType t subTable; return (ns',t')  |  (ns,t) <- stuff]
    return cd { hsConDeclName = hsName', hsConDeclRecArg = stuff', hsConDeclExists = es }

renameHsBangTypes :: [HsBangType] -> SubTable -> ScopeSM ([HsBangType])
renameHsBangTypes = mapRename renameHsBangType

renameHsBangType :: HsBangType -> SubTable -> ScopeSM (HsBangType)
renameHsBangType (HsBangedTy hsType) subTable = do
    hsType' <- renameHsType hsType subTable
    return (HsBangedTy hsType')
renameHsBangType (HsUnBangedTy hsType) subTable = do
    hsType' <- renameHsType hsType subTable
    return (HsUnBangedTy hsType')

renameHsType t st = do
    t <- renameHsType' True t st
    HsErrors.hsType t
    return t

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
        -- False <- return dovar
        subTable' <- updateSubTableWithHsNames subTable (map hsTyVarBindName ts)
        ts' <- renameAny ts subTable'
        v' <- renameHsQualType v subTable'
        return $ HsTyForall ts' v'
    rt (HsTyExists ts v) subTable  = do
        -- False <- return dovar
        subTable' <- updateSubTableWithHsNames subTable (map hsTyVarBindName ts)
        ts' <- renameAny ts subTable'
        v' <- renameHsQualType v subTable'
        return $ HsTyExists ts' v'
    rt (HsTyAssoc) subTable = return HsTyAssoc
    rt (HsTyEq a b) subTable = return HsTyEq `ap` (flip renameAny subTable a) `ap` (flip renameAny subTable b)
    pp t | not dovar = t
    pp t = t


class RenameAny a where
    renameAny :: a -> SubTable -> ScopeSM a
    renameAny x _ = return x

instance RenameAny SrcLoc where

instance RenameAny a => RenameAny [a] where
    renameAny xs t = mapM (`renameAny` t) xs

instance (RenameAny a,RenameAny b) => RenameAny (a,b) where
    renameAny (a,b) t = liftM2 (,) (renameAny a t) (renameAny b t)

instance RenameAny a => RenameAny (Maybe a) where
    renameAny Nothing _ = return Nothing
    renameAny (Just x) t = liftM Just (renameAny x t)

instance RenameAny HsTyVarBind where
    renameAny tvb@HsTyVarBind { hsTyVarBindName = n } t = do
        n' <- renameTypeHsName n t
        return tvb { hsTyVarBindName = n' }

instance RenameAny HsExp where
    renameAny = renameHsExp

instance RenameAny HsMatch where
    renameAny = renameHsMatch

instance RenameAny HsName where
    renameAny = renameHsName
instance RenameAny HsType where
    renameAny = renameHsType


-- note that for renameHsMatch, the 'wheres' dominate the 'pats'

renameHsMatch :: HsMatch -> SubTable -> ScopeSM HsMatch
renameHsMatch (HsMatch srcLoc hsName hsPats hsRhs {-where-} hsDecls) subTable = do
    setSrcLoc srcLoc
    hsName' <- renameHsName hsName subTable
    subTable' <- updateSubTableWithHsPats subTable hsPats srcLoc
    hsPats' <- renameAny hsPats subTable'
    subTable'' <- updateSubTableWithHsDecls subTable' hsDecls
    hsDecls' <- renameHsDecls hsDecls subTable''
    hsRhs' <- renameHsRhs hsRhs subTable''
    return (HsMatch srcLoc hsName' hsPats' hsRhs' {-where-} hsDecls')



instance RenameAny HsPat where
    renameAny = renameHsPat

renameHsPat :: HsPat -> SubTable -> ScopeSM (HsPat)
renameHsPat (HsPVar hsName) subTable = do
      hsName' <- renameHsName hsName subTable
      return (HsPVar hsName')
renameHsPat (HsPInfixApp hsPat1 hsName hsPat2) subTable = do
      hsPat1' <- renameHsPat hsPat1 subTable
      hsPat2' <- renameHsPat hsPat2 subTable
      hsName' <- renameHsName hsName subTable
      return (HsPInfixApp hsPat1' hsName' hsPat2')
renameHsPat (HsPApp hsName hsPats) subTable = do
      hsPats' <- renameAny hsPats subTable
      hsName' <- renameHsName hsName subTable
      return (HsPApp hsName' hsPats')  -- NOTE: Bryn changed this so we also rename hsName and not just the hsPats
renameHsPat (HsPRec hsName hsPatFields) subTable = do
      hsName' <- renameHsName hsName subTable
      hsPatFields' <- renameHsPatFields hsPatFields subTable
      fls <- gets fieldLabels
      buildRecPat fls hsName' hsPatFields'
renameHsPat (HsPAsPat hsName hsPat) subTable = do
      hsName' <- renameHsName hsName subTable
      hsPat' <- renameHsPat hsPat subTable
      return (HsPAsPat hsName' hsPat')
renameHsPat (HsPTypeSig sl hsPat qt) subTable = do
      hsPat' <- renameHsPat hsPat subTable
      qt' <- renameHsQualType qt subTable
      return (HsPTypeSig sl hsPat' qt')
renameHsPat p subTable = traverseHsPat (flip renameHsPat subTable) p

buildRecPat :: FieldMap -> HsName -> [HsPatField] -> ScopeSM HsPat
buildRecPat (amp,fls) n us = case Map.lookup (toName DataConstructor n) amp of
    Nothing -> failRename $ "Unknown Constructor: " ++ show n
    Just t -> do
        let f (HsPFieldPat x p) = case  Map.lookup (toName FieldLabel x) fls of
                Nothing -> failRename $ "Field Label does not exist: " ++ show x
                Just cs -> case lookup n [ (nameName x,(y)) | (x,y) <- cs ] of
                    Nothing -> failRename $ "Field Label does not belong to constructor: " ++ show (x,n)
                    Just i -> return (i,HsPParen p)
        fm <- mapM f us
        let g i | Just e <- lookup i fm = return e
                | otherwise = do
                    v <- newVar
                    return $ HsPVar v
        rs <- mapM g [0 .. t - 1 ]
        return $ HsPApp n rs

renameHsPatFields :: [HsPatField] -> SubTable -> ScopeSM ([HsPatField])
renameHsPatFields = mapRename renameHsPatField

-- although the hsNames here must be unique (field names),
-- I rename them for the sake of completeness
renameHsPatField :: HsPatField -> SubTable -> ScopeSM (HsPatField)
renameHsPatField (HsPFieldPat hsName hsPat) subTable = do
    gt <- gets globalSubTable      -- field names are not shadowed by local definitions.
    hsName' <- renameHsName hsName gt
    hsPat' <- renameHsPat hsPat subTable
    return (HsPFieldPat hsName' hsPat')


renameHsRhs :: HsRhs -> SubTable -> ScopeSM HsRhs
renameHsRhs (HsUnGuardedRhs hsExp) subTable
  = do
      hsExp' <- renameHsExp hsExp subTable
      return (HsUnGuardedRhs hsExp')
renameHsRhs (HsGuardedRhss hsGuardedRhss) subTable
  = do
      hsGuardedRhss' <- renameHsGuardedRhss hsGuardedRhss subTable
      return (HsGuardedRhss hsGuardedRhss')


renameHsGuardedRhss :: [HsGuardedRhs] -> SubTable -> ScopeSM ([HsGuardedRhs])
renameHsGuardedRhss = mapRename renameHsGuardedRhs

renameHsGuardedRhs :: HsGuardedRhs -> SubTable -> ScopeSM HsGuardedRhs
renameHsGuardedRhs (HsGuardedRhs srcLoc hsExp1 hsExp2) subTable = do
    setSrcLoc srcLoc
    hsExp1' <- renameHsExp hsExp1 subTable
    hsExp2' <- renameHsExp hsExp2 subTable
    return (HsGuardedRhs srcLoc hsExp1' hsExp2')


renameHsExps :: [HsExp] -> SubTable -> ScopeSM ([HsExp])
renameHsExps = mapRename renameHsExp


uqFuncNames :: V.FuncNames HsName
Identity uqFuncNames = T.mapM (return . nameName . toUnqualified) sFuncNames

func_fromRational = (HsVar $ V.func_fromRational uqFuncNames)

newVar = do
    unique <- newUniq
    mod <- getCurrentModule
    let hsName'' = (Qual mod (HsIdent $ show unique {- ++ fromHsName hsName' -} ++ "_var@"))
    return hsName''

wrapInAsPat e = do
    unique <- newUniq
    mod <- getCurrentModule
    let hsName'' = (Qual mod (HsIdent $ show unique {- ++ fromHsName hsName' -} ++ "_as@"))
    return (HsAsPat hsName''  e )


renameHsExp :: HsExp -> SubTable -> ScopeSM HsExp
renameHsExp (HsVar hsName) subTable = do
    hsName' <- renameHsName hsName subTable
    return (HsVar hsName')
--    wrapInAsPat (HsVar hsName')

renameHsExp (HsCon hsName) subTable = do
    hsName' <- renameHsName hsName subTable
    wrapInAsPat (HsCon hsName')

renameHsExp i@(HsLit (HsInt _num)) _st = do return i
    --let fi = if abs num > 500000000 then func_fromInteger else func_fromInt
    --z <- renameHsExp fi st
    --return $ HsParen (HsApp z i)
renameHsExp i@(HsLit (HsFrac _)) st = do
    z <- renameHsExp func_fromRational st
    return $ HsParen (HsApp z i)
renameHsExp (HsLambda srcLoc hsPats hsExp) subTable = do
    setSrcLoc srcLoc
    subTable' <- updateSubTableWithHsPats subTable hsPats srcLoc
    hsPats' <- renameAny hsPats subTable'
    hsExp' <- renameHsExp hsExp subTable'
    return (HsLambda srcLoc hsPats' hsExp')
renameHsExp (HsLet hsDecls hsExp) subTable = do
    subTable' <- updateSubTableWithHsDecls subTable hsDecls
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
renameHsExp (HsList hsExps) subTable = do
    unique <- newUniq
    hsExps' <- renameHsExps hsExps subTable
    mod <- getCurrentModule
    let hsName' = Qual mod (HsIdent $ show unique ++ "_as@")
    return (HsAsPat hsName' $ HsList hsExps')
renameHsExp (HsRecConstr hsName hsFieldUpdates) subTable = do
    hsName' <- renameHsName hsName subTable  -- do I need to change this name?
    hsFieldUpdates' <- renameHsFieldUpdates hsFieldUpdates subTable
    fls <- gets fieldLabels
    buildRecConstr fls (hsName':: HsName) (hsFieldUpdates'::[HsFieldUpdate]) -- HsRecConstr hsName' hsFieldUpdates')
renameHsExp (HsRecUpdate hsExp hsFieldUpdates) subTable = do
    hsExp' <- renameHsExp hsExp subTable
    hsFieldUpdates' <- renameHsFieldUpdates hsFieldUpdates subTable
    fls <- gets fieldLabels
    buildRecUpdate fls hsExp' hsFieldUpdates' -- HsRecConstr hsName' hsFieldUpdates')
    --return (HsRecUpdate hsExp' hsFieldUpdates')
renameHsExp (HsEnumFrom hsExp) subTable = do
    let x = desugarEnum "enumFrom" [hsExp]
    hsExp' <- renameHsExp x subTable
    return ( hsExp')
renameHsExp (HsEnumFromTo hsExp1 hsExp2) subTable = do
    let x = desugarEnum "enumFromTo" [hsExp1, hsExp2]
    hsExp' <- renameHsExp x subTable
    return ( hsExp')
renameHsExp (HsEnumFromThen hsExp1 hsExp2) subTable = do
    let x = desugarEnum "enumFromThen" [hsExp1, hsExp2]
    hsExp' <- renameHsExp x subTable
    return ( hsExp')
renameHsExp (HsEnumFromThenTo hsExp1 hsExp2 hsExp3) subTable = do
    let x = desugarEnum "enumFromThenTo" [hsExp1, hsExp2, hsExp3]
    hsExp' <- renameHsExp x subTable
    return ( hsExp')
renameHsExp (HsListComp hsExp hsStmts) subTable = do
    (hsStmts',subTable') <- renameHsStmts hsStmts subTable
    hsExp' <- renameHsExp hsExp subTable'
    return (HsListComp hsExp' hsStmts')
renameHsExp (HsExpTypeSig srcLoc hsExp hsQualType) subTable = do
    hsExp' <- renameHsExp hsExp subTable
    subTable' <- updateSubTableWithHsQualType subTable hsQualType
    hsQualType' <- renameHsQualType hsQualType subTable'
    return (HsExpTypeSig srcLoc hsExp' hsQualType')
renameHsExp (HsAsPat hsName hsExp) subTable = do
    hsName' <- renameHsName hsName subTable
    hsExp' <- renameHsExp hsExp subTable
    return (HsAsPat hsName' hsExp')
renameHsExp (HsWildCard sl) _ = do
    setSrcLoc sl
    e <- createError HsErrorUnderscore ("_")
    return e
renameHsExp p subTable = traverseHsExp (flip renameHsExp subTable) p

desugarEnum s as = foldl HsApp (HsVar (nameName $ toName Val s)) as


createError et s = do
    sl <- gets srcLoc
    return $ HsError { hsExpSrcLoc = sl, hsExpErrorType = et, hsExpString = (show sl ++ ": " ++ s) }

failRename s = do
    sl <- gets srcLoc
    fail (show sl ++ ": " ++ s)


buildRecConstr ::  FieldMap -> HsName -> [HsFieldUpdate] -> ScopeSM HsExp
buildRecConstr (amp,fls) n us = do
    undef <- createError HsErrorUninitializedField "Uninitialized Field"
    case Map.lookup (toName DataConstructor n) amp of
        Nothing -> failRename $ "Unknown Constructor: " ++ show n
        Just t -> do
            let f (HsFieldUpdate x e) = case  Map.lookup (toName FieldLabel x) fls of
                    Nothing -> failRename $ "Field Label does not exist: " ++ show x
                    Just cs -> case lookup n [ (nameName x,(y)) | (x,y) <- cs ] of
                        Nothing -> failRename $ "Field Label does not belong to constructor: " ++ show (x,n)
                        Just i -> return (i,hsParen e)
            fm <- mapM f us
            let rs = map g [0 .. t - 1 ]
                g i | Just e <- lookup i fm = e
                    | otherwise = undef
            con <- wrapInAsPat (HsCon n)
            return $ foldl HsApp con rs

buildRecUpdate ::  FieldMap -> HsExp -> [HsFieldUpdate] -> ScopeSM HsExp
buildRecUpdate (amp,fls) n us = do
        sl <- gets srcLoc
        let f (HsFieldUpdate x e) = case  Map.lookup (toName FieldLabel x) fls of
                Nothing -> failRename $ "Field Label does not exist: " ++ show x
                Just cs -> return [ (x,(y,hsParen e)) | (x,y) <- cs ]
        fm <- liftM concat $ mapM f us
        let fm' = sortGroupUnderFG fst snd fm
        let g (c,zs) = case Map.lookup c amp of
                Nothing -> failRename $ "Unknown Constructor: " ++ show n
                Just t -> do
                    vars <- replicateM t newVar
                    let vars' = (map HsVar vars)
                    let c' = nameName c
                    con <- wrapInAsPat (HsCon c')
                    let x = foldl HsApp con [ maybe v id (lookup i zs) | v <- vars' | i <- [ 0 .. t - 1] ]
                    return $ HsAlt sl (HsPApp c' (map HsPVar vars))  (HsUnGuardedRhs x) []
        as <- mapM g fm'
        pe <- createError HsErrorRecordUpdate "Record Update Error"
        return $ HsCase n (as ++ [HsAlt sl HsPWildCard (HsUnGuardedRhs pe) []])
--    undef <- createError "Uninitialized Field"
--    case Map.lookup (toName DataConstructor n) amp of
--        Nothing -> failRename $ "Unknown Constructor: " ++ show n
--        Just t -> do

--buildRecUpdate ::  FieldMap -> HsExp -> [HsFieldUpdate] -> ScopeSM HsExp
--buildRecUpdate _ _ _ = failRename "Can't handle field updates just yet."

renameHsAlts :: [HsAlt] -> SubTable -> ScopeSM [HsAlt]
renameHsAlts = mapRename renameHsAlt

-- note for renameHsAlt, the 'wheres' dominate the 'pats'

renameHsAlt :: HsAlt -> SubTable -> ScopeSM (HsAlt)
renameHsAlt (HsAlt srcLoc hsPat hsGuardedAlts {-where-} hsDecls) subTable = do
    setSrcLoc srcLoc
    subTable' <- updateSubTableWithHsPats subTable [hsPat] srcLoc
    hsPat' <- renameHsPat hsPat subTable'
    subTable'' <- updateSubTableWithHsDecls subTable' hsDecls
    hsDecls' <- renameHsDecls hsDecls subTable''
    hsGuardedAlts' <- renameHsRhs hsGuardedAlts subTable''
    return (HsAlt srcLoc hsPat' hsGuardedAlts' hsDecls')




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
renameHsFieldUpdate (HsFieldUpdate hsName hsExp) subTable = do
    gt <- gets globalSubTable     -- field names are global and not shadowed
    hsName' <- renameHsName hsName gt      -- TODO field names should have own namespace
    hsExp' <- renameHsExp hsExp subTable
    return (HsFieldUpdate hsName' hsExp')


renameHsNames :: [HsName] -> SubTable -> ScopeSM ([HsName])
renameHsNames = mapRename renameHsName

-- This looks up a replacement name in the subtable.
-- Regardless of whether the name is found, if it's not qualified
-- it will be qualified with the current module's prefix.
renameHsName :: HsName -> SubTable -> ScopeSM (HsName)
renameHsName hsName subTable
    | Qual (Module ('@':m)) (HsIdent i) <- hsName = return $ Qual (Module m) (HsIdent i)
renameHsName hsName subTable = case Map.lookup hsName subTable of
    Just name@(Qual _ _) -> return name
    Just _ -> error "renameHsName"
    Nothing
        | Just n <- V.fromTupname hsName -> return hsName
        | otherwise -> do
            sl <- gets srcLoc
            et <- gets errorTable
            let err = case Map.lookup hsName et of {
                Just s -> s;
                Nothing -> "Unknown name: " ++ show hsName }
            warn sl "undefined-name" err
            -- e <- createError ("Undefined Name: " ++ show hsName)
            return $ hsName
            --return (Qual modName name)



renameTypeHsName hsName subTable  =  gets typeSubTable  >>= \t -> case Map.lookup hsName t of
    Just _ -> renameHsName hsName t
    Nothing -> renameHsName hsName subTable

---------------------------------------
-- utility functions

-- clobberHsName(s) is called by the updateSubTableWith* functions to
-- deal with newly declared identifiers

-- clobberHsName(s) adds new mappings to the SubTable.
-- If a name already appeared, it's mapping is altered to the new one.

-- clobberHsNamesAndUpdateIdentTable also adds a mapping from this
-- renamed name to its source location and binding type

clobberHsNamesAndUpdateIdentTable :: [(HsName,SrcLoc)] -> SubTable -> ScopeSM (SubTable)
clobberHsNamesAndUpdateIdentTable ((hsName,srcLoc):hsNamesAndASrcLocs) subTable = do
      subTable'  <- clobberHsName hsName subTable
      subTable'' <- clobberHsNamesAndUpdateIdentTable hsNamesAndASrcLocs subTable'
      return (subTable'')
clobberHsNamesAndUpdateIdentTable [] subTable = return (subTable)

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
clobberHsNames (hsName:hsNames) subTable = do
      subTable'  <- clobberHsName  hsName  subTable
      subTable'' <- clobberHsNames hsNames subTable'
      return (subTable'')
clobberHsNames [] subTable = return subTable

clobberHsName :: HsName -> SubTable -> ScopeSM (SubTable)
clobberHsName hsName subTable = do
      unique     <- newUniq
      currModule <- getCurrentModule
      let hsName'     = renameAndQualify hsName unique currModule
          subTable'   = Map.insert hsName hsName' subTable
      return (subTable')



renameAndQualify :: HsName -> Int -> Module -> HsName
renameAndQualify name unique currentMod
    = case rename name unique of
           UnQual name' -> Qual currentMod name'
           qual_name    -> qual_name

-- renames a haskell name with its unique number
rename :: HsName -> Int -> HsName
rename n unique = hsNameIdent_u (hsIdentString_u ((show unique ++ "_") ++)) n

-- | unRename gets the original identifier name from the renamed version

unRename :: HsName -> HsName
unRename name
   = case isRenamed name of
          False -> name
          True  -> case name of
                      UnQual i   -> UnQual   $ unrenameIdent i
                      Qual mod i -> Qual mod $ unrenameIdent i

unrenameIdent :: HsIdentifier -> HsIdentifier
unrenameIdent = hsIdentString_u unRenameString

isRenamed :: HsName -> Bool
isRenamed (UnQual i)    = isIdentRenamed i
isRenamed (Qual _mod i) = isIdentRenamed i

-- an identifier is renamed if it starts with one or more digits
-- such an identifier would normally be illegal in Haskell
isIdentRenamed :: HsIdentifier -> Bool
isIdentRenamed i = not $ null $ takeWhile isDigit $ hsIdentString i




unRenameString :: String -> String
unRenameString s = (dropUnderscore . dropDigits) s where
   dropUnderscore ('_':rest) = rest
   dropUnderscore otherList = otherList
   dropDigits = dropWhile isDigit



--------------------------------------------------------
----This section of code updates the current SubTable to reflect the present scope


updateSubTableWithHsDecls :: SubTable -> [HsDecl] ->  ScopeSM (SubTable)
updateSubTableWithHsDecls subTable [] = return subTable
updateSubTableWithHsDecls subTable (hsDecl:hsDecls) = do
    let hsNamesAndASrcLocs = getHsNamesAndASrcLocsFromHsDecl hsDecl
    subTable'  <- clobberHsNamesAndUpdateIdentTable hsNamesAndASrcLocs subTable
    subTable'' <- updateSubTableWithHsDecls subTable' hsDecls
    return (subTable'')

updateSubTableWithHsPats :: SubTable -> [HsPat] -> SrcLoc -> ScopeSM (SubTable)
updateSubTableWithHsPats subTable (hsPat:hsPats) srcLoc = do
    let hsNamesAndASrcLocs = zip (getNamesFromHsPat hsPat) (repeat srcLoc)
    subTable'  <- clobberHsNamesAndUpdateIdentTable hsNamesAndASrcLocs subTable
    subTable'' <- updateSubTableWithHsPats subTable' hsPats srcLoc
    return subTable''
updateSubTableWithHsPats subTable [] _srcLoc  = do return (subTable)

-- Only one HsStmt should be added at a time because each new identifier is only valid
-- below the point at which it is defined

updateSubTableWithHsStmt :: SubTable -> HsStmt -> ScopeSM (SubTable)
updateSubTableWithHsStmt subTable hsStmt = do
    let hsNamesAndASrcLocs = getHsNamesAndASrcLocsFromHsStmt hsStmt
    subTable' <- clobberHsNamesAndUpdateIdentTable hsNamesAndASrcLocs subTable
    return (subTable')

----------------------------------------------------------
-- the following updateSubTableWith* functions do not need to alter the identTable aswell
--


-- takes a list of HsNames representing type variables in a data decl and
-- adds them to the current subTable

updateSubTableWithHsNames :: SubTable -> [HsName] -> ScopeSM (SubTable)
updateSubTableWithHsNames subTable hsNames = do
      subTable' <- clobberHsNames hsNames subTable
      return (subTable')

-- takes an HsQualType (a type signature) and adds the names of its variables
-- to the current subTable

updateSubTableWithHsQualType :: SubTable -> HsQualType -> ScopeSM (SubTable)
updateSubTableWithHsQualType subTable hsQualType = do
      let hsNames = nub $ getHsNamesFromHsQualType hsQualType
      subTable' <- clobberHsNames hsNames subTable
      return (subTable')

updateSubTableWithHsTypes :: SubTable -> [HsType] -> ScopeSM (SubTable)
updateSubTableWithHsTypes subTable hsType = do
      let hsNames = nub $ concatMap getHsNamesFromHsType hsType
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


-- | Collect all names defined in a module as well as their declaration points and
-- any subnames they might have.

collectDefsHsModule :: HsModule -> ([(Name,SrcLoc,[Name])],[(Name,Int)])
collectDefsHsModule m = execWriter (mapM_ f (hsModuleDecls m)) where
    --g (b,n,sl,ns) = (b,mod n, sl, map mod ns)
    mod = qualifyName (hsModuleName m)
    toName t n = Name.toName t (mod n)
    -- f :: HsDecl -> Writer [(Name,SrcLoc,[Name])] ()
    tellF xs = tell (xs,[]) >> return ()
    tellS xs = tell ([],xs) >> return ()
    f (HsForeignDecl a _ n _)  = tellF [(toName Val n,a,[])]
    f (HsForeignExport a e _ _)  = tellF [(ffiExportName e,a,[])]
    f (HsFunBind [])  = return ()
    f (HsFunBind (HsMatch a n _ _ _:_))  = tellF [(toName Val n,a,[])]
    f (HsPatBind srcLoc p _ _) = tellF [ (toName Val n,srcLoc,[]) | n <- (getNamesFromHsPat p) ]
    f (HsActionDecl srcLoc p _) = tellF [ (toName Val n,srcLoc,[]) | n <- (getNamesFromHsPat p) ]
    f (HsTypeDecl sl n _ _) = tellF [(toName TypeConstructor n,sl,[])]
    f HsDataDecl { hsDeclSrcLoc =sl, hsDeclName = n, hsDeclCons = cs } = do tellF $ (toName TypeConstructor n,sl,snub [ x |(x,_,_) <- cs']): cs' ; zup cs where
        cs' = concatMap (namesHsConDecl' toName) cs
    f (HsNewTypeDecl sl _ n _ c _) = do tellF $ (toName TypeConstructor n,sl,snub [ x |(x,_,_) <- cs']): cs' ; zup [c] where
        cs' = namesHsConDecl' toName c
    f cd@(HsClassDecl sl _ ds) = tellF $ (toName Name.ClassName (nameName z),sl,snub $ fsts cs):[ (n,a,[]) | (n,a) <- cs]  where
        z = case maybeGetDeclName cd of
            Just x | nameType x == ClassName -> x
            --       | otherwise ->  parseName ClassName (show x ++ show (nameType x))
        cs = fst (mconcatMap (namesHsDeclTS' toName) ds)
    f _ = return ()
    zup cs = tellS (map g cs) where
        g ca = (toName DataConstructor (hsConDeclName ca), length $ hsConDeclArgs ca)

namesHsConDecl' toName c = ans where
    dc = (toName DataConstructor $ hsConDeclName c,sl,fls')
    sl = hsConDeclSrcLoc c
    ans = dc : [ (toName Val n,sl,[]) |  n <- fls ]  ++  [ (n,sl,[]) |  n <- fls' ]
    fls' = map (toName FieldLabel) fls
    fls = case c of
        HsRecDecl { hsConDeclRecArg = ra } -> concatMap fst ra -- (map (rtup (hsConDeclSrcLoc c). toName FieldLabel) . fst) ra
        _ -> []

namesHsDeclTS' toName (HsTypeSig sl ns _) = ((map (rtup sl . toName Val) ns),[])
namesHsDeclTS' toName (HsTypeDecl sl n _ _) = ([(toName TypeConstructor n,sl)],[])
namesHsDeclTS' _ _ = ([],[])


namesHsDecl :: HsDecl -> ([(HsName, SrcLoc)],[(HsName, SrcLoc)])
namesHsDecl (HsForeignDecl a _ n _)  = ([(n,a)],[])
namesHsDecl (HsFunBind hsMatches)  = (getHsNamesAndASrcLocsFromHsMatches hsMatches, [])
namesHsDecl (HsPatBind srcLoc p _ _) = (map (rtup srcLoc) (getNamesFromHsPat p),[])
namesHsDecl (HsTypeDecl sl n _ _) = ([],[(n,sl)])
namesHsDecl HsDataDecl { hsDeclSrcLoc = sl, hsDeclName = n, hsDeclCons = cs } = ( (concatMap namesHsConDecl cs) ,[(n,sl)])
namesHsDecl (HsNewTypeDecl sl _ n _ c _) = ( (namesHsConDecl c),[(n,sl)])
namesHsDecl cd@(HsClassDecl sl _ ds) = (mconcatMap namesHsDeclTS ds) `mappend` ([],[(nameName z,sl)]) where
    z = case maybeGetDeclName cd of
        Just x | nameType x == ClassName -> x
        --       | otherwise ->  parseName ClassName (show x ++ show (nameType x))
namesHsDecl _ = mempty

namesHsDeclTS (HsTypeSig sl ns _) = ((map (rtup sl) ns),[])
namesHsDeclTS _ = ([],[])

namesHsConDecl c = (hsConDeclName c,hsConDeclSrcLoc c) : case c of
    -- HsRecDecl { hsConDeclRecArg = ra } -> concatMap (map (rtup (hsConDeclSrcLoc c)) . fst) ra
    _ -> []

-- _hsNames that are constructors can be ignored.


getHsNamesAndASrcLocsFromHsStmt :: HsStmt -> [(HsName, SrcLoc)]
getHsNamesAndASrcLocsFromHsStmt (HsGenerator srcLoc hsPat _hsExp)
  = zip (getNamesFromHsPat hsPat) (repeat srcLoc)
getHsNamesAndASrcLocsFromHsStmt (HsQualifier _hsExp)
  = []
getHsNamesAndASrcLocsFromHsStmt (HsLetStmt hsDecls)
  = concat $ map getHsNamesAndASrcLocsFromHsDecl hsDecls


-- the getNew... functions are used only inside class declarations to avoid _re_ renaming things
-- that should be left as is.


getHsNamesFromHsQualType :: HsQualType -> [HsName]
getHsNamesFromHsQualType (HsQualType _hsContext hsType) = getHsNamesFromHsType hsType

getHsNamesFromHsType :: HsType -> [HsName]
getHsNamesFromHsType t = execWriter (getNamesFromType t)
getNamesFromType (HsTyVar hsName) = tell [hsName]
getNamesFromType t = traverseHsType_ getNamesFromType t
-- XXX getHsNamesFromHsType (HsTyForall _bs t) = getHsNamesFromHsQualType t -- TODO, scoping?
-- XXX getHsNamesFromHsType (HsTyExists _bs t) = getHsNamesFromHsQualType t -- TODO, scoping?




--------------------------------------------------------------------------------


qualifyName :: Module -> HsName -> HsName
qualifyName _ name@(Qual {}) = name
qualifyName mod (UnQual name) = Qual mod name


