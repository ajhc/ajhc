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
import Data.FunctorM
import Data.FiniteMap
import Data.Monoid
import List
import qualified Data.Map as Map

import Doc.DocLike(tupled)
import FrontEnd.Desugar (doToExp)
import FrontEnd.SrcLoc hiding(srcLoc)
import FrontEnd.Utils
import GenUtil hiding(replicateM)
import HsErrors
import HsSyn
import Name.Name as Name hiding(qualifyName)
import Name.Names
import qualified Name.VConsts as V
import Util.ContextMonad
import Util.Gen
import Util.Inst()
import Warning


type FieldMap =  (Map.Map Name Int,Map.Map Name [(Name,Int)])

--------------------------------------------------------------------------------

--instance (Show a, Show b) => Show (FiniteMap a b) where
--    show fm = show (fmToList fm)


-- a 'Substitution Table' which is a map from old names to new names
-- All names in the current scope are stored in here, with their renamings

type SubTable = FiniteMap HsName HsName

-- an Identifier Table is a map from renamed names to that identifier's source
-- location and binding type


-- the monadic state

data ScopeState = ScopeState {
    currentModule  :: Module,
    unique         :: !Int,
    globalSubTable :: FiniteMap HsName HsName,  -- Current substition
    typeSubTable   :: FiniteMap HsName HsName,  -- type substition table
    errorTable     :: FiniteMap HsName String,  -- special error message. else it's just unknown.
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



getUnique :: ScopeSM Int
getUnique = gets unique

getCurrentModule :: ScopeSM Module
getCurrentModule = gets currentModule

getGlobalSubTable :: ScopeSM SubTable
getGlobalSubTable = gets globalSubTable

setSrcLoc e = modify (\s -> s { srcLoc =  e `mappend` srcLoc s })

-- functions to modify the ScopeSM

incUnique :: ScopeSM ()
incUnique = modify (\state -> state {unique = (unique state) + 1})


-----------------------------------------------------------
-- The renaming code:
--


addTopLevels ::  [HsDecl]  -> ScopeSM ()
addTopLevels  []  = return ()
addTopLevels  hsDecls = do
    mod <- getCurrentModule
    let (ns,ts) = mconcat (map namesHsDecl hsDecls)
        nm = listToFM $ foldl f [] (fsts ns)
        tm = listToFM $ foldl f [] (fsts ts)
        f r hsName@Qual {}
            | Just _ <- V.fromTupname hsName, Module "Prelude" <- mod
                = let nn = hsName in (nn,nn):r
            | otherwise = error $ "strong bad: " ++ show hsName
        f r z@(UnQual n) = let nn = Qual mod n in (z,nn):(nn,nn):r
        z ns = mapM mult (filter (\x -> length x > 1) $ groupBy (\a b -> fst a == fst b) (sort ns))
        mult xs@((n,sl):_) = warn sl "multiply-defined" (show n ++ " is defined multiple times: " ++ show xs )
    z ns >> z ts
    modify (\s -> s { globalSubTable = nm `plusFM` globalSubTable s })
    modify (\s -> s { typeSubTable = tm `plusFM` typeSubTable s })
    return ()

 {-
collectRenameHsSyns ::  SubTable -> [HsDecl] -> ScopeSM [HsDecl]
collectRenameHsSyns sub (d@(HsTypeDecl sl name args ty):ds) = liftM2 (:) (renameHsTypeDecl sub sl name args ty) (collectRenameHsSyns sub ds)
collectRenameHsSyns sub (_:ds) = (collectRenameHsSyns sub ds)
collectRenameHsSyns sub [] = []

renameHsTypeDecl sub sl name args ty = do
    setSrcLoc sl
    hsName' <- renameHsName name subTable
    subTable' <- updateSubTableWithHsNames subTable hsNames
    hsNames' <- renameHsNames hsNames subTable'
    t' <- renameHsType t subTable'
    return (HsTypeDecl srcLoc  hsName' hsNames' t')
-}


ambig x ys = "Ambiguous Name: " ++ show x ++ "\nCould refer to: " ++ tupled (map show ys)

-- | Main entry point.

{-# NOINLINE renameModule #-}
renameModule :: MonadWarn m => FieldMap -> [(Name,[Name])] -> HsModule -> m HsModule
renameModule fls ns m = mapM_ addWarning (errors finalState) >> return renamedMod where
    initialGlobalSubTable = listToFM [ (x,y) | ((typ,x),[y]) <- ns', typ == Val || typ == DataConstructor ]
    initialTypeSubTable = listToFM [ (x,y) | ((typ,x),[y]) <- ns', typ == TypeConstructor || typ == ClassName ]
    ns' = map fn ns
    fn (n,ns) = (fromName n, map nameName ns)

    errorTab =  listToFM [ (x,ambig x ys) | ((typ,x),ys@(_:_:_)) <- ns' ]

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
    initialGlobalSubTable = listToFM [ (x,y) | ((typ,x),[y]) <- ns', typ == Val || typ == DataConstructor ]
    initialTypeSubTable = listToFM [ (x,y) | ((typ,x),[y]) <- ns', typ == TypeConstructor || typ == ClassName ]
    ns' = map fn ns
    fn (n,ns) = (fromName n, map nameName ns)

    errorTab =  listToFM [ (x,ambig x ys) | ((typ,x),ys@(_:_:_)) <- ns' ]

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


{-
-- takes a list of qualified HsNames that the current module needs to know
-- about (i.e. ones imported from Prelude), and then in the renaming process
-- any of those names appearing in unqualified form will get qualified
-- e.g. we pass in [Qual (Module "Prelude") "take"] and then in code we see
-- foo = take 3 [1..10], so we translate this to (something like)
-- Main.foo = Prelude.take 3 [1..10]
renameTidyModule :: [HsDecl] -> [HsName] -> [HsName] -> HsModule -> (HsModule, [Warning])
renameTidyModule syns importedNames impTypeNames tidyMod
    = mapSnd errors z {- (renamedTidyMod, errors finalState) -} where
    initialGlobalSubTable :: SubTable
    initialGlobalSubTable = listToFM (map makeTranslation importedNames)
    initialTypeSubTable = listToFM (map makeTranslation impTypeNames)
    makeTranslation qname@(Qual _ str) = (UnQual str, qname)
    makeTranslation unqname = error $ "renameTidyModule passed an unqualified importedName " ++ show unqname

    startState = ScopeState {
        typeSubTable   = initialTypeSubTable,
        errorTable     = emptyFM,
        errors         = [],
        synonyms       = syns,
        srcLoc         = bogusASrcLoc,
        unique         = 1,   -- start the counting at 1
        globalSubTable = initialGlobalSubTable,
        currentModule  = hsModuleName tidyMod
        }

    z@(renamedTidyMod, finalState) = runScopeSM startState (renameDecls tidyMod initialGlobalSubTable)
-}

-- This is Bryn's modification to make the code a bit easier to understand for
-- functions like renameHsNames, renameHsFileUpdates
mapRename :: (a -> SubTable -> ScopeSM a) -> [a] -> SubTable -> ScopeSM [a]
mapRename renameIndividual individuals subTable
    = mapM (`renameIndividual` subTable) individuals



renameDecls :: HsModule -> SubTable -> ScopeSM HsModule
renameDecls tidy subTable = do
        addTopLevels $ hsModuleDecls tidy
        subTable'a <- gets globalSubTable
        let subTable' = subTable `plusFM` subTable'a
        --addError (show $ fmToList subTable')
        decls' <-  renameHsDecls (hsModuleDecls tidy) subTable' ; return decls'
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
renameHsDecls decls subtable = do
    ans <- mapRename renameHsDecl (expandTypeSigs decls) subtable
    mapM_ HsErrors.hsDecl ans
    return ans


expandTypeSigs :: [HsDecl] -> [HsDecl]
expandTypeSigs ds =  (concatMap f ds) where
    f (HsTypeSig sl ns qt) =  [ HsTypeSig sl [n] qt | n <- ns]
    f d = return d

renameHsDecl :: HsDecl -> SubTable -> ScopeSM (HsDecl)
renameHsDecl (HsPatBind srcLoc hsPat hsRhs {-where-} hsDecls) subTable = do
    setSrcLoc srcLoc
    hsPat'    <- renameHsPat hsPat subTable
    subTable' <- updateSubTableWithHsDecls subTable hsDecls LetFun
    hsDecls'  <- renameHsDecls hsDecls subTable'
    hsRhs'    <- renameHsRhs hsRhs subTable'
    let patbind' = (HsPatBind srcLoc hsPat' hsRhs' {-where-} hsDecls')
    return patbind'

renameHsDecl (HsForeignDecl a b c n t) subTable = do
    setSrcLoc a
    n <- renameHsName n subTable
    subTable' <- updateSubTableWithHsQualType subTable t
    --addDiag $ show (n, "foreigna",t)
    t <- renameHsQualType t subTable'
    --addDiag $ show (n, "foreignb",t)
    return  (HsForeignDecl a b c n t)

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
renameHsDecl (HsDataDecl srcLoc hsContext hsName hsNames1 hsConDecls hsNames2) subTable = do
    setSrcLoc srcLoc
    hsName' <- renameTypeHsName hsName subTable
    subTable' <- updateSubTableWithHsNames subTable hsNames1
    hsContext' <- renameHsContext hsContext subTable'
    hsNames1' <- renameHsNames hsNames1 subTable'
    hsConDecls' <- renameHsConDecls hsConDecls subTable'
    -- don't need to rename the hsNames2 as it is just a list of TypeClasses
    hsNames2' <- mapM (`renameTypeHsName` subTable') hsNames2
    return (HsDataDecl srcLoc hsContext' hsName' hsNames1' hsConDecls' hsNames2')
renameHsDecl (HsTypeDecl srcLoc name hsNames t) subTable = do
    setSrcLoc srcLoc
    hsName' <- renameTypeHsName name subTable
    subTable' <- updateSubTableWithHsNames subTable hsNames
    --subTable' <- updateSubTableWithHsNames subTable hsNames
    hsNames' <- renameHsNames hsNames subTable'
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
--renameHsDecl (HsNewTypeDecl srcLoc hsContext hsName hsNames1 hsConDecl hsNames2) subTable = do
--    setSrcLoc srcLoc
--    subTable' <- updateSubTableWithHsNames subTable hsNames1
--    hsContext' <- renameHsContext hsContext subTable'
--    -- don't need to rename the hsName (it is a constructor)
--    hsNames1' <- renameHsNames hsNames1 subTable'
--    hsConDecl' <- renameHsConDecl hsConDecl subTable'
--    -- don't need to rename the hsNames2 as it is just a list of TypeClasses
--    hsNames2' <- mapM (`renameTypeHsName` subTable') hsNames2
--    return (HsNewTypeDecl srcLoc hsContext' hsName hsNames1' hsConDecl' hsNames2')
-- here, we have to create a separate subTable (called the typeSigSubTable) to be passed down
-- because the part that renames the hsQualType in the type signatures needs a subTable with
-- _only_ the class's QualType in it.
-- Yes this is complicated and nasty. It is due mainly to the fact that some (but not all of
-- the type variables in the type sigs of the class's member functions must be renamed and
-- the new variables are used on the fly and not declared in an orderly manner.
renameHsDecl (HsClassDecl srcLoc hsQualType hsDecls) subTable = do
    setSrcLoc srcLoc
    startingSubTable <- return subTable
    {- WAS: typeSigSubTable <- updateSubTableWithHsQualType initialSubTable hsQualType -}
    typeSigSubTable <- updateSubTableWithHsQualType startingSubTable hsQualType
    hsQualType' <- renameHsQualType hsQualType typeSigSubTable
    doesClassMakeSense hsQualType'
    hsDecls' <- renameHsDecls hsDecls subTable
    return (HsClassDecl srcLoc hsQualType' hsDecls')
renameHsDecl (HsInstDecl srcLoc hsQualType hsDecls) subTable = do
    setSrcLoc srcLoc
    subTable' <- updateSubTableWithHsQualType subTable hsQualType
    hsQualType' <- renameHsQualType hsQualType subTable'
    hsDecls' <- renameHsDecls hsDecls subTable'
    return (HsInstDecl srcLoc hsQualType' hsDecls')
renameHsDecl (HsInfixDecl srcLoc assoc int hsNames) subTable = do
    setSrcLoc srcLoc
    hsNames' <- renameHsNames hsNames subTable
    return $ HsInfixDecl srcLoc assoc int hsNames'
renameHsDecl (HsPragmaProps srcLoc prop hsNames) subTable = do
    setSrcLoc srcLoc
    hsNames' <- renameHsNames hsNames subTable
    return (HsPragmaProps  srcLoc prop hsNames')
renameHsDecl prules@HsPragmaRules { hsDeclSrcLoc = srcLoc, hsDeclFreeVars = fvs, hsDeclLeftExpr = e1, hsDeclRightExpr = e2 } subTable = do
    setSrcLoc srcLoc
    subTable' <- updateSubTableWithHsNames subTable fvs
    fvs' <- renameHsNames fvs subTable'
    e1' <- renameHsExp e1 subTable'
    e2' <- renameHsExp e2 subTable'
    return prules {  hsDeclFreeVars = fvs', hsDeclLeftExpr = e1', hsDeclRightExpr = e2' }
renameHsDecl prules@HsPragmaSpecialize { hsDeclSrcLoc = srcLoc, hsDeclName = n, hsDeclType = t } subTable = do
    setSrcLoc srcLoc
    n <- renameAny n subTable
    t <- renameAny t subTable
    return prules {  hsDeclName = n, hsDeclType = t }

renameHsDecl otherHsDecl _ = return otherHsDecl


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

renameHsAsst :: HsAsst -> SubTable -> ScopeSM (HsAsst)
renameHsAsst (hsName1, hsName2) subTable = do
      hsName1' <- renameTypeHsName hsName1 subTable  -- for class names
      hsName2' <- renameTypeHsName hsName2 subTable
      return (hsName1', hsName2')

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
    pp t | not dovar = t
    pp t = t


class RenameAny a where
    renameAny :: a -> SubTable -> ScopeSM a
    renameAny x _ = return x

instance RenameAny SrcLoc where

instance RenameAny a => RenameAny [a] where
    renameAny xs t = mapM (`renameAny` t) xs

instance RenameAny HsTyVarBind where
    renameAny tvb@HsTyVarBind { hsTyVarBindName = n } t = do
        n' <- renameTypeHsName n t
        return tvb { hsTyVarBindName = n' }


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
    subTable' <- updateSubTableWithHsPats subTable hsPats srcLoc FunPat
    hsPats' <- renameAny hsPats subTable'
    subTable'' <- updateSubTableWithHsDecls subTable' hsDecls WhereFun
    hsDecls' <- renameHsDecls hsDecls subTable''
    hsRhs' <- renameHsRhs hsRhs subTable''
    return (HsMatch srcLoc hsName' hsPats' hsRhs' {-where-} hsDecls')



instance RenameAny HsPat where
    renameAny = renameHsPat

renameHsPat :: HsPat -> SubTable -> ScopeSM (HsPat)
renameHsPat (HsPVar hsName) subTable = do
      hsName' <- renameHsName hsName subTable
      return (HsPVar hsName')
renameHsPat (HsPLit hsLiteral) _subTable
  = return (HsPLit hsLiteral)
renameHsPat (HsPNeg hsPat) subTable = do
      hsPat' <- renameHsPat hsPat subTable
      return (HsPNeg hsPat')
renameHsPat (HsPInfixApp hsPat1 hsName hsPat2) subTable = do
      hsPat1' <- renameHsPat hsPat1 subTable
      hsPat2' <- renameHsPat hsPat2 subTable
      hsName' <- renameHsName hsName subTable
      return (HsPInfixApp hsPat1' hsName' hsPat2')
renameHsPat (HsPApp hsName hsPats) subTable = do
      hsPats' <- renameAny hsPats subTable
      hsName' <- renameHsName hsName subTable
      return (HsPApp hsName' hsPats')  -- NOTE: Bryn changed this so we also rename hsName and not just the hsPats
renameHsPat (HsPTuple hsPats) subTable = do
      hsPats' <- renameAny hsPats subTable
      return (HsPTuple hsPats')
renameHsPat (HsPList hsPats) subTable = do
      hsPats' <- renameAny hsPats subTable
      return (HsPList hsPats')
renameHsPat (HsPParen hsPat) subTable = do
      hsPat' <- renameHsPat hsPat subTable
      return (HsPParen hsPat')
renameHsPat (HsPRec hsName hsPatFields) subTable = do
      hsName' <- renameHsName hsName subTable
      hsPatFields' <- renameHsPatFields hsPatFields subTable
      fls <- gets fieldLabels
      buildRecPat fls hsName' hsPatFields'
  --    return (HsPRec hsName hsPatFields)
renameHsPat (HsPAsPat hsName hsPat) subTable = do
      hsName' <- renameHsName hsName subTable
      hsPat' <- renameHsPat hsPat subTable
      return (HsPAsPat hsName' hsPat')
renameHsPat HsPWildCard subTable = do
      unique <- getUnique
      incUnique
      mod <- getCurrentModule
      let hsName' = Qual mod (HsIdent $ show unique ++ "_wild@")
      return (HsPVar hsName')
--renameHsPat (HsPWildCard) _subTable
--  = return HsPWildCard
renameHsPat (HsPIrrPat hsPat) subTable = do
      hsPat' <- renameHsPat hsPat subTable
      return (HsPIrrPat hsPat')

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
{-
renameHsPatField (HsPFieldPun hsName) subTable
  = do
      hsName' <- renameHsName hsName subTable
      return (HsPFieldPun hsName')
-}
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
Identity uqFuncNames = fmapM (return . nameName . toUnqualified) sFuncNames

func_fromInt = (HsVar $ V.func_fromInt uqFuncNames)
func_fromInteger = (HsVar $ V.func_fromInteger uqFuncNames)
func_fromRational = (HsVar $ V.func_fromRational uqFuncNames)

newVar = do
    unique <- getUnique
    incUnique
    mod <- getCurrentModule
    let hsName'' = (Qual mod (HsIdent $ show unique {- ++ fromHsName hsName' -} ++ "_var@"))
    return hsName''

wrapInAsPat e = do
    unique <- getUnique
    incUnique
    mod <- getCurrentModule
    let hsName'' = (Qual mod (HsIdent $ show unique {- ++ fromHsName hsName' -} ++ "_as@"))
    return (HsAsPat hsName''  e )


renameHsExp :: HsExp -> SubTable -> ScopeSM HsExp
renameHsExp (HsVar hsName) subTable = do
    hsName' <- renameHsName hsName subTable
    wrapInAsPat (HsVar hsName')
--    unique <- getUnique
--    incUnique
--    mod <- getCurrentModule
--    let hsName'' = (Qual mod (HsIdent $ show unique ++ fromHsName hsName' ++ "_as@"))
--    return (HsAsPat hsName'' $   HsVar hsName' )
renameHsExp (HsCon hsName) subTable = do
    hsName' <- renameHsName hsName subTable
    wrapInAsPat (HsCon hsName')
--    unique <- getUnique
--    incUnique
--    mod <- getCurrentModule
--    let hsName'' = (Qual mod (HsIdent $ show unique ++ fromHsName hsName' ++ "_as@"))
--    return (HsAsPat hsName'' $ HsCon hsName')

renameHsExp i@(HsLit (HsInt num)) st = do
    let fi = if abs num > 500000000 then func_fromInteger else func_fromInt
    z <- renameHsExp fi st
    --ic <- renameHsExp (HsCon (UnQual (HsIdent "Integer"))) st
    return $ HsParen (HsApp z i)
renameHsExp i@(HsLit (HsFrac _)) st = do
    z <- renameHsExp func_fromRational st
    --ic <- renameHsExp (HsCon (UnQual (HsIdent "Integer"))) st
    return $ HsParen (HsApp z i)
renameHsExp (HsLit hsLiteral) _subTable = do
    return (HsLit hsLiteral)
renameHsExp (HsInfixApp hsExp1 hsExp2 hsExp3) subTable = do
    hsExp1' <- renameHsExp hsExp1 subTable
    hsExp2' <- renameHsExp hsExp2 subTable
    hsExp3' <- renameHsExp hsExp3 subTable
    return (HsInfixApp hsExp1' hsExp2' hsExp3')
renameHsExp (HsApp hsExp1 hsExp2) subTable = do
    hsExp1' <- renameHsExp hsExp1 subTable
    hsExp2' <- renameHsExp hsExp2 subTable
    return (HsApp hsExp1' hsExp2')
renameHsExp (HsNegApp hsExp) subTable = do
    hsExp' <- renameHsExp hsExp subTable
    return (HsNegApp hsExp')
renameHsExp (HsLambda srcLoc hsPats hsExp) subTable = do
    setSrcLoc srcLoc
    subTable' <- updateSubTableWithHsPats subTable hsPats srcLoc LamPat
    hsPats' <- renameAny hsPats subTable'
    hsExp' <- renameHsExp hsExp subTable'
    return (HsLambda srcLoc hsPats' hsExp')
renameHsExp (HsLet hsDecls hsExp) subTable = do
    subTable' <- updateSubTableWithHsDecls subTable hsDecls LetFun
    hsDecls' <- renameHsDecls hsDecls subTable'
    hsExp' <- renameHsExp hsExp subTable'
    return (HsLet hsDecls' hsExp')
renameHsExp (HsIf hsExp1 hsExp2 hsExp3) subTable = do
    hsExp1' <- renameHsExp hsExp1 subTable
    hsExp2' <- renameHsExp hsExp2 subTable
    hsExp3' <- renameHsExp hsExp3 subTable
    return (HsIf hsExp1' hsExp2' hsExp3')
renameHsExp (HsCase hsExp hsAlts) subTable = do
    hsExp' <- renameHsExp hsExp subTable
    hsAlts' <- renameHsAlts hsAlts subTable
    return (HsCase hsExp' hsAlts')
renameHsExp (HsDo hsStmts) subTable = do
    let e = doToExp hsStmts
    renameHsExp e subTable
    --(hsStmts',_) <- renameHsStmts hsStmts subTable
    --return (doToExp hsStmts')
renameHsExp (HsTuple hsExps) subTable = do
    hsExps' <- renameHsExps hsExps subTable
    return (HsTuple hsExps')
renameHsExp (HsList hsExps) subTable = do
    unique <- getUnique
    incUnique
    hsExps' <- renameHsExps hsExps subTable
    mod <- getCurrentModule
    let hsName' = Qual mod (HsIdent $ show unique ++ "_as@")
    return (HsAsPat hsName' $ HsList hsExps')
renameHsExp (HsParen hsExp) subTable = do
    hsExp' <- renameHsExp hsExp subTable
    return (hsParen hsExp')
renameHsExp (HsLeftSection hsExp1 hsExp2) subTable = do
    hsExp1' <- renameHsExp hsExp1 subTable
    hsExp2' <- renameHsExp hsExp2 subTable
    return (HsLeftSection hsExp1' hsExp2')
renameHsExp (HsRightSection hsExp1 hsExp2) subTable = do
    hsExp1' <- renameHsExp hsExp1 subTable
    hsExp2' <- renameHsExp hsExp2 subTable
    return (HsRightSection hsExp1' hsExp2')
-- XXX I'm not 100% sure that this bit works.
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
    --return (HsEnumFrom hsExp')
    return ( hsExp')
renameHsExp (HsEnumFromTo hsExp1 hsExp2) subTable = do
    let x = desugarEnum "enumFromTo" [hsExp1, hsExp2]
    hsExp' <- renameHsExp x subTable
    return ( hsExp')
    --hsExp' <- renameHsExp x subTable
    --hsExp1' <- renameHsExp hsExp1 subTable
    --hsExp2' <- renameHsExp hsExp2 subTable
    --return (HsEnumFromTo hsExp1' hsExp2')
renameHsExp (HsEnumFromThen hsExp1 hsExp2) subTable = do
    let x = desugarEnum "enumFromThen" [hsExp1, hsExp2]
    hsExp' <- renameHsExp x subTable
    return ( hsExp')
    --hsExp1' <- renameHsExp hsExp1 subTable
    --hsExp2' <- renameHsExp hsExp2 subTable
    --return (HsEnumFromThen hsExp1' hsExp2')
renameHsExp (HsEnumFromThenTo hsExp1 hsExp2 hsExp3) subTable = do
    let x = desugarEnum "enumFromThenTo" [hsExp1, hsExp2, hsExp3]
    hsExp' <- renameHsExp x subTable
    return ( hsExp')
    --hsExp1' <- renameHsExp hsExp1 subTable
    --hsExp2' <- renameHsExp hsExp2 subTable
    --hsExp3' <- renameHsExp hsExp3 subTable
    --return (HsEnumFromThenTo hsExp1' hsExp2' hsExp3')
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
    e <- createError ("_")
    return e
renameHsExp (HsIrrPat hsExp) subTable = do
    hsExp' <- renameHsExp hsExp subTable
    return (HsIrrPat hsExp')

desugarEnum s as = foldl HsApp (HsVar (nameName $ toName Val s)) as


createError s = do
    sl <- gets srcLoc
    pe <- wrapInAsPat (HsVar (nameName v_error))
    return $ HsParen $ HsApp pe (HsLit (HsString (show sl ++ ": " ++ s)))

failRename s = do
    sl <- gets srcLoc
    fail (show sl ++ ": " ++ s)


buildRecConstr ::  FieldMap -> HsName -> [HsFieldUpdate] -> ScopeSM HsExp
buildRecConstr (amp,fls) n us = do
    undef <- createError "Uninitialized Field"
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
                    vars' <- mapM wrapInAsPat (map HsVar vars)
                    let c' = nameName c
                    con <- wrapInAsPat (HsCon c')
                    let x = foldl HsApp con [ maybe v id (lookup i zs) | v <- vars' | i <- [ 0 .. t - 1] ]
                    return $ HsAlt sl (HsPApp c' (map HsPVar vars))  (HsUnGuardedRhs x) []
        as <- mapM g fm'
        pe <- createError "Record Update Error"
        v <- newVar
        return $ HsCase n (as ++ [HsAlt sl (HsPVar v) (HsUnGuardedRhs pe) []])
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
    subTable' <- updateSubTableWithHsPats subTable [hsPat] srcLoc CasePat
    hsPat' <- renameHsPat hsPat subTable'
    subTable'' <- updateSubTableWithHsDecls subTable' hsDecls WhereFun
    hsDecls' <- renameHsDecls hsDecls subTable''
    hsGuardedAlts' <- renameHsRhs hsGuardedAlts subTable''
    return (HsAlt srcLoc hsPat' hsGuardedAlts' hsDecls')


renameHsGuardedRhsList :: [HsGuardedRhs] -> SubTable -> ScopeSM [HsGuardedRhs]
renameHsGuardedRhsList = mapRename renameHsGuardedRhs


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
renameHsName hsName subTable = case lookupFM subTable  hsName of
    Just name@(Qual _ _) -> return name
    Just _ -> error "renameHsName"
    Nothing
        | Just n <- V.fromTupname hsName -> return hsName
        | otherwise -> do
            sl <- gets srcLoc
            et <- gets errorTable
            let err = case lookupFM et hsName of {
                Just s -> s;
                Nothing -> "Unknown name: " ++ show hsName }
            warn sl "undefined-name" err
            -- e <- createError ("Undefined Name: " ++ show hsName)
            return $ hsName
            --return (Qual modName name)


--renameTypeHsName hsName subTable  = case hsIdentString (hsNameIdent hsName) of
--    xs@(x:_) | isUpper x -> do
--        t <- gets typeSubTable
--        renameHsName hsName t
--    _ -> renameHsName hsName subTable


renameTypeHsName hsName subTable  =  gets typeSubTable  >>= \t -> case lookupFM t hsName of
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
clobberHsName hsName subTable
  = do
      unique     <- getUnique
      currModule <- getCurrentModule
      let hsName'     = renameAndQualify hsName unique currModule
          subTable'   = addToFM subTable hsName hsName'
      incUnique
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



-- takes a list of decls and examines only the class decls
-- to get the names of variables used in their type sigs

updateSubTableWithClasses :: SubTable -> [HsDecl] -> ScopeSM (SubTable)
updateSubTableWithClasses subTable []
  = return subTable
updateSubTableWithClasses subTable (hsDecl:hsDecls)
  = do
      let hsNames = getHsNamesFromClass hsDecl
      subTable'  <- clobberHsNames hsNames subTable
      subTable'' <- updateSubTableWithClasses subTable' hsDecls
      return (subTable'')

getHsNamesAndASrcLocsFromHsDecl :: HsDecl -> [(HsName, SrcLoc)]
getHsNamesAndASrcLocsFromHsDecl (HsPatBind srcLoc (HsPVar hsName) _ _) = [(hsName, srcLoc)]
-- This will cause errors on code with PatBinds of the form (x,y) = blah...
-- and should be changed for a more general renamer (but is fine for thih)
getHsNamesAndASrcLocsFromHsDecl (HsPatBind sloc _ _ _)
  = error $ "non simple pattern binding found (sloc): " ++ show sloc
-- getHsNamesAndASrcLocsFromHsDecl (HsFunBind _ hsMatches)
getHsNamesAndASrcLocsFromHsDecl (HsFunBind hsMatches) = getHsNamesAndASrcLocsFromHsMatches hsMatches
getHsNamesAndASrcLocsFromHsDecl (HsForeignDecl a _ _ n _) = [(n,a)]
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
    f (HsForeignDecl a _ _ n _)  = tellF [(toName Val n,a,[])]
    f (HsFunBind [])  = return ()
    f (HsFunBind (HsMatch a n _ _ _:_))  = tellF [(toName Val n,a,[])]
    f (HsPatBind srcLoc p _ _) = tellF [ (toName Val n,srcLoc,[]) | n <- (getHsNamesFromHsPat p) ]
    f (HsTypeDecl sl n _ _) = tellF [(toName TypeConstructor n,sl,[])]
    f (HsDataDecl sl _ n _ cs _) = do tellF $ (toName TypeConstructor n,sl,snub [ x |(x,_,_) <- cs']): cs' ; zup cs where
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
namesHsDeclTS' _ _ = ([],[])

{-
collectDefsHsModule :: HsModule -> [(Bool,HsName,SrcLoc,[HsName])]
collectDefsHsModule m = map g $ snd $ runWriter (mapM_ f (hsModuleDecls m)) where
    g (b,n,sl,ns) = (b,mod n, sl, map mod ns)
    mod = qualifyName (hsModuleName m)
    f (HsForeignDecl a _ _ n _)  = tell [(False,n,a,[])]
    f (HsFunBind [])  = return ()
    f (HsFunBind (HsMatch a n _ _ _:_))  = tell [(False,n,a,[])]
    f (HsPatBind srcLoc p _ _) = tell [ (False,n,srcLoc,[]) | n <- (getHsNamesFromHsPat p) ]
    f (HsTypeDecl sl n _ _) = tell [(True,n,sl,[])]
    f (HsDataDecl sl _ n _ cs _) = tell $ (True,n,sl,fsts cs'):[ (False,n,sl,[]) | (n,sl) <- cs'] where
        cs' = concatMap namesHsConDecl cs
    f (HsNewTypeDecl sl _ n _ c _) =  tell $ (True,n,sl,fsts cs'):[ (False,n,sl,[]) | (n,sl) <- cs'] where
        cs' = namesHsConDecl c
    f cd@(HsClassDecl sl _ ds) = tell $ (True,z,sl,fsts cs):[ (False,n,a,[]) | (n,a) <- cs]  where
        Just z = maybeGetDeclName cd
        cs = fst (mconcatMap namesHsDeclTS ds)
    f _ = return ()

-- | Collect all names which are defined in a given module.
namesHsModule ::
    HsModule   -- ^ Module to collect names from.
    -> ([(HsName, SrcLoc)],[(HsName, SrcLoc)])  -- ^ (value-like names,type-like names)
namesHsModule m = mconcatMap namesHsDecl (hsModuleDecls m)
-}

namesHsDecl :: HsDecl -> ([(HsName, SrcLoc)],[(HsName, SrcLoc)])
namesHsDecl (HsForeignDecl a _ _ n _)  = ([(n,a)],[])
namesHsDecl (HsFunBind hsMatches)  = (getHsNamesAndASrcLocsFromHsMatches hsMatches, [])
namesHsDecl (HsPatBind srcLoc p _ _) = (map (rtup srcLoc) (getHsNamesFromHsPat p),[])
namesHsDecl (HsTypeDecl sl n _ _) = ([],[(n,sl)])
namesHsDecl (HsDataDecl sl _ n _ cs _) = ( (concatMap namesHsConDecl cs) ,[(n,sl)])
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

getHsNamesFromHsPat :: HsPat -> [HsName]
getHsNamesFromHsPat (HsPVar hsName) = [hsName]
getHsNamesFromHsPat (HsPLit _hsName) = []
getHsNamesFromHsPat (HsPNeg hsPat) = getHsNamesFromHsPat hsPat
-- _hsName can be ignored as it is a Constructor (e.g. in (x:xs) we only want to know what's in scope; that is x and xs)
getHsNamesFromHsPat (HsPInfixApp hsPat1 _hsName hsPat2) = getHsNamesFromHsPat hsPat1 ++ getHsNamesFromHsPat hsPat2
getHsNamesFromHsPat (HsPApp _hsName hsPats) = concat (map getHsNamesFromHsPat hsPats)
getHsNamesFromHsPat (HsPTuple hsPats) = concat (map getHsNamesFromHsPat hsPats)
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
getHsNamesFromHsPatField (HsPFieldPat _hsName hsPat)
  = getHsNamesFromHsPat hsPat

getHsNamesAndASrcLocsFromHsStmt :: HsStmt -> [(HsName, SrcLoc)]
getHsNamesAndASrcLocsFromHsStmt (HsGenerator srcLoc hsPat _hsExp)
  = zip (getHsNamesFromHsPat hsPat) (repeat srcLoc)
getHsNamesAndASrcLocsFromHsStmt (HsQualifier _hsExp)
  = []
getHsNamesAndASrcLocsFromHsStmt (HsLetStmt hsDecls)
  = concat $ map getHsNamesAndASrcLocsFromHsDecl hsDecls


-- the getNew... functions are used only inside class declarations to avoid _re_ renaming things
-- that should be left as is.

getNewHsNamesFromHsQualType :: SubTable -> HsQualType -> [HsName]
getNewHsNamesFromHsQualType subTable (HsQualType _hsContext hsType)
  = getNewHsNamesFromHsType subTable hsType

getNewHsNamesFromHsType :: SubTable -> HsType -> [HsName]
getNewHsNamesFromHsType subTable (HsTyFun hsType1 hsType2)
  = (getNewHsNamesFromHsType subTable hsType1) ++ (getNewHsNamesFromHsType subTable hsType2)
getNewHsNamesFromHsType subTable (HsTyTuple hsTypes)
  = concat $ map (getNewHsNamesFromHsType subTable) hsTypes
getNewHsNamesFromHsType subTable (HsTyApp hsType1 hsType2)
  = (getNewHsNamesFromHsType subTable hsType1) ++ (getNewHsNamesFromHsType subTable hsType2)
getNewHsNamesFromHsType subTable (HsTyVar hsName)
  | lookupFM subTable hsName == Nothing = [hsName]
  | otherwise                           = []
getNewHsNamesFromHsType _subTable (HsTyCon _hsName)
  = [] -- don't rename the Constructors

getHsNamesFromHsQualType :: HsQualType -> [HsName]
getHsNamesFromHsQualType (HsQualType _hsContext hsType) = getHsNamesFromHsType hsType

getHsNamesFromHsType :: HsType -> [HsName]
getHsNamesFromHsType (HsTyFun hsType1 hsType2) = (getHsNamesFromHsType hsType1) ++ (getHsNamesFromHsType hsType2)
getHsNamesFromHsType (HsTyTuple hsTypes) = concat $ map getHsNamesFromHsType hsTypes
getHsNamesFromHsType (HsTyApp hsType1 hsType2) = (getHsNamesFromHsType hsType1) ++ (getHsNamesFromHsType hsType2)
getHsNamesFromHsType (HsTyVar hsName) = [hsName]
getHsNamesFromHsType (HsTyCon _hsName) = [] -- don't rename the Constructors
getHsNamesFromHsType (HsTyForall _bs t) = getHsNamesFromHsQualType t -- TODO, scoping?
getHsNamesFromHsType (HsTyExists _bs t) = getHsNamesFromHsQualType t -- TODO, scoping?


-- gets the names of the functions declared in a class declaration

getHsNamesFromClass :: HsDecl -> [HsName]
getHsNamesFromClass (HsClassDecl _srcLoc _hsQualType hsDecls)
  = getHsNamesFromTypeSigs hsDecls
getHsNamesFromClass _otherDecl
  = []

-- gets the names of the functions whose types are declared in class decls

getHsNamesFromTypeSigs :: [HsDecl] -> [HsName]
getHsNamesFromTypeSigs ((HsTypeSig _srcLoc hsNames _hsQualType):hsDecls)
  = hsNames ++ getHsNamesFromTypeSigs hsDecls
getHsNamesFromTypeSigs (_otherDecl:hsDecls)
  = getHsNamesFromTypeSigs hsDecls
getHsNamesFromTypeSigs []
  = []

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
    replaceName f object
      = let a # b = a $ (replaceName f b)
        in case object of
            HsAssocNone  ->
                HsAssocNone
            HsAssocLeft  ->
                HsAssocLeft
            HsAssocRight  ->
                HsAssocRight


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
--    replaceName f object
--      = let a # b = a $ (replaceName f b)
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
            HsWildCard sl 			 ->
                HsWildCard sl
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

instance Renameable HsTyVarBind where
    replaceName f = hsTyVarBindName_u (replaceName f)

instance Renameable (HsAlt) where
    replaceName f object
      = let a # b = a $ (replaceName f b)
        in case object of
            HsAlt  srcloc pat guardedalts objects ->
                HsAlt  # srcloc # pat # guardedalts # objects



instance Renameable HsName where
    replaceName f name = f name

instance (Renameable a, Renameable b) => Renameable (a,b) where
    replaceName f (x,y) = (replaceName f x, replaceName f y)
instance Renameable a => Renameable [a] where
    replaceName f xs = map (replaceName f) xs


-- Ident table stuff
type IdentTable = FiniteMap HsName (SrcLoc, Binding)
addToIdentTable _ _ = return ()

data Binding
   = TopFun             -- function binding at the top level
   | ClassMethod        -- name of a method in a class
   | Instance           -- an instance decl lifted to a top-level binding
   | WhereFun           -- function binding in a where clause
   | LetFun             -- function binding in a let expression (used to include topbinds too)
   | LamPat             -- pattern binding in a lambda expression
   | CasePat            -- pattern binding in a case expression
   | GenPat             -- pattern binding in a generator statement
   | FunPat             -- pattern binding in a function declaration
   | Constr             -- name is a data constructor


qualifyName :: Module -> HsName -> HsName
qualifyName _ name@(Qual {}) = name
qualifyName mod (UnQual name) = Qual mod name
