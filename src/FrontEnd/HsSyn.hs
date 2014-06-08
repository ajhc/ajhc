module FrontEnd.HsSyn(module FrontEnd.HsSyn, module FrontEnd.SrcLoc) where

import Data.Binary
import Data.Generics

import C.FFI
import Data.Foldable(Foldable)
import Data.Traversable(Traversable)
import FrontEnd.SrcLoc
import Name.Internals(Module(..))
import Name.Names
import Options
import StringTable.Atom
import StringTable.Atom()

type LHsType = Located HsType
type LHsExp = Located HsExp
type LHsPat = Located HsPat

type HsBangType = HsBangType' HsType
type HsContext = [HsAsst]

type HsName = Name
type HsFieldUpdate = HsField HsExp
type HsPatField = HsField HsPat

data HsModule = HsModule {
    hsModuleName    :: Module,
    hsModuleSrcLoc  :: SrcLoc,
    hsModuleExports :: (Maybe [HsExportSpec]),
    hsModuleImports :: [HsImportDecl],
    hsModuleDecls   :: [HsDecl],
    hsModuleOptions :: [String],
    hsModuleOpt     :: Opt
    }
  {-! derive: update !-}

-- Export/Import Specifications

data HsExportSpec
    = HsEVar Name                      -- variable
    | HsEAbs Name                      -- T
    | HsEThingAll Name                 -- T(..)
    | HsEThingWith Name [Name]       -- T(C_1,...,C_n)
    | HsEModuleContents Module           -- module M   (not for imports)
    | HsEQualified NameType HsExportSpec -- class Foo, type Bar, kind ANY
  deriving(Eq,Show,Ord)

data HsImportDecl = HsImportDecl {
    hsImportDeclSrcLoc    :: SrcLoc,
    hsImportDeclModule    :: Module,
    hsImportDeclQualified :: !Bool,
    hsImportDeclAs        :: (Maybe Module),
    hsImportDeclSpec      :: (Maybe (Bool,[HsExportSpec]))
    }
  deriving(Eq,Show,Ord)

data HsAssoc = HsAssocNone | HsAssocLeft | HsAssocRight | HsAssocPrefix | HsAssocPrefixy
  deriving(Eq,Show,Ord)
  {-! derive: Binary !-}

data DeclType = DeclTypeData | DeclTypeNewtype | DeclTypeKind
    deriving(Eq,Show,Ord)

data HsDecl
    = HsTypeFamilyDecl {
        hsDeclSrcLoc  :: SrcLoc,
        hsDeclFamily  :: !Bool,    -- whether 'family' is explicity mentioned.
        hsDeclData    :: !Bool,
        hsDeclName    :: !Name,
        hsDeclTArgs   :: [HsType],
        hsDeclHasKind :: Maybe HsKind
        }
    | HsTypeDecl	 {
        hsDeclSrcLoc :: SrcLoc,
        hsDeclName   :: !Name,
        hsDeclTArgs  :: [HsType],
        hsDeclType   :: HsType
        }
    | HsDataDecl	 {
        hsDeclDeclType :: !DeclType,
        hsDeclSrcLoc   :: SrcLoc,
        hsDeclContext  :: HsContext,
        hsDeclName     :: Name,
        hsDeclArgs     :: [Name],
        hsDeclCons     :: [HsConDecl],
        hsDeclHasKind  :: Maybe HsKind,
        hsDeclCTYPE    :: Maybe String,
        {- deriving -} hsDeclDerives :: [Name]
        }
    | HsInfixDecl   {
        hsDeclSrcLoc :: SrcLoc,
        hsDeclAssoc  :: !HsAssoc,
        hsDeclInt    :: !Int,
        hsDeclNames  :: [Name]
        }
    | HsClassDecl   {
        hsDeclSrcLoc    :: SrcLoc,
        hsDeclClassHead :: HsClassHead,
        hsDeclDecls     :: [HsDecl]
        }
    | HsClassAliasDecl {
        hsDeclSrcLoc   :: SrcLoc,
        hsDeclName     :: !Name,
        hsDeclTypeArgs :: [HsType],
        {- rhs -} hsDeclContext :: HsContext,
                  hsDeclClasses :: HsContext,
        hsDeclDecls :: [HsDecl]
        }
    | HsInstDecl    {
        hsDeclSrcLoc    :: SrcLoc,
        hsDeclClassHead :: HsClassHead,
        hsDeclIsDerived :: !Bool,
        hsDeclDecls     :: [HsDecl]
        }
    | HsDefaultDecl {
        hsDeclSrcLoc :: SrcLoc,
        hsDeclType   :: HsType
        }
    | HsTypeSig	{
        hsDeclSrcLoc   :: SrcLoc,
        hsDeclNames    :: [Name],
        hsDeclQualType :: HsQualType }
    | HsFunBind  [HsMatch]
    | HsPatBind	 {
        hsDeclSrcLoc :: SrcLoc,
        hsDeclPat    :: HsPat,
        hsDeclRhs    :: HsRhs,
        hsDeclDecls  :: [HsDecl]
        }
    | HsActionDecl {
        hsDeclSrcLoc   :: SrcLoc,
        hsDeclPat      :: HsPat,
        hsDeclExp      :: HsExp
        }
    | HsSpaceDecl {
        hsDeclSrcLoc   :: SrcLoc,
        hsDeclName     :: Name,
        hsDeclExp      :: HsExp,
        hsDeclCName    :: Maybe String,
        hsDeclCount    :: Int,
        hsDeclQualType :: HsQualType
        }
    | HsForeignDecl {
        hsDeclSrcLoc   :: SrcLoc,
        hsDeclForeign  :: FfiSpec,
        hsDeclName     :: Name,
        hsDeclQualType :: HsQualType
        }
    | HsForeignExport {
        hsDeclSrcLoc    :: SrcLoc,
        hsDeclFFIExport :: FfiExport,
        hsDeclName      :: Name,
        hsDeclQualType  :: HsQualType
        }
    | HsPragmaProps SrcLoc String [Name]
    | HsPragmaRules [HsRule]
    | HsPragmaSpecialize {
        hsDeclUniq   :: (Module,Int),
        hsDeclSrcLoc :: SrcLoc,
        hsDeclBool   :: Bool,
        hsDeclName   :: Name,
        hsDeclType   :: HsType
        }
    | HsDeclDeriving {
        hsDeclSrcLoc    :: SrcLoc,
        hsDeclClassHead :: HsClassHead
        }
  deriving(Eq,Show,Ord)
  {-! derive: is !-}

data HsRule = HsRule {
    hsRuleUniq      :: (Module,Int),
    hsRuleSrcLoc    :: SrcLoc,
    hsRuleIsMeta    :: Bool,
    hsRuleString    :: String,
    hsRuleFreeVars  :: [(Name,Maybe HsType)],
    hsRuleLeftExpr  :: HsExp,
    hsRuleRightExpr :: HsExp
    }
  deriving(Eq,Show,Ord)

data HsPragmaExp = HsPragmaExp String [HsExp]

data HsMatch = HsMatch {
    hsMatchSrcLoc :: SrcLoc,
    hsMatchName   :: Name,
    hsMatchPats   :: [HsPat],
    hsMatchRhs    :: HsRhs,
    {-where-} hsMatchDecls :: [HsDecl]
    }
  deriving(Eq,Show,Ord)

data HsConDecl
    = HsConDecl {
        hsConDeclSrcLoc :: SrcLoc,
        hsConDeclExists :: [HsTyVarBind],
        hsConDeclName   :: Name,
        hsConDeclConArg :: [HsBangType]
        }
    | HsRecDecl {
        hsConDeclSrcLoc :: SrcLoc,
        hsConDeclExists :: [HsTyVarBind],
        hsConDeclName   :: Name,
        hsConDeclRecArg :: [([Name],HsBangType)]
        }
  deriving(Eq,Show,Ord)
  {-! derive: is, update !-}

hsConDeclArgs HsConDecl { hsConDeclConArg = as } = as
hsConDeclArgs HsRecDecl { hsConDeclRecArg = as } = concat [ replicate (length ns) t | (ns,t) <- as]

data HsBangType' a
	 = HsBangedTy   { hsBangType :: a }
	 | HsUnBangedTy { hsBangType :: a }
  deriving(Eq,Show,Functor,Ord,Traversable,Foldable)

data HsRhs = HsUnGuardedRhs HsExp | HsGuardedRhss [HsComp]
  deriving(Eq,Show,Ord)

data HsQualType = HsQualType {
    hsQualTypeContext :: HsContext,
    hsQualTypeType :: HsType
    } deriving(Data,Typeable,Eq,Ord,Show)
  {-! derive: Binary !-}

data HsType
    = HsTyFun HsType HsType
    | HsTyTuple [HsType]
    | HsTyUnboxedTuple [HsType]
    | HsTyApp HsType HsType
    | HsTyVar { hsTypeName :: Name }
    | HsTyCon { hsTypeName :: Name }
    | HsTyForall {
       hsTypeVars :: [HsTyVarBind],
       hsTypeType :: HsQualType }
    | HsTyExists {
       hsTypeVars :: [HsTyVarBind],
       hsTypeType :: HsQualType }
    | HsTyExpKind {
        hsTyLType :: LHsType,
        hsTyKind  :: HsKind }
    | HsTyStrictType {
        hsTyStrict :: !Bool,
        hsTyLType  :: LHsType
    }
    -- the following is used internally
    | HsTyAssoc
    | HsTyEq HsType HsType
  deriving(Data,Typeable,Eq,Ord,Show)
  {-! derive: Binary, is !-}

data HsTyVarBind = HsTyVarBind {
    hsTyVarBindSrcLoc :: SrcLoc,
    hsTyVarBindName   :: Name,
    hsTyVarBindKind   :: Maybe HsKind
    } deriving(Data,Typeable,Eq,Ord,Show)
  {-! derive: Binary, update !-}

hsTyVarBind = HsTyVarBind {
    hsTyVarBindSrcLoc = bogusASrcLoc,
    hsTyVarBindName   = undefined,
    hsTyVarBindKind   = Nothing
    }

data HsAsst = HsAsst Name [Name] | HsAsstEq HsType HsType
  deriving(Data,Typeable,Eq,Ord, Show)
    {-! derive: Binary !-}

data HsLiteral
	= HsInt		!Integer
	| HsChar	!Char
	| HsString	String
	| HsFrac	Rational
	-- unboxed literals:
	| HsCharPrim	Char
	| HsStringPrim	String
	| HsIntPrim	Integer
	| HsFloatPrim	Rational
	| HsDoublePrim	Rational
	-- GHC extension:
	| HsLitLit	String
  deriving(Eq,Show,Ord)
    {-! derive: is !-}

data HsErrorType
    = HsErrorPatternFailure
    | HsErrorFieldSelect
    | HsErrorRecordUpdate
    | HsErrorSource
    | HsErrorUnderscore
    | HsErrorUninitializedField
 deriving(Eq,Show,Ord)

data HsExp
    = HsVar { hsExpName :: Name }
    | HsCon { hsExpName :: Name }
    | HsLit HsLiteral
    | HsApp HsExp HsExp
    | HsLambda SrcLoc [HsPat] HsExp
    | HsLet [HsDecl] HsExp
    | HsIf HsExp HsExp HsExp
    | HsCase { hsExpExp :: HsExp, hsExpAlts :: [HsAlt] }
    | HsLCase { hsExpAlts :: [HsAlt] }
    | HsTuple [HsExp]
    | HsUnboxedTuple [HsExp]
    | HsList [HsExp]
    | HsRecConstr Name [HsFieldUpdate]
    | HsRecUpdate HsExp [HsFieldUpdate]
    | HsExpTypeSig SrcLoc HsExp HsQualType
    | HsAsPat { hsExpName :: Name, hsExpExp :: HsExp }
    | HsError { hsExpSrcLoc :: SrcLoc, hsExpErrorType :: HsErrorType, hsExpString :: String }
    | HsWildCard SrcLoc
    | HsIrrPat { hsExpLExp :: LHsExp }
    | HsBangPat { hsExpLExp :: LHsExp }
    | HsLocatedExp LHsExp
    -- desugared away
    | HsEnumFrom HsExp
    | HsEnumFromTo HsExp HsExp
    | HsEnumFromThen HsExp HsExp
    | HsEnumFromThenTo HsExp HsExp HsExp
    | HsNegApp HsExp
    | HsLeftSection HsExp HsExp
    | HsRightSection HsExp HsExp
    | HsDo { hsExpStatements :: [HsStmt] }
    | HsListComp { hsExpComp :: HsComp }
    -- removed after fixity
    | HsWords    { hsExpExps :: [HsExp] }  -- precedence parser does applications
    | HsParen    HsExp
    | HsBackTick HsExp
    | HsInfixApp HsExp HsExp HsExp
    deriving(Eq,Show,Ord)
        {-! derive: is, update !-}

data HsClassHead = HsClassHead {
    hsClassHeadContext :: HsContext,
    hsClassHead        :: Name,
    hsClassHeadArgs    :: [HsType]
    } deriving(Eq,Show,Ord)
    {-! derive: update !-}

data HsPat
    = HsPVar     { hsPatName :: Name }
    | HsPApp     { hsPatName :: Name, hsPatPats :: [HsPat] }
    | HsPAsPat   { hsPatName :: Name, hsPatPat  :: HsPat }
    | HsPBangPat { hsPatLPat :: LHsPat }
    | HsPIrrPat  { hsPatLPat :: LHsPat }
    | HsPLit     { hsPatLit  :: HsLiteral }
    | HsPInfixApp HsPat Name HsPat
    | HsPList [HsPat]
    | HsPNeg HsPat
    | HsPParen HsPat
    | HsPRec Name [HsPatField]
    | HsPTuple [HsPat]
    | HsPUnboxedTuple [HsPat]
    | HsPWildCard
    -- | scoped type variable extension
    | HsPTypeSig SrcLoc HsPat HsQualType
    -- | advanced patterns need to be parsed as expressions
    | HsPatBackTick HsPat
    | HsPatWords [HsPat]
 deriving(Eq,Ord,Show)
 {-! derive: is !-}

data HsField a = HsField Name a
    deriving(Eq,Ord,Show,Functor,Traversable,Foldable)

data HsComp = HsComp {
    hsCompSrcLoc :: SrcLoc,
    hsCompStmts :: [HsStmt],
    hsCompBody  :: HsExp
    } deriving(Eq,Ord,Show)

data HsStmt
    = HsGenerator SrcLoc HsPat HsExp
    | HsQualifier HsExp
    | HsLetStmt [HsDecl]
 deriving(Eq,Show,Ord)

data HsAlt = HsAlt SrcLoc HsPat HsRhs [HsDecl]
  deriving(Eq,Show,Ord)

data HsKind = HsKind Name | HsKindFn HsKind HsKind
  deriving(Data,Typeable,Eq,Ord,Show)
  {-! derive: Binary !-}

-- instances

instance HasLocation HsAlt where
    srcLoc (HsAlt sl _ _ _) = sl

instance HasLocation HsConDecl where
    srcLoc d = hsConDeclSrcLoc d

instance HasLocation HsExp where
    srcLoc (HsCase _ xs) = srcLoc xs
    srcLoc (HsExpTypeSig sl _ _) = sl
    srcLoc (HsLambda sl _ _) = sl
    srcLoc HsError { hsExpSrcLoc = sl } = sl
    srcLoc _ = bogusASrcLoc

instance Binary Module where
    get = do
        ps <- get
        return (Module $ fromAtom ps)
    put (Module n) = put (toAtom n)

instance HasLocation HsModule where
    srcLoc x = hsModuleSrcLoc x

instance HasLocation HsImportDecl where
    srcLoc x = hsImportDeclSrcLoc x

instance HasLocation HsDecl where
    srcLoc (HsPragmaRules rs) = srcLoc rs
    srcLoc (HsFunBind     ms) = srcLoc ms
    srcLoc (HsPragmaProps sl _ _) = sl
    srcLoc d = hsDeclSrcLoc d

instance HasLocation HsRule where
    srcLoc HsRule { hsRuleSrcLoc = sl } = sl

instance HasLocation HsMatch where
    srcLoc (HsMatch sl _ _ _ _) = sl

instance HasLocation HsTyVarBind where
    srcLoc = hsTyVarBindSrcLoc

-- default values
hsModule = HsModule {
    hsModuleName    = error "unknown module name",
    hsModuleSrcLoc  = bogusASrcLoc,
    hsModuleExports = Nothing,
    hsModuleImports = [],
    hsModuleDecls   = [],
    hsModuleOptions = [],
    hsModuleOpt     = error "hsModuleOpt"
    }

hsDataDecl = HsDataDecl {
    hsDeclDeclType = DeclTypeData,
    hsDeclSrcLoc = bogusASrcLoc,
    hsDeclContext = [],
    hsDeclName = error "hsDataDecl.hsDeclName",
    hsDeclArgs = [],
    hsDeclCons = [],
    hsDeclHasKind = Nothing,
    hsDeclCTYPE = Nothing,
    hsDeclDerives = []
    }

hsNewTypeDecl = hsDataDecl {
    hsDeclDeclType = DeclTypeNewtype,
    hsDeclName = error "hsNewTypeDecl.hsDeclName"
    }

-- utility

hsParen x@HsVar {} = x
hsParen x@HsCon {} = x
hsParen x@HsParen {} = x
hsParen x@HsLit {} = x
hsParen x@HsTuple {} = x
hsParen x@HsUnboxedTuple {} = x
hsParen x@HsBackTick {} = x
hsParen x = HsParen x

hsNameIdent_u f n = mapName (id,f) n

hsKindStar = HsKind s_Star
hsKindHash = HsKind s_Hash
hsKindBang = HsKind s_Bang
hsKindQuest = HsKind s_Quest
hsKindQuestQuest = HsKind s_QuestQuest
hsKindStarBang = HsKind s_StarBang
