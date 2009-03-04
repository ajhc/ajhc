module FrontEnd.HsSyn where



import StringTable.Atom
import StringTable.Atom()
import Data.Binary
import C.FFI
import Data.Generics
import FrontEnd.SrcLoc





instance HasLocation HsAlt where
    srcLoc (HsAlt sl _ _ _) = sl

instance HasLocation HsExp where
    srcLoc (HsCase _ xs) = srcLoc xs
    srcLoc (HsExpTypeSig sl _ _) = sl
    srcLoc (HsLambda sl _ _) = sl
    srcLoc HsError { hsExpSrcLoc = sl } = sl
    srcLoc _ = bogusASrcLoc



newtype Module = Module String
  deriving(Eq,Data,Typeable,Ord,ToAtom,FromAtom)

instance Show Module where
    showsPrec _ (Module n) = showString n

fromModule (Module s) = s

-- Names

data HsName
	= Qual { hsNameModule :: Module, hsNameIdent ::  HsIdentifier}
	| UnQual { hsNameIdent :: HsIdentifier}
  deriving(Data,Typeable,Eq,Ord)
  {-! derive: is, update, Binary !-}


instance ToAtom HsName where
    toAtom = toAtom . show

instance Show HsName where
   showsPrec _ (Qual (Module m) s) =
	showString m . showString "." . shows s
   showsPrec _ (UnQual s) = shows s

newtype HsIdentifier = HsIdent { hsIdentString :: String }
  deriving(Data,Typeable,Eq,Ord)

instance Binary Module where
    get = do
        ps <- get
        return (Module $ fromAtom ps)
    put (Module n) = put (toAtom n)

instance Binary HsIdentifier where
    get = do
        ps <- get
        return (HsIdent $ fromAtom ps)
    put (HsIdent n) = put (toAtom n)

hsIdentString_u f x = x { hsIdentString = f $ hsIdentString x }

--	| HsSymbol {hsIdentString :: String }
--	| HsSpecial {hsIdentString :: String }

instance Show HsIdentifier where
   showsPrec _ (HsIdent s) = showString s
--   showsPrec _ (HsSymbol s) = showString s
--   showsPrec _ (HsSpecial s) = showString s

instance HasLocation HsModule where
    srcLoc x = hsModuleSrcLoc x

data HsModule = HsModule {
    hsModuleName :: Module,
    hsModuleSrcLoc :: SrcLoc,
    hsModuleExports :: (Maybe [HsExportSpec]),
    hsModuleImports :: [HsImportDecl],
    hsModuleDecls :: [HsDecl],
    hsModuleOptions :: [String]
    }
  {-! derive: update !-}

-- Export/Import Specifications

data HsExportSpec
	 = HsEVar HsName		-- variable
	 | HsEAbs HsName		-- T
	 | HsEThingAll HsName		-- T(..)
	 | HsEThingWith HsName [HsName]	-- T(C_1,...,C_n)
	 | HsEModuleContents Module	-- module M   (not for imports)
  deriving(Eq,Show)

instance HasLocation HsImportDecl where
    srcLoc x = hsImportDeclSrcLoc x


data HsImportDecl = HsImportDecl {
    hsImportDeclSrcLoc :: SrcLoc,
    hsImportDeclModule :: Module,
    hsImportDeclQualified :: !Bool,
    hsImportDeclAs :: (Maybe Module),
    hsImportDeclSpec :: (Maybe (Bool,[HsImportSpec]))
    }
  deriving(Eq,Show)

data HsImportSpec
	 = HsIVar HsName		-- variable
	 | HsIAbs HsName		-- T
	 | HsIThingAll HsName		-- T(..)
	 | HsIThingWith HsName [HsName]	-- T(C_1,...,C_n)
  deriving(Eq,Show)

data HsAssoc = HsAssocNone | HsAssocLeft | HsAssocRight
  deriving(Eq,Show)
  {-! derive: Binary !-}

instance HasLocation HsDecl where
    srcLoc HsTypeDecl	  { hsDeclSrcLoc  = sl } = sl
    srcLoc HsDeclDeriving { hsDeclSrcLoc  = sl } = sl
    srcLoc HsSpaceDecl    { hsDeclSrcLoc  = sl } = sl
    srcLoc HsDataDecl	  { hsDeclSrcLoc  = sl } = sl
    srcLoc HsInfixDecl    { hsDeclSrcLoc = sl } = sl
    srcLoc HsNewTypeDecl  { hsDeclSrcLoc = sl } = sl
    srcLoc HsPragmaSpecialize { hsDeclSrcLoc = sl } = sl
    srcLoc (HsPragmaRules rs) = srcLoc rs
    srcLoc HsForeignDecl  { hsDeclSrcLoc = sl } = sl
    srcLoc HsActionDecl   { hsDeclSrcLoc = sl } = sl
    srcLoc (HsForeignExport sl _ _ _) = sl
    srcLoc (HsClassDecl	 sl _ _) = sl
    srcLoc HsClassAliasDecl { hsDeclSrcLoc = sl } = sl
    srcLoc (HsInstDecl	 sl _ _) = sl
    srcLoc (HsDefaultDecl sl _) = sl
    srcLoc (HsTypeSig	 sl _ _) = sl
    srcLoc (HsFunBind     ms) = srcLoc ms
    srcLoc (HsPatBind	 sl _ _ _) = sl
    srcLoc (HsPragmaProps sl _ _) = sl

instance HasLocation HsRule where
    srcLoc HsRule { hsRuleSrcLoc = sl } = sl

hsDataDecl = HsDataDecl {
    hsDeclKindDecl = False,
    hsDeclSrcLoc = bogusASrcLoc,
    hsDeclContext = [],
    hsDeclName = error "hsDataDecl.hsDeclName",
    hsDeclArgs = [],
    hsDeclCons = [],
    hsDeclHasKind = Nothing,
    hsDeclDerives = []
    }

hsNewTypeDecl = HsNewTypeDecl {
    hsDeclSrcLoc = bogusASrcLoc,
    hsDeclContext = [],
    hsDeclName = error "hsNewTypeDecl.hsDeclName",
    hsDeclArgs = [],
    hsDeclCon = error "hsNewTypeDecl.hsDeclCon",
    hsDeclDerives = []
    }

data HsDecl
    = HsTypeDecl	 { hsDeclSrcLoc :: SrcLoc, hsDeclName :: HsName, hsDeclTArgs :: [HsType], hsDeclType :: HsType }
    | HsDataDecl	 {
        hsDeclKindDecl :: Bool,
        hsDeclSrcLoc :: SrcLoc,
        hsDeclContext :: HsContext,
        hsDeclName :: HsName,
        hsDeclArgs :: [HsName],
        hsDeclCons :: [HsConDecl],
        hsDeclHasKind :: Maybe HsKind,
        {- deriving -} hsDeclDerives :: [HsName]
        }
    | HsNewTypeDecl {
        hsDeclSrcLoc :: SrcLoc,
        hsDeclContext :: HsContext,
        hsDeclName :: HsName,
        hsDeclArgs :: [HsName],
        hsDeclCon :: HsConDecl,
        {- deriving -} hsDeclDerives :: [HsName]
        }
    | HsInfixDecl   { hsDeclSrcLoc :: SrcLoc, hsDeclAssoc :: HsAssoc, hsDeclInt :: !Int, hsDeclNames :: [HsName]  }
    | HsClassDecl   { hsDeclSrcLoc :: SrcLoc, hsDeclQualType :: HsQualType, hsDeclDecls :: [HsDecl] }
    | HsClassAliasDecl {
        hsDeclSrcLoc :: SrcLoc,
        hsDeclName :: HsName,
        hsDeclTypeArgs :: [HsType],
        {- rhs -} hsDeclContext :: HsContext,
                  hsDeclClasses :: HsContext,
        hsDeclDecls :: [HsDecl]
        }
    | HsInstDecl    { hsDeclSrcLoc :: SrcLoc, hsDeclQualType :: HsQualType, hsDeclDecls :: [HsDecl] }
    | HsDefaultDecl SrcLoc HsType
    | HsTypeSig	 SrcLoc [HsName] HsQualType
    | HsFunBind     [HsMatch]
    | HsPatBind	 SrcLoc HsPat HsRhs {-where-} [HsDecl]
    | HsActionDecl {
        hsDeclSrcLoc   :: SrcLoc,
        hsDeclPat      :: HsPat,
        hsDeclExp      :: HsExp
        }
    | HsSpaceDecl {
        hsDeclSrcLoc   :: SrcLoc,
        hsDeclName     :: HsName,
        hsDeclExp      :: HsExp,
        hsDeclCName    :: Maybe String,
        hsDeclCount    :: Int,
        hsDeclQualType :: HsQualType
        }
    | HsForeignDecl {
        hsDeclSrcLoc   :: SrcLoc,
        hsDeclForeign  :: FfiSpec,
        hsDeclName     :: HsName,
        hsDeclQualType :: HsQualType
        }
    | HsForeignExport {
        hsDeclSrcLoc :: SrcLoc,
        hsDeclFFIExport :: FfiExport,
        hsDeclName :: HsName,
        hsDeclQualType ::HsQualType
        }
    | HsPragmaProps SrcLoc String [HsName]
    | HsPragmaRules [HsRule]
    | HsPragmaSpecialize {
        hsDeclUniq :: (Module,Int),
        hsDeclSrcLoc :: SrcLoc,
        hsDeclBool :: Bool,
        hsDeclName :: HsName,
        hsDeclType :: HsType
        }
    | HsDeclDeriving { hsDeclSrcLoc :: SrcLoc, hsDeclClassHead :: HsClassHead }
  deriving(Eq,Show)
  {-! derive: is !-}

data HsRule = HsRule {
    hsRuleUniq :: (Module,Int),
    hsRuleSrcLoc :: SrcLoc,
    hsRuleIsMeta :: Bool,
    hsRuleString :: String,
    hsRuleFreeVars :: [(HsName,Maybe HsType)],
    hsRuleLeftExpr :: HsExp,
    hsRuleRightExpr :: HsExp
    }
  deriving(Eq,Show)

instance HasLocation HsMatch where
    srcLoc (HsMatch sl _ _ _ _) = sl

data HsMatch
	 = HsMatch SrcLoc HsName [HsPat] HsRhs {-where-} [HsDecl]
  deriving(Eq,Show)

data HsConDecl
	 = HsConDecl { hsConDeclSrcLoc :: SrcLoc, hsConDeclExists :: [HsTyVarBind], hsConDeclName :: HsName, hsConDeclConArg :: [HsBangType] }
	 | HsRecDecl { hsConDeclSrcLoc :: SrcLoc, hsConDeclExists :: [HsTyVarBind], hsConDeclName :: HsName, hsConDeclRecArg :: [([HsName],HsBangType)] }
  deriving(Eq,Show)
  {-! derive: is, update !-}

hsConDeclArgs HsConDecl { hsConDeclConArg = as } = as
hsConDeclArgs HsRecDecl { hsConDeclRecArg = as } = concat [ replicate (length ns) t | (ns,t) <- as]

data HsBangType
	 = HsBangedTy   { hsBangType :: HsType }
	 | HsUnBangedTy { hsBangType :: HsType }
  deriving(Eq,Show)

data HsRhs
	 = HsUnGuardedRhs HsExp
	 | HsGuardedRhss  [HsGuardedRhs]
  deriving(Eq,Show)

data HsGuardedRhs
	 = HsGuardedRhs SrcLoc HsExp HsExp
  deriving(Eq,Show)

data HsQualType
	 = HsQualType   { hsQualTypeContext :: HsContext, hsQualTypeType :: HsType }
  deriving(Data,Typeable,Eq,Ord,Show)
  {-! derive: Binary !-}

hsQualTypeHsContext HsQualType { hsQualTypeContext = c } = c

data HsType
	 = HsTyFun   HsType HsType
	 | HsTyTuple [HsType]
	 | HsTyUnboxedTuple [HsType]
	 | HsTyApp   HsType HsType
	 | HsTyVar   { hsTypeName :: HsName }
	 | HsTyCon   { hsTypeName :: HsName }
         | HsTyForall {
            hsTypeVars :: [HsTyVarBind],
            hsTypeType :: HsQualType }
         | HsTyExists {
            hsTypeVars :: [HsTyVarBind],
            hsTypeType :: HsQualType }
         | HsTyExpKind { hsTySrcLoc :: SrcLoc, hsTyType :: HsType, hsTyKind :: HsKind }
         -- the following are used internally
         | HsTyAssoc
         | HsTyEq HsType HsType
  deriving(Data,Typeable,Eq,Ord,Show)
  {-! derive: Binary, is !-}

data HsTyVarBind = HsTyVarBind {
    hsTyVarBindSrcLoc :: SrcLoc,
    hsTyVarBindName :: HsName,
    hsTyVarBindKind :: Maybe HsKind }
  deriving(Data,Typeable,Eq,Ord,Show)
  {-! derive: Binary, update !-}

hsTyVarBind = HsTyVarBind { hsTyVarBindSrcLoc = bogusASrcLoc, hsTyVarBindName = undefined, hsTyVarBindKind = Nothing }

instance HasLocation HsTyVarBind where
    srcLoc = hsTyVarBindSrcLoc

type HsContext = [HsAsst]
--type HsAsst    = (HsName,[HsType])	-- for multi-parameter type classes
--type HsAsst    = (HsName,HsName)	-- clobber

data HsAsst = HsAsst HsName [HsName] | HsAsstEq HsType HsType
  deriving(Data,Typeable,Eq,Ord, Show)
    {-! derive: Binary !-}

data HsLiteral
	= HsInt		!Integer
	| HsChar	!Char
	| HsString	String
	| HsFrac	Rational
	-- GHC unboxed literals:
	| HsCharPrim	Char
	| HsStringPrim	String
	| HsIntPrim	Integer
	| HsFloatPrim	Rational
	| HsDoublePrim	Rational
	-- GHC extension:
	| HsLitLit	String
  deriving(Eq,Ord, Show)
    {-! derive: is !-}

hsParen x@HsVar {} = x
hsParen x@HsCon {} = x
hsParen x@HsParen {} = x
hsParen x@HsLit {} = x
hsParen x@HsTuple {} = x
hsParen x@HsUnboxedTuple {} = x
hsParen x = HsParen x

data HsErrorType = HsErrorPatternFailure | HsErrorSource | HsErrorFieldSelect | HsErrorUnderscore | HsErrorUninitializedField | HsErrorRecordUpdate
 deriving(Eq,Show)

type LHsExp = Located HsExp

data HsExp
	= HsVar { {- hsExpSrcSpan :: SrcSpan,-} hsExpName :: HsName }
	| HsCon { {-hsExpSrcSpan :: SrcSpan,-} hsExpName :: HsName }
	| HsLit HsLiteral
	| HsInfixApp HsExp HsExp HsExp
	| HsApp HsExp HsExp
	| HsNegApp HsExp
	| HsLambda SrcLoc [HsPat] HsExp
	| HsLet [HsDecl] HsExp
	| HsIf HsExp HsExp HsExp
	| HsCase HsExp [HsAlt]
	| HsDo { hsExpStatements :: [HsStmt] }
	| HsTuple [HsExp]
	| HsUnboxedTuple [HsExp]
	| HsList [HsExp]
	| HsParen HsExp
	| HsLeftSection HsExp HsExp
	| HsRightSection HsExp HsExp
	| HsRecConstr HsName [HsFieldUpdate]
	| HsRecUpdate HsExp [HsFieldUpdate]
	| HsEnumFrom HsExp
	| HsEnumFromTo HsExp HsExp
	| HsEnumFromThen HsExp HsExp
	| HsEnumFromThenTo HsExp HsExp HsExp
	| HsListComp HsExp [HsStmt]
	| HsExpTypeSig SrcLoc HsExp HsQualType
	| HsAsPat { hsExpName :: HsName, hsExpExp :: HsExp }  -- pattern only
        | HsError { hsExpSrcLoc :: SrcLoc, hsExpErrorType :: HsErrorType, hsExpString :: String }
	| HsWildCard SrcLoc			-- ditto
	| HsIrrPat { hsExpLExp :: LHsExp }
	| HsBangPat { hsExpLExp :: LHsExp }
        | HsLocatedExp LHsExp
 deriving(Eq,Show)
    {-! derive: is, update !-}

data HsClassHead = HsClassHead { hsClassHeadContext :: HsContext, hsClassHead :: HsName, hsClassHeadArgs :: [HsType] }
 deriving(Eq,Show)
    {-! derive: update !-}

type LHsPat = Located HsPat

data HsPat
	= HsPVar { hsPatName :: HsName }
	| HsPLit { hsPatLit :: HsLiteral }
	| HsPNeg HsPat
	| HsPInfixApp HsPat HsName HsPat
	| HsPApp { hsPatName :: HsName, hsPatPats :: [HsPat] }
	| HsPTuple [HsPat]
	| HsPUnboxedTuple [HsPat]
	| HsPList [HsPat]
	| HsPParen HsPat
	| HsPRec HsName [HsPatField]
	| HsPAsPat { hsPatName :: HsName, hsPatPat :: HsPat }
	| HsPWildCard
	| HsPIrrPat { hsPatLPat :: LHsPat }
	| HsPBangPat { hsPatLPat :: LHsPat }
	| HsPTypeSig SrcLoc HsPat HsQualType  -- scoped type variable extension
 deriving(Eq,Ord,Show)
 {-! derive: is !-}

data HsPatField
	= HsPFieldPat HsName HsPat
 deriving(Eq,Ord,Show)

data HsStmt
	= HsGenerator SrcLoc HsPat HsExp       -- srcloc added by bernie
	| HsQualifier HsExp
	| HsLetStmt [HsDecl]
 deriving(Eq,Show)

data HsFieldUpdate
	= HsFieldUpdate HsName HsExp
  deriving(Eq,Show)

data HsAlt = HsAlt SrcLoc HsPat HsRhs [HsDecl]
  deriving(Eq,Show)

data HsKind = HsKind HsName | HsKindFn HsKind HsKind
  deriving(Data,Typeable,Eq,Ord,Show)
  {-! derive: Binary !-}

hsKindStar = HsKind (Qual (Module "Jhc@") (HsIdent "*"))
hsKindHash = HsKind (Qual (Module "Jhc@") (HsIdent "#"))
hsKindBang = HsKind (Qual (Module "Jhc@") (HsIdent "!"))
hsKindQuest = HsKind (Qual (Module "Jhc@") (HsIdent "?"))
hsKindQuestQuest = HsKind (Qual (Module "Jhc@") (HsIdent "??"))
hsKindStarBang   = HsKind (Qual (Module "Jhc@") (HsIdent "*!"))

-----------------------------------------------------------------------------
-- Builtin names.

