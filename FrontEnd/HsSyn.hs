module HsSyn where



import Atom
import Binary
import C.FFI
import Data.Generics
import PackedString
import FrontEnd.SrcLoc





instance HasLocation HsAlt where
    srcLoc (HsAlt sl _ _ _) = sl

instance HasLocation HsExp where
    srcLoc (HsCase _ xs) = srcLoc xs
    srcLoc (HsExpTypeSig sl _ _) = sl
    srcLoc (HsLambda sl _ _) = sl
    srcLoc _ = bogusASrcLoc



newtype Module = Module String
  deriving(Data,Typeable,Eq,Ord,ToAtom,FromAtom)

instance Show Module where
    showsPrec _ (Module n) = showString n

fromModule (Module s) = s

-- Names

data HsName
	= Qual { hsNameModule :: Module, hsNameIdent ::  HsIdentifier}
	| UnQual { hsNameIdent :: HsIdentifier}
  deriving(Data,Typeable,Eq,Ord)
  {-! derive: is, update, GhcBinary !-}


instance ToAtom HsName where
    toAtom = Atom.fromString . show

instance Show HsName where
   showsPrec _ (Qual (Module m) s) =
	showString m . showString "." . shows s
   showsPrec _ (UnQual s) = shows s

newtype HsIdentifier = HsIdent { hsIdentString :: String }
  deriving(Data,Typeable,Eq,Ord)

instance Binary Module where
    get bh = do
        ps <- get bh
        return (Module $ unpackPS ps)
    put_ bh (Module n) = put_ bh (packString n)

instance Binary HsIdentifier where
    get bh = do
        ps <- get bh
        return (HsIdent $ unpackPS ps)
    put_ bh (HsIdent n) = put_ bh (packString n)

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
  deriving(Data,Typeable, Show)
  {-! derive: update !-}

-- Export/Import Specifications

data HsExportSpec
	 = HsEVar HsName		-- variable
	 | HsEAbs HsName		-- T
	 | HsEThingAll HsName		-- T(..)
	 | HsEThingWith HsName [HsName]	-- T(C_1,...,C_n)
	 | HsEModuleContents Module	-- module M   (not for imports)
  deriving(Data,Typeable,Eq,Show)

instance HasLocation HsImportDecl where
    srcLoc x = hsImportDeclSrcLoc x


data HsImportDecl = HsImportDecl {
    hsImportDeclSrcLoc :: SrcLoc,
    hsImportDeclModule :: Module,
    hsImportDeclQualified :: !Bool,
    hsImportDeclAs :: (Maybe Module),
    hsImportDeclSpec :: (Maybe (Bool,[HsImportSpec]))
    }
  deriving(Data,Typeable,Eq,Show)

data HsImportSpec
	 = HsIVar HsName		-- variable
	 | HsIAbs HsName		-- T
	 | HsIThingAll HsName		-- T(..)
	 | HsIThingWith HsName [HsName]	-- T(C_1,...,C_n)
  deriving(Data,Typeable,Eq,Show)

data HsAssoc = HsAssocNone | HsAssocLeft | HsAssocRight
  deriving(Data,Typeable,Eq,Show)
  {-! derive: GhcBinary !-}

instance HasLocation HsDecl where
    srcLoc HsTypeDecl	 { hsDeclSrcLoc  = sl } = sl
    srcLoc HsDataDecl	 { hsDeclSrcLoc  = sl } = sl
    srcLoc HsInfixDecl   { hsDeclSrcLoc = sl } = sl
    srcLoc HsNewTypeDecl { hsDeclSrcLoc = sl } = sl
    srcLoc HsPragmaSpecialize { hsDeclSrcLoc = sl } = sl
    srcLoc (HsPragmaRules rs) = srcLoc rs
    srcLoc HsForeignDecl { hsDeclSrcLoc = sl } = sl
    srcLoc (HsForeignExport sl _ _ _) = sl
    srcLoc (HsClassDecl	 sl _ _) = sl
    srcLoc (HsInstDecl	 sl _ _) = sl
    srcLoc (HsDefaultDecl sl _) = sl
    srcLoc (HsTypeSig	 sl _ _) = sl
    srcLoc (HsFunBind     ms) = srcLoc ms
    srcLoc (HsPatBind	 sl _ _ _) = sl
    srcLoc (HsPragmaProps sl _ _) = sl

instance HasLocation HsRule where
    srcLoc HsRule { hsRuleSrcLoc = sl } = sl

data HsDecl
	 = HsTypeDecl	 { hsDeclSrcLoc :: SrcLoc, hsDeclName :: HsName, hsDeclTArgs :: [HsType], hsDeclType :: HsType }
	 | HsDataDecl	 { hsDeclSrcLoc :: SrcLoc, hsDeclContext :: HsContext, hsDeclName :: HsName, hsDeclArgs :: [HsName], hsDeclCons :: [HsConDecl], {- deriving -} hsDeclDerives :: [HsName] }
	 | HsInfixDecl   { hsDeclSrcLoc :: SrcLoc, hsDeclAssoc :: HsAssoc, hsDeclInt :: !Int, hsDeclNames :: [HsName]  }
	 | HsNewTypeDecl { hsDeclSrcLoc :: SrcLoc, hsDeclContext :: HsContext, hsDeclName :: HsName, hsDeclArgs :: [HsName], hsDeclCon :: HsConDecl, {- deriving -} hsDeclDerives :: [HsName] }
	 | HsClassDecl	 { hsDeclSrcLoc :: SrcLoc, hsDeclQualType :: HsQualType, hsDeclDecls :: [HsDecl] }
	 | HsInstDecl    { hsDeclSrcLoc :: SrcLoc, hsDeclQualType :: HsQualType, hsDeclDecls :: [HsDecl] }
	 | HsDefaultDecl SrcLoc HsType
	 | HsTypeSig	 SrcLoc [HsName] HsQualType
	 | HsFunBind     [HsMatch]
	 | HsPatBind	 SrcLoc HsPat HsRhs {-where-} [HsDecl]
         | HsForeignDecl { hsDeclSrcLoc   :: SrcLoc,
                           hsDeclForeign  :: FfiSpec,
                           hsDeclName     :: HsName,
                           hsDeclQualType :: HsQualType
                         }
         | HsForeignExport SrcLoc FfiExport HsName HsQualType
         | HsPragmaProps SrcLoc String [HsName]
	 | HsPragmaRules [HsRule]
         | HsPragmaSpecialize { hsDeclUniq :: (Module,Int), hsDeclSrcLoc :: SrcLoc, hsDeclBool :: Bool, hsDeclName :: HsName, hsDeclType :: HsType }
  deriving(Data,Typeable,Eq,Show)
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
  deriving(Data,Typeable,Eq,Show)

instance HasLocation HsMatch where
    srcLoc (HsMatch sl _ _ _ _) = sl

data HsMatch
	 = HsMatch SrcLoc HsName [HsPat] HsRhs {-where-} [HsDecl]
  deriving(Data,Typeable,Eq,Show)

data HsConDecl
	 = HsConDecl { hsConDeclSrcLoc :: SrcLoc, hsConDeclExists :: [HsTyVarBind], hsConDeclName :: HsName, hsConDeclConArg :: [HsBangType] }
	 | HsRecDecl { hsConDeclSrcLoc :: SrcLoc, hsConDeclExists :: [HsTyVarBind], hsConDeclName :: HsName, hsConDeclRecArg :: [([HsName],HsBangType)] }
  deriving(Data,Typeable,Eq,Show)
  {-! derive: is, update !-}

hsConDeclArgs HsConDecl { hsConDeclConArg = as } = as
hsConDeclArgs HsRecDecl { hsConDeclRecArg = as } = concat [ replicate (length ns) t | (ns,t) <- as]

data HsBangType
	 = HsBangedTy   { hsBangType :: HsType }
	 | HsUnBangedTy { hsBangType :: HsType }
  deriving(Data,Typeable,Eq,Show)

data HsRhs
	 = HsUnGuardedRhs HsExp
	 | HsGuardedRhss  [HsGuardedRhs]
  deriving(Data,Typeable,Eq,Show)

data HsGuardedRhs
	 = HsGuardedRhs SrcLoc HsExp HsExp
  deriving(Data,Typeable,Eq,Show)

data HsQualType
	 = HsQualType   { hsQualTypeContext :: HsContext, hsQualTypeType :: HsType }
  deriving(Data,Typeable,Eq,Ord,Show)
  {-! derive: GhcBinary !-}

hsQualTypeHsContext HsQualType { hsQualTypeContext = c } = c

data HsType
	 = HsTyFun   HsType HsType
	 | HsTyTuple [HsType]
	 | HsTyApp   HsType HsType
	 | HsTyVar   { hsTypeName :: HsName }
	 | HsTyCon   { hsTypeName :: HsName }
         | HsTyForall {
            hsTypeVars :: [HsTyVarBind],
            hsTypeType :: HsQualType }
         | HsTyExists {
            hsTypeVars :: [HsTyVarBind],
            hsTypeType :: HsQualType }
         -- the following are used internally
         | HsTyAssoc
         | HsTyEq HsType HsType
  deriving(Data,Typeable,Eq,Ord,Show)
  {-! derive: GhcBinary, is !-}

data HsTyVarBind = HsTyVarBind {
    hsTyVarBindSrcLoc :: SrcLoc,
    hsTyVarBindName :: HsName,
    hsTyVarBindKind :: Maybe HsKind }
  deriving(Data,Typeable,Eq,Ord,Show)
  {-! derive: GhcBinary, update !-}

hsTyVarBind = HsTyVarBind { hsTyVarBindSrcLoc = bogusASrcLoc, hsTyVarBindName = undefined, hsTyVarBindKind = Nothing }

instance HasLocation HsTyVarBind where
    srcLoc = hsTyVarBindSrcLoc

type HsContext = [HsAsst]
--type HsAsst    = (HsName,[HsType])	-- for multi-parameter type classes
--type HsAsst    = (HsName,HsName)	-- clobber

data HsAsst = HsAsst HsName [HsName] | HsAsstEq HsType HsType
  deriving(Data,Typeable,Eq,Ord, Show)
    {-! derive: GhcBinary !-}

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
  deriving(Data,Typeable,Eq,Ord, Show)
    {-! derive: is !-}

hsParen x@HsVar {} = x
hsParen x@HsCon {} = x
hsParen x@HsParen {} = x
hsParen x@HsLit {} = x
hsParen x@HsTuple {} = x
hsParen x = HsParen x

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
	| HsWildCard SrcLoc			-- ditto
	| HsIrrPat HsExp		-- ditto
 deriving(Data,Typeable,Eq,Show)
    {-! derive: is, update !-}

data HsPat
	= HsPVar { hsPatName :: HsName }
	| HsPLit { hsPatLit :: HsLiteral }
	| HsPNeg HsPat
	| HsPInfixApp HsPat HsName HsPat
	| HsPApp { hsPatName :: HsName, hsPatPats :: [HsPat] }
	| HsPTuple [HsPat]
	| HsPList [HsPat]
	| HsPParen HsPat
	| HsPRec HsName [HsPatField]
	| HsPAsPat { hsPatName :: HsName, hsPatPat :: HsPat }
	| HsPWildCard
	| HsPIrrPat HsPat
	| HsPTypeSig SrcLoc HsPat HsQualType  -- scoped type variable extension
 deriving(Data,Typeable,Eq,Ord,Show)
 {-! derive: is !-}

data HsPatField
	= HsPFieldPat HsName HsPat
 deriving(Data,Typeable,Eq,Ord,Show)

data HsStmt
	= HsGenerator SrcLoc HsPat HsExp       -- srcloc added by bernie
	| HsQualifier HsExp
	| HsLetStmt [HsDecl]
 deriving(Data,Typeable,Eq,Show)

data HsFieldUpdate
	= HsFieldUpdate HsName HsExp
  deriving(Data,Typeable,Eq,Show)

data HsAlt = HsAlt SrcLoc HsPat HsRhs [HsDecl]
  deriving(Data,Typeable,Eq,Show)

data HsKind = HsKind {-# UNPACK #-} !Atom | HsKindFn HsKind HsKind
  deriving(Data,Typeable,Eq,Ord,Show)
  {-! derive: GhcBinary !-}

hsKindStar = HsKind (fromString "*")

-----------------------------------------------------------------------------
-- Builtin names.

prelude_mod	      = Module "Prelude"
main_mod	      = Module "Main"

--unit_con_name	      = Qual prelude_mod (HsSpecial "()")
unit_con_name	      = UnQual (HsIdent "()")
--tuple_con_name i      = Qual prelude_mod (HsIdent ("("++replicate i ','++")"))
tuple_con_name i      = Qual (Module "Jhc.Basics") (HsIdent ("("++replicate i ','++")"))

unit_con	      = HsCon { {-hsExpSrcSpan = bogusSrcSpan,-} hsExpName = unit_con_name }
tuple_con i	      = HsCon { {-hsExpSrcSpan = bogusSrcSpan,-} hsExpName = (tuple_con_name i) }

as_name	              = UnQual $ HsIdent "as"
qualified_name        = UnQual $ HsIdent "qualified"
hiding_name	      = UnQual $ HsIdent "hiding"
minus_name	      = UnQual $ HsIdent "-"
pling_name	      = UnQual $ HsIdent "!"
star_name	      = UnQual $ HsIdent "*"
dot_name	      = UnQual $ HsIdent "."

unit_tycon_name       = unit_con_name
fun_tycon_name        = Qual prelude_mod (HsIdent "->")
list_tycon_name       = UnQual (HsIdent "[]")
--list_tycon_name       = Qual prelude_mod (HsIdent "[]")
tuple_tycon_name i    = tuple_con_name i

unit_tycon	      = HsTyCon unit_tycon_name
fun_tycon	      = HsTyCon fun_tycon_name
list_tycon	      = HsTyCon list_tycon_name
tuple_tycon i	      = HsTyCon (tuple_tycon_name i)
