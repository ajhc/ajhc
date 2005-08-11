module C.Gen where


--import Pretty
import qualified Text.PrettyPrint.HughesPJ as P
import Text.PrettyPrint.HughesPJ(nest,render,($$),($+$))
import List(partition)
import Control.Monad.State
import GenUtil
import Numeric
import Char
import Atom
import Doc.DocLike
import Doc.PPrint
import List
import Maybe


data CType = CTypeBasic String | CTypePointer CType | CTypeStruct String
    deriving(Ord,Eq)
data CDecl = CFunc CType String [(CType,String)] [CStatement] | CVar CType String | CStruct String [(CType,String)]
    deriving(Ord,Eq)
data CStatement = CSAssign CExpr CExpr | CSExpr CExpr | CSAuto CType String | CSReturn CExpr | CSDoc String | CSSwitch CExpr [(Maybe String,[CStatement])]
    deriving(Ord,Eq)
data CExpr = CEIdent String | CEFunCall String [CExpr] | CELiteral CLit | CEDot CExpr String | CEIndirect CExpr String | CESizeof CType | CECast CType CExpr | CEEval CExpr | CEDoc String | CEVar CType String | CETernary CExpr CExpr CExpr | CEOp String CExpr CExpr  | CEUOp String CExpr
    deriving(Ord,Eq)
data CLit = CLitChar Char | CLitInt Int | CLitNull
    deriving(Ord,Eq)

data CFunction = CFunction {
    cFuncComments :: String,
    cFuncName :: String,
    cFuncReturnType :: CType,
    cFuncArgs :: [(CType,String)],
    cFuncPublic :: Bool,
    cFuncBody :: [CStatement]
    }

cfunction = CFunction { cFuncComments = "", cFuncName = "_unknown", cFuncReturnType = CTypeBasic "void", cFuncArgs = [], cFuncPublic = False, cFuncBody = [] }

instance PPrint P.Doc CFunction where
    pprint = prettyFunc

instance DocLike d => PPrint d CExpr where
    pprint = prettyExpr

prettyFunc :: CFunction -> P.Doc
prettyFunc cf =  ans where
    comm = if null (cFuncComments cf) then empty else  text "/*" <+> text (cFuncComments cf) <+> text "*/"
    ans = comm $$ prettyDecl (fdecl cf)

prettyFuncP cf = prettyProto (fdecl cf)

fdecl cf = CFunc (cFuncReturnType cf) (cFuncName cf) (cFuncArgs cf) (cFuncBody cf)


data CCode = CCode {
    cCodeIncludes :: [String],
    cCodeFunctions :: [CFunction]
    --cCodeGlobalVars :: [(CType,String)]
    }

newtype CIdent = CIdent String

class ToCIdent a where
    toCIdent :: a -> CIdent


instance ToCIdent String where
    toCIdent xs = CIdent $ concatMap f xs where
        f '.' = "XD"
        f '@' = "XA"
        f ',' = "XC"
        f '(' = "XL"
        f ')' = "XR"
        f '_' = "_"
        f 'X' = "XX"
        f c | isAlphaNum c = [c]
        f c = 'X':showHex (ord c) ""

instance ToCIdent Atom where
    toCIdent a = toCIdent (fromAtom a :: String)

instance Show CIdent where
    show (CIdent x) = x


-----------------------------------------
-- high level monad for generating C code
-----------------------------------------


data CGenState = CGenState {
    genStateDecls :: [CDecl],
    genStateStatements :: [CStatement],
    genUnique :: {-# UNPACK #-} !Int
    }

cGenState = CGenState {
    genStateDecls = [],
    genStateStatements = [],
    genUnique = 1
    }

newtype CGen m a = CGen (StateT CGenState m a)
    deriving(Monad, MonadState CGenState, MonadTrans)

runCGen u (CGen x) = runStateT x (cGenState { genUnique = u })

runSubCGen :: Monad m => CGen m a -> CGen m ([CStatement], a)
runSubCGen x = do
    CGenState { genUnique = v } <- get
    (r,CGenState { genStateDecls = d, genStateStatements = s, genUnique = v' }) <- lift $ runCGen v x -- runStateT x ([],[],v)
    addDecls d
    modify (\cg -> cg { genUnique = v' })
    return (s,r)

addDecls :: Monad m => [CDecl] -> CGen m ()
addDecls d' = modify f where
    f cg = cg { genStateDecls = genStateDecls cg ++ d'}

addStmts :: Monad m => [CStatement] -> CGen m ()
addStmts s' = modify f where
    f cg  =  cg { genStateStatements = genStateStatements cg ++ s'}

newIdent :: Monad m => CGen m String
newIdent = do
    let f cg  =  cg { genUnique = genUnique cg + 1}
    CGenState { genUnique = i } <- get
    modify f
    return ('_':show i)

{-

instance Monad m => Unique (CGen m) where
    modifyGetUniqueState f = do
	modify (\(x,y,z) -> (x,y,f z))
	(_,_,z) <- get
	return z

instance Monad m => UniqueProducer (CGen m) where
    newUniq = newUniq_d
-}

---------------------------------------
-- utility functions for declaring code
---------------------------------------

-- naming helpers

func n = 'f':show n
auto n = 'a':show n
var n = 'v':show n

funcE n = ceIdent (func n)
autoE n = ceIdent (auto n)
varE n = ceIdent (var n)

-- simple constructors

ptr p = CTypePointer p
cInt i = (CELiteral (CLitInt i))
cVar v = (ceIdent v)

structT n = ptr $ CTypeStruct ('s':show n)
ceIdent = CEIdent
ceFunCall = CEFunCall
ceError s = CEDoc (text "error_thunk" <> parens (text (show s)))

-- simple values
tEval = CTypeBasic "eval_fn_t"
cVoid = CTypeBasic "void"
cThunk = CTypePointer (CTypeBasic "thunk_t")
cNull = (CELiteral CLitNull)
cVoidStar = CTypePointer cVoid
ctInt = CTypeBasic "int"
tEv = (CTypePointer $ CTypeBasic "eval_thunk_t")




cAssign n e = CSAssign (autoE n) e

addComment s = addStmts [CSDoc (text "/* " <> text s <> text " */")]

{-
cCase :: Monad m => CExpr -> ([(CLit,(CGen m CExpr))],(CGen m CExpr)) -> (CGen m CExpr)
cCase e (as,d) = do
    r <- newIdent
    te <- newIdent
    fas <- mapM (f r) as
    gd <- g r d
    addStmts [CSAuto cVoidStar r, CSAuto tEv te,CSAssign (ceIdent te) e]
    addStmts  [CSDoc ( text "switch" <> parens (prettyExpr $ CECast ctInt e) <> text "{" $$ nest 8 (vcat fas $$ gd) $$ text "}")]
    return (ceIdent r)  where
	f r (l,v)  = do
	    s <- cBlock (v >>= \e -> addStmts [CSAssign (ceIdent r) e])
	    return $ (text "case" <+> prettyLit l <> colon ) $$  prettyCode s $$ text "break;"
	g r v = do
	    s <- cBlock (v >>= \e -> addStmts [CSAssign (ceIdent r) e])
	    return $ text "default:" $$ prettyCode s $$ text "break;"
-}
{-
cCase :: Monad m => CExpr -> ([(CLit,(CGen m CExpr))],(CGen m CExpr)) -> (CGen m CExpr)
cCase e (as,d) = do
    r <- newIdent
    te <- newIdent
    fas <- mapM (f r) as
    gd <- g r d
    addStmts [CSAuto cVoidStar r, CSAuto tEv te,CSAssign (ceIdent te) e]
    addStmts  [CSDoc ( text "switch" <> parens (prettyExpr $ CECast ctInt (CEEval (ceIdent te))) <> text "{" $$ nest 8 (vcat fas $$ gd) $$ text "}")]
    return (ceIdent r)  where
	f r (l,v)  = do
	    s <- cBlock (v >>= \e -> addStmts [CSAssign (ceIdent r) e])
	    return $ (text "case" <+> prettyLit l <> colon ) $$  prettyCode s $$ text "break;"
	g r v = do
	    s <- cBlock (v >>= \e -> addStmts [CSAssign (ceIdent r) e])
	    return $ text "default:" $$ prettyCode s $$ text "break;"
-}

cBlock :: Monad m => CGen m () -> CGen m [CStatement]
cBlock v = do
    (s,()) <- runSubCGen v
    let (as, ns) = partition isAuto s
    addStmts as
    return ns
{-

cBlock v = do
    (_,_,i) <- get
    ((),(d,s,ni)) <- lift ( runCGen i v) -- runStateT v ([],[], i))
    addDecls d
    let (as, ns) = partition isAuto s
    addStmts as
    modify $ liftT3 (id,id,const ni)
    return ns

-}


declThunk :: String -> CDecl
declThunk n = CVar (CTypePointer (CTypeBasic "thunk_t")) n

cThunkInd :: String -> CExpr
cThunkInd n = CEIndirect (ceIdent "thunk") n

cInd n v = CEIndirect (autoE n) ('v':show v)
cTInd n = CEIndirect (ceIdent "thunk") ('v':show n)

cStructClosure n vs = [CStruct n ((cVoidStar, "eval"):map f vs)] where
    f n = (cThunk, n)

cAlloc t = CECast (CTypePointer t) $ CEFunCall "malloc" [CESizeof t]
cAllocThunk i = cAlloc (CTypeStruct ('s':show i))


----------------------------------
-- code emmission, Pretty Printing
----------------------------------


prettyC :: [CDecl] -> String
prettyC (cf) = render (header $$$
    ((vcat $ map prettyDecl sts) $$$ (vcat $ map prettyProto fns) $$$
	(vcat $ map prettyDecl vars) $$$ (vcat $ map prettyDecl fns)) $$$ text "")  where
    vars = filter isVar cf
    fns = filter isFn cf
    sts = filter isStruct cf
    isVar (CVar _ _) = True
    isVar _ = False
    isFn (CFunc _ _ _ _) = True
    isFn _ = False
    isStruct (CStruct _ _) = True
    isStruct _ = False
    header =  text "#include <malloc.h>" $$
	text "#include \"jhc_rts.h\"" $$ text ""

--a $$ b = a <> char '\n' <>  b

--a $+$ b = a $$ b
--semi = char ';'
--nest _ x = x

a $$$ b = a $$ text "" $$ b


prettyArgs [] = text "void"
prettyArgs args = hcat (punctuate (text ", ") (map (\(t,i) -> prettyType t <+> text i) $ args))

prettyDecl (CFunc rt n args code) = text "static" <+> prettyType rt $$ text n <> text "(" <> prettyArgs args <> text ")" $+$
    text "{" $+$ nest 8 (prettyCode code) $+$ text "}"
prettyDecl  (CVar t n) = prettyType t <+> text n <> semi
prettyDecl (CStruct n vs) = text "struct" <+> text n <+> text "{" $$ nest 8 (vcat (map sd vs)) $$ text "};" where
    sd (t,n) = prettyType t <+> text n <> semi

prettyProto (CFunc rt n args _) = text "static" <+> prettyType rt <+> text n <> parens (prettyArgs args) <> semi
prettyProto (CStruct n _) = text "struct" <+> text n <> semi
--prettyProto (CStruct n vs) = text "struct" <+> text n <+> text "{" $$ nest 8 (vcat (map sd vs)) $$ text "};" where
--    sd (t,n) = prettyType t <+> text n <> semi

prettyCode = prettyCode' True
prettyCode' showSa (ss) = vcat $ map ps ((if showSa then snub sa else [])  ++ sb) where
    ps (CSAssign n e) = prettyExpr n <+> text "=" <+> prettyExpr e <> text ";"
    ps (CSExpr e) = prettyExpr e <> semi
    ps (CSAuto t n) = prettyType t <+> text n <> semi
    ps (CSReturn e) = text "return" <+> prettyExpr e <> semi
    ps (CSSwitch e ts) = text "switch" <+> parens (prettyExpr e) <+> char '{' <$> vcat (map sc ts) <$> md <$>  char '}' where
        sc (Just x,ss) = text "case" <+> text x <> char ':' $$ nest 4  (prettyCode' False ss $$ text "break;")
        sc (Nothing,ss) = text "default:" $$ nest 4  (prettyCode' False ss) $$ text "break;"
        md = if any isNothing (fsts ts) then empty else text "default: jhc_case_fell_off(__LINE__);"
    ps (CSDoc d) = text d
    sa = collectAuto ss
    sb = filter (not . isAuto) ss
    collectAuto ss = filter isAuto ss ++ concatMap f ss where
        f (CSSwitch _ ts) = concat [collectAuto x | (_,x) <- ts]
        f _ = []
    --(sa, sb) = partition isAuto ss

isAuto (CSAuto _ _) = True
isAuto _ = False

prettyLit :: DocLike d => CLit -> d
prettyLit (CLitInt i) = text (show i)
prettyLit (CLitChar c) = text $ show c
prettyLit CLitNull = text "NULL"



prettyExpr :: DocLike d => CExpr -> d
prettyExpr (CEIdent n) = text n
prettyExpr (CELiteral l) = prettyLit l
prettyExpr (CEFunCall n ce) = text n <> parens (hcat (intersperse (text ", ") (map prettyExpr ce)))
prettyExpr (CEDot (CEIndirect (CEIdent n) x) y) = text n <> text "->" <> text x <> text "." <> text y
prettyExpr (CEDot (CEIndirect e x) y) = (parens $ prettyExpr e) <> text "->" <> text x <> text "." <> text y
prettyExpr (CEIndirect (CEIdent i) n) = text i <> text "->" <> text n
prettyExpr (CEIndirect e n) = (parens $ prettyExpr e) <> text "->" <> text n
prettyExpr (CEDot e n) = (parens $ prettyExpr e) <> text "." <> text n
prettyExpr (CESizeof t) = text "sizeof" <>(parens $ prettyType t)
prettyExpr (CECast t e) = parens (prettyType t) <> prettyExpr e
prettyExpr (CEEval e) = (prettyExpr (CEIndirect e "eval"))  <>  parens (prettyExpr e)
prettyExpr (CEDoc d) = text d
prettyExpr (CEOp s a b) = parens $ prettyExpr a <+> text s <+> prettyExpr b
prettyExpr (CEUOp s a) = parens $ text s <+> prettyExpr a
prettyExpr (CETernary x a b) = parens $ prettyExpr x <+> char '?' <+> prettyExpr a <+> char ':' <+> prettyExpr b

prettyType :: DocLike d => CType -> d
prettyType (CTypeBasic s) = text s
prettyType (CTypePointer t) = prettyType t <> text "*"
prettyType (CTypeStruct s) = text "struct" <+> text s


