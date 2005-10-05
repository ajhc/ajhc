module C.Gen(
    CType(..),
    CDecl(..),
    CStatement(..),
    CExpr(..),
    CLit(..),
    CFunction(..),
    CGenState(..),
    CIdent,
    CGen,
    runCGen,
    runSubCGen,
    addDecl,
    addStmts,
    addStatement,
    MonadCGen(..),
    addRequires,
    prettyC,
    ceIdent,
    ceFunCall,
    cAssign,
    addComment,
    ToCIdent(..),
    cfunction,
    prettyFuncP,
    prettyFunc,
    prettyDecl,
    anonField,
    addFunction

    ) where


import Char
import Control.Monad.State
import Data.Monoid
import List
import Maybe
import Numeric
import qualified Data.Map as Map
import qualified Text.PrettyPrint.HughesPJ as P
import Text.PrettyPrint.HughesPJ(nest,render,($$),($+$))

import Atom
import C.Prims
import Doc.DocLike
import Doc.PPrint
import GenUtil


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
    genStateAnonStructs :: Map.Map [CType] CType,
    genStateDecls :: [CDecl],
    genStateStatements :: [CStatement],
    genStateFunctions :: [CFunction],
    genStateRequires :: Requires,
    genUnique :: {-# UNPACK #-} !Int
    }
    {-! derive: update !-}

cGenState = CGenState {
    genStateAnonStructs = Map.empty,
    genStateDecls = [],
    genStateStatements = [],
    genStateFunctions = [],
    genStateRequires = mempty,
    genUnique = 1
    }

newtype CGen m a = CGen (StateT CGenState m a)
    deriving(Monad, MonadTrans)


class Monad m => MonadCGen m where
    addDecls :: [CDecl] -> m ()
    addStatements :: [CStatement] -> m ()
    newIdent :: m String
    newAnonStruct :: [CType] -> m CType


instance (Monad (m t), MonadTrans m, MonadCGen t) => MonadCGen (m t) where
    newIdent = lift newIdent
    addStatements x = lift $ addStatements x
    addDecls x = lift $ addDecls x
    newAnonStruct xs = lift $ newAnonStruct xs

instance Monad m => MonadCGen (CGen m) where
    addDecls d' = CGen $ modify f where
        f cg = cg { genStateDecls = genStateDecls cg ++ d'}
    addStatements s' = CGen $ modify f where
        f cg  =  cg { genStateStatements = genStateStatements cg ++ s'}
    newIdent = newIdent'
    newAnonStruct xs =  newAnonStruct' xs


runCGen u (CGen x) = runStateT x (cGenState { genUnique = u })
unCGen (CGen x) = x

addStmts x = addStatements x

runSubCGen :: Monad m => CGen m a -> CGen m ([CStatement], a)
runSubCGen (CGen x) = CGen $ do
    CGenState { genUnique = v, genStateAnonStructs = anonS } <- get
    (r,CGenState { genStateRequires = req, genStateAnonStructs = as, genStateDecls = d, genStateStatements = s, genUnique = v' }) <- lift $ runStateT x cGenState { genUnique = v, genStateAnonStructs = anonS }
    unCGen $ addDecls d
    modify (genUnique_s v' . genStateRequires_u (mappend req) . genStateAnonStructs_s as)
    return (s,r)

addDecl d = addDecls [d]

addStatement s = addStmts [s]
addRequires r = CGen $ modify (genStateRequires_u (mappend r))


addFunction :: Monad m => CFunction -> CGen m ()
addFunction fn = CGen $ modify f where
    f cg = cg { genStateFunctions = genStateFunctions cg ++ [fn]}

newIdent' :: Monad m => CGen m String
newIdent' = CGen $ do
    let f cg  =  cg { genUnique = genUnique cg + 1}
    CGenState { genUnique = i } <- get
    modify f
    return ('_':show i)

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
cBlock :: Monad m => CGen m () -> CGen m [CStatement]
cBlock v = do
    (s,()) <- runSubCGen v
    let (as, ns) = partition isAuto s
    addStmts as
    return ns
  -}


tup_names = [ 't':show i | i <- [ (1::Int) .. ]]




anonField :: CExpr -> Int -> CExpr
anonField c i = CEDot c (tup_names !! i)

newAnonStruct' :: Monad m => [CType] -> CGen m CType
newAnonStruct' ts = CGen $ do
    x <- gets genStateAnonStructs
    case Map.lookup ts x of
        Just t -> return t
        Nothing -> do
            id <- unCGen newIdent
            let nid = ("tup" ++ id)
            unCGen $ addDecls [CStruct  nid (zip ts tup_names)]
            let tp = (CTypeStruct nid)
            modify (genStateAnonStructs_u $ Map.insert ts tp)
            return tp




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



instance MonadState x m => MonadState x (CGen m) where
    get = lift $ get
    put x = lift $ put x

