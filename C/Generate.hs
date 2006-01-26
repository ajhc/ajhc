module C.Generate(
    Function(),
    Type(),
    Name(),
    Expression(),
    Statement(),
    sizeof,
    cast,
    tif,
    functionCall,
    function,
    dereference,
    reference,
    projectAnon,
    project,
    project',
    operator,
    uoperator,
    anonStructType,
    constant,
    string,
    enum,
    switch',
    number,
    character,
    isEmptyExpression,
    newVar,
    emptyExpression,
    drawG,
    toName,
    nullPtr,
    expr,
    name,
    variable,
    localVariable,
    creturn,
    Draw(err),
    basicType,
    withVars,
    ptrType,
    voidStarType,
    structAnon,
    assign,
    renderG,
    generateC,
    expressionRaw,
    statementRaw,
    voidType,
    test
    ) where

import Data.Monoid
import Control.Monad
import Control.Monad.RWS
import Char
import Text.PrettyPrint.HughesPJ(Doc,render,nest,($$),($+$))
import Numeric
import Maybe(isNothing)
import List(intersperse)

import Util.UniqueMonad
import GenUtil
import Doc.DocLike
import C.Prims

newtype G a = G (RWS () [()] Int a)
    deriving(Monad,MonadWriter [()],MonadState Int)


newtype Name = Name String


instance Show Name where
    show (Name n) = n

data Type = T (G Doc)
data TypeHint = ThNone | ThConst | ThPtr
data Expression = Exp TypeHint E
data Statement = SD (G Doc)
newtype Constant = C (G Doc)

data E = ED (G Doc) | EP E | EE

terr s = text "/* ERROR: " <> text s <> text " */"

class Draw d where
    draw :: d -> G Doc
    pdraw :: d -> G Doc
    pdraw = draw
    err :: String -> d
    err s = error s

instance Draw Statement where
    draw (SD g) = g
    err s = SD $ terr s

instance Draw E where
    draw (ED g) = g
    draw (EP e) = draw e
    draw EE = empty
    pdraw (ED g) = g
    pdraw (EP e) = parens (draw e)
    pdraw EE = empty
    err s = ED $ terr s

instance Draw Expression where
    draw (Exp _ e) = draw e
    pdraw (Exp _ e) = pdraw e
    err s = (Exp ThNone (err s))


instance Draw Name where
    draw (Name s) = text s
    err s = Name $ terr s

instance Draw Constant where
    draw (C x) = x
    err s = C $ terr s

instance Draw Type where
    draw (T x) = x
    err s = T $ terr s

-- expressions
sizeof :: Type -> Expression
sizeof t = expC (parens $ draw t)

cast :: Type -> Expression -> Expression
cast t e = expD (parens (draw t) <> pdraw e)


tif :: Expression -> Expression -> Expression -> Expression
tif x y z = expDC (draw x <+> char '?' <+> draw y <+> char ':' <+> draw z)

functionCall :: Name -> [Expression] -> Expression
functionCall n es = expD (draw n <> tupled (map draw es))

dereference :: Expression -> Expression
dereference x = expDC $ char '*' <> pdraw x

reference :: Expression -> Expression
reference x = expDC $ char '&' <> pdraw x

project :: Name -> Expression -> Expression
project n e = expD (pdraw e <> char '.' <> draw n)

project' :: Name -> Expression -> Expression
project' n e = project n $ dereference e

projectAnon :: Int -> Expression -> Expression
projectAnon n e = project (Name $ 'a':show n) e

variable :: Name -> Expression
variable n = expD (draw n)

localVariable :: Type -> Name -> Expression
localVariable _t n = variable n

emptyExpression = Exp ThNone EE

expressionRaw s = expD $ text s
statementRaw s = SD (text s)

isEmptyExpression (Exp _ EE) = True
isEmptyExpression _ = False
{-
structUnnamed :: Type -> [Expression] -> Expression
globalVar :: Name -> Type -> Expression
-}

structAnon :: [Expression] -> Expression
structAnon es = err "structAnon"


operator :: String -> Expression -> Expression -> Expression
operator o x y = expDC (pdraw x <+> text o <+> pdraw y)

uoperator :: String -> Expression -> Expression
uoperator o x = expDC (text o <> pdraw x)

constant :: Constant -> Expression
constant c = expD (draw c)

string :: String -> Expression
string s = Exp ThPtr (ED (return $ text (show s))) -- TODO, use C quoting conventions

nullPtr = Exp ThPtr (ED $ text "NULL")

name = Name

expD x = Exp ThNone (ED x)
expDC x = Exp ThNone (EP $ ED x)
expC x = Exp ThConst (ED x)

-- Constant
enum :: Name -> Constant
enum n = C (draw n)

number :: Int -> Constant
number i = C (tshow i)

character :: Char -> Constant
character c = C (tshow c)

-- statements
expr :: Expression -> Statement
expr e | isEmptyExpression e = mempty
expr e = SD $ draw e <> char ';'

instance Monoid Statement where
    mempty = SD empty
    mconcat = statements
    mappend a b = mconcat [a,b]

statements :: [Statement] -> Statement
statements ss = SD $ do
    ss <- mapM draw ss
    return $ vcat ss -- foldl ($+$) empty ss

creturn :: Expression -> Statement
creturn e = SD $ text "return " <> draw e <> char ';'

assign :: Expression -> Expression -> Statement
assign a b = expr $ operator "=" a b

withVars xs act = undefined
--SD $ do
--    us <- mapM (const newUniq) xs
--    let ss = act [ variable (name $ 'v':show u) | u <- us]
--    draw ss

newVar t = do
    u <- newUniq
    return (localVariable t (name $ '_':'u':show u))


--switch :: Expression -> [(Constant,Statement)] -> Maybe Statement -> Statement


switch' :: Expression -> [(Maybe Constant,Statement)]  -> Statement

switch' e ts = SD $ text "switch" <+> parens (draw e) <+> char '{' <$> vcat (map sc ts) <$> md <$>  char '}' where
    sc (Just x,ss) = do ss <- draw ss ; x <- draw x; return $ text "case" <+> x <> char ':' $$ nest 4 (ss $$ text "break;")
    sc (Nothing,ss) = do ss <- draw ss; return $ text "default:"  $$  ( nest 4 ss $$ text "break;")
    md = if any isNothing (fsts ts) then empty else text "default: jhc_case_fell_off(__LINE__);"
{-
creturn_ :: Statement
withVars :: [Type] -> ([Expression] -> Statement) -> Statement




-- functions

-- bfunction :: Name -> Type -> [Type] (\[Expression] -> Statement ) -> Function

-}

data Function = F {
    functionAnnotations :: String,
    functionName :: Name,
    functionReturnType :: Type,
    functionArgs :: [(Name,Type)],
    functionOptions :: [FunctionOpts],
    functionBody :: Statement
    }

data FunctionOpts = Public

function :: Name -> Type -> [(Name,Type)] -> [FunctionOpts] -> Statement -> Function
function n t as o s = F "" n t as o s


drawFunction f = do
    frt <- draw (functionReturnType f)
    body <- draw (functionBody f)
    name <- draw (functionName f)
    fas <- flip mapM (functionArgs f) $ \ (n,t) -> do
        n <- draw n
        t <- draw t
        return $ t <+> n
    let proto = text "static" <+> frt <+> name <> tupled fas <> semi
        proto' = text "static" <+> frt $$ name <> tupled fas
    return (proto, proto' $+$ char '{' $+$ nest 8 body $+$ char '}')

-- types
anonStructType :: [Type] -> Type
anonStructType ts = err "anonStructType"

basicType :: String -> Type
basicType s = T (text s)

ptrType :: Type -> Type
ptrType t = T (draw t <> char '*')

--namedStructType :: Name -> [(Name,Type)] -> Type
--structType :: Name -> [Type] -> Type
--enumType :: Name -> [Name] -> Type

-- type constants
voidStarType :: Type
voidStarType = ptrType voidType

voidType :: Type
voidType = basicType "void"

typeChar = basicType "char"
typeCharStar = ptrType typeChar


class Annotate e where
    annotate :: String -> e -> e

instance Annotate Statement where
    annotate c s = SD ((text "/* " <> text c <> text " */") <$> draw s)


mangleIdent xs =  concatMap f xs where
        f '.' = "__"
        f '@' = "_a"
        f ',' = "_c"
        f '(' = "_L"
        f ')' = "_R"
        f '$' = "_d"
        f '%' = "_P"
        f '#' = "_h"
        f '/' = "_s"
        f '=' = "_e"
        f '+' = "_p"
        f '-' = "_m"
        f '!' = "_b"
        f '>' = "_r"
        f '<' = "_l"
        f '\'' = "_t"
        f '_' = "_u"
        f c | isAlphaNum c = [c]
        f c = '_':'x':showHex (ord c) ""

toName s = Name (mangleIdent s)


generateC :: [Function]        -- ^ functions
    -> [(Name,[(Name,Type)])]  -- ^ extra structures
    -> (Doc,Doc)               -- ^ final result
generateC fs ss = ans where
    G ga = do
        fs <- mapM drawFunction fs
        let (protos,bodys) = unzip fs
        return (vcat protos $$ line $$  vsep bodys)
    (fns,_,written) = runRWS ga () 1
    ans = (empty,fns)

line = text ""
vsep xs = vcat $ intersperse line xs

test1 = constant (number 3)
test2 = operator "-" (operator "+" test1 test1) test1
test3 = expr $ functionCall (toName "foo") [test1,test2]
test4 = statements [test3,expr test2,test3]

showIt x = putStrLn (render $ drawG x)


test = do
    showIt test2
    showIt test3
    showIt test4


renderG x = render $ drawG x

drawG :: Draw d => d -> Doc
drawG x = fns where
    G ga = draw x
    (fns,_,_) = runRWS ga () 1


