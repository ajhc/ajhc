module C.Generate(
    Function(),
    Type(),
    Name(),
    Expression(),
    Statement(),
    Draw(err),
    sizeof,
    cast,
    tif,
    functionCall,
    dereference,
    reference,
    projectUnnamed,
    project,
    operator,
    uoperator,
    constant,
    string,
    enum,
    number,
    character,
    isEmptyExpression,
    emptyExpression,
    drawG,
    toName,
    nullPtr,
    expr,
    name,
    variable,
    creturn,
    Draw(err),
    basicType,
    withVars,
    ptrType,
    voidStarType,
    assign,
    test
    ) where

import Data.Monoid
import Control.Monad
import Control.Monad.Identity
import Char
import Text.PrettyPrint.HughesPJ(Doc,render)
import Numeric

import GenUtil
import Doc.DocLike
import C.Prims

newtype G a = G (Identity a)
    deriving(Monad)

data Function

newtype Name = Name String

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

projectUnnamed :: Int -> Expression -> Expression
projectUnnamed n e = project (Name $ 'a':show n) e

variable :: Name -> Expression
variable n = expD (draw n)

emptyExpression = Exp ThNone EE


isEmptyExpression (Exp _ EE) = True
isEmptyExpression _ = False
{-
structUnnamed :: Type -> [Expression] -> Expression
globalVar :: Name -> Type -> Expression
-}


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
expr e = SD $ draw e <> char ';'

instance Monoid Statement where
    mempty = SD empty
    mconcat = statements
    mappend a b = mconcat [a,b]

statements :: [Statement] -> Statement
statements ss = SD $ vcat (map draw ss)

creturn :: Expression -> Statement
creturn e = SD $ text "return " <> draw e <> char ';'

assign :: Expression -> Expression -> Statement
assign a b = expr $ operator "=" a b

withVars xs act = act (map (const (err "wv")) xs) 

{-
switch :: Expression -> [(Constant,Statement)] -> Maybe Statement -> Statement
creturn_ :: Statement
withVars :: [Type] -> ([Expression] -> Statement) -> Statement



data FunctionOpts = Public

-- functions
function :: Name -> Type -> [(Name,Type)] -> [FunctionOpts] -> Statement -> Function

-- bfunction :: Name -> Type -> [Type] (\[Expression] -> Statement ) -> Function

-}

-- types
--anonStructType :: [Type] -> Type

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

test1 = constant (number 3)
test2 = operator "-" (operator "+" test1 test1) test1
test3 = expr $ functionCall (toName "foo") [test1,test2]
test4 = statements [test3,expr test2,test3]

showIt x = do
    let G m = draw x
    putStrLn (render $ runIdentity $ m)



test = do
    showIt test2
    showIt test3
    showIt test4

drawG :: Draw d => d -> Doc
drawG x = let G m = draw x in (runIdentity $ m)

