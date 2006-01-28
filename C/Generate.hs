module C.Generate(
    Annotate(..),
    Function(),
    Type(),
    Name(),
    Expression(),
    Statement(),
    FunctionOpts(..),
    sizeof,
    cast,
    cif,
    commaExpression,
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
    structType,
    assign,
    renderG,
    generateC,
    expressionRaw,
    statementRaw,
    voidType,
    test
    ) where

import Char
import Control.Monad.RWS
import Data.Monoid
import List(intersperse)
import Maybe(isNothing)
import Numeric
import qualified Data.Map as Map
import Text.PrettyPrint.HughesPJ(Doc,render,nest,($$),($+$))

import Doc.DocLike
import GenUtil


newtype G a = G (RWS () [(Name,Type)] (Int,Map.Map [Type] Name) a)
    deriving(Monad,MonadWriter [(Name,Type)],MonadState (Int,Map.Map [Type] Name))


newtype Name = Name String
    deriving(Eq,Ord)


instance Show Name where
    show (Name n) = n

data TypeHint = ThNone | ThConst | ThPtr
data Expression = Exp TypeHint E
newtype Statement = SD (G Doc)
newtype Constant = C (G Doc)

data Type = TB String | TPtr Type | TAnon [Type] | TNStruct Name
    deriving(Eq,Ord)

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
    draw (TB x) = text x
    draw (TPtr x) = draw x <> char '*'
    draw (TAnon ts) = do
        (n,mp) <- get
        case Map.lookup ts mp of
            Just x -> text "struct" <+> draw x
            Nothing -> do
                let nm = name ("tup" ++ show n)
                put (n + 1,Map.insert ts nm mp)
                text "struct" <+> draw nm
    draw (TNStruct n) = text "struct" <+> draw n

    err s = TB $ terr s

-- expressions
sizeof :: Type -> Expression
sizeof t = expC (text "sizeof" <> parens $ draw t)

cast :: Type -> Expression -> Expression
cast t e = expDC (parens (draw t) <> pdraw e)


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
project' n e = expD (pdraw e <> text "->" <> draw n)

projectAnon :: Int -> Expression -> Expression
projectAnon n e = project (Name $ 't':show n) e

variable :: Name -> Expression
variable n = expD (draw n)

localVariable :: Type -> Name -> Expression
localVariable t n = expD $ do
    tell [(n,t)]
    draw n

emptyExpression = Exp ThNone EE

expressionRaw s = expD $ text s
statementRaw s = SD (text s)

isEmptyExpression (Exp _ EE) = True
isEmptyExpression _ = False
{-
structUnnamed :: Type -> [Expression] -> Expression
globalVar :: Name -> Type -> Expression
-}

commaExpression :: [Expression] -> Expression
commaExpression [] = emptyExpression
commaExpression [e] = e
commaExpression ss = expD $ do
    ds <- mapM draw ss
    return (tupled ds)

structAnon :: [(Expression,Type)] -> Expression
--structAnon _ = err "structAnon"
structAnon es = Exp ThNone $ ED $ do
    (n,mp) <- get
    put (n + 1,mp)
    let nm = name ("_t" ++ show n)
        lv = localVariable (anonStructType (snds es)) nm
    draw $ commaExpression $ [operator "=" (projectAnon i lv) e | e <- fsts es | i <- [0..] ] ++ [lv]


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
    return (localVariable t (name $ 'x':show u))


--switch :: Expression -> [(Constant,Statement)] -> Maybe Statement -> Statement


switch' :: Expression -> [(Maybe Constant,Statement)]  -> Statement

switch' e ts = SD $ text "switch" <+> parens (draw e) <+> char '{' <$> vcat (map sc ts) <$> md <$>  char '}' where
    sc (Just x,ss) = do ss <- draw ss ; x <- draw x; return $ text "case" <+> x <> char ':' $$ nest 4 (ss $$ text "break;")
    sc (Nothing,ss) = do ss <- draw ss; return $ text "default:"  $$  ( nest 4 ss $$ text "break;")
    md = if any isNothing (fsts ts) then empty else text "default: jhc_case_fell_off(__LINE__);"


cif :: Expression -> Statement -> Statement -> Statement
cif exp thn els = SD $ do
    thn <- draw thn
    els <- draw els
    exp <- draw exp
    return $ text "if" <+> parens exp <+> lbrace <$> nest 4 thn <$> rbrace <+> text "else" <+> lbrace <$> nest 4 els <$> rbrace

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

data FunctionOpts = Public | Attribute String
    deriving(Eq)

function :: Name -> Type -> [(Name,Type)] -> [FunctionOpts] -> Statement -> Function
function n t as o s = F "" n t as o s


drawFunction f = do
    frt <- draw (functionReturnType f)
    (body,uv) <- listen (draw (functionBody f))
    uv' <- flip mapM [ (x,t) | (x,t) <- snubUnder fst uv, x `notElem` fsts (functionArgs f)] $ \ (n,t) -> do
        t <- draw t
        return $ t <+> tshow n <> semi
    name <- draw (functionName f)
    fas <- flip mapM (functionArgs f) $ \ (n,t) -> do
        n <- draw n
        t <- draw t
        return $ t <+> n
    let fas' = if null fas then [text "void"] else fas
        proto = static <+> frt <+> name <> tupled fas' <> parms <> semi
        proto' = static <+> frt <> parms $$ name <> tupled fas'
        static = if Public `elem` functionOptions f then empty else text "static"
        parms = char ' ' <> hsep [ text s | Attribute s <- functionOptions f]
    return (proto, proto' $+$ lbrace $+$ nest 8 (vcat uv' $$ body) $+$ rbrace)

-- types
anonStructType :: [Type] -> Type
anonStructType ts = TAnon ts

basicType :: String -> Type
basicType s = TB s

structType :: Name -> Type
structType n = TNStruct n

ptrType :: Type -> Type
ptrType t = TPtr t

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
        let shead = vcat $ map (text . (++ ";") . ("struct " ++) . show . fst) ss
        shead2 <- declStructs True ss
        return (shead $$ line $$ shead2, vcat protos $$ line $$  vsep bodys)
    ((hd,fns),(_,ass),written) = runRWS ga () (1,Map.empty)

    anons = [ (n, fields ts ) | (ts,n) <- Map.toList ass ] where
        fields :: [Type] -> [(Name,Type)]
        fields ts = [ (name ('t':show tn),t) | t <- ts | tn <- [0::Int .. ]]
    G anons' = declStructs False anons
    (anons'',_,_) = runRWS anons' () (1,Map.empty)

    declStructs ht ss = liftM vsep $ flip mapM ss $ \ (n,ts) -> do
            ts' <- flip mapM ts $ \ (n,t) -> do
                t <- draw t
                return $ t <+> tshow n <> semi
            return $ text "struct" <+> tshow n <+> lbrace $$ nest 4 (vcat $ (if ht then text "tag_t tag;" else empty):ts') $$ rbrace <> semi
    ans = (hd $$ anons'',fns)

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
    (fns,_,_) = runRWS ga () (1,Map.empty)


