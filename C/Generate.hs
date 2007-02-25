module C.Generate(
    Annotate(..),
    anonStructType,
    assign,
    basicType,
    cast,
    cif,
    constant,
    creturn,
    dereference,
    reference,
    Draw(err),
    drawG,
    emptyExpression,
    enum,
    expr,
    Expression(),
    expressionRaw,
    indexArray,
    function,
    Function(functionName),
    functionCall,
    FunctionOpts(..),
    generateC,
    goto,
    indentBlock,
    isEmptyExpression,
    label,
    localVariable,
    name,
    Name(),
    newVar,
    forLoop,
    nullPtr,
    number,
    operator,
    project,
    project',
    projectAnon,
    ptrType,
    renderG,
    sizeof,
    Statement(),
    string,
    structAnon,
    structType,
    switch',
    toName,
    Type(),
    uoperator,
    variable,
    test,
    voidType
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

data StatementInfo = StatementGoto Name | StatementLabel Name | StatementNoInfo | StatementEmpty

data TypeHint = ThNone | ThConst | ThPtr
data Expression = Exp TypeHint E
data Statement = SD StatementInfo (G Doc)
newtype Constant = C (G Doc)

sd = SD StatementNoInfo

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
    draw (SD _ g) = g
    err s = sd $ terr s

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

indexArray :: Expression -> Expression -> Expression
indexArray w i = expD (pdraw w <> char '[' <> pdraw i <> char ']')

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
expr e = sd $ draw e <> char ';'

instance Monoid Statement where
    mempty = SD StatementEmpty empty
    mappend (SD StatementEmpty _) x = x
    mappend x (SD StatementEmpty _) = x
    mappend x@(SD (StatementGoto _) _) (SD (StatementGoto _) _) = x
    mappend (SD (StatementGoto l) _) y@(SD (StatementLabel l') _) | l == l' = y
--    mappend x y@(SD l@StatementGoto {} _) = combine l x y
--    mappend x@(SD l@StatementLabel {} _) y  = combine l x y
    mappend a b = combine StatementNoInfo a b

combine l a b = SD l $ do
    a <- draw a
    b <- draw b
    return $ vcat [a,b]


creturn :: Expression -> Statement
creturn e = SD (StatementGoto (Name "")) $ text "return " <> draw e <> char ';'

assign :: Expression -> Expression -> Statement
assign a b = expr $ operator "=" a b

label :: Name -> Statement
label n@(Name s) = SD (StatementLabel n) $ text s <> char ':' <+> text "0;"

goto :: Name -> Statement
goto n@(Name s) = SD (StatementGoto n) $ text "goto" <+> text s <> char ';'

--SD $ do
--    us <- mapM (const newUniq) xs
--    let ss = act [ variable (name $ 'v':show u) | u <- us]
--    draw ss

newVar t = do
    u <- newUniq
    return (localVariable t (name $ 'x':show u))


--switch :: Expression -> [(Constant,Statement)] -> Maybe Statement -> Statement


switch' :: Expression -> [(Maybe Constant,Statement)]  -> Statement

switch' e ts = sd $ text "switch" <+> parens (draw e) <+> char '{' <$> vcat (map sc ts) <$> md <$>  char '}' where
    sc (Just x,ss) = do ss <- draw ss ; x <- draw x; return $ text "case" <+> x <> char ':' $$ nest 4 (ss $$ text "break;")
    sc (Nothing,ss) = do ss <- draw ss; return $ text "default:"  $$  ( nest 4 ss $$ text "break;")
    md = if any isNothing (fsts ts) then empty else text "default: jhc_case_fell_off(__LINE__);"


cif :: Expression -> Statement -> Statement -> Statement
cif exp thn els = sd $ do
    thn <- draw thn
    els <- draw els
    exp <- draw exp
    return $ text "if" <+> parens exp <+> lbrace <$> nest 4 thn <$> rbrace <+> text "else" <+> lbrace <$> nest 4 els <$> rbrace

indentBlock sd@(SD si _) = SD si $ do
    x <- draw sd
    return $ nest 4 x

forLoop :: Expression -> Expression -> Expression -> Statement -> Statement
forLoop i from to body = sd $ do
    i <- draw i
    from <- draw from
    to <- draw to
    body <- draw body
    return $ text "for" <> parens (i <+> equals <+> from <> semi <+> i <> text "++" <> semi <+> i <+> text "<" <+> to) <+> lbrace <$> nest 4 body <$> rbrace


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



class Annotate e where
    annotate :: String -> e -> e

instance Annotate Statement where
    annotate c s@(SD si _) = SD si ((text "/* " <> text c <> text " */") <$> draw s)


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
    ((hd,fns),(_,ass),_written) = runRWS ga () (1,Map.empty)

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
test4 = mconcat [test3,expr test2,test3]

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


