module C.Generate(
    Annotate(..),
    Structure(..),
    (=*),
    (&),
    ToExpression(..),
    ToStatement(..),
    eq,
    basicStructure,
    anonStructType,
    basicType,
    basicGCType,
    cast,
    noAssign,
    cif,
    constant,
    cTrue,cFalse,
    creturn,
    dereference,
    reference,
    Draw(err),
    drawG,
    emptyExpression,
    enum,
    Expression(),
    Constant(),
    expressionRaw,
    statementOOB,
    indexArray,
    function,
    Function(functionName),
    functionCall,
    indirectFunctionCall,
    FunctionOpts(..),
    generateC,
    goto,
    subBlock,
    isEmptyExpression,
    label,
    localVariable,
    name,
    Name(),
    newVar,
    newDeclVar,
    newAssignVar,
    newTmpVar,
    forLoop,
    nullPtr,
    number,
    floating,
    operator,
    project,
    project',
    projectAnon,
    ptrType,
    funPtrType,
    renderG,
    sizeof,
    Statement(),
    string,
    structAnon,
    structType,
    switch',
    character,
    toName,
    Type(),
    uoperator,
    variable,
    voidType,
    voidStarType
    ) where

import Char
import Control.Monad.RWS
import Control.Monad.Writer
import Control.Monad
import Data.List(intersperse)
import Data.Maybe(isNothing)
import Data.Monoid
import Numeric
import Text.PrettyPrint.HughesPJ(Doc,render,nest,($$),($+$))
import qualified Data.Foldable as Seq
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Traversable as Seq
import qualified Data.Set as Set

import Doc.DocLike
import Util.Gen
import Util.SetLike


data Env = Env {
    envUsedLabels :: Set.Set Name,
    envInScope    :: Set.Set Name
    }

--envInScope_u f e = e { envInScope = f $ envInScope e }

emptyEnv = Env { envUsedLabels = mempty, envInScope = mempty }

newtype G a = G (RWS Env [(Name,Type)] (Int,Map.Map [Type] Name) a)
    deriving(Monad,MonadWriter [(Name,Type)],MonadState (Int,Map.Map [Type] Name),MonadReader Env,MonadFix)


newtype Name = Name String
    deriving(Eq,Ord)


instance Show Name where
    show (Name n) = n


data TypeHint = TypeHint {
    thPtr :: Bool,
    thConst :: Bool,
    thNoAssign :: Bool,
    thOmittable :: Bool
    }

hintConst = typeHint { thConst = True, thOmittable = True }
hintPtr   = typeHint { thPtr = True }

typeHint = TypeHint { thPtr = False, thConst = False, thNoAssign = False, thOmittable = False }

data Expression = Exp TypeHint E

newtype Statement = St (Seq.Seq Stmt)

data Stmt =
    SD (G Doc)
    | SGoto Name
    | SLabel Name
    | SReturn Expression
    | SBlock Statement
    | SIf Expression Statement Statement
    | SSwitch Expression [(Maybe Constant,Statement)]

newtype Constant = C (G Doc)

sd x = stmt (SD x)
stmt s = St (Seq.singleton s)

stmtMapStmt :: Monad m => (Stmt -> m Stmt) -> Stmt -> m Stmt
stmtMapStmt f s = g s where
    g (SBlock sb) = return SBlock `ap` h sb
    g (SIf e s1 s2) = return (SIf e) `ap` h s1 `ap` h s2
    g (SSwitch e ss) = do
        ss <- forM ss $ \ (x,y) -> do
            y <- h y
            return (x,y)
        return $ SSwitch e ss
    g s = return s
    h (St sms) = return St `ap` Seq.mapM f sms


-- The Bool in TB and is whether the GC needs to consider the types to
-- possibly contain garbage collectable pointers.
data Type = TB String Bool | TPtr Type | TAnon [Type] | TNStruct Name | TFunPtr Type [Type]
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
    draw (St ss) = vcat (map draw $ Seq.toList ss)
    err s = sd $ terr s

instance Draw Stmt where
    err s = SD (terr s)

    draw (SD g) = g
    draw (SReturn e) | isEmptyExpression e = text "return;"
    draw (SReturn e) = text "return " <> draw e <> char ';'
    draw (SLabel n@(Name s)) = do
        ls <- asks envUsedLabels
        if n `member` ls then  text s <> char ':' <> char ';' else return mempty
    draw (SGoto (Name s)) = text "goto" <+> text s <> char ';'
    draw (SBlock s) = do
        s <- subBlockBody s
        return $ vcat [char '{', nest 4 s, char '}']
    draw (SIf exp thn els) = do
        exp <- draw exp
        thn <- subBlockBody thn
        els <- subBlockBody els
        return $ text "if" <+> parens exp <+> lbrace <$> nest 4 thn <$> rbrace <+> text "else" <+> lbrace <$> nest 4 els <$> rbrace
    draw (SSwitch e ts) = text "switch" <+> parens (draw e) <+> char '{' <$> vcat (map sc ts) <$> md <$>  char '}' where
        sc (Just x,ss) = do ss <- draw (SBlock ss); x <- draw x; return $ text "case" <+> x <> char ':' $$ nest 4 (ss $$ text "break;")
        sc (Nothing,ss) = do ss <- draw (SBlock ss); return $ text "default:"  $$  ( nest 4 ss $$ text "break;")
        md = if any isNothing (fsts ts) then empty else text "default: jhc_case_fell_off(__LINE__);"

--subBlockBody s = draw s
subBlockBody s = do
    let vcmp (n,t@(TB _ b)) = (not b,n)
        vcmp (n,t) = (True,n)
    is <- asks envInScope
    (body,uv) <-  censor (const []) $ listen (draw s)
    uv' <- forM [ (x,t) | (x,t) <- snubUnder vcmp uv, (x /= toName "v0") && (x `Set.notMember` is)] $ \ (n,t) -> do
        t <- draw t
        return $ t <+> tshow n <> semi
    return $ vcat uv' $$ body

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
    err s = (Exp typeHint (err s))


instance Draw Name where
    draw (Name s) = text s
    err s = Name $ terr s

instance Draw Constant where
    draw (C x) = x
    err s = C $ terr s

instance Draw Type where
    draw (TB x _) = text x
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
    draw (TFunPtr r as) = draw r <+> text "(*)" <> tupled (map draw as)

    err s = TB (terr s) False

-- expressions
sizeof :: Type -> Expression
sizeof t = expC (text "sizeof" <> parens $ draw t)

cast :: Type -> Expression -> Expression
cast t e = expDC (parens (draw t) <> pdraw e)


functionCall' fe es = expD (draw fe <> tupled (map draw es))

functionCall :: Name -> [Expression] -> Expression
functionCall = functionCall'

indirectFunctionCall :: Expression -> [Expression] -> Expression
indirectFunctionCall e = functionCall' (expD (parens (draw e)))

dereference :: Expression -> Expression
dereference x = expDC $ char '*' <> pdraw x

noAssign :: Expression -> Expression
noAssign (Exp th v) = Exp th { thNoAssign = True } v

reference :: Expression -> Expression
reference x = expDC $ char '&' <> pdraw x

indexArray :: Expression -> Expression -> Expression
indexArray w i = expDO (pdraw w <> char '[' <> pdraw i <> char ']')

project :: Name -> Expression -> Expression
project n e = expDO (pdraw e <> char '.' <> draw n)

project' :: Name -> Expression -> Expression
project' n e = expDO (pdraw e <> text "->" <> draw n)

projectAnon :: Int -> Expression -> Expression
projectAnon n e = project (Name $ 't':show n) e

variable :: Name -> Expression
variable n = expDO (draw n)

localVariable :: Type -> Name -> Expression
localVariable t n = expD $ do
    tell [(n,t)]
    draw n

statementOOB :: Statement -> Statement
statementOOB s = (sd $ draw s >> return empty)



instance Monoid Statement where
    mempty = St mempty
    mappend (St as) (St bs) = St $ pairOpt stmtPairOpt as bs

stmtPairOpt a b = f a b where
    f (SGoto l) y@(SLabel l')
        | l == l' = Just y
        | otherwise = Nothing
    f SReturn {} SLabel {} = Nothing
    f x@SGoto {} _  = Just x
    f x@SReturn {} _  = Just x
    f _ _ = Nothing

-- combine two sequences, attempting pairwise peephole optimizations

pairOpt :: (s -> s -> Maybe s) -> Seq.Seq s -> Seq.Seq s -> Seq.Seq s
pairOpt peep as bs = f as bs where
    f as bs | as' Seq.:> a <- Seq.viewr as, b Seq.:< bs' <- Seq.viewl bs = case peep a b of
        Just ab -> as' `f` Seq.singleton ab `f` bs'
        Nothing -> as Seq.>< bs
    f as bs =  as Seq.>< bs


emptyExpression = Exp typeHint EE

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
structAnon es = Exp typeHint $ ED $ do
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
string s = Exp hintPtr (ED (return $ text (show s))) -- TODO, use C quoting conventions

nullPtr = Exp hintPtr (ED $ text "NULL")

name = Name

expDO x = Exp typeHint { thOmittable = True } (ED x)
expD x = Exp typeHint (ED x)
expDC x = Exp hintConst (EP $ ED x)
expC x = Exp hintConst (ED x)

-- Constant
enum :: Name -> Constant
enum n = C (draw n)

number :: Integer -> Constant
number i = C (tshow i)

floating :: Double -> Constant
floating i = C (tshow i)

character :: Char -> Constant
character c = C (tshow c)

cTrue = C (text "true")
cFalse = C (text "false")

-- statements
expr :: Expression -> Statement
expr e | isEmptyExpression e = mempty
expr e = sd $ draw e <> char ';'


creturn :: Expression -> Statement
creturn e = stmt $ SReturn e

assign :: Expression -> Expression -> Statement
assign (Exp TypeHint { thNoAssign = True} _) (Exp TypeHint { thOmittable = True } _)  = mempty
assign (Exp TypeHint { thNoAssign = True} _) b = expr b
assign a b = expr $ operator "=" a b

label :: Name -> Statement
label n = stmt $ SLabel n

goto :: Name -> Statement
goto n = stmt $ SGoto n

massign a b = if isEmptyExpression b then toStatement a else assign a b

newTmpVar t e = do
    u <- newUniq
    let n = name $ 'x':show u
        d = sd $ do
            va <- draw (variable n `massign` e)
            t <- draw t
            return $ t <+> va
    return (d,variable n)

newAssignVar t n e = do
    let d = sd $ do
            va <- draw (variable n `massign` e)
            t <- draw t
            return $ t <+> va
    return d

newVar t = do
    u <- newUniq
    let n = name $ 'x':show u
    return (localVariable t n)


newDeclVar t = do
    u <- newUniq
    let n = name $ 'x':show u
    return (sd (tell [(n,t)] >> return mempty),variable n)


labelPull :: Statement -> (Statement,Statement)
labelPull (St ss) = f ss mempty where
    f ss rr | ss' Seq.:> l@SLabel {} <- Seq.viewr ss = f ss' (Seq.singleton l `mappend` rr)
            | otherwise = (St ss,St rr)



switch' :: Expression -> [(Maybe Constant,Statement)]  -> Statement
switch' e es = (stmt $ SSwitch e es') `mappend` ls where
     (es',ls) = runWriter $ mapM f es
     f (c,s) = tell l >> return (c,s') where (s',l) = labelPull s


cif :: Expression -> Statement -> Statement -> Statement
cif exp thn els = (stmt $ SIf exp thn' els') `mappend` la `mappend` lb where
    (thn',la) = labelPull thn
    (els',lb) = labelPull els

subBlock st = stmt (SBlock st') `mappend` la  where
    (st',la) = labelPull st

forLoop :: Expression -> Expression -> Expression -> Statement -> Statement
forLoop i from to body = sd $ do
    i <- draw i
    from <- draw from
    to <- draw to
    body <- draw body
    return $ text "for" <> parens (i <+> equals <+> from <> semi <+> i <+> text "<" <+> to <> semi <+> i <> text "++" ) <+> lbrace <$> nest 4 body <$> rbrace



data Function = F {
    functionName :: Name,
    functionReturnType :: Type,
    functionArgs :: [(Name,Type)],
    functionOptions :: [FunctionOpts],
    functionBody :: Statement
    }

data FunctionOpts = Public | Attribute String
    deriving(Eq)

function :: Name -> Type -> [(Name,Type)] -> [FunctionOpts] -> Statement -> Function
function n t as o s = F n t as o s


drawFunction :: Function -> G (Doc,Doc)
drawFunction f = do
    frt <- draw (functionReturnType f)
    cenv <- ask
    let env = cenv { envUsedLabels = ul, envInScope = Set.fromList $ fsts (functionArgs f) } where
        ul = Set.fromList $ Seq.toList $ Seq.foldMap (travCollect stmtMapStmt g) stseq
        St stseq = functionBody f
        g (SGoto n) = Seq.singleton n
        g s = mempty
        vcmp (n,t@(TB _ b)) = (not b,n)
        vcmp (n,t) = (True,n)
    (body,uv) <- local (const env) $ listen (draw (functionBody f))
    uv' <- forM [ (x,t) | (x,t) <- snubUnder vcmp uv, x `notElem` fsts (functionArgs f)] $ \ (n,t) -> do
        t <- draw t
        return $ t <+> tshow n <> semi
    name <- draw (functionName f)
    fas <- forM (functionArgs f) $ \ (n,t) -> do
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
basicType s = TB s False

-- | a basic type the garbage collector might want to follow, guarenteed to be
-- the size of a pointer.
basicGCType :: String -> Type
basicGCType s = TB s True

structType :: Name -> Type
structType n = TNStruct n

ptrType :: Type -> Type
ptrType t = TPtr t

funPtrType :: Type -> [Type] -> Type
funPtrType r as = TFunPtr r as

-- type constants
voidStarType :: Type
voidStarType = ptrType voidType

voidType :: Type
voidType = basicType "void"



class Annotate e where
    annotate :: String -> e -> e

instance Annotate Statement where
    --annotate c s@(SD si _) = SD si ((text "/* " <> text c <> text " */") <$> draw s)
    annotate c s = sd (text "/* " <> text c <> text " */") `mappend` s


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

data Structure = Structure {
    structureName :: Name,
    structureFields :: [(Name,Type)],
    structureNeedsDiscriminator :: Bool,    -- ^ emit a macro that declares a discriminator when needed
    structureHasDiscriminator   :: Bool,    -- ^ the first field must appear first in the on memory layout, don't move it.
    structureAligned :: Bool                -- ^ this structure needs to be aligned to a pointer boundry, even if it woudn't be otherwise.
    }

basicStructure = Structure {
    structureName = error "basicStructure: Name",
    structureFields = [],
    structureNeedsDiscriminator = False,
    structureHasDiscriminator   = False,
    structureAligned            = False
    }

generateC
    :: [Function]              -- ^ functions
    -> [Structure]             -- ^ extra structures
    -> (Doc,Doc)               -- ^ final result
generateC fs ss = ans where
    G ga = do
        fs <- mapM drawFunction fs
        let (protos,bodys) = unzip fs
        let shead = vcat [ text "struct" <+> tshow (structureName s) <> (if structureAligned s then text " A_ALIGNED" else empty) <> text ";" | s <- ss ]
        shead2 <- declStructs ss
        return (shead $$ line $$ shead2 $$ vcat protos, line $$  vsep bodys)
    ((hd,fns),(_,ass),_written) = runRWS ga emptyEnv (1,Map.empty)

    anons = [ basicStructure { structureName = n, structureFields = fields ts }  | (ts,n) <- Map.toList ass ] where
        fields :: [Type] -> [(Name,Type)]
        fields ts = [ (name ('t':show tn),t) | t <- ts | tn <- [0::Int .. ]]
    G anons' = declStructs anons
    (anons'',_,_) = runRWS anons' emptyEnv (1,Map.empty)
    ans = (hd $$ anons'',fns)

    declStructs ss = liftM vsep $ forM ss $ \ s@Structure { structureName = n, structureFields = ts } -> do
        let tsort [] = []
            tsort (t:ts) | structureHasDiscriminator s = t:rsort ts
                         | otherwise = rsort (t:ts)
            rsort = sortUnder (cmp . snd)
            numGC = length [ () | (_,TB _ True) <- ts ]
            ppri = if numGC /= 0 then 2 else 5
            -- pointers first, garbage collected, then rest
            cmp (TB _ True) = (1::Int)
            cmp TFunPtr {}  = ppri
            cmp TPtr {}     = ppri
            cmp (TB s _)    = maybe 5  id (lookup s tmap)
            cmp _ = 5
            tmap = [ "uintmax_t" ==> 3, "uintptr_t" ==> ppri, "double" ==> 4, "uint32_t" ==> 6, "float" ==> 6, "uint16_t" ==> 7, "uint8_t" ==> 8]
            x ==> y = (x,y)

        ts' <- forM (tsort ts) $ \ (n,t) -> do
            t <- draw t
            return $ t <+> tshow n <> semi
        return $ text "struct" <+> tshow n <+> lbrace $$ nest 4 (vcat $ (if structureNeedsDiscriminator s  then text "what_t what;" else empty):ts') $$ rbrace <> semi


line = text ""
vsep xs = vcat $ intersperse line xs


instance Show Expression where
    show e = renderG e

renderG x = render $ drawG x

drawG :: Draw d => d -> Doc
drawG x = fns where
    G ga = draw x
    (fns,_,_) = runRWS ga emptyEnv (1,Map.empty)


------------
-- C helpers
------------

infix 3 `eq`

eq :: Expression -> Expression -> Expression
eq = operator "=="

infix 2 =*

(=*) :: Expression -> Expression -> Statement
x =* y = x `assign` y

class ToStatement a  where
    toStatement :: a -> Statement

instance ToStatement Statement where
    toStatement x = x

instance ToStatement Expression where
    toStatement x = expr x

class ToExpression a where
    toExpression :: a -> Expression

instance ToExpression Expression where
    toExpression e = e

instance ToExpression Constant where
    toExpression c = constant c

instance ToExpression Name where
    toExpression c = variable c

instance ToExpression Char where
    toExpression c = constant $ character c


infixl 1 &

(&) :: (ToStatement a,ToStatement b) => a -> b -> Statement
x & y = toStatement x `mappend` toStatement y

