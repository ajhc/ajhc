
module C.FromGrin(compileGrin) where

import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.RWS
import Control.Monad.State
import Data.Monoid
import List(intersperse)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Text.PrettyPrint.HughesPJ as P
import Text.PrettyPrint.HughesPJ(nest,($$))

import Atom
import Support.CanType
import Grin.Noodle
import C.FFI
import C.Generate
import C.Prims
import Doc.DocLike
import Doc.PPrint
import Support.FreeVars
import GenUtil
import Grin.Grin
import Grin.HashConst
import Grin.Show
import RawFiles
import Util.UniqueMonad


newtype C a = C (RWST (Todo,Map.Map Atom (Name,[Expression])) Requires HcHash Uniq a)
    deriving(Monad,UniqueProducer,MonadState HcHash)

data Todo = TodoReturn | TodoExp Expression | TodoNothing

runC :: C a -> (a,HcHash,Requires)
runC (C m) =  execUniq1 (runRWST m (TodoNothing,mempty) emptyHcHash)


fetchVar :: Var -> Ty -> C Expression
fetchVar v@(V n) _ | n < 0 = return $ (variable  $ varName v)
fetchVar v ty = do
    t <- convertType ty
    return $ (localVariable t (varName v))

varName (V n) | n < 0 = name $ 'g':show (- n)
varName (V n) = name $ 'v':show n

node_t = basicType "node_t"
pnode_t = ptrType node_t
size_t = basicType "size_t"
tag_t = basicType "tag_t"

profile_update_inc = expr $ functionCall (name "update_inc") []
profile_case_inc = expr $ functionCall (name "case_inc") []
profile_function_inc = expr $ functionCall (name "function_inc") []

convertVal :: Val -> C Expression
convertVal (Var v ty) = fetchVar v ty
convertVal (Const (NodeC h _)) | h == tagHole = return nullPtr
convertVal (Const h) = do
    (_,i) <- newConst h
    return $ variable (name $  'c':show i )
convertVal h@NodeC {} | valIsConstant h = do
    (_,i) <- newConst h
    return $ variable (name $  'c':show i )
convertVal (Lit i _) = return (constant $ number (fromIntegral i))
convertVal (Tup [x]) = convertVal x
convertVal (Tup []) = return emptyExpression
convertVal (Tup xs) = do
    ts <- mapM convertType (map getType xs)
    xs <- mapM convertVal xs
    return (structAnon (zip xs ts))
convertVal (Tag t) = return $ constant (enum $ nodeTagName t)
convertVal (ValPrim (APrim p _) [] _) = case p of
    CConst s _ -> return $ expressionRaw s
    AddrOf t -> return $ expressionRaw ('&':t)
    PrimTypeInfo { primArgType = arg, primTypeInfo = PrimSizeOf } -> return $ expressionRaw ("sizeof(" ++ arg ++ ")")
    x -> return $ err ("convertVal: " ++ show x)
convertVal (ValPrim (APrim p _) [x] _) = do
    x' <- convertVal x
    case p of
        CCast _ to -> return $ cast (basicType to) x'
        Operator n [_] r ->  return $ cast (basicType r) (uoperator n x')
        x -> return $ err ("convertVal: " ++ show x)
convertVal (ValPrim (APrim p _) [x,y] _) = do
    x' <- convertVal x
    y' <- convertVal y
    case p of
        Operator n [_,_] r -> return $ cast (basicType r) (operator n x' y')
        x -> return $ err ("convertVal: " ++ show x)

convertVal x = return $ err ("convertVal: " ++ show x)

convertExp :: Exp -> C (Statement,Expression)
convertExp (Error s t) = do
    let f (TyPtr _) = return nullPtr
        f TyNode = return nullPtr
        f (TyTup []) = return emptyExpression
        f (TyTup xs) = do ts <- mapM convertType xs; xs <- mapM f xs ; return $ structAnon (zip xs ts)
        f (Ty x) = return $ cast (basicType (show x)) (constant $ number 0)
        f TyTag  = return $ constant (enum $ nodeTagName tagHole)
        f x = return $ err $ "error-type " ++ show x
    ev <- f t
    if null s
      then return (expr $ functionCall (name "jhc_exit") [constant $ number 255],ev)
       else return (expr $ functionCall (name "jhc_error") [string s],ev)
convertExp (App a vs _) = do
    lm <- C $ asks snd
    vs' <- mapM convertVal vs
    case a `Map.lookup` lm of
        Just (nm,as) -> do
            let ss = [ a `assign` v | a <- as | v <- vs' ]
            return (mconcat ss `mappend` goto nm, emptyExpression)
        Nothing -> return $ (mempty, functionCall (toName (toString a)) vs')
convertExp (Fetch v) = do
    v <- convertVal v
    return (mempty,v)
convertExp (Store n@NodeV {}) = newNode n
convertExp (Return n@NodeV {}) = newNode n
convertExp (Store n@NodeC {}) = newNode n
convertExp (Return n@NodeC {}) = newNode n
convertExp (Store n@Var {}) = do
    (ss,nn) <- newNode (NodeC tagHole [])
    tmp <- newVar pnode_t
    n <- convertVal n
    let tag = project' anyTag n
        update = expr (functionCall (name "memcpy") [tmp,n,functionCall  (name "jhc_sizeof") [tag]])
    return (ss `mappend` (tmp `assign` nn) `mappend` update, tmp)
convertExp (Return v) = do
    v <- convertVal v
    return (mempty,v)
convertExp (Prim p vs) | APrim _ req <- primAPrim p  =  do
    addRequires req
    e <- convertPrim p vs
    return (mempty,e)
convertExp (Update v@Var {} (NodeC t as)) = do
    v' <- convertVal v
    as' <- mapM convertVal as
    nt <- nodeTypePtr t
    let tmp' = cast nt v'
        s = project' tag tmp' `assign` constant (enum (nodeTagName t))
        ass = [project' (arg i) tmp' `assign` a | a <- as' | i <- [(1 :: Int) ..] ]
    return (mconcat $ profile_update_inc:s:ass,emptyExpression)
convertExp (Update v@Var {} (NodeV t [])) = do
    v' <- convertVal v
    t' <- convertVal (Var t TyTag)
    let tag = project' anyTag v'
    return (tag `assign` t',emptyExpression)

convertExp (Update v z) = do  -- TODO eliminate unknown updates
    v' <- convertVal v
    z' <- convertVal z
    let tag = project' anyTag z'
    return $ (profile_update_inc,functionCall (name "memcpy") [v',z',functionCall  (name "jhc_sizeof") [tag]])
convertExp e = return (err (show e),err "nothing")

convertType TyTag = return tag_t
convertType TyNode = return pnode_t
convertType (TyPtr TyNode) = return pnode_t
convertType (Ty t) = return (basicType (toString t))
convertType (TyTup []) = return voidType
convertType (TyTup [x]) = convertType x
convertType (TyTup xs) = do
    xs <- mapM convertType xs
    return (anonStructType xs)

addRequires req = C $ tell req

nodeTagName :: Atom -> Name
nodeTagName a = toName (toString a)

nodeFuncName :: Atom -> Name
nodeFuncName a = toName (toString a)

nodeStructName :: Atom -> Name
nodeStructName a = toName ('s':toString a)

nodeType a = return $ structType (nodeStructName a)
nodeTypePtr a = liftM ptrType (nodeType a)

jhc_malloc sz = functionCall (name "jhc_malloc") [sz]
jhc_malloc_atomic sz = functionCall (name "jhc_malloc_atomic") [sz]
tag = name "tag"
anyTag = name "any.tag"
arg i = name $ 'a':show i

newNode (NodeV t []) = do
    tmp <- newVar pnode_t
    var <- fetchVar t TyTag
    let tmp' = project' anyTag tmp
        malloc =  tmp `assign` jhc_malloc (sizeof  node_t)
        tagassign = tmp' `assign` var
    return (mappend malloc tagassign, tmp)
newNode (NodeC t _) | t == tagHole = do
    return $  (mempty,jhc_malloc (sizeof node_t))
newNode (NodeC t as) = do
    st <- nodeType t
    as' <- mapM convertVal as
    tmp <- newVar pnode_t
    let tmp' = project' (nodeStructName t) tmp
        malloc =  tmp `assign` wmalloc (sizeof  (if tagIsWHNF t then st else node_t))
        tagassign = project tag tmp' `assign` constant (enum $ nodeTagName t)
        wmalloc = if tagIsWHNF t && all (nonPtr . getType) as then jhc_malloc_atomic else jhc_malloc
        ass = [ project (arg i) tmp' `assign` a | a <- as' | i <- [(1 :: Int) ..] ]
        nonPtr TyPtr {} = False
        nonPtr TyNode = False
        nonPtr (TyTup xs) = all nonPtr xs
        nonPtr _ = True
    return (mconcat $ malloc:tagassign:ass, cast pnode_t tmp)

--convertPrim p vs = return (mempty,err $ show p)
convertPrim p vs
    | APrim (CConst s _) _ <- primAPrim p = do
        return $ expressionRaw s
    | APrim (CCast _ to) _ <- primAPrim p, [a] <- vs = do
        a' <- convertVal a
        return $ cast (basicType to) a'
    | APrim (Operator n [ta] r) _ <- primAPrim p, [a] <- vs = do
        a' <- convertVal a
        return $ cast (basicType r) (uoperator n a')
    | APrim (Operator n [ta,tb] r) _ <- primAPrim p, [a,b] <- vs = do
        a' <- convertVal a
        b' <- convertVal b
        return $ cast (basicType r) (operator n a' b')
    | APrim (Func _ n as r) _ <- primAPrim p = do
        vs' <- mapM convertVal vs
        return $ cast (basicType r) (functionCall (name n) [ cast (basicType t) v | v <- vs' | t <- as ])
    | APrim (Peek t) _ <- primAPrim p, [v] <- vs = do
        v' <- convertVal v
        return $ expressionRaw ("*((" <> t <+> "*)" <> (parens $ renderG v') <> char ')')
    | APrim (Poke t) _ <- primAPrim p, [v,x] <- vs = do
        v' <- convertVal v
        x' <- convertVal x
        return $ expressionRaw ("*((" <> t <+> "*)" <> (parens $ renderG v') <> text ") = " <> renderG x')
    | APrim (AddrOf t) _ <- primAPrim p, [] <- vs = do
        return $ expressionRaw ('&':t)


localJumps xs (C action) = C $ local (\ (x,y) -> (x,Map.fromList xs `mappend` y)) action

convertBody :: Exp -> C Statement
convertBody (Prim p [a,b] :>>= Tup [q,r] :-> e') | primName p == toAtom "@primQuotRem" = do
    a' <- convertVal a
    b' <- convertVal b
    r' <- convertVal r
    q' <- convertVal q
    ss' <- convertBody e'
    return $ mconcat [ assign q' (operator "/" a' b'), assign r' (operator "%" a' b'), ss' ]

convertBody Let { expDefs = defs, expBody = body } = do
    u <- newUniq
    nn <- flip mapM defs $ \FuncDef { funcDefName = name, funcDefBody = Tup as :-> _ } -> do
        vs' <- mapM convertVal as
        let nm = (toName (show name ++ show u))
        return (name,(nm,vs'))
    localJumps nn $ do
    let done = (toName $ "done" ++ show u)
    ss <- (convertBody body)
    rs <- flip mapM defs $ \FuncDef { funcDefName = name, funcDefBody = Tup as :-> b } -> do
       ss <- convertBody b
       return (annotate (show as) (label (toName (show name ++ show u))) `mappend` indentBlock ss)
    return (ss `mappend` goto done `mappend` mconcat (intersperse (goto done) rs) `mappend` label done);


convertBody (Return v :>>= (NodeC t as) :-> e') = nodeAssign v t as e'
convertBody (Fetch v :>>= (NodeC t as) :-> e') = nodeAssign v t as e'
convertBody (Return v :>>= (NodeV t []) :-> e') = nodeAssignV v t e'
convertBody (Fetch v :>>= (NodeV t []) :-> e') = nodeAssignV v t e'
convertBody (e :>>= v@(Var _ _) :-> e') = do
    v' <- convertVal v
    ss <- localTodo (TodoExp v')  (convertBody e)
    ss' <- convertBody e'
    return (ss `mappend` ss')
convertBody (e :>>= Tup [] :-> e') = do
    ss <- localTodo TodoNothing (convertBody e)
    ss' <- convertBody e'
    return (ss `mappend` ss')
convertBody (e :>>= Tup [x] :-> e') = convertBody (e :>>= x :-> e')
convertBody (e :>>= Tup xs :-> e') = do
    ts <- mapM ( convertType . getType) xs
    st <- newVar (anonStructType ts)
    ss <- localTodo (TodoExp st) (convertBody e)
    ss' <- convertBody e'
    vs <- mapM convertVal xs
    return $  ss `mappend` mconcat [ v `assign` projectAnon i st | v <- vs | i <- [0..] ] `mappend` ss'

convertBody (Case v@(Var _ ty) [p1@(NodeC t _) :-> e1,p2 :-> e2]) | ty == TyNode = do
    scrut <- convertVal v
    let tag = project' anyTag scrut
        da v@Var {} _ = do
            v'' <- convertVal v
            return $ assign v'' scrut
        da n1@(NodeC t _) (Return n2@NodeC {}) | n1 == n2 = convertBody (Return v)
        da (NodeC t as) e = do
            as' <- mapM convertVal as
            let tmp = project' (nodeStructName t) scrut
                ass = mconcat [if needed a then assign  a' (project (arg i) tmp) else mempty | a' <- as' | a <- as | i <- [(1 :: Int) ..] ]
                fve = freeVars e
                needed (Var v _) = v `Set.member` fve
            return ass
        am | isVar p2 = id
           | otherwise = annotate (show p2)
    e1' <- convertBody e1
    e2' <- convertBody e2
    p1' <- da p1 e1
    p2' <- liftM am $ da p2 e2
    return $ profile_case_inc `mappend` cif (operator "==" (constant $ enum (nodeTagName t)) tag) (p1' `mappend` e1') (p2' `mappend` e2')

convertBody (Case v@(Var _ t) ls) | t == TyNode = do
    scrut <- convertVal v
    let tag = project' anyTag scrut
        da (v@(Var {}) :-> e) = do
            v'' <- convertVal v
            e' <- convertBody e
            return $ (Nothing,assign v'' scrut `mappend` e')
        da (n1@(NodeC t _) :-> Return n2@NodeC {}) | n1 == n2 = do
            e' <- convertBody (Return v)
            return (Just (enum (nodeTagName t)),e')
        da ((NodeC t as) :-> e) = do
            as' <- mapM convertVal as
            e' <- convertBody e
            let tmp = project' (nodeStructName t) scrut
                ass = mconcat [if needed a then assign  a' (project (arg i) tmp) else mempty | a' <- as' | a <- as | i <- [(1 :: Int) ..] ]
                fve = freeVars e
                needed (Var v _) = v `Set.member` fve
            return $ (Just (enum (nodeTagName t)), ass `mappend` e')
    ls' <- mapM da ls
    return $ profile_case_inc `mappend` switch' tag ls'

convertBody (Case v@(Var _ t) [p1 :-> e1, p2 :-> e2]) | Set.null ((freeVars p2 :: Set.Set Var) `Set.intersection` freeVars e2) = do
    scrut <- convertVal v
    let ptrs = [Ty $ toAtom "HsPtr", Ty $ toAtom "HsFunPtr"]
        scrut' = (if t `elem` ptrs then cast (basicType "uintptr_t") scrut else scrut)
        cp (Lit i _) = constant (number $ fromIntegral i)
        cp (Tag t) = constant (enum (nodeTagName t))
        am | isVar p2 = id
           | otherwise = annotate (show p2)
    e1' <- convertBody e1
    e2' <- convertBody e2
    return $ profile_case_inc `mappend` cif (operator "==" (cp p1) scrut') e1' (am e2')

convertBody (Case v@(Var _ t) ls) = do
    scrut <- convertVal v
    let ptrs = [Ty $ toAtom "HsPtr", Ty $ toAtom "HsFunPtr"]
        scrut' = (if t `elem` ptrs then cast (basicType "uintptr_t") scrut else scrut)
        da (v@(Var {}) :-> e) = do
            v'' <- convertVal v
            e' <- convertBody e
            return (Nothing,assign v'' scrut `mappend` e')
        da ((Lit i _) :-> e) = do
            e' <- convertBody e
            return $ (Just (number $ fromIntegral i), e')
        da (Tag t :-> e) = do
            e' <- convertBody e
            return $ (Just (enum (nodeTagName t)), e')
        da (Tup [x] :-> e) = da ( x :-> e )
    ls' <- mapM da ls
    return $ profile_case_inc `mappend` switch' scrut' ls'


convertBody e = do
    (x,_) <- C ask
    (ss,er) <- convertExp e -- lift $  runSubCGen $ cexp e
    case x of
        TodoReturn -> return (ss `mappend` creturn er)
        TodoExp v | isEmptyExpression er -> return ss
        TodoExp v -> return (ss `mappend` (v `assign` er))
        TodoNothing | isEmptyExpression er -> return ss
        TodoNothing -> return (ss `mappend` expr er)

convertBody e = return $ err (show e)

nodeAssign v t as e' = do
    v' <- convertVal v
    let tmp = project' (nodeStructName t) v'
        fve = freeVars e'
    as' <- mapM convertVal as
    let ass = concat [perhapsM (a `Set.member` fve) $ assign  a' (project (arg i) tmp) | a' <- as' | Var a _ <- as |  i <- [( 1 :: Int) ..] ]
    ss' <- convertBody e'
    return $  mconcat ass `mappend` ss'

nodeAssignV v t e' = do
    v' <- convertVal v
    var <- fetchVar t TyTag
    ss' <- convertBody e'
    return $ assign var (project' anyTag v') `mappend` ss'


localTodo :: Todo -> C a -> C a
localTodo todo (C act) = C $ local (\ (_,y) -> (todo,y)) act


convertFunc :: (Atom,Lam) -> C Function
convertFunc (n,Tup as :-> body) = do
        s <- localTodo TodoReturn (convertBody body)
        let bt = getType body
            mmalloc (TyPtr _) = [Attribute "A_MALLOC"]
            mmalloc TyNode = [Attribute "A_MALLOC"]
            mmalloc _ = []
            ats = Attribute "A_REGPARM":mmalloc bt
        fr <- convertType bt
        as' <- flip mapM as $ \ (Var v t) -> do
            t' <- convertType t
            return (varName v,t')
        return $ function (nodeFuncName n) fr as' ats (profile_function_inc `mappend` s)


convertFfiExport :: (Atom,Lam) -> FfiExport -> C Function
convertFfiExport (n,Tup as :-> body) (FfiExport cn Safe CCall) = do
        s <- localTodo TodoReturn (convertBody body)
        let bt = getType body
            mmalloc (TyPtr _) = [Attribute "A_MALLOC"]
            mmalloc TyNode = [Attribute "A_MALLOC"]
            mmalloc _ = []
            ats = Public : mmalloc bt
        fr <- convertType bt
        as' <- flip mapM as $ \ (Var v t) -> do
            t' <- convertType t
            return (varName v,t')
        return $ function (name cn) fr as' ats (profile_function_inc `mappend` s)


{-# NOINLINE compileGrin #-}
compileGrin :: Grin -> (String,[String])
compileGrin grin = (hsffi_h ++ jhc_rts_c ++ P.render ans ++ "\n", snub (reqLibraries req))  where
    ans = vsep $ [vcat includes,enum_tag_t,header,union_node,text "/* CAFS */", vcat $ map ccaf (grinCafs grin), text "/* Constant Data */", jhc_sizeof_data, buildConstants finalHcHash,text  "/* Functions */",jhc_sizeof,body]
    includes =  map include (snub $ reqIncludes req)
    (header,body) = generateC (functions) structs

    -- this is a list of every tag used in the program
    tags = (tagHole,[]):sortUnder (show . fst) [ (t,runIdentity $ findArgs (grinTypeEnv grin) t) | t <- Set.toList $ freeVars (snds $ grinFuncs grin) `mappend` freeVars (snds $ grinCafs grin), tagIsTag t]
    ((functions,structs),finalHcHash,req) = runC $ do
        funcs <- flip mapM (grinFuncs grin) $ \(a,l) -> do
                   case Map.lookup a (grinEntryPoints grin) of
                     Nothing -> convertFunc  (a,l)
                     Just fe -> convertFfiExport (a,l) fe
        sts <- flip mapM tags $ \ (n,ts) -> do
            ts' <- mapM convertType ts
            return (nodeStructName n,zip [ name $ 'a':show i | i <-  [1 ..] ] ts')
        return (funcs,sts)


    enum_tag_t = text "typedef enum {" $$ nest 4 (P.fsep (punctuate P.comma (map (tshow . nodeTagName . fst) tags))) $$ text  "} tag_t;"
    jhc_sizeof_data =  text $ "static const uint8_t JHC_SIZEOF[] = {\n" ++ concatMap sizeof (fsts tags) ++ "};"  where
        sizeof t = text "  sizeof(struct " <> tshow (nodeStructName t) <>  text "),\n"
    jhc_sizeof = text "static inline size_t jhc_sizeof(tag_t tag) { return (size_t) JHC_SIZEOF[tag]; }"
    union_node = text $  "union node {\n  struct { tag_t tag; } any;\n" <> mconcat (map cu (fsts tags)) <> text "};" where
        cu t = text "  struct" <+> (tshow $ nodeStructName t) <+> (tshow $ nodeStructName t) <> text ";\n"

include fn = text "#include <" <> text fn <> text ">"
line = text ""
vsep xs = vcat $ intersperse line xs

buildConstants fh = P.vcat (map cc (Grin.HashConst.toList fh)) where
    cc nn@(HcNode a zs,i) = comm $$ cd $$ def where
        comm = text "/* " <> tshow (nn) <> text " */"
        cd = text "static struct " <> tshow (nodeStructName a) <+> text "_c" <> tshow i <+> text "= {" <> hsep (punctuate P.comma (tshow (nodeTagName a):rs)) <> text "};"
        def = text "#define c" <> tshow i <+> text "((node_t *)&_c" <> tshow i <> text ")"
        rs = [ f z undefined |  z <- zs ]
        f (Right i) = text $ 'c':show i
        f (Left (Var n _)) = tshow $ varName n
        f (Left v) | Just e <- convertConst v = text (show $ drawG e)
--        f (Left (Lit i _)) = tshow i
--        f (Left vp@(ValPrim {})) = tshow i
--        f (Left (Tag t)) = tshow (nodeTagName t)

convertConst :: Monad m => Val -> m Expression
convertConst (Const (NodeC h _)) | h == tagHole = return nullPtr
convertConst (Lit i _) = return (constant $ number (fromIntegral i))
convertConst (Tup [x]) = convertConst x
convertConst (Tup []) = return emptyExpression
convertConst (Tag t) = return $ constant (enum $ nodeTagName t)
convertConst (ValPrim (APrim p _) [] _) = case p of
    CConst s _ -> return $ expressionRaw s
    AddrOf t -> return $ expressionRaw ('&':t)
    x -> return $ err (show x)
convertConst (ValPrim (APrim p _) [x] _) = do
    x' <- convertConst x
    case p of
        CCast _ to -> return $ cast (basicType to) x'
        Operator n [_] r ->  return $ cast (basicType r) (uoperator n x')
        x -> return $ err (show x)
convertConst (ValPrim (APrim p _) [x,y] _) = do
    x' <- convertConst x
    y' <- convertConst y
    case p of
        Operator n [_,_] r -> return $ cast (basicType r) (operator n x' y')
        x -> return $ err (show x)

convertConst x = fail "convertConst"



ccaf :: (Var,Val) -> P.Doc
ccaf (v,val) = text "/* " <> text (show v) <> text " = " <> (text $ render (prettyVal val)) <> text "*/\n" <> text "static node_t _" <> tshow (varName v) <> text ";\n" <> text "#define " <> tshow (varName v) <+>  text "(&_" <> tshow (varName v) <> text ")\n";




