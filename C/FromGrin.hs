
module C.FromGrin(compileGrin) where

import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.RWS
import Control.Monad.State
import Data.Monoid
import List(intersperse)
import qualified Data.Set as Set
import qualified Text.PrettyPrint.HughesPJ as P
import Text.PrettyPrint.HughesPJ(nest,($$))

import Atom
import Support.CanType
import C.Generate
import C.Prims
import Doc.DocLike
import Doc.PPrint
import FreeVars
import GenUtil
import Grin.Grin
import Grin.HashConst
import Grin.Show
import RawFiles
import Util.UniqueMonad


newtype C a = C (RWST Todo Requires HcHash Uniq a)
    deriving(Monad,UniqueProducer,MonadState HcHash)

data Todo = TodoReturn | TodoExp Expression | TodoNothing

runC :: C a -> (a,HcHash,Requires)
runC (C m) =  execUniq1 (runRWST m TodoNothing emptyHcHash)


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
convertVal (Lit i _) = return (constant $ number (fromIntegral i))
convertVal (Tup [x]) = convertVal x
convertVal (Tup []) = return emptyExpression
convertVal (Tup xs) = do
    ts <- mapM convertType (map getType xs)
    xs <- mapM convertVal xs
    return (structAnon (zip xs ts))
convertVal (Tag t) = return $ constant (enum $ nodeTagName t)
convertVal x = return $ err (show x)

convertExp :: Exp -> C (Statement,Expression)
convertExp (Error s t) = do
    let f (TyPtr _) = return nullPtr
        f TyNode = return nullPtr
        f (TyTup []) = return emptyExpression
        f (TyTup xs) = do ts <- mapM convertType xs; xs <- mapM f xs ; return $ structAnon (zip xs ts)
        f (Ty x) = return $ cast (basicType (show x)) (constant $ number 0)
        f x = return $ err $ "error-type " ++ show x
    ev <- f t
    return (expr $ functionCall (name "jhc_error") [string s],ev)
convertExp (App a vs _) = do
    vs' <- mapM convertVal vs
    return $ (mempty, functionCall (toName (toString a)) vs')
convertExp (Fetch v) = do
    v <- convertVal v
    return (mempty,v)
convertExp (Store n@NodeV {}) = newNode n
convertExp (Return n@NodeV {}) = newNode n
convertExp (Store n@NodeC {}) = newNode n
convertExp (Return n@NodeC {}) = newNode n
convertExp (Return v) = do
    v <- convertVal v
    return (mempty,v)
convertExp (Cast x t) = do
    x' <- convertVal x
    t' <- convertType t
    return $ (mempty, cast t' x')
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
        malloc =  tmp `assign` jhc_malloc (sizeof  (if tagIsWHNF t then st else node_t))
        tagassign = project tag tmp' `assign` constant (enum $ nodeTagName t)
        ass = [ project (arg i) tmp' `assign` a | a <- as' | i <- [(1 :: Int) ..] ]
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


convertBody :: Exp -> C Statement
convertBody (Prim p [a,b] :>>= Tup [q,r] :-> e') | primName p == toAtom "@primQuotRem" = do
    a' <- convertVal a
    b' <- convertVal b
    r' <- convertVal r
    q' <- convertVal q
    ss' <- convertBody e'
    return $ mconcat [ assign q' (operator "/" a' b'), assign r' (operator "%" a' b'), ss' ]


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

convertBody (Case v@(Var _ t) ls) | t == TyNode = do
    scrut <- convertVal v
    let tag = project' anyTag scrut
        da (v@(Var {}) :-> e) = do
            v'' <- convertVal v
            e' <- convertBody e
            return $ (Nothing,assign v'' scrut `mappend` e')
        da ((NodeC t as) :-> e) = do
            as' <- mapM convertVal as
            e' <- convertBody e
            let tmp = project' (nodeStructName t) scrut
            let ass = mconcat [assign  a (project (arg i) tmp) | a <- as' | i <- [(1 :: Int) ..] ]
            return $ (Just (enum (nodeTagName t)), ass `mappend` e')
    ls' <- mapM da ls
    return $ profile_case_inc `mappend` switch' tag ls'

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
    x <- C ask
    (ss,er) <- convertExp e -- lift $  runSubCGen $ cexp e
    case x of
        TodoReturn -> return (ss `mappend` creturn er)
        TodoExp v -> return (ss `mappend` (v `assign` er))
        TodoNothing | isEmptyExpression er -> return ss
        TodoNothing -> return (ss `mappend` expr er)

convertBody e = return $ err (show e)

nodeAssign v t as e' = do
    v' <- convertVal v
    let tmp = project' (nodeStructName t) v'
    as' <- mapM convertVal as
    let ass = [assign  a (project (arg i) tmp) | a <- as' | i <- [( 1 :: Int) ..] ]
    ss' <- convertBody e'
    return $  mconcat ass `mappend` ss'

nodeAssignV v t e' = do
    v' <- convertVal v
    var <- fetchVar t TyTag
    ss' <- convertBody e'
    return $ assign var (project' anyTag v') `mappend` ss'


localTodo :: Todo -> C a -> C a
localTodo todo (C act) = C $ local (const todo) act


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
        return $ function (nodeFuncName n) fr as' ats s

{-# NOINLINE compileGrin #-}
compileGrin :: Grin -> (String,[String])
compileGrin grin = (hsffi_h ++ jhc_rts_c ++ P.render ans ++ "\n", snub (reqLibraries req))  where
    ans = vsep $ [vcat includes,enum_tag_t,header,union_node,text "/* CAFS */", vcat $ map ccaf (grinCafs grin), text "/* Constant Data */", buildConstants finalHcHash,text  "/* Functions */",body]
    includes =  map include (snub $ reqIncludes req)
    (header,body) = generateC (jhc_sizeof:functions) structs

    -- this is a list of every tag used in the program
    tags = (tagHole,[]):sortUnder (show . fst) [ (t,runIdentity $ findArgs (grinTypeEnv grin) t) | t <- Set.toList $ freeVars (snds $ grinFunctions grin) `mappend` freeVars (snds $ grinCafs grin), tagIsTag t]
    ((functions,structs),finalHcHash,req) = runC $ do
        funcs <- mapM convertFunc $ grinFunctions grin
        sts <- flip mapM tags $ \ (n,ts) -> do
            ts' <- mapM convertType ts
            return (nodeStructName n,zip [ name $ 'a':show i | i <-  [1 ..] ] ts')
        return (funcs,sts)


    enum_tag_t = text "typedef enum {" $$ nest 4 (P.fsep (punctuate P.comma (map (tshow . nodeTagName . fst) tags))) $$ text  "} tag_t;"
    jhc_sizeof =  function (name "jhc_sizeof") size_t [(name "tag",tag_t)] [] ( statementRaw $  "switch(tag) {\n" ++ concatMap cs (fsts tags) ++ "}\n_exit(33);")  where
        cs t = text "  case " <> tshow (nodeTagName t) <> char ':' <+> text "return sizeof(struct " <> tshow (nodeStructName t) <>  text ");\n"
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
        f (Left (Lit i _)) = tshow i
        f (Left (Tag t)) = tshow (nodeTagName t)


ccaf :: (Var,Val) -> P.Doc
ccaf (v,val) = text "/* " <> text (show v) <> text " = " <> (text $ render (prettyVal val)) <> text "*/\n" <> text "static node_t _" <> tshow (varName v) <> text ";\n" <> text "#define " <> tshow (varName v) <+>  text "(&_" <> tshow (varName v) <> text ")\n";


