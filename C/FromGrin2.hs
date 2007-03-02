
module C.FromGrin2(compileGrin) where

import Control.Monad.Identity
import Control.Monad.RWS
import Data.List
import Data.Maybe
import Data.Monoid
import Text.PrettyPrint.HughesPJ(nest,($$))
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Text.PrettyPrint.HughesPJ as P


import Atom
import C.Arch
import C.FFI
import C.Generate
import C.Prims
import Doc.DocLike
import Doc.PPrint
import Grin.Grin
import Grin.HashConst
import Grin.Noodle
import Grin.Show
import Grin.Val
import PackedString
import RawFiles
import Support.CanType
import Support.FreeVars
import Util.Gen
import Util.UniqueMonad


---------------
-- C Monad
---------------

type Structure = (Name,[(Name,Type)])
data Todo = TodoReturn | TodoExp Expression | TodoNothing


data Written = Written {
    wRequires :: Requires,
    wStructures :: Map.Map Name [(Name,Type)],
    wTags :: Set.Set Atom,
    wEnums :: Map.Map Name Int,
    wFunctions :: Map.Map Name Function
    }
    {-! derive: Monoid !-}

data Env = Env {
    rTodo :: Todo,
    rEMap :: Map.Map Atom (Name,[Expression]),
    rGrin :: Grin
    }
    {-! derive: update !-}


newtype C a = C (RWST Env Written HcHash Uniq a)
    deriving(Monad,UniqueProducer,MonadState HcHash,MonadWriter Written,MonadReader Env)


runC :: Grin -> C a -> (a,HcHash,Written)
runC grin (C m) =  execUniq1 (runRWST m Env { rGrin = grin, rTodo = TodoNothing, rEMap = mempty } emptyHcHash)

tellFunctions :: [Function] -> C ()
tellFunctions fs = tell mempty { wFunctions = Map.fromList $ map (\x -> (functionName x,x)) fs }

localTodo :: Todo -> C a -> C a
localTodo todo (C act) = C $ local (\ r -> r { rTodo = todo }) act


--------------
-- entry point
--------------

{-# NOINLINE compileGrin #-}
compileGrin :: Grin -> (String,[String])
compileGrin grin = (hsffi_h ++ jhc_rts_header_h ++ jhc_rts_alloc_c ++ jhc_rts_c ++ jhc_rts2_c ++ P.render ans ++ "\n", snub (reqLibraries req))  where
    ans = vcat $ includes ++ [text "", enum_tag_t, header, cafs,buildConstants (grinTypeEnv grin) finalHcHash, body]
    includes =  map include (snub $ reqIncludes req)
    include fn = text "#include <" <> text fn <> text ">"
    (header,body) = generateC False (Map.elems fm) (Map.assocs sm)
    ((),finalHcHash,Written { wRequires = req, wFunctions = fm, wEnums = wenum, wStructures = sm, wTags = ts }) = runC grin go
    enum_tag_t = text "enum {" $$ nest 4 (P.vcat (punctuate P.comma $ map (uncurry f) (Map.toList wenum) ++ (zipWith f (Set.toList (Set.map nodeTagName ts)) [0 ..]))) $$ text "};" where
        f t n = tshow t <> text " = " <> tshow (n :: Int)
    go = do
        funcs <- flip mapM (grinFuncs grin) $ \(a,l) -> do
                    convertFunc (Map.lookup a (grinEntryPoints grin)) (a,l)
        tellFunctions funcs
        h <- get
        let tset = Set.fromList [ n | (HcNode n _,_) <- Grin.HashConst.toList h]
        mapM_ declareStruct  (Set.toList tset)
        mapM_ tellTags (Set.toList tset)
    cafs = text "/* CAFS */" $$ (vcat $ map ccaf (grinCafs grin))

convertFunc :: Maybe FfiExport -> (Atom,Lam) -> C Function
convertFunc ffie (n,Tup as :-> body) = do
        s <- localTodo TodoReturn (convertBody body)
        let bt = getType body
            mmalloc (TyPtr _) = [a_MALLOC]
            mmalloc TyNode = [a_MALLOC]
            mmalloc _ = []
            ats = (if isNothing ffie then a_STD else Public):mmalloc bt
            fnname = case ffie of
                Nothing -> nodeFuncName n
                Just (FfiExport cn Safe CCall) -> name cn
        fr <- convertType bt
        as' <- flip mapM as $ \ (Var v t) -> do
            t' <- convertType t
            return (varName v,t')
        return $ function fnname fr as' ats (profile_function_inc & s)


fetchVar :: Var -> Ty -> C Expression
fetchVar v@(V n) _ | n < 0 = return $ (variable  $ varName v)
fetchVar v ty = do
    t <- convertType ty
    return $ (localVariable t (varName v))


convertVal :: Val -> C Expression
convertVal (Var v ty) = fetchVar v ty
convertVal (Const (NodeC h _)) | h == tagHole = return (cast sptr_t (f_VALUE (constant $ number 0)))
convertVal (Const h) = do
    tyenv <- asks (grinTypeEnv . rGrin)
    case h of
        NodeC a ts | Just bn <- basicNode tyenv a ts -> return (cast sptr_t bn)
        _ -> do
            (_,i) <- newConst h
            return $ variable (name $  'c':show i )
convertVal h@(NodeC a ts) | valIsConstant h = do
    tyenv <- asks (grinTypeEnv . rGrin)
    case basicNode tyenv a ts of
        Just bn -> return bn
        _ -> do
            (_,i) <- newConst h
            return $ variable (name $  'c':show i )
convertVal (Lit i _) = return (constant $ number (fromIntegral i))
convertVal (Tup [x]) = convertVal x
convertVal (Tup []) = return emptyExpression
convertVal (Tup xs) = do
    ts <- mapM convertType (map getType xs)
    xs <- mapM convertVal xs
    return (structAnon (zip xs ts))
convertVal (Tag t) = do tellTags t ; return $ constant (enum $ nodeTagName t)
convertVal (ValPrim (APrim p _) [] _) = case p of
    CConst s _ -> return $ expressionRaw s
    AddrOf t -> return $ expressionRaw ('&':unpackPS t)
    PrimTypeInfo { primArgType = arg, primTypeInfo = PrimSizeOf } -> return $ expressionRaw ("sizeof(" ++ arg ++ ")")
    PrimString s -> return $ expressionRaw (show s)
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

convertType TyTag = return tag_t
convertType TyNode = return wptr_t
convertType (TyPtr TyNode) = return sptr_t
convertType (TyPtr (TyPtr TyNode)) = return $ ptrType sptr_t
convertType (Ty t) = return (basicType (toString t))
convertType (TyTup []) = return voidType
convertType (TyTup [x]) = convertType x
convertType (TyTup xs) = do
    xs <- mapM convertType xs
    return (anonStructType xs)


convertBody :: Exp -> C Statement
convertBody (Prim p [a,b] :>>= Tup [q,r] :-> e') | primName p == toAtom "@primQuotRem" = do
    a' <- convertVal a
    b' <- convertVal b
    r' <- convertVal r
    q' <- convertVal q
    ss' <- convertBody e'
    return $ mconcat [ q' =* (operator "/" a' b'), r' =* (operator "%" a' b'), ss' ]
convertBody Let { expDefs = defs, expBody = body } = do
    u <- newUniq
    nn <- flip mapM defs $ \FuncDef { funcDefName = name, funcDefBody = Tup as :-> _ } -> do
        vs' <- mapM convertVal as
        let nm = (toName (show name ++ show u))
        return (name,(nm,vs'))
    let done = (toName $ "done" ++ show u)
        localJumps xs = local (rEMap_u (Map.fromList xs `mappend`))
    localJumps nn $ do
    ss <- (convertBody body)
    rs <- flip mapM defs $ \FuncDef { funcDefName = name, funcDefBody = Tup as :-> b } -> do
       ss <- convertBody b
       return (annotate (show as) (label (toName (show name ++ show u))) & indentBlock ss)
    return (ss & goto done & mconcat (intersperse (goto done) rs) & label done);
convertBody (e :>>= v@(Var _ _) :-> e') = do
    v' <- convertVal v
    ss <- localTodo (TodoExp v')  (convertBody e)
    ss' <- convertBody e'
    return (ss & ss')
convertBody (e :>>= Tup [x] :-> e') = convertBody (e :>>= x :-> e')
convertBody (e :>>= Tup [] :-> e') = do
    ss <- localTodo TodoNothing (convertBody e)
    ss' <- convertBody e'
    return (ss & ss')
convertBody (e :>>= Tup xs :-> e') = do
    ts <- mapM ( convertType . getType) xs
    st <- newVar (anonStructType ts)
    ss <- localTodo (TodoExp st) (convertBody e)
    ss' <- convertBody e'
    vs <- mapM convertVal xs
    return $  ss & mconcat [ v =* projectAnon i st | v <- vs | i <- [0..] ] & ss'
convertBody (Return v :>>= (NodeC t as) :-> e') = nodeAssign v t as e'
convertBody (Fetch v :>>= (NodeC t as) :-> e') = nodeAssign v t as e'
convertBody (Case v@(Var _ ty) [p1@(NodeC t _) :-> e1,p2 :-> e2]) | ty == TyNode = do
    scrut <- convertVal v
    tellTags t
    let tag = getWhat scrut
        da v@Var {} e = do
            v'' <- convertVal v
            e' <- convertBody e
            return $ v'' =* scrut & e'
        da n1@(NodeC t _) (Return n2@NodeC {}) | n1 == n2 = convertBody (Return v)
        da (NodeC t as) e = do
            tellTags t
            as' <- mapM convertVal as
            let tmp = concrete t  scrut
                ass = mconcat [if needed a then a' =* (project' (arg i) tmp) else mempty | a' <- as' | a <- as | i <- [(1 :: Int) ..] ]
                fve = freeVars e
                needed (Var v _) = v `Set.member` fve
            e' <- convertBody e
            return (ass & e')
        am Var {} e = e
        am (NodeC t2 _) e = annotate (show p2) (f_assert ((constant $ enum (nodeTagName t2)) `eq` tag) & e)
    p1' <- da p1 e1
    p2' <- liftM (am p2) $ da p2 e2
    return $ profile_case_inc & cif ((constant $ enum (nodeTagName t)) `eq` tag) p1' p2'

-- zero is usually faster to test for than other values, so flip them if zero is being tested for.
convertBody (Case v@Var {} [v1, v2@(Lit n _ :-> _)]) | n == 0 = convertBody (Case v [v2,v1])
convertBody (Case v@(Var _ t) [p1 :-> e1, p2 :-> e2]) | Set.null ((freeVars p2 :: Set.Set Var) `Set.intersection` freeVars e2) = do
    scrut <- convertVal v
    let ptrs = [Ty $ toAtom "HsPtr", Ty $ toAtom "HsFunPtr"]
        scrut' = (if t `elem` ptrs then cast uintptr_t scrut else scrut)
        cp (Lit i _) = constant (number $ fromIntegral i)
        cp (Tag t) = constant (enum (nodeTagName t))
        am e | isVar p2 = e
             | otherwise = annotate (show p2) (f_assert ((cp p2) `eq` scrut') & e)
    e1' <- convertBody e1
    e2' <- convertBody e2
    return $ profile_case_inc & cif (cp p1 `eq` scrut') e1' (am e2')
convertBody (Case v@(Var _ t) ls) | t == TyNode = do
    scrut <- convertVal v
    let tag = getWhat scrut
        da (v@(Var {}) :-> e) = do
            v'' <- convertVal v
            e' <- convertBody e
            return $ (Nothing,v'' =* scrut & e')
        da (n1@(NodeC t _) :-> Return n2@NodeC {}) | n1 == n2 = do
            tellTags t
            e' <- convertBody (Return v)
            return (Just (enum (nodeTagName t)),e')
        da ((NodeC t as) :-> e) = do
            tellTags t
            as' <- mapM convertVal as
            e' <- convertBody e
            let tmp = concrete t scrut
                ass = mconcat [if needed a then a' =* (project' (arg i) tmp) else mempty | a' <- as' | a <- as | i <- [(1 :: Int) ..] ]
                fve = freeVars e
                needed (Var v _) = v `Set.member` fve
            return $ (Just (enum (nodeTagName t)), ass & e')
    ls' <- mapM da ls
    return $ profile_case_inc & switch' tag ls'
convertBody (Case v@(Var _ t) ls) = do
    scrut <- convertVal v
    let ptrs = [Ty $ toAtom "HsPtr", Ty $ toAtom "HsFunPtr"]
        scrut' = (if t `elem` ptrs then cast uintptr_t scrut else scrut)
        da (v@(Var {}) :-> e) = do
            v'' <- convertVal v
            e' <- convertBody e
            return (Nothing,v'' =* scrut & e')
        da ((Lit i _) :-> e) = do
            e' <- convertBody e
            return $ (Just (number $ fromIntegral i), e')
        da (Tag t :-> e) = do
            e' <- convertBody e
            return $ (Just (enum (nodeTagName t)), e')
        da (Tup [x] :-> e) = da ( x :-> e )
    ls' <- mapM da ls
    return $ profile_case_inc & switch' scrut' ls'


convertBody e = do
    x <- asks rTodo
    (ss,er) <- convertExp e
    case x of
        TodoReturn -> return (ss & creturn er)
        TodoExp v | isEmptyExpression er -> return ss
        TodoExp v -> return (ss & (v =* er))
        TodoNothing | isEmptyExpression er -> return ss
        TodoNothing -> return (ss & er)

nodeAssign v t as e' = do
    v' <- convertVal v
    as' <- mapM convertVal as
    let ass = concat [perhapsM (a `Set.member` fve) $ a' =* (project' (arg i) (concrete t v')) | a' <- as' | Var a _ <- as |  i <- [( 1 :: Int) ..] ]
        fve = freeVars e'
    ss' <- convertBody e'
    return $  mconcat ass & ss'


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
convertExp (Prim p vs) | APrim _ req <- primAPrim p  =  do
    tell mempty { wRequires = req }
    e <- convertPrim p vs
    return (mempty,e)
convertExp (Store v) | TyPtr TyNode == getType v = do
    v <- convertVal v
    tmp <- newVar (ptrType sptr_t)
    return ((tmp =* jhc_malloc (sizeof sptr_t)) & (dereference tmp =* v),tmp)
convertExp (Fetch v) | getType v == TyPtr (TyPtr TyNode) = do
    v <- convertVal v
    return (mempty,dereference v)
convertExp (Fetch v) | getType v == TyPtr TyNode = do
    v <- convertVal v
    return (mempty,f_fetch v)
convertExp (Update v z) | getType z == TyPtr TyNode = do
    v' <- convertVal v
    z' <- convertVal z
    return $ (dereference v' =* z',emptyExpression)
--convertExp (App a [fn,x] _) | a == funcApply = do
--    fn' <- convertVal fn
--    x' <- convertVal x
--    return (mempty,(functionCall (name "eval") [v']))
convertExp (App a [v] _) | a == funcEval = do
    v' <- convertVal v
    return (mempty,f_eval v')
convertExp (Store n@NodeC {})  = newNode n >>= \(x,y) -> return (x,cast sptr_t y)
convertExp (Return n@NodeC {}) = newNode n >>= \(x,y) -> return (x,cast wptr_t y)
convertExp (Store n@Var {}) | getType n == TyNode = do
    n' <- convertVal n
    return (mempty,cast sptr_t n')
convertExp (Return v) = do
    v <- convertVal v
    return (mempty,v)
--convertExp (App a vs _) | a `notElem` [funcApply,funcEval] = do
convertExp (App a vs _) = do
    lm <- asks rEMap
    vs' <- mapM convertVal vs
    case a `Map.lookup` lm of
        Just (nm,as) -> do
            let ss = [ a =* v | a <- as | v <- vs' ]
            return (mconcat ss & goto nm, emptyExpression)
        Nothing -> return $ (mempty, functionCall (toName (toString a)) vs')
convertExp (Update v@(Var vv _) tn@(NodeC t as)) | getType v == TyPtr TyNode = do
    v' <- convertVal v
    as' <- mapM convertVal as
    nt <- nodeTypePtr t
    let tmp' = cast nt (if vv < v0 then f_DETAG v' else v')
    if not (tagIsSuspFunction t) && vv < v0 then do
        (nns, nn) <- newNode tn
        return (nns & getHead (f_NODEP(f_DETAG v')) =* cast fptr_t nn,emptyExpression)
     else do
        s <- tagAssign tmp' t
        let ass = [project' (arg i) tmp' =* a | a <- as' | i <- [(1 :: Int) ..] ]
        return (mconcat $ profile_update_inc:s:ass,emptyExpression)
convertExp e = return (err (show e),err "nothing")

ccaf :: (Var,Val) -> P.Doc
ccaf (v,val) = text "/* " <> text (show v) <> text " = " <> (text $ P.render (pprint val)) <> text "*/\n" <>
     text "static node_t _" <> tshow (varName v) <> text ";\n" <>
     text "#define " <> tshow (varName v) <+>  text "(EVALTAG(&_" <> tshow (varName v) <> text "))\n";


buildConstants tyenv fh = P.vcat (map cc (Grin.HashConst.toList fh)) where
    cc nn@(HcNode a zs,i) = comm $$ cd $$ def where
        comm = text "/* " <> tshow (nn) <> text " */"
        cd = text "const static struct " <> tshow (nodeStructName a) <+> text "_c" <> tshow i <+> text "= {" <> hsep (punctuate P.comma (ntag ++ rs)) <> text "};"
        Just TyTy { tySiblings = sibs } = findTyTy tyenv a
        ntag = case sibs of
            Just [a'] | a' == a -> []
            _ -> [tshow (nodeTagName a)]
        def = text "#define c" <> tshow i <+> text "((sptr_t)&_c" <> tshow i <> text ")"
        rs = [ f z undefined |  z <- zs ]
        f (Right i) = text $ 'c':show i
        f (Left (Var n _)) = tshow $ varName n
        f (Left v) | Just e <- convertConst v = text (show $ drawG e)

convertConst :: Monad m => Val -> m Expression
convertConst (Const (NodeC h _)) | h == tagHole = return nullPtr
convertConst (Lit i _) = return (constant $ number (fromIntegral i))
convertConst (Tup [x]) = convertConst x
convertConst (Tup []) = return emptyExpression
convertConst (Tag t) = return $ constant (enum $ nodeTagName t)
convertConst (ValPrim (APrim p _) [] _) = case p of
    CConst s _ -> return $ expressionRaw s
    AddrOf t -> return $ expressionRaw ('&':unpackPS t)
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
        return $ cast (basicType r) (functionCall (name $ unpackPS n) [ cast (basicType t) v | v <- vs' | t <- as ])
    | APrim (Peek t) _ <- primAPrim p, [v] <- vs = do
        v' <- convertVal v
        return $ expressionRaw ("*((" <> t <+> "*)" <> (parens $ renderG v') <> char ')')
    | APrim (Poke t) _ <- primAPrim p, [v,x] <- vs = do
        v' <- convertVal v
        x' <- convertVal x
        return $ expressionRaw ("*((" <> t <+> "*)" <> (parens $ renderG v') <> text ") = " <> renderG x')
    | APrim (AddrOf t) _ <- primAPrim p, [] <- vs = do
        return $ expressionRaw ('&':unpackPS t)


tagAssign :: Expression -> Atom -> C Statement
tagAssign e t | tagIsSuspFunction t = do
    en <- declareEvalFunc t
    return $ getHead e =* f_EVALTAG (reference (variable en))
tagAssign e t = do
    declareStruct t
    tyenv <- asks (grinTypeEnv . rGrin)
    TyTy { tySiblings = sib } <- findTyTy tyenv t
    tellTags t
    case sib of
        Just [n'] | n' == t -> return mempty
        _ -> do return $ getWhat e =* constant (enum $ nodeTagName t)

tellTags :: Atom -> C ()
tellTags t | tagIsSuspFunction t = return ()
tellTags t = do
    tyenv <- asks (grinTypeEnv . rGrin)
    TyTy { tySiblings = sib } <- findTyTy tyenv t
    case sib of
        Just [n'] | n' == t -> return ()
        Just rs -> tell mempty { wEnums = Map.fromList (zip (map nodeTagName rs) [0..]) }
        Nothing -> tell mempty { wTags = Set.singleton t }



newNode (NodeC t _) | t == tagHole = do
    fail "newNode.tagHole"
newNode (NodeC t as) = do
    tyenv <- asks (grinTypeEnv . rGrin)
    let sf = tagIsSuspFunction t
    case basicNode tyenv t as of
      Just e -> return (mempty,e)
      Nothing -> do
        st <- nodeType t
        as' <- mapM convertVal as
        tmp <- newVar (if sf then sptr_t else wptr_t)
        let tmp' = concrete t tmp
            wmalloc = if not sf && all (nonPtr . getType) as then jhc_malloc_atomic else jhc_malloc
            malloc =  tmp =* wmalloc (sizeof st)
            ass = [ if isValUnknown aa then mempty else project' i tmp' =* a | a <- as' | aa <- as | i <- map arg [(1 :: Int) ..] ]
            nonPtr TyPtr {} = False
            nonPtr TyNode = False
            nonPtr (TyTup xs) = all nonPtr xs
            nonPtr _ = True
        tagassign <- tagAssign tmp' t
        let res = if sf then (f_EVALTAG tmp) else tmp
        return (mconcat $ malloc:tagassign:ass,res)


{-

newNode (NodeC t as) | tagIsSuspFunction t = do
    en <- declareEvalFunc t
    st <- nodeType t
    as' <- mapM convertVal as
    tmp <- newVar sptr_t
    let tmp' = concrete t tmp
        malloc =  tmp =* jhc_malloc (sizeof st)
        tagassign = getTag tmp' =* f_EVALTAG (reference (variable en))
        ass = [ if isValUnknown aa then mempty else project' i tmp' =* a | a <- as' | aa <- as | i <- map arg [(1 :: Int) ..] ]
        nonPtr TyPtr {} = False
        nonPtr TyNode = False
        nonPtr (TyTup xs) = all nonPtr xs
        nonPtr _ = True
        tmp'' = f_EVALTAG tmp
    return (mconcat $ malloc:tagassign:ass, cast sptr_t tmp'')
newNode (NodeC t as) | tagIsWHNF t = do -- && not (tagIsPartialAp t) = do
    st <- nodeType t
    tell mempty { wTags = Set.singleton t }
    declareStruct t
    as' <- mapM convertVal as
    tmp <- newVar wptr_t
    let tmp' = concrete t tmp
        wmalloc = if all (nonPtr . getType) as then jhc_malloc_atomic else jhc_malloc
        malloc =  tmp =* wmalloc (sizeof st)
        tagassign = getTag tmp' =* constant (enum $ nodeTagName t)
        wmalloc = if tagIsWHNF t && all (nonPtr . getType) as then jhc_malloc_atomic else jhc_malloc
        ass = [ if isValUnknown aa then mempty else project' i tmp' =* a | a <- as' | aa <- as | i <- map arg [(1 :: Int) ..] ]
        nonPtr TyPtr {} = False
        nonPtr TyNode = False
        nonPtr (TyTup xs) = all nonPtr xs
        nonPtr _ = True
    return (mconcat $ malloc:tagassign:ass, tmp)
newNode e = return (err (show e),err "newNode")

-}

------------------
-- declaring stuff
------------------

declareStruct n = do
    grin <- asks rGrin
    let TyTy { tySlots = ts, tySiblings = ss } = runIdentity $ findTyTy (grinTypeEnv grin) n
    ts' <- mapM convertType ts
    let tag | tagIsSuspFunction n = [(name "head",fptr_t)]
            | Just [n'] <- ss, n == n' = []
            | otherwise = [(name "what",what_t)]
        fields = (tag ++ zip [ name $ 'a':show i | i <-  [1 ..] ] ts')
    unless (null fields) $ tell mempty { wStructures = Map.singleton (nodeStructName n) fields }


basicNode :: TyEnv -> Atom -> [Val] -> (Maybe Expression)
basicNode tyenv a [] | isJust s = ans where
    Just TyTy { tySiblings = s@(~(Just ss)) } = findTyTy tyenv a
    ans = case ss of
        [n'] | n' == a -> Just (f_VALUE (constant $ number 0))
        _ -> Nothing
basicNode _ _ _ = Nothing


declareEvalFunc n = do
    fn <- tagToFunction n
    grin <- asks rGrin
    declareStruct n
    nt <- nodeType n
    let ts = runIdentity $ findArgs (grinTypeEnv grin) n
        fname = toName $ "jhc_eval_" ++ show fn
        aname = name "arg";
        rvar = localVariable wptr_t (name "r");
        atype = ptrType nt
        body = rvar =* functionCall (toName (show $ fn)) [ project' (arg i) (variable aname) | _ <- ts | i <- [1 .. ] ]
        update =  f_update (cast sptr_t (variable aname)) rvar
    tellFunctions [function fname wptr_t [(aname,atype)] [a_STD] (body & update & creturn rvar )]
    return fname


--------
-- shape
--------


toShape TyPtr {} = ShapeNativePtr
toShape TyNode = ShapeNativePtr
toShape (Ty bt)
    | show bt == "int" = ShapeNativeInt
    | show bt == "HsPtr" = ShapeNativePtr
    | show bt == "HsFunPtr" = ShapeNativePtr
toShape (Ty bt) = case genericPrimitiveInfo (show bt) of
    Just v -> ShapeBits $ primTypeSizeOf v
    Nothing -> error $ "toShape: " ++ show bt
toShape t = error $ "toShape: " ++ show t

newtype Shapes = Shapes [Shape]
    deriving(Eq,Ord)

data Shape = ShapeNativePtr | ShapeNativeInt | ShapeBits !Int
    deriving(Eq,Ord)

instance Show Shape where
    showsPrec _ ShapeNativeInt = ('i':)
    showsPrec _ ShapeNativePtr = ('p':)
    showsPrec _ (ShapeBits n) = ('b':) . shows n

instance Show Shapes where
    showsPrec _ (Shapes s) = foldr (.) id (map shows s)



----------------------------
-- c constants and utilities
----------------------------

jhc_malloc sz = functionCall (name "jhc_malloc") [sz]
f_assert e    = functionCall (name "assert") [e]
f_DETAG e     = functionCall (name "DETAG") [e]
f_NODEP e     = functionCall (name "NODEP") [e]
f_EVALTAG e   = functionCall (name "EVALTAG") [e]
f_VALUE e     = functionCall (name "VALUE") [e]
f_ISVALUE e   = functionCall (name "ISVALUE") [e]
f_eval e      = functionCall (name "eval") [e]
f_fetch e     = functionCall (name "fetch") [e]
f_update x y  = functionCall (name "update") [x,y]
jhc_malloc_atomic sz = functionCall (name "jhc_malloc_atomic") [sz]
profile_update_inc   = expr $ functionCall (name "jhc_update_inc") []
profile_case_inc     = expr $ functionCall (name "jhc_case_inc") []
profile_function_inc = expr $ functionCall (name "jhc_function_inc") []

arg i = name $ 'a':show i


varName (V n) | n < 0 = name $ 'g':show (- n)
varName (V n) = name $ 'v':show n

nodeTagName :: Atom -> Name
nodeTagName a = toName (toString a)
nodeFuncName :: Atom -> Name
nodeFuncName a = toName (toString a)

sptr_t    = basicType "sptr_t"
fptr_t    = basicType "fptr_t"
what_t    = basicType "what_t"
wptr_t    = basicType "wptr_t"
size_t    = basicType "size_t"
tag_t     = basicType "tag_t"
uintptr_t = basicType "uintptr_t"

a_STD = Attribute "A_STD"
a_MALLOC = Attribute "A_MALLOC"

concrete :: Atom -> Expression -> Expression
concrete t e = cast (ptrType $ structType (nodeStructName t)) e


getHead :: Expression -> Expression
getHead e = project' (name "head") e

getWhat :: Expression -> Expression
getWhat e = project' (name "what") e

nodeTypePtr a = liftM ptrType (nodeType a)
nodeType a = return $ structType (nodeStructName a)
nodeStructName :: Atom -> Name
nodeStructName a = toName ('s':toString a)

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


infixl 1 &

(&) :: (ToStatement a,ToStatement b) => a -> b -> Statement
x & y = toStatement x `mappend` toStatement y


{-
convertExp (Fetch (Index base off)) | getType base == TyPtr (TyPtr TyNode) = do
    base <- convertVal base
    off <- convertVal off
    ure.eturn (mempty,indexArray base off)
convertExp (Store n@NodeV {}) = newNode n
convertExp (Return n@NodeV {}) = newNode n
convertExp (Store n@NodeC {}) = newNode n
convertExp (Return n@NodeC {}) = newNode n

convertExp (Store n@Var {}) | getType n == TyNode = do
    (ss,nn) <- newNode (NodeC tagHole [])
    tmp <- newVar pnode_t
    n <- convertVal n
    let tag = project' anyTag n
        update = expr (functionCall (name "memcpy") [tmp,n,functionCall  (name "jhc_sizeof") [tag]])
    return (ss `mappend` (tmp `assign` nn) `mappend` update, tmp)
convertExp Alloc { expValue = v, expCount = c, expRegion = r } | r == region_heap, TyPtr TyNode == getType v  = do
    v' <- convertVal v
    c' <- convertVal c
    tmp <- newVar ppnode_t
    let malloc = tmp `assign` jhc_malloc (operator "*" (sizeof pnode_t) c')
    fill <- case v of
        ValUnknown _ -> return mempty
        _ -> do
            i <- newVar (basicType "int")
            return $ forLoop i (expressionRaw "0") c' $ indexArray tmp i `assign` v'
    return (malloc `mappend` fill, tmp)
convertExp e@(Update v z) | getType v /= TyPtr (getType z) = do
    return (err (show e),err "nothing")
convertExp (Update (Index base off) z) | getType z == TyPtr TyNode = do
    base <- convertVal base
    off <- convertVal off
    z' <- convertVal z
    return $ (indexArray base off `assign` z',emptyExpression)
convertExp (Update v z) | getType z == TyNode = do  -- TODO eliminate unknown updates
    v' <- convertVal v
    z' <- convertVal z
    let tag = project' anyTag z'
    return $ (profile_update_inc,functionCall (name "memcpy") [v',z',functionCall  (name "jhc_sizeof") [tag]])
convertExp e = return (err (show e),err "nothing")
newNode (NodeV t []) = do
    tmp <- newVar pnode_t
    var <- fetchVar t TyTag
    let tmp' = getTag tmp
        malloc =  tmp =* jhc_malloc (sizeof  node_t)
        tagassign = tmp' =* var
    return (mappend malloc tagassign, tmp)

-}
{-
convertBody (Return v :>>= (NodeV t []) :-> e') = nodeAssignV v t e'
convertBody (Fetch v :>>= (NodeV t []) :-> e') = nodeAssignV v t e'
-}



