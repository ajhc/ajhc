module C.FromGrin(compileGrin) where

import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Monoid
import List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Text.PrettyPrint.HughesPJ as P
import Text.PrettyPrint.HughesPJ(nest,($$),($+$))

import Atom
import C.Gen
import C.Prims
import Doc.DocLike
import Doc.PPrint
import E.Pretty(render)
import FreeVars
import GenUtil
import Grin.Grin
import Grin.HashConst
import Grin.Show
import qualified Util.Seq as Seq
import RawFiles
import Name.VConsts
import CanType


toType TyTag = return tag_t
toType (TyNode) = return pnode_t
toType (TyTup []) = return $ CTypeBasic "void"
toType (TyPtr TyNode) = toType TyNode  -- for now, we use pointers to nodes for everything
toType (TyTup [x]) = toType x
toType (TyTup xs) = do xs' <- mapM toType xs; newAnonStruct xs'
toType (Ty s) = return $ CTypeBasic $ fromAtom s

toType' TyTag = return tag_t
toType' (TyNode) = return pnode_t
toType' (TyTup []) = return $ CTypeBasic "void"
toType' (TyPtr TyNode) = toType' TyNode  -- for now, we use pointers to nodes for everything
toType' (TyTup [x]) = toType' x
toType' (Ty s) = return $ CTypeBasic $ fromAtom s

--toType (TyPtr t) = CTypePointer (toType t)
--    | a == tIntzh = CTypeBasic "HsInt"
--    | a == tCharzh = CTypeBasic "HsChar"
--    | otherwise = CTypeBasic (fromAtom s)


toStruct t = text $ 's':(show $ toCIdent $ t)
toTag t = text $ (show $ toCIdent $ t)

toStructT t = CTypeStruct (toStruct t)
toStructTP t = CTypePointer (toStructT t)

size_t = CTypeBasic "size_t"
tag_t = CTypeBasic "tag_t"
pnode_t = CTypePointer node_t
node_t = (CTypeBasic "node_t")

toVName (V n) | n < 0 = text $ 'g':show (- n)
toVName (V n) = text $ 'v':show n


data Todo = TodoReturn | TodoExp CExpr | TodoNothing

ccaf :: (Var,Val) -> P.Doc
ccaf (v,val) = text "/* " <> text (show v) <> text " = " <> (text $ render (prettyVal val)) <> text "*/\n" <> text "static node_t _" <> toVName v <> text ";\n" <> text "#define " <> toVName v <+>  text "(&_" <> toVName v <> text ")\n";

--cfunc :: Monad m => TyEnv -> (Atom,Lam) -> m CFunction
cfunc te (n,Tup as :-> body) = do
        s <- runReaderT (cb body) TodoReturn
        fr <-  toType r
        as' <- flip mapM as $ \ (Var v t) -> do
            t' <- toType t
            return (t',toVName v)
        return $ cfunction { cFuncComments = show n, cFuncReturnType = fr, cFuncName = toTag n, cFuncArgs = as', cFuncBody = finc:s  } where
    Identity (_,r) = findArgsType te n
    finc = CSExpr (CEFunCall "function_inc" [])


--cVal :: Val -> ReaderT Todo (CGen (State HcHash)) CExpr
cVal (Var n _) = return $  CEIdent (toVName n)
cVal (Const (NodeC h _)) | h == tagHole = return $ CEIdent "NULL"
cVal (Const h) = do
    (_,i) <- newConst h
    return $ CEIdent ( 'c':show i )
cVal (Lit i _) = return $ CEDoc (show i)
cVal (Tag t) = return $ CEIdent (toTag t)
cVal (Tup [x]) = cVal x
cVal (Tup []) = return $ CEDoc "/* () */"
cVal (Tup xs) = do
    xs' <- mapM cVal xs
    ts <-   mapM (toType . getType) xs
    t <-   newAnonStruct ts
    tup <-  newAuto t
    addStmts [ anonField tup i `CSAssign` x  | i <- [0..] | x <- xs' ]
    return tup



cVal x = return $  CEDoc  ("/* ERROR cVal: " ++ show x  ++ " */")


-- cb (Fetch (Var n _) :>>= Var n' t :-> e ) = return [CSAuto (toType t) (toVName n'), CSAssign (CEIdent (]

--statement s = tell $ (Seq.single s,mempty)

statement s = addStatement s
newAuto t = do
    nn <- newIdent
    let n = "auto" ++ nn
    addStatement (CSAuto t n)
    return $ CEIdent n

newNode (NodeC t _) | t == tagHole = do
    return $  CEFunCall "malloc" [CESizeof node_t]
newNode (NodeC t as) = do
    statement (CSAuto pnode_t "tmp")
    --let tmp = CEVar (CTypePointer (toStructT t)) "tmp"
    let tmp = CEIdent "tmp"
        --tmp' = CECast (toStructTP t) tmp
        tmp' = CEIndirect tmp (toStruct t)
    statement (CSAssign tmp $ CEFunCall "malloc" [CESizeof (if tagIsWHNF t then toStructT t else node_t)])
    statement (CSAssign  (CEDot tmp' "tag") (CEDoc (toTag t)) )
    as' <- mapM cVal as
    mapM_ statement [CSAssign  (CEDot tmp' ('a':show i)) a | a <- as' | i <- [1 ..] ]
    return $ CECast pnode_t tmp


cexp (Update v@Var {} (NodeC t as)) = do
    v' <- cVal v
    as' <- mapM cVal as
    let tmp' = CECast (toStructTP t) v'
    statement (CSExpr $ CEFunCall "update_inc" [])
    statement (CSAssign  (CEIndirect tmp' "tag") (CEDoc (toTag t)) )
    as' <- mapM cVal as
    mapM_ statement [CSAssign  (CEIndirect tmp' ('a':show i)) a | a <- as' | i <- [1 ..] ]
    return $ CEDoc ""

cexp (Update v z) = do  -- TODO eliminate unknown updates
    v' <- cVal v
    z' <- cVal z
    let tag = CEIndirect z' "any.tag"
    statement (CSExpr $ CEFunCall "update_inc" [])
    return $ CEFunCall "memcpy" [v',z',CEFunCall "jhc_sizeof" [tag]]
cexp (Fetch v) = cVal v
cexp (Store n@NodeC {}) = newNode n
cexp (Return n@NodeC {}) = newNode n
cexp (Return x) = cVal x
cexp (Cast x t) = do
    x' <- cVal x
    t' <-  toType t
    return $ CECast t' x'
cexp (Error s t) = do
    statement (CSExpr (CEFunCall "jhc_error" [CEDoc (show s)]))
    t' <- toType t
    case t' of
        CTypeStruct _ -> newAuto t'
        _ -> return $ CECast t' (CEDoc "0")
cexp (App a vs _) = do
    vs' <- mapM cVal vs
    return $ CEFunCall (toTag a) vs'
cexp (Prim p vs) | APrim _ req <- primAPrim p  =  (addRequires req) >> convertPrim p vs
cexp e = return $ CEDoc ("/* ERROR " ++ show e ++ " */")

convertPrim p vs
    | APrim (CConst s _) _ <- primAPrim p = do
        return $ CEDoc s
    | APrim (CCast _ to) _ <- primAPrim p, [a] <- vs = do
        a' <- cVal a
        return $ CECast (CTypeBasic to) a'
    | APrim (Operator n [ta] r) _ <- primAPrim p, [a] <- vs = do
        a' <- cVal a
        return $ CECast (CTypeBasic r) (CEUOp n a')
    | APrim (Operator n [ta,tb] r) _ <- primAPrim p, [a,b] <- vs = do
        a' <- cVal a
        b' <- cVal b
        return $ CECast (CTypeBasic r) (CEOp n a' b') -- (CECast (CTypeBasic ta) a') (CECast  | v <- vs' | t <- as ])
    | APrim (Func _ n as r) _ <- primAPrim p = do
        vs' <- mapM cVal vs
        return $ CECast (CTypeBasic r) (CEFunCall n [ CECast (CTypeBasic t) v | v <- vs' | t <- as ])
    | APrim (Peek t) _ <- primAPrim p, [v] <- vs = do
        v' <- cVal v
        return $ CEDoc ("*((" <> t <+> "*)" <> (parens $ pprint v') <> char ')')
    | APrim (Poke t) _ <- primAPrim p, [v,x] <- vs = do
        v' <- cVal v
        x' <- cVal x
        return $ CEDoc ("*((" <> t <+> "*)" <> (parens $ pprint v') <> text ") = " <> pprint x')
    | APrim (AddrOf t) _ <- primAPrim p, [] <- vs = do
        return $ CEDoc ('&':t)

--    | Just r <- getPrefix "prim_const." pName , [] <- vs = do
--        return $ CEDoc r
--    | Just r <- getPrefix "prim_op_aaa." pName , [a,b] <- vs = do
--        a' <- cVal a
--        b' <- cVal b
--        return $ CEOp r a' b'
--    | Just r <- getPrefix "prim_op_aaB." pName , [a,b] <- vs = do
--        a' <- cVal a
--        b' <- cVal b
--        return $ CEOp r a' b'
--    | Just r <- getPrefix "prim_op_aa." pName , [a] <- vs = do
--        a' <- cVal a
--        return $ CEUOp r a'
--    |  "@primEq" `isPrefixOf` pName, [a,b] <- vs = do
--        a' <- cVal a
--        b' <- cVal b
--        (_,true) <- newConst vTrue
--        (_,false) <- newConst vFalse
--        return $ CETernary (CEOp "==" a' b') (CEIdent ('c':show true)) (CEIdent ('c':show false))
--    |  "@primCompare" `isPrefixOf` pName, [a,b] <- vs = do
--        a' <- cVal a
--        b' <- cVal b
--        (_,eq) <- newConst $ vOrdering EQ
--        (_,gt) <- newConst $ vOrdering GT
--        (_,lt) <- newConst $ vOrdering LT
--        let ti x = CEIdent ('c':show x)
--        return $ CETernary (CEOp ">" a' b') (ti gt)  (CETernary (CEOp "==" a' b') (ti eq) (ti lt))
--    |  toAtom "@primNegate" ==  primName p, [a] <- vs = do
--        a' <- cVal a
--        return $  (CEOp "-" (CEDoc "0") a')
--    | toAtom "@putChar" == primName p, [a] <- vs = do
--        a' <- cVal a
--        return $ CEFunCall "putchar" [a']
--    | toAtom "@getChar" == primName p, [] <- vs = do
--        tell [CSAuto (CTypeBasic "int") "gc"]
--        tell [CSAssign (CEIdent "gc") (CEFunCall "getchar" [])]
--        return $ CEIdent "gc"
 --   | Just n <- lookup (primName p) bops, [a,b] <- vs = doBop n a b
--    where pName = fromAtom $ primName p

--doBop n a b = do
--    a' <- cVal a
--    b' <- cVal b
--    return $ CEOp n a' b'

--bops = [(toAtom "@primTimes", "*"), (toAtom "@primPlus", "+"), (toAtom "@primMinus","-")]

declVar (Var n t) = do
    t' <- toType t
    return $ CSAuto t' (toVName n)

cb :: Exp -> ReaderT Todo (CGen (State HcHash)) [CStatement]
cb (Prim p [a,b] :>>= Tup [q,r] :-> e') | primName p == toAtom "@primQuotRem" = do
    a' <- cVal a
    b' <- cVal b
    r' <- cVal r
    q' <- cVal q
    ss' <- cb e'
    q'' <- declVar q
    r'' <- declVar r
    return $ [q'', r'' , CSAssign q' (CEOp "/" a' b'), CSAssign r' (CEOp "%" a' b') ] ++ ss'

cb (Return v :>>= (NodeC t as) :-> e') = do
    v' <- cVal v
    --let tmp = CECast (toStructTP t)  v'
    let tmp = CEIndirect v' (toStruct t)
    as' <- mapM cVal as
    --let ass = [CSAssign  a (CEIndirect tmp ('a':show i)) | a <- as' | i <- [1 ..] ]
    let ass = [CSAssign  a (CEDot tmp ('a':show i)) | a <- as' | i <- [1 ..] ]
    ss' <- cb e'
    as' <- mapM declVar as
    return (as'  ++ ass ++ ss')
cb (Fetch v :>>= (NodeC t as) :-> e') = do
    v' <- cVal v
    --let tmp = CECast (toStructTP t)  v'
    let tmp = CEIndirect v' (toStruct t)
    as' <- mapM cVal as
    --let ass = [CSAssign  a (CEIndirect tmp ('a':show i)) | a <- as' | i <- [1 ..] ]
    let ass = [CSAssign  a (CEDot tmp ('a':show i)) | a <- as' | i <- [1 ..] ]
    ss' <- cb e'
    as' <- mapM declVar as
    return (as'  ++ ass ++ ss')
cb (e :>>= Tup [] :-> e') = do
    ss <- local (const (TodoNothing)) (cb e)
    ss' <- cb e'
    return (ss ++ ss')
cb (e :>>= v@(Var _ _) :-> e') = do
    v' <- cVal v
    ss <- local (const (TodoExp v')) (cb e)
    ss' <- cb e'
    v'' <- declVar v
    return (v'':ss ++ ss')
cb (e :>>= Tup [x] :-> e') = cb (e :>>= x :-> e')
cb (e :>>= Tup xs :-> e') = do
    (rs,ret) <- lift $ runSubCGen $ do
        ts <- mapM (toType . getType) xs
        st <- newAnonStruct ts
        ret <- newAuto st
        return ret
    ss <- local (const (TodoExp ret)) (cb e)
    ss' <- cb e'
    vs <- mapM cVal xs
    vds <- mapM declVar xs
    return $ vds ++ rs ++ ss ++ [ v `CSAssign` anonField ret i | v <- vs | i <- [0..] ]  ++ ss'

{-
    return []

    v' <- cVal v
    ss <- local (const (TodoExp v')) (cb e)
    ss' <- cb e'
    v'' <- declVar v
    return (v'':ss ++ ss')
    -}
cb (Case v@(Var _ t) ls) | t == TyNode = do
    v' <- cVal v
    let tag = CEIndirect v' "any.tag"
        da (v@(Var {}) :-> e) = do
            v'' <- cVal v
            e' <- cb e
            v''' <- declVar v
            return $ (Nothing,[v''',CSAssign v'' v'] ++ e')
        da ((NodeC t as) :-> e) = do
            as' <- mapM cVal as
            e' <- cb e
            --let tmp = CECast (toStructTP t)  v'
            let tmp = CEIndirect v' (toStruct t)
            --let ass = [CSAssign  a (CEIndirect tmp ('a':show i)) | a <- as' | i <- [1 ..] ]
            let ass = [CSAssign  a (CEDot tmp ('a':show i)) | a <- as' | i <- [1 ..] ]
            as' <- mapM declVar as
            return $ (Just (toTag t), as' ++ ass ++ e')
    ls' <- mapM da ls
    return [case_inc, CSSwitch tag ls' ]
cb (Case v@(Var _ t) ls) = do
    v' <- cVal v
    v'' <- return (if t `elem` ptrs then CECast (CTypeBasic "uintptr_t") v' else v')
    let da (v@(Var {}) :-> e) = do
            v'' <- cVal v
            e' <- cb e
            v''' <- declVar v
            return $ (Nothing,[v''',CSAssign v'' v'] ++ e')
        da ((Lit i _) :-> e) = do
            e' <- cb e
            return $ (Just (show i), e')
        da (Tup [x] :-> e) = da ( x :-> e )
    ls' <- mapM da ls
    return [case_inc, CSSwitch v'' ls' ]



cb e = do
    x <- ask
    (ss,e) <- lift $  runSubCGen $ cexp e
    case x of
        TodoReturn -> return $ ss ++ [CSReturn e]
        TodoExp v -> return $ ss ++ [CSAssign v e]
        TodoNothing | e == CEDoc "" -> return ss
        TodoNothing -> return $ ss ++ [CSExpr e]

case_inc = (CSExpr $ CEFunCall "case_inc" [])

ptrs = [Ty $ toAtom "HsPtr", Ty $ toAtom "HsFunPtr"]

include fn = text "#include <" <> text fn <> text ">"

{-# NOINLINE compileGrin #-}
compileGrin :: Grin -> (String,[String])
compileGrin grin = (hsffi_h ++ jhc_rts_c ++ P.render ans ++ "\n", snub (reqLibraries req))  where
    tags = (tagHole,[]):sortUnder (show . fst) [ (t,runIdentity $ findArgs (grinTypeEnv grin) t) | t <- Set.toList $ freeVars (snds $ grinFunctions grin) `mappend` freeVars (snds $ grinCafs grin), tagIsTag t]
    et = text "typedef enum {" $$ nest 4 (P.fsep (punctuate P.comma (map (toTag . fst) tags))) $$ text  "} tag_t;"
    --ans = vcat $ [text "#include \"HsFFI.h\"",text "#include <stdlib.h>",text "#include <stdio.h>",text "#include <string.h>",text "#include <unistd.h>",text "#include <malloc.h>",text "",et,text "",text "typedef union node node_t;",text ""] ++ map cs tags ++ [text "",cn,text "",so,text "",text "/* Begin CAFS */"] ++ map ccaf (grinCafs grin) ++ [text "", consts, text "",text  "/* Begin Functions */",jhc_error] ++ map prettyFuncP funcs ++ (map prettyFunc funcs) ++ [mf]
    ans = vcat $ map include (snub $ reqIncludes req) ++ [text "",et,text ""] ++ decls' ++ map cs tags ++ [text "",cn,text "",so,text "",text "/* Begin CAFS */"] ++ map ccaf (grinCafs grin) ++ [text "", consts, text "",text  "/* Begin Functions */"] ++ map prettyFuncP funcs ++ (map prettyFunc funcs)
    cs (t,ts) = prettyDecl $ CStruct (toStruct t) ((tag_t, "tag"):map cst (zip [1..] ts))
    cst (i,t) = (runIdentity $ toType' t, text $ 'a':show i)
    cn = text $  "union node {\n  struct { tag_t tag; } any;\n" <> mconcat (map cu (fsts tags)) <> text "};"
    cu t = text "  struct" <+> (toStruct t) <+> toStruct t <> text ";\n"
    so = prettyDecl $ CFunc size_t "jhc_sizeof" [(tag_t,"tag")] [CSDoc $ "switch(tag) {\n" ++ concatMap cs (fsts tags) ++ "}\n_exit(33);"]  where
        cs t = text "  case " <> toTag t <> char ':' <+> text "return sizeof(struct " <> toStruct t <> text ");\n"
    funcs = sortUnder cFuncName funcs'
    decls' = map prettyDecl decls
    --(funcs',fh) =  runState sdo emptyHcHash
    ((funcs',CGenState { genStateRequires = req, genStateDecls = decls }),fh) = runState  (runCGen 1 (mapM (cfunc $ grinTypeEnv grin) $ grinFunctions grin)) emptyHcHash
    consts = P.vcat (map cc (Grin.HashConst.toList fh)) where
        cc nn@(HcNode a zs,i) = comm $$ cd $$ def where
            comm = text "/* " <> tshow (nn) <> text " */"
            cd = text "static struct " <> toStruct a <+> text "_c" <> tshow i <+> text "= {" <> hsep (punctuate P.comma (toTag a:rs)) <> text "};"
            def = text "#define c" <> tshow i <+> text "((node_t *)&_c" <> tshow i <> text ")"
            rs = [ f z undefined |  z <- zs ]
            --ts = findArgs (grinTypeEnv grin) a
            f (Right i) _ = text $ 'c':show i
            --f (Left i) _ = tshow i
            f (Left (Var n _)) _ =  toVName n
            f (Left (Lit i _)) _ = tshow i
            f (Left (Tag t)) _ = toTag t



--mf = text "int main(int argc, char *argv[]) { XAmain(); return 0; }"
--jhc_error = text "static void jhc_error(char *s) { fputs(s,stderr); fputs(\"\\n\",stderr);  exit(1); }"


