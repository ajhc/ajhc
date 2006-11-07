module E.ToHs(compileToHs) where

import Char
import Control.Monad.RWS
import Data.FunctorM
import System.IO
import Text.PrettyPrint.HughesPJ
import qualified System
import qualified Data.Set as Set

import Doc.PPrint
import E.E
import DataConstructors
import C.Prims
import E.Subst
import E.Program
import E.Values
import Support.CanType
import Name.Name
import Name.Names
import Name.Prim
import Name.VConsts
import Numeric
import Options
import RawFiles(viaghc_hs)
import Util.Gen
import qualified FlagDump as FD

progress str = wdump FD.Progress $  (putErrLn str) >> hFlush stderr

{-# NOINLINE compileToHs #-}
compileToHs :: Program -> IO ()
compileToHs prog = do
    (v,_,Coll { collNames = ns, collPrims = prims }) <- runRWST (fromTM $ transE (programE prog)) emptyEnvironment 1


    let rv = render (text "theRealMain = " <> v)
    let data_decls = render (transDataTable (progDataTable prog) ns) ++ "\n"
    let foreign_decls = render (transForeign $ Set.toList prims) ++ "\n"
    let fn = optOutName options
    let cf = (fn ++ "_code.hs")
    progress ("Writing " ++ show cf)
    name <- System.getProgName
    args <- getArguments
    let argstring = simpleQuote (name:args)
        comm = shellQuote $ ["ghc", cf, "-o", fn ]
    writeFile cf $ unlines ["-- " ++ argstring,"-- " ++ comm,"",viaghc_hs,data_decls,rv,"",foreign_decls]
    progress ("Running: " ++ comm)
    r <- System.system comm
    when (r /= System.ExitSuccess) $ fail "Hs code did not compile."
    return ()

showCType "wchar_t" = "Char#"
showCType "HsChar" = "Char#"
showCType "HsPtr" =  "Addr#"
showCType "HsFunPtr" =  "Addr#"
showCType _ =  "Int#"

transForeign ps = vcat (map f ps) where
    f (AddrOf s) = text $ "foreign import ccall \"&" ++ s ++ "\" addr_" ++ mangleIdent s ++ " :: Ptr ()"
    f furc@Func { funcName = fn, funcIOLike = True, primArgTypes = as, primRetType = "void" } = ans where
        ans = text $ "foreign import ccall unsafe \"" ++ fn ++ "\" " ++ cfuncname furc ++ " :: " ++ concatInter " -> " ("World__":map showCType as ++ ["World__"])
    f furc@Func { funcName = fn, funcIOLike = False, primArgTypes = as, primRetType = rt } = ans where
        ans = text $ "foreign import ccall unsafe \"" ++ fn ++ "\" " ++ cfuncname furc ++ " :: " ++ concatInter " -> " (map showCType (as ++ [rt]))

transDataTable dataTable ns = vcat (map g $ Set.toList (mconcatMap f (Set.toList ns))) where
    f (n,_,_) | n `elem` builtIns = Set.empty
    f (n,_,_) | Just _ <- fromUnboxedNameTuple n = Set.empty
    f (n,_,_) | Just _ <- fromTupname n = Set.empty
    f w@(n,_,tl) = case (nameType n,tl) of
        (DataConstructor,False) -> Set.fromList $ do
            c <- getConstructor n dataTable
            return (conInhabits c)
        (TypeConstructor,True) -> Set.singleton n
        (_RawType,_) -> Set.empty
        (nt,_) -> error (show (w,nt))
    g n = ans where
        ans = text "data" <+> hsep (showCName n:[ text ('x':show i) | _ <- conSlots con | i <- [2::Int,4 ..] ]) <+> dchildren
        Just con = getConstructor n dataTable
        Just childs = conChildren con
        dchildren | null childs = empty
                  | otherwise =  text "=" <+> hcat (punctuate (text " | ") (map dc childs))
    dc cn = ans where
        ans = hsep (showCName cn: map showSlot (conSlots con))
        Just con = getConstructor cn dataTable
    showSlot (EVar v) = pprint v
    showSlot (ELit LitCons { litArgs = es, litAliasFor = Just af }) = showSlot (foldl eAp af es)
    showSlot (EPi TVr { tvrType = a } b) = parens $ showSlot a <+> text "->" <+> showSlot b
    showSlot (ELit (LitCons { litName = c, litArgs = as })) = showCon c (map showSlot as)
    builtIns = [tc_Int,tc_Char,dc_Int,dc_Char,rt_int,rt_HsChar,rt_Worldzh,rt_HsPtr]

data Environment = Env { envParen :: Bool, envType :: Bool }
emptyEnvironment = Env { envParen = False, envType = False }

data Collect = Coll { collNames :: Set.Set (Name,Int,Bool), collPrims :: Set.Set Prim }
    {-! derive: Monoid !-}

newtype TM a = TM { fromTM :: RWST Environment Collect Int IO a }
    deriving(MonadState Int,MonadReader Environment,MonadWriter Collect,Monad,Functor,MonadIO)


mparen xs = do
    Env { envParen = p } <- ask
    x <- local (\e -> e { envParen = True }) xs
    if p then return $ parens x else return x

nparen xs = do local (\e -> e { envParen = True }) xs

noParens x = local (\e -> e { envParen = False }) x

tshow x = text (show x)

showCon c ts | Just _ <- fromUnboxedNameTuple c = text "(# " <> hsep (punctuate comma ts) <> text " #)"
showCon c ts | Just _ <- fromTupname c = text "(" <> hsep (punctuate comma ts) <> text ")"
showCon c [] | c == rt_Worldzh = text "World__"
showCon c [] | c == rt_int = text "Int#"
showCon c [] | c == rt_HsChar = text "Char#"
showCon c [] | c == rt_HsPtr = text "Addr#"
showCon c [] | c == tc_Int = text "Int"
showCon c [] | c == tc_Char = text "Char"
--showCon c [x,xs] | c == dc_Cons = parens $ x <> text ":" <> xs
--showCon c [] | c == dc_EmptyList = text "[]"

showCon c [] = showCName c
showCon c ts = parens $ hsep (showCName c:ts)

showCName c  | c == dc_Char = text "C#"
showCName c  | c == dc_Int = text "I#"
showCName c = text $ case nameType c of
    DataConstructor -> 'D':mangleIdent (show c)
    TypeConstructor -> 'T':mangleIdent (show c)
    n -> 'U':mangleIdent (show n ++ "_" ++ show c)

transType :: E -> TM Doc
transType (EPi TVr {tvrType = a } b) = local (\e -> e { envType = True }) $ mparen $ do
    a <- transType a
    b <- transType b
    return $ a <+> text "->" <+> b
transType (ELit LitCons { litArgs = es, litAliasFor = Just af }) = transType (foldl eAp af es)
transType (ELit LitCons { litName = c, litArgs =  ts }) = nparen $ do
    Env { envType = inType } <- ask
    tell mempty { collNames = Set.singleton (c,length ts,inType) }
    ts <- mapM transType ts
    return $ showCon c ts

transE :: E -> TM Doc
transE (EError s _) = mparen $ return (text "error" <+> tshow s)
transE (ELit (LitInt num t)) | t == tCharzh || t == rawType "wchar_t" = return $ text (show $ chr $ fromIntegral num) <> text "#"
transE (ELit (LitInt num _)) = return $ text (show num) <> text "#"
transE (ELit LitCons { litName = c, litArgs =  ts }) = nparen $ do
    Env { envType = inType } <- ask
    tell mempty { collNames = Set.singleton (c,length ts,inType) }
    ts <- mapM transE ts
    return $ showCon c ts
transE ee | (e,ts@(_:_)) <- fromLam ee  = mparen $ do
    ts' <- mapM transTVr ts
    e <- noParens $ transE e
    return $ text "\\" <> hsep ts' <+> text "->" <+> e
transE (EVar tvr) = transTVr tvr
transE ee | (e,es@(_:_)) <- fromAp ee = mparen $ do
    e <- transE e
    es <- mapM transE es
    return (hsep (e:es))
--transE ELetRec { eDefs = [(t,d)], eBody = e } | (d,bs) <- fromLam d = mparen $ noParens $ do
--    t <- transTVr t
--    bs <- mapM transTVr bs
--    d <- transE d
--    e <- transE e
--    return (text "let {" <+> hsep (t:bs) <+> text "=" <+> d <+> text " } in" <+> e)
transE ELetRec { eDefs = ds, eBody = e } = mparen $ do
    ds' <- flip mapM ds $ \ (t,e) -> do
        let (b,bs) = fromLam e
        tt <- noParens $ transType (tvrType t)
        t <- transTVr t
        bs <- mapM transTVr bs
        e <- noParens $ transE b
        return (t <+> text "::" <+> tt <> semi $$ hsep (t:bs) <+> text "=" <+> e)
    e <- noParens $ transE e
    return (text "let {" $$ nest 4 (vcat (punctuate (text ";") ds')) $$ text "} in" <+> e)
transE ECase { eCaseBind = TVr { tvrIdent = 0, tvrType = tt }, eCaseScrutinee = scrut, eCaseDefault = Just body, eCaseAlts = [] } | isLifted tt = mparen $ do
    scrut <- transE scrut
    body <- transE body
    return (scrut <+> text "`seq`" <+> body)
transE ECase { eCaseBind = bind, eCaseScrutinee = scrut, eCaseDefault = md, eCaseAlts = as } = mparen $ do
    scrut <- noParens $ transE scrut
    let dobind = 0 /= tvrIdent bind
    b <- transTVr bind
    md <- fmapM transE md
    let md' = flip fmap md $ \e ->  b <+> text "->" <+> if dobind && isLifted (getType bind) then text "seq" <+> b <+> e else e
    as <- mapM (transAlt dobind b) as
    let alts = as ++ maybeToMonad md'
    return (text "case" <+> scrut <+> text "of {" $$ nest 4  (vcat (punctuate semi alts)) $$ text "}")
transE e | Just (e',_) <- from_unsafeCoerce e = mparen $ do
    e' <- transE e'
    return (text "unsafeCoerce#" <+> e')
transE (EPrim (APrim Operator { primOp = op, primRetType = rt } _) [x,y] _) | Just z <- op2Table (op,rt) = mparen $ do
    x <- transE x
    y <- transE y
    return (hsep [text z,x,y])
transE (EPrim (APrim (AddrOf addr) _) [] _) = mparen $ do
    tell mempty { collPrims = Set.singleton (AddrOf addr) }
    return (text $ "unPtr addr_" ++ mangleIdent addr)

transE (EPrim (APrim func@Func {} _) xs _) = mparen $ do
    tell mempty { collPrims = Set.singleton func }
    xs <- mapM transE xs
    return (hsep (text (cfuncname func):xs))
transE (EPrim (APrim cast@CCast { primArgType = at, primRetType = rt } _) [x] _) = mparen $ transE x >>= \x ->  case (showCType at,showCType rt) of
    (a,b) | a == b -> return x
    ("Int#","Char#") -> return (text "chr#" <+> x)
    ("Char#","Int#") -> return (text "ord#" <+> x)
    ("Addr#","Int#") -> return (text "addr2Int#" <+> x)
    ("Int#","Addr#") -> return (text "int2Addr#" <+> x)
    ("Word#","Int#") -> return (text "addr2Int#" <+> x)
    ("Int#","Word#") -> return (text "int2Word#" <+> x)

transE e = return $ text "{- ERROR " <> tshow e <> text " -} undefined"

cfuncname Func { funcName = fn, funcIOLike = iol, primArgTypes = as, primRetType = r  } =  ("func_" ++ (if iol then "io" else "pure") ++ "_" ++ fn ++ concatInter "_" (r:as))

op2Table (op,rt) = lookup rt table >>= lookup op where
    table = [ ("int",intTable)]
    intTable = [
        ("+","(+#)"),
        ("-","(-#)"),
        ("*","(*#)")
        ]

transAlt :: Bool -> Doc -> Alt E -> TM Doc
transAlt dobind b (Alt LitInt { litNumber = i } e) = do
    e <- noParens $ transE e
    return ( (if dobind then b <> char '@' else empty) <> tshow i <> text "#" <+> text "->" <+> e)
transAlt dobind b (Alt LitCons { litName = c, litArgs = ts } e) = do
    ts <- mapM transTVr ts
    e <- noParens $ transE e
    return ( (if dobind then b <> char '@' else empty) <> showCon c ts <+> text "->" <+> e)




transTVr :: TVr -> TM Doc
transTVr tvr = return (text $ 'v':mangleIdent (pprint tvr))

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


