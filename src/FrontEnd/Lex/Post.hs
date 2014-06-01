-- post process source after initial parsing
module FrontEnd.Lex.Post(
    checkContext,
    checkDataHeader,
    checkPattern,
    checkPatterns,
    checkSconType,
    checkValDef,
    doForeign,
    qualTypeToClassHead,
    checkBangType,
    implicitName,
    mkRecConstrOrUpdate,
    fixupHsDecls
) where

import C.FFI
import Util.Std
import Data.Char
import FrontEnd.HsSyn
import FrontEnd.Lex.ParseMonad
import FrontEnd.SrcLoc
import Name.Names
import Options
import PackedString
import qualified Data.Set as Set
import qualified Data.Traversable as T
import qualified FlagOpts as FO
import qualified FrontEnd.Lex.Fixity as F

checkContext :: HsType -> P HsContext
checkContext (HsTyTuple []) = return []
checkContext (HsTyTuple ts) = mapM checkAssertion ts
checkContext t = (:[]) <$> checkAssertion t

data Scon = SconApp Scon [Scon] | SconType Bool HsType | SconOp Name
    deriving(Show)
checkSconType  :: [Either Name HsType] -> P (Name, [HsBangType])
checkSconType xs = do
    let f (SconOp n) = return n
        f (SconType False (HsTyCon c)) = return c
        f (SconType False (HsTyTuple [])) = return $ quoteName tc_Unit
        f (SconType False _) = parseErrorK "Needs constructor as head."
        f ~(SconType True _) = parseErrorK "Only fields may be made strict."
    let g (SconType False t) = HsUnBangedTy t
--        f (SconApp (SconOp x) [ta,tb]) | x == tc_Arrow = do HsTyFun <$> f tb <*> f ta
        g ~(SconType True t) = HsBangedTy t
    s <- checkSconType' xs
    case s of
        SconApp t ts -> do t <- f t; return (nameTyLevel_s termLevel t, reverse $ map g ts)
        _ -> do s <- f s; return (nameTyLevel_s termLevel s,[])

checkBangType  :: [Either Name HsType] -> P HsBangType
checkBangType xs = checkSconType' xs >>= \s -> case s of
        SconType True ty -> return $ HsBangedTy ty
        _ -> HsUnBangedTy <$> f s
    where
    f (SconType False ty) = return ty
    f (SconApp (SconOp x) [ta,tb]) | x == tc_Arrow = do HsTyFun <$> f tb <*> f ta
    f (SconType True _) = parseErrorK "(!) annotation in wrong place."
    f SconOp {} =  parseErrorK "unexpected operator in type signature."
    f (SconApp t ts) = do foldl HsTyApp <$> f t <*> mapM f (reverse ts)

checkSconType' :: [Either Name HsType] -> P Scon
checkSconType' xs = ans where
    ans = do F.shunt sconShuntSpec xs
    sconShuntSpec = F.shuntSpec { F.lookupToken, F.application, F.operator } where
        lookupToken (Left t) | t == vu_Bang = return (Right (F.Prefix,11))
        lookupToken (Left t) | t == tc_Arrow = return (Right (F.R,0))
        lookupToken (Left t) = return (Right (F.L,9))
        lookupToken (Right t) = return (Left (SconType False t))
        application e1 e2 = return $ app e1 e2
--        operator (Left t) [SconType False ta,SconType False tb] | t == tc_Arrow = return $ SconType False (HsTyFun ta tb)
        operator (Left t) [SconType _ ty] | t == vu_Bang = return $ SconType True ty
        operator ~(Left t) as = return $ foldl app (SconOp $ nameTyLevel_s typeLevel t) as
        app (SconApp a bs) e2 =  SconApp a (e2:bs)
        app e1 e2 =  SconApp e1 [e2]

checkAssertion :: HsType -> P HsAsst
checkAssertion t =  f [] t where
    f ts (HsTyCon c) =  tast (c,ts)
    f ts (HsTyApp a t) = f (t:ts) a
    f _ _ = parseErrorK "malformed class assertion"
    tast (a,[HsTyVar n]) = return (HsAsst a [n]) -- (a,n)
    tast x = parseErrorK $ "Invalid Class. multiparameter classes not yet supported:" ++ show x

checkValDef :: SrcLoc -> HsExp -> HsRhs -> [HsDecl] -> P HsDecl
checkValDef srcloc lhs rhs whereBinds = withSrcLoc srcloc $ do
        bangPatterns <- flip fopts' FO.BangPatterns <$> getOptions
        --parseWarn $ show lhs

            -- The top level pattern parsing to determine whether it is a function
            -- or pattern definition is done without knowledge of precedence.
        let patShuntSpec = F.shuntSpec { F.lookupToken, F.application, F.operator } where
                lookupToken (HsBackTick (HsVar v)) | bangPatterns && v == vu_Bang = return (Right (F.Prefix,11))
                lookupToken (HsBackTick (HsVar v)) | v == vu_Twiddle = return (Right (F.Prefix,11))
                lookupToken (HsBackTick (HsVar v)) | v == vu_At = return (Right (F.R,12))
                lookupToken (HsBackTick t) = return (Right (F.L,9))
                lookupToken t = return (Left t)
                application e1 e2 = return $ HsApp e1 e2
                operator ~(HsBackTick t) as = f t as where
                    f (HsVar v) [e] | v == vu_Bang && bangPatterns = do sl <- getSrcSpan; return $ HsBangPat (Located sl e)
                    f (HsVar v) [e] | v == vu_Twiddle = do sl <- getSrcSpan; return $ HsIrrPat (Located sl e)
                    f (HsVar v) [HsVar ap, e] | v == vu_At = do sl <- getSrcSpan; return $ HsAsPat ap e
                    f (HsVar v) [HsWildCard {}, e] | v == vu_At = do sl <- getSrcSpan; return e
                    f t as = return $ foldl HsApp t as
            isFullyConst p = f p where
                f (HsPApp _ as) = all f as
                f (HsPList as) = all f as
                f (HsPTuple as) = all f as
                f (HsPParen a) = f a
                f HsPLit {} = True
                f _ = False
        let f HsInfixApp {} = parseErrorK "Unexpected Infix application"
            f HsApp {}  = parseErrorK "Unexpected application"
            f HsBackTick {} = parseErrorK "Did not expect operator binding"
            f (HsLocatedExp (Located sl e)) = withSrcSpan sl $ f e
            f HsWords { .. } = F.shunt patShuntSpec hsExpExps >>= g []
            f _ = return Nothing
            g es (HsApp f e) = g (e:es) f
            g es@(_:_) (HsVar v) = return $ Just (v,es)
            g _ _ = return Nothing
        f lhs >>= \x -> case x of
            Just (f,es) -> do
                es <- mapM checkPattern es
                return (HsFunBind [HsMatch srcloc f es rhs whereBinds])
            Nothing -> do
                lhs <- tryP $ checkPattern lhs
                when (isFullyConst lhs) $
                    parseErrorK "pattern binding cannot be fully const"
                return (HsPatBind srcloc lhs rhs whereBinds)

-- when we have a sequence of patterns we don't allow top level binary
-- operators, but instead give a sequence of responses. This is used in lambda
-- notation. \ x y -> .. is \ x -> \ y -> .. not \ (x y) -> ..
checkPatterns :: HsExp -> P [HsPat]
checkPatterns HsWords { .. } = do
    bangPatterns <- flip fopts' FO.BangPatterns <$> getOptions
    let patShuntSpec = F.shuntSpec { F.lookupToken, F.application, F.operator } where
            lookupToken (HsBackTick (HsVar v)) | bangPatterns && v == vu_Bang = return (Right (F.Prefix,11))
            lookupToken (HsBackTick (HsVar v)) | v == vu_Twiddle = return (Right (F.Prefix,11))
            lookupToken (HsBackTick (HsVar v)) | v == vu_At = return (Right (F.R,12))
            lookupToken HsBackTick {} = fail "sequence of pattern bindings can't have top level operators."
            lookupToken t = do
                p <- checkPattern t
                return (Left [p])
            application e1 e2 = return $ e1 ++ e2
            operator (HsBackTick (HsVar v)) [[e]] | bangPatterns && v == vu_Bang = do
                sl <- getSrcSpan; return $ [HsPBangPat (Located sl e)]
            operator (HsBackTick (HsVar v)) [[e]] | v == vu_Twiddle = do
                sl <- getSrcSpan; return $ [HsPIrrPat (Located sl e)]
            operator (HsBackTick (HsVar v)) [[HsPVar ap],[e]] | v == vu_At = do
                sl <- getSrcSpan; return $ [HsPAsPat ap e]
            operator (HsBackTick (HsVar v)) [ap,[e]] | v == vu_At = do
                parseErrorK "as pattern must bind variable"
                return [e]
            operator t as = fail "unexpected operator in checkPatterns"
    F.shunt patShuntSpec hsExpExps
checkPatterns e = (:[]) <$> checkPattern e

checkPattern :: HsExp -> P HsPat
checkPattern be = do
    bangPatterns <- flip fopts' FO.BangPatterns <$> getOptions
    let checkPatField :: HsFieldUpdate -> P HsPatField
        checkPatField (HsField n e) = HsField n <$> checkPattern e
    case be of
        HsInfixApp {} -> parseErrorK "unexpected infix application"
        HsLocatedExp (Located sl e) -> withSrcSpan sl $ checkPattern e
        HsApp e1 e2 -> f e1 [e2] where
            f (HsApp e1 e2) es = f e1 (e2:es)
            f (HsCon c) es = HsPApp c <$> mapM checkPattern es
            f _ _ = parseErrorK "invalid application in pattern."
        (HsBackTick (HsVar x))
            | bangPatterns && x == vu_Bang -> return (HsPatBackTick $ HsPVar u_Bang)
            | x == vu_Twiddle -> return (HsPatBackTick $ HsPVar u_Twiddle)
            | x == vu_At      -> return (HsPatBackTick $ HsPVar u_At)
        HsVar x -> do
            when (isJust $ getModule x) $
                parseErrorK "Qualified name in binding position."
            return (HsPVar x)
        HsCon c -> return (HsPApp c [])
        HsBackTick t -> HsPatBackTick <$> checkPattern t
        HsLit l  -> return (HsPLit l)
        HsError { hsExpErrorType = HsErrorUnderscore } -> return HsPWildCard
        HsTuple es -> HsPTuple <$> mapM checkPattern es
        HsUnboxedTuple es -> HsPUnboxedTuple <$> mapM checkPattern es
        HsList es  -> HsPList <$> mapM checkPattern es
        HsParen e -> HsPParen <$> checkPattern e
        HsAsPat n e -> HsPAsPat n <$> checkPattern e
        HsWildCard {} -> return HsPWildCard
        HsIrrPat e  -> HsPIrrPat <$> T.traverse checkPattern e
        HsBangPat e -> HsPBangPat <$> T.traverse checkPattern e
        HsWords es -> HsPatWords <$> mapM checkPattern es
        HsRecConstr c fs -> HsPRec c <$> mapM checkPatField fs
        HsNegApp e -> HsPNeg <$> checkPattern e
        HsExpTypeSig sl e t -> HsPTypeSig sl <$> checkPattern e <*> return t
        HsLambda {} -> parseErrorK "Lambda not allowed in pattern."
        HsLet {} -> parseErrorK "let not allowed in pattern."
        HsDo {} -> parseErrorK "do not allowed in pattern."
        HsIf {} -> parseErrorK "if not allowed in pattern."
        HsCase {} -> parseErrorK "case not allowed in pattern."
        e -> parseErrorK ("pattern error " ++  show e)

fixupHsDecls :: [HsDecl] -> [HsDecl]
fixupHsDecls ds = f ds where
    f (d@(HsFunBind matches):ds) =  (HsFunBind newMatches) : f different where
        funName = matchName $ head matches
        (same, different) = span (sameFun funName) (d:ds)
        newMatches =  collectMatches same
    f (d:ds) =  d : f ds
    f [] = []
    -- get the variable name bound by a match
    matchName HsMatch { .. } = hsMatchName

    -- True if the decl is a HsFunBind and binds the same name as the
    -- first argument, False otherwise
    sameFun :: Name -> HsDecl -> Bool
    sameFun name (HsFunBind matches@(_:_)) = name == (matchName $ head matches)
    sameFun _ _ = False
    collectMatches :: [HsDecl] -> [HsMatch]
    collectMatches [] = []
    collectMatches (d:ds) = case d of
        (HsFunBind matches) -> matches ++ collectMatches ds
        _                   -> collectMatches ds

checkDataHeader :: HsQualType -> P (HsContext,Name,[Name])
checkDataHeader (HsQualType cs t) = do
	(c,ts) <- checkSimple "data/newtype" t []
	return (cs,c,ts)

checkSimple :: String -> HsType -> [Name] -> P ((Name,[Name]))
checkSimple kw t xs = f t xs where
    f (HsTyApp l (HsTyVar a)) xs = checkSimple kw l (a:xs)
    f (HsTyCon t)   xs = return (t,xs)
    f (HsTyTuple []) [] = return (tc_Unit,[])
    f _ _ = fail ("Illegal " ++ kw ++ " declaration")

qualTypeToClassHead :: HsQualType -> P HsClassHead
qualTypeToClassHead qt = do
    let fromHsTypeApp t = f t [] where
            f (HsTyApp a b) rs = f a (b:rs)
            f t rs = (t,rs)
    case fromHsTypeApp $ hsQualTypeType qt of
        (HsTyCon className,as) -> return HsClassHead { hsClassHeadContext = hsQualTypeContext qt, hsClassHead = toName ClassName className, hsClassHeadArgs = as }
        _ -> fail "Invalid Class Head"

doForeign :: Monad m => SrcLoc -> [Name] -> Maybe (String,Name) -> HsQualType -> m HsDecl
doForeign srcLoc names ms qt = ans where
    ans = do
        (mstring,vname@(nameParts -> (_,Nothing,cname)),names') <- case ms of
            Just (s,n) -> return (Just s,n,names)
            Nothing -> do
                (n:ns) <- return $ reverse names
                return (Nothing,n,reverse ns)
        let f ["import","primitive"] cname = return $ HsForeignDecl srcLoc (FfiSpec (Import cname mempty) Safe Primitive) vname qt
            f ["import","dotnet"] cname = return $ HsForeignDecl srcLoc (FfiSpec (Import cname mempty) Safe DotNet) vname qt
            f ("import":rs) cname = do
                let (safe,conv) = pconv rs
                im <- parseImport conv mstring vname
                conv <- return (if conv == CApi then CCall else conv)
                return $ HsForeignDecl srcLoc (FfiSpec im safe conv) vname qt
            f ("export":rs) cname = do
                let (safe,conv) = pconv rs
                return $ HsForeignExport srcLoc (FfiExport cname safe conv undefined undefined) vname qt
            f _ _ = error "ParseUtils: bad."
        f (map show names') (maybe cname id mstring) where
    pconv rs = g Safe CCall rs where
        g _ cc ("safe":rs) = g Safe cc rs
        g _ cc ("unsafe":rs) = g Unsafe cc rs
        g s _  ("ccall":rs)  = g s CCall rs
        g s _  ("capi":rs)  = g s CApi rs
        g s _  ("stdcall":rs) = g s StdCall rs
        g s c  [] = (s,c)
        g _ _ rs = error $ "FrontEnd.ParseUtils: unknown foreign flags " ++ show rs

-- FFI parsing

-- parseExport :: Monad m => String -> Name -> m String
-- parseExport cn hn =
--     case words cn of
--       [x] | isCName x -> return x
--       []              -> return (show hn)
--       _               -> fail ("Invalid cname in export declaration: "++show cn)

parseImport :: Monad m => CallConv -> Maybe String -> Name -> m FfiType
parseImport _ Nothing hn = return $ Import (show hn) mempty
parseImport cc (Just cn) hn =
    case words cn of
      ["dynamic"]   -> return Dynamic
      ["wrapper"]   -> return Wrapper
      []            -> return $ Import (show hn) mempty
      ("static":xs) -> parseIS cc xs
      xs            -> parseIS cc xs

parseIS cc rs = f Set.empty rs where
    f s ['&':n] | isCName n = return $ ImportAddr n $ Requires s
    f s [n]     | isCName n = return $ Import     n $ Requires s
    f s ["&",n] | isCName n = return $ ImportAddr n $ Requires s
    f s (i:r)               = f (Set.insert (cc,packString i) s) r
    f s x                   = fail ("Syntax error parsing foreign import: "++show x)

isCName []     = False
isCName (c:cs) = p1 c && all p2 cs
    where p1 c = isAlpha c    || any (c==) oa
          p2 c = isAlphaNum c || any (c==) oa
          oa   = "_-$"

implicitName :: Name -> P Name
implicitName n = do
    opt <- getOptions
    return $ if fopts' opt FO.Prelude
        then quoteName n
        else toUnqualified n

mkRecConstrOrUpdate :: HsExp -> [(Name,Maybe HsExp)] -> P HsExp
mkRecConstrOrUpdate e fs = f e fs where
    f (HsCon c) fs       = return (HsRecConstr c fs')
    f e         fs@(_:_) = return (HsRecUpdate e fs')
    f _         _        = fail "Empty record update"
    g (x,Just y) = HsField x y
    g (x,Nothing) | x == u_DotDot = HsField u_DotDot (HsVar u_DotDot)
    g (x,Nothing) = HsField x (HsVar $ toName Val x)
    fs' = map g fs
