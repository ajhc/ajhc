-- post process source after initial parsing
module FrontEnd.Lex.Post(
    checkContext,
    checkDataHeader,
    checkPattern',
    checkPattern,
    checkSconType,
    checkValDef,
    doForeign,
    qualTypeToClassHead,
    fixupHsDecls
) where

import C.FFI
import Control.Applicative
import Data.Char
import Data.Monoid
import FrontEnd.HsSyn
import FrontEnd.Lex.ParseMonad
import FrontEnd.SrcLoc
import Name.Names
import PackedString
import qualified Data.Set as Set
import qualified Data.Traversable as T
import qualified FrontEnd.Lex.Fixity as F

checkContext :: HsType -> P HsContext
checkContext (HsTyCon (toName TypeConstructor -> name)) | name == tc_Unit = return []
checkContext (HsTyTuple []) = return []
checkContext (HsTyTuple ts) = mapM checkAssertion ts
checkContext t = (:[]) <$> checkAssertion t

data Scon = SconApp Scon [Scon] | SconType Bool HsType | SconOp Name
    deriving(Show)
checkSconType :: [Either Name HsType] -> P (Name, [HsBangType])
checkSconType xs = ans where
    ans = do
        s <- F.shunt sconShuntSpec xs
        let f s = case s of
                SconOp n -> n
                SconType False (HsTyCon c) -> c
            g (SconType False t) = HsUnBangedTy t
            g (SconType True t) = HsBangedTy t
        case s of
            SconApp t ts -> return (f t, map g ts)
            _ -> return (f s,[])

    sconShuntSpec = F.shuntSpec { F.lookupToken, F.application, F.operator } where
        lookupToken (Left t) | t == u_Bang = return (Right (F.Prefix,11))
        lookupToken (Left t) = return (Right (F.L,9))
        lookupToken (Right t) = return (Left (SconType False t))
        application e1 e2 = return $ app e1 e2
        operator (Left t) [SconType _ ty] | t == u_Bang = return $ SconType True ty
        operator (Left t) as = return $ foldl app (SconOp t) as
        app (SconApp a bs) e2 =  SconApp a (e2:bs)
        app e1 e2 =  SconApp e1 [e2]

checkAssertion :: HsType -> P HsAsst
checkAssertion t =  f [] t where
    f ts (HsTyCon c) =  tast (c,ts)
    f ts (HsTyApp a t) = f (t:ts) a
    f _ _ = parseErrorK "malformed class assertion"
    tast (a,[HsTyVar n]) = return (HsAsst a [n]) -- (a,n)
    tast _ = parseErrorK "Invalid Class. multiparameter classes not yet supported"

checkValDef :: SrcLoc -> HsExp -> HsRhs -> [HsDecl] -> P HsDecl
checkValDef srcloc lhs rhs whereBinds = withSrcLoc srcloc $ ans lhs where
    ans HsWords { .. } = F.shunt patShuntSpec hsExpExps >>= ans
    ans (HsLocatedExp (Located sl e)) = withSrcSpan sl $ ans e
    ans lhs = case isFunLhs lhs [] of
        Just (f,es) -> do
--            ps <- mapM checkPattern es
            return (HsFunBind [HsMatch srcloc f (map HsPatExp es) rhs whereBinds])
        Nothing     -> do
--            lhs <- checkPattern lhs
            return (HsPatBind srcloc (HsPatExp lhs) rhs whereBinds)
    isFunLhs (HsInfixApp l (HsVar ( op)) r) es = Just (op, l:r:es)
    isFunLhs (HsApp (HsVar ( f)) e) es = Just (f, e:es)
    isFunLhs (HsApp (HsParen f) e) es = isFunLhs f (e:es)
    isFunLhs (HsApp f e) es = isFunLhs f (e:es)
    isFunLhs _ _ = Nothing

-- A variable binding is parsed as an HsPatBind.

checkPattern :: HsExp -> P HsPat
checkPattern e = return $ HsPatExp e

checkPattern' :: HsExp -> P HsPat
checkPattern' e = checkPat e [] where
    checkPat :: HsExp -> [HsPat] -> P HsPat
    checkPat (HsCon c) args = return (HsPApp c args)
    checkPat (HsApp f x) args = do
            x <- checkPat x []
            checkPat f (x:args)
    checkPat (HsLocatedExp (Located sl e)) xs = withSrcSpan sl $ checkPat e xs
    checkPat e [] = case e of
            HsVar x            -> return (HsPVar x)
            HsLit l            -> return (HsPLit l)
            HsError { hsExpErrorType = HsErrorUnderscore } -> return HsPWildCard
            HsInfixApp l op r  -> do
                l <- checkPat l []
                r <- checkPat r []
                case op of
                    HsCon c -> return (HsPInfixApp l c r)
                    _ -> patFail
            HsTuple es -> do
                ps <- mapM (\e -> checkPat e []) es
                return (HsPTuple ps)
            HsUnboxedTuple es  -> do
                ps <- mapM (\e -> checkPat e []) es
                return (HsPUnboxedTuple ps)
            HsList es	   -> do
                ps <- mapM (\e -> checkPat e []) es
                return (HsPList ps)
            HsParen e	   -> do
                p <- checkPat e []
                return (HsPParen p)
            HsAsPat n e	   -> do
                p <- checkPat e []
                return (HsPAsPat n p)
            HsWildCard {} -> return HsPWildCard
            HsIrrPat e         -> do
                p <- T.mapM checkPattern e
                return (HsPIrrPat p)
            HsBangPat e         -> do
                p <- T.mapM checkPattern e
                return (HsPBangPat p)
            HsRecConstr c fs   -> do
                fs <- mapM checkPatField fs
                return (HsPRec c fs)
            HsNegApp (HsLit l) -> return (HsPNeg (HsPLit l))
            HsExpTypeSig sl e t -> do
                p <- checkPat e []
                return (HsPTypeSig sl p t)
            e -> parseErrorK ("pattern error " ++  show e)
    checkPat e _ = parseErrorK ("pattern error " ++  show e)
    patFail = parseErrorK "pattern error"
    checkPatField :: HsFieldUpdate -> P HsPatField
    checkPatField (HsField n e) = do
            p <- checkPat e []
            return (HsField n p)

-- The top level pattern parsing to determine whether it is a function
-- or pattern definition is done without knowledge of precedence
patShuntSpec = F.shuntSpec { F.lookupToken, F.application, F.operator } where
    lookupToken (HsBackTick t) = return (Right (F.L,9))
--    lookupToken v@(HsVar bt) | isOpLike bt = return (Right (F.L,9))
--    lookupToken v@(HsCon bt) | isOpLike bt = return (Right (F.L,9))
    lookupToken t = return (Left t)
    application e1 e2 = return $ HsApp e1 e2
    operator t as = return $ foldl HsApp t as

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
checkSimple kw (HsTyApp l (HsTyVar a)) xs = checkSimple kw l (a:xs)
checkSimple _kw (HsTyCon t)   xs = return (t,xs)
checkSimple kw _ _ = fail ("Illegal " ++ kw ++ " declaration")

qualTypeToClassHead :: HsQualType -> P HsClassHead
qualTypeToClassHead qt = do
    let fromHsTypeApp t = f t [] where
            f (HsTyApp a b) rs = f a (b:rs)
            f t rs = (t,rs)
    case fromHsTypeApp $ hsQualTypeType qt of
        (HsTyCon className,as) -> return HsClassHead { hsClassHeadContext = hsQualTypeContext qt, hsClassHead = className, hsClassHeadArgs = as }
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

parseExport :: Monad m => String -> Name -> m String
parseExport cn hn =
    case words cn of
      [x] | isCName x -> return x
      []              -> return (show hn)
      _               -> fail ("Invalid cname in export declaration: "++show cn)

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



{-

splitTyConApp :: HsType -> P (Name,[HsType])
splitTyConApp t0 = split t0 []
 where
	split :: HsType -> [HsType] -> P (Name,[HsType])
	split (HsTyApp t u) ts = split t (u:ts)
	split (HsTyCon t) ts = return (t,ts)
	split _ _ = fail "Illegal data/newtype declaration"
--	split a b = fail $ "Illegal data/newtype declaration: " ++ show (a,b)

-----------------------------------------------------------------------------
-- Various Syntactic Checks

-- Changed for multi-parameter type classes

checkAssertion :: HsType -> P HsAsst
checkAssertion t =  checkAssertion' [] t
	where	checkAssertion' ts (HsTyCon c) =  tast (c,ts)
		checkAssertion' ts (HsTyApp a t) = checkAssertion' (t:ts) a
		checkAssertion' _ _ = fail "Illegal class assertion"
                tast (a,[HsTyVar n]) = return (HsAsst a [n]) -- (a,n)
                tast _ = fail "Invalid Class. multiparameter classes not yet supported"
--checkAssertion = checkAssertion' []
--	where	checkAssertion' ts (HsTyCon c) = return (c,ts)
--		checkAssertion' ts (HsTyApp a t) = checkAssertion' (t:ts) a
--		checkAssertion' _ _ = fail "Illegal class assertion"

checkPatterns :: [HsExp] -> P [HsPat]
checkPatterns es = mapM checkPattern es

--checkSimple kw t ts = fail ("Illegal " ++ kw ++ " declaration: " ++ show (t,ts))

{-
checkInstHeader :: HsQualType -> P (HsContext,Name,[HsType])
checkInstHeader (HsQualType cs t) = do
	(c,ts) <- checkInsts t []
	return (cs,c,ts)

checkInsts :: HsType -> [HsType] -> P ((Name,[HsType]))
checkInsts (HsTyApp l t) ts = checkInsts l (t:ts)
checkInsts (HsTyCon c)   ts = return (c,ts)
checkInsts _ _ = fail "Illegal instance declaration"
-}

-----------------------------------------------------------------------------
-- Checking Patterns.

-- We parse patterns as expressions and check for valid patterns below,
-- converting the expression into a pattern at the same time.

-----------------------------------------------------------------------------
-- Check Expression Syntax

checkExpr :: HsExp -> P HsExp
checkExpr e = case e of
	HsVar _			  -> return e
	HsCon _			  -> return e
	HsLit _			  -> return e
	HsInfixApp e1 op e2	  -> check2Exprs e1 e2 (flip HsInfixApp op)
	HsApp e1 e2		  -> check2Exprs e1 e2 HsApp
	HsNegApp e		  -> check1Expr e HsNegApp
	HsLambda loc ps e	  -> check1Expr e (HsLambda loc ps)
	HsLet bs e		  -> check1Expr e (HsLet bs)
	HsIf e1 e2 e3		  -> check3Exprs e1 e2 e3 HsIf
	HsCase e alts		  -> do
				     alts <- mapM checkAlt alts
				     e <- checkExpr e
				     return (HsCase e alts)
	HsDo stmts		  -> do
				     stmts <- mapM checkStmt stmts
				     return (HsDo stmts)
	HsTuple es		  -> checkManyExprs es HsTuple
	HsUnboxedTuple es	  -> checkManyExprs es HsUnboxedTuple
	HsList es		  -> checkManyExprs es HsList
	HsParen e		  -> check1Expr e HsParen
	HsLeftSection e op	  -> check1Expr e (flip HsLeftSection op)
	HsRightSection op e	  -> check1Expr e (HsRightSection op)
	HsRecConstr c fields	  -> do
				     fields <- mapM checkField fields
				     return (HsRecConstr c fields)
	HsRecUpdate e fields	  -> do
				     fields <- mapM checkField fields
				     e <- checkExpr e
				     return (HsRecUpdate e fields)
	HsEnumFrom e		  -> check1Expr e HsEnumFrom
	HsEnumFromTo e1 e2	  -> check2Exprs e1 e2 HsEnumFromTo
	HsEnumFromThen e1 e2      -> check2Exprs e1 e2 HsEnumFromThen
	HsEnumFromThenTo e1 e2 e3 -> check3Exprs e1 e2 e3 HsEnumFromThenTo
	HsListComp e stmts        -> do
				     stmts <- mapM checkStmt stmts
				     e <- checkExpr e
				     return (HsListComp e stmts)
	HsExpTypeSig loc e ty     -> do
				     e <- checkExpr e
				     return (HsExpTypeSig loc e ty)
        HsAsPat _ _     -> fail "@ only valid in pattern"
        HsWildCard sl   -> return $ HsWildCard sl -- TODO check for strict mode
        HsIrrPat _      -> fail "~ only valid in pattern"
	_                         -> fail "Parse error in expression"

-- type signature for polymorphic recursion!!
check1Expr :: HsExp -> (HsExp -> a) -> P a
check1Expr e1 f = do
	e1 <- checkExpr e1
	return (f e1)

check2Exprs :: HsExp -> HsExp -> (HsExp -> HsExp -> a) -> P a
check2Exprs e1 e2 f = do
	e1 <- checkExpr e1
	e2 <- checkExpr e2
	return (f e1 e2)

check3Exprs :: HsExp -> HsExp -> HsExp -> (HsExp -> HsExp -> HsExp -> a) -> P a
check3Exprs e1 e2 e3 f = do
	e1 <- checkExpr e1
	e2 <- checkExpr e2
	e3 <- checkExpr e3
	return (f e1 e2 e3)

checkManyExprs :: [HsExp] -> ([HsExp] -> a) -> P a
checkManyExprs es f = do
	es <- mapM checkExpr es
	return (f es)

checkAlt :: HsAlt -> P HsAlt
checkAlt (HsAlt loc p galts bs) = do
	galts <- checkGAlts galts
	return (HsAlt loc p galts bs)

checkGAlts :: HsRhs -> P HsRhs
checkGAlts (HsUnGuardedRhs e) = check1Expr e HsUnGuardedRhs
checkGAlts (HsGuardedRhss galts) = do
	galts <- mapM checkGAlt galts
	return (HsGuardedRhss galts)

checkGAlt :: HsGuardedRhs -> P HsGuardedRhs
checkGAlt (HsGuardedRhs loc e1 e2) = check2Exprs e1 e2 (HsGuardedRhs loc)

checkStmt :: HsStmt -> P HsStmt
checkStmt (HsGenerator loc p e) = check1Expr e (HsGenerator loc p)
checkStmt (HsQualifier e) = check1Expr e HsQualifier
checkStmt s@(HsLetStmt _) = return s

checkField :: HsFieldUpdate -> P HsFieldUpdate
checkField (HsField n e) = check1Expr e (HsField n)

-----------------------------------------------------------------------------
-- Check Equation Syntax

-----------------------------------------------------------------------------
-- In a class or instance body, a pattern binding must be of a variable.

{-
checkClassBody :: [HsDecl] -> P [HsDecl]
checkClassBody decls = do
	mapM_ checkMethodDef decls
	return decls

checkMethodDef :: HsDecl -> P ()
checkMethodDef (HsPatBind _ (HsPVar _) _ _) = return ()
checkMethodDef (HsPatBind loc _ _ _) =
	fail "illegal method definition" `atSrcLoc` loc
checkMethodDef _ = return ()
-}

-----------------------------------------------------------------------------
-- Check that an identifier or symbol is unqualified.
-- For occasions when doing this in the grammar would cause conflicts.

checkUnQual :: Name -> P Name
checkUnQual n = if isJust (getModule n) then fail "Illegal qualified name" else return n
--checkUnQual (Qual _ _) = fail "Illegal qualified name"
--checkUnQual n@(UnQual _) = return n
--checkUnQual (Special _) = fail "Illegal special name"

-----------------------------------------------------------------------------
-- Miscellaneous utilities

checkPrec :: Integer -> P Int
checkPrec i | 0 <= i && i <= 9 = return (fromInteger i)
checkPrec i | otherwise	       = fail ("Illegal precedence " ++ show i)

mkRecConstrOrUpdate :: HsExp -> [HsFieldUpdate] -> P HsExp
mkRecConstrOrUpdate (HsCon c) fs       = return (HsRecConstr c fs)
mkRecConstrOrUpdate e         fs@(_:_) = return (HsRecUpdate e fs)
mkRecConstrOrUpdate _         _        = fail "Empty record update"

-----------------------------------------------------------------------------
-- Reverse a list of declarations, merging adjacent HsFunBinds of the
-- same name and checking that their arities match.

{-
checkRevDecls :: [HsDecl] -> P [HsDecl]
checkRevDecls = mergeFunBinds []
    where
	mergeFunBinds revDs [] = return revDs
	mergeFunBinds revDs (HsFunBind ms1@(HsMatch _ name ps _ _:_):ds1) =
		mergeMatches ms1 ds1
	    where
		arity = length ps
		mergeMatches ms' (HsFunBind ms@(HsMatch loc name' ps' _ _:_):ds)
		    | name' == name =
			if length ps' /= arity
			then fail ("arity mismatch for '" ++ show name ++ "'")
			     `atSrcLoc` loc
			else mergeMatches (ms++ms') ds
		mergeMatches ms' ds = mergeFunBinds (HsFunBind ms':revDs) ds
	mergeFunBinds revDs (d:ds) = mergeFunBinds (d:revDs) ds
-}

-- this used to be done in post-process

-- collect associated funbind equations (matches) into a single funbind
-- intended as a post-processer for the parser output
fixupHsDecls :: [HsDecl] -> [HsDecl]
fixupHsDecls (d@(HsFunBind matches):ds) =  (HsFunBind newMatches) : fixupHsDecls different where
    funName = matchName $ head matches
    (same, different) = span (sameFun funName) (d:ds)
    newMatches =  collectMatches same
fixupHsDecls (d:ds) =  d : fixupHsDecls ds
fixupHsDecls [] = []
-- get the variable name bound by a match
matchName (HsMatch _sloc name _pats _rhs _whereDecls) = name

-- True if the decl is a HsFunBind and binds the same name as the
-- first argument, False otherwise
sameFun :: Name -> HsDecl -> Bool
sameFun name (HsFunBind matches@(_:_)) = name == (matchName $ head matches)
sameFun _ _ = False


doForeignEq :: Monad m => SrcLoc -> [Name] -> Maybe (String,Name) -> HsQualType -> HsExp -> m HsDecl
doForeignEq srcLoc names ms qt e = undefined

-- collects all the HsMatch equations from any FunBinds
-- from a list of HsDecls

-- Stolen from Hugs' Prelude

readInteger :: String -> Integer
readInteger ('0':'o':ds) = readInteger2  8 isOctDigit ds
readInteger ('0':'x':ds) = readInteger2 16 isHexDigit ds
readInteger          ds  = readInteger2 10 isDigit    ds

readInteger2 :: Integer -> (Char -> Bool) -> String -> Integer
readInteger2 radix _ ds = foldl1 (\n d -> n * radix + d) (map (fromIntegral . digitToInt) ds)

-- Hack...

readRational :: String -> Rational
readRational xs = (readInteger (i++m))%1 * 10^^(case e of {[] -> 0;  ('+':e2) -> read e2; _ -> read e} - length m)
  where (i,r1) = span isDigit xs
        (m,r2) = span isDigit (dropWhile (=='.') r1)
        e      = dropWhile (=='e') r2
-}
