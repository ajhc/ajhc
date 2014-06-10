    -- various desugaring routines
--
-- The general desugaring routine creates selectors for data
-- constructors with named fields, changes all pattern bindings
-- into 'simple' pattern bindings, and adds failure cases to lambda
-- expressions which have failable patterns

module FrontEnd.Desugar (
    desugarHsModule
    ,desugarHsStmt
    ,listCompToExp
    ,isSimplePat
    ,isFailablePat
    ,doToComp
    ,doCompToExp) where

import FrontEnd.Syn.Traverse
import FrontEnd.Warning
import Name.Names
import Ty.Level
import Util.Std
import Util.UniqueMonad

type PatSM = Uniq

instance MonadSrcLoc PatSM where
instance MonadSetSrcLoc PatSM where
    withSrcLoc' _ a = a

-- a new (unique) name introduced in pattern selector functions
newPatVarName :: HsName
newPatVarName = toName Val ("pv@"::String)

desugarHsModule :: HsModule -> HsModule
--desugarHsModule m = m
desugarHsModule m = hsModuleDecls_s ds' m where
    (ds', _) = runUniq 0 (dsm (hsModuleDecls m)) -- (0::Int)
    dsm ds = fmap concat $ mapM desugarDecl ds

desugarHsStmt :: Monad m => HsStmt -> m HsStmt
desugarHsStmt s = return $ fst $ runUniq 0 (desugarStmt s)

desugarDecl :: HsDecl -> PatSM [HsDecl]
desugarDecl (HsFunBind matches) = do
    newMatches <- mapM desugarMatch matches
    return [HsFunBind newMatches]

--variable pattern bindings remain unchanged
desugarDecl HsPatBind { hsDeclPat = hsDeclPat@HsPVar {}, .. } = do
    hsDeclRhs <- desugarRhs hsDeclRhs
    hsDeclDecls <- concat <$> mapM desugarDecl hsDeclDecls
    return [HsPatBind { .. }]
-- desugarDecl HsPatBind { .. } = do
--     hsDeclRhs <- desugarRhs hsDeclRhs
--     hsDeclDecls <- concat <$> mapM desugarDecl hsDeclDecls
--     return [HsPatBind { .. }]
desugarDecl HsPatBind { .. } = do
    hsDeclRhs <- desugarRhs hsDeclRhs
    hsDeclDecls <- concat <$> mapM desugarDecl hsDeclDecls
    unique <- newUniq
    let newRhsName = toName Val ("rhs@" ++ show unique)
    let newBinds = genBindsForPat hsDeclPat hsDeclSrcLoc newRhsName
    newBinds <- concat <$> mapM desugarDecl newBinds
    let newTopDeclForRhs = HsPatBind { hsDeclPat = HsPVar newRhsName, .. }
    return (newTopDeclForRhs : newBinds)
desugarDecl HsInstDecl { .. } = do
    hsDeclDecls <- concat <$> mapM desugarDecl hsDeclDecls
    return [HsInstDecl { .. }]
desugarDecl HsClassDecl { .. } = do
    hsDeclDecls <- concat <$> mapM desugarDecl hsDeclDecls
    return [HsClassDecl { .. }]
-- XXX we currently discard instance specializations
desugarDecl HsPragmaSpecialize { hsDeclName = n } | n == u_instance = return []
desugarDecl anyOtherDecl = return [anyOtherDecl]

desugarMatch :: (HsMatch) -> PatSM (HsMatch)
desugarMatch (HsMatch sloc funName pats rhs wheres) = do
        newWheres <- mapM desugarDecl wheres
        newRhs <- desugarRhs rhs
        return (HsMatch sloc funName pats newRhs (concat newWheres))

-- generate the pattern bindings for each variable in a pattern

genBindsForPat :: HsPat -> SrcLoc -> HsName -> [HsDecl]
genBindsForPat pat sloc rhs = ans where
    ans = [HsPatBind sloc (HsPVar pn) (HsUnGuardedRhs selector) [] | (pn, selector) <- selFuns]
    selFuns = getPatSelFuns sloc pat rhs

-- generate selector functions for each of the variables that
-- are bound in a pattern

getPatSelFuns :: SrcLoc -> HsPat -> Name -> [(Name, HsExp)]
getPatSelFuns sloc pat rhsvar = ans where
    ans = [(v, kase (replaceVarNamesInPat v pat)) | v <- getNamesFromHsPat pat, nameType v == Val]
    kase p =  HsCase (HsVar rhsvar) (if isFailablePat p then [a1, a2 ] else [a1]) where
       a1 =  HsAlt sloc p (HsUnGuardedRhs (HsVar newPatVarName)) []
       a2 =  HsAlt sloc HsPWildCard (HsUnGuardedRhs (HsError { hsExpSrcLoc = sloc, hsExpErrorType = HsErrorPatternFailure, hsExpString = show sloc ++ " failed pattern match" })) []

-- replaces all occurrences of a name with a new variable
-- and every other name with underscore

replaceVarNamesInPat :: HsName -> HsPat -> HsPat
replaceVarNamesInPat name p = f p where
    f (HsPVar name2)
       | name == name2 = HsPVar newPatVarName
       | getTyLevel name2 == Just termLevel = HsPWildCard
    f (HsPAsPat asName pat)
       | name == asName = HsPAsPat newPatVarName (f pat)
       | getTyLevel asName == Just termLevel = f pat
    f p = runIdentity $ traverseHsPat (return . f) p

--    f name p = error $ "replaceVarNamesInPat: " ++ show (name,p)

desugarRhs :: HsRhs -> PatSM HsRhs
desugarRhs  = traverseHsExp desugarExp

desugarExp :: HsExp -> PatSM HsExp
desugarExp (HsLet decls e) = do
    newDecls <- mapM desugarDecl decls
    HsLet (concat newDecls) <$> desugarExp e
desugarExp (HsCase e alts) = do
    newE <- desugarExp e
    newAlts <- mapM desugarAlt alts
    return (HsCase newE newAlts)
desugarExp (HsDo stmts) = HsDo `liftM` mapM desugarStmt stmts
desugarExp e = traverseHsExp desugarExp e

desugarAlt :: (HsAlt) -> PatSM (HsAlt)
desugarAlt (HsAlt sloc pat gAlts wheres) = do
    newGAlts <- desugarRhs gAlts
    newWheres <- mapM desugarDecl wheres
    return (HsAlt sloc pat newGAlts (concat newWheres))

desugarStmt :: (HsStmt) -> PatSM (HsStmt)
desugarStmt (HsLetStmt decls) = do
    newDecls <- mapM desugarDecl decls
    return (HsLetStmt $ concat newDecls)
desugarStmt (HsGenerator srcLoc pat e) = HsGenerator srcLoc pat <$> desugarExp e
desugarStmt (HsQualifier e) = HsQualifier <$> desugarExp e

listCompToExp :: Monad m => m HsName -> HsExp -> [HsStmt] -> m HsExp
listCompToExp newName exp ss = hsParen `liftM` f ss where
    f [] = return $ HsList [exp]
    f (gen:HsQualifier q1:HsQualifier q2:ss)  = f (gen:HsQualifier (hsApp (HsVar v_and) [q1,q2]):ss)
    f ((HsLetStmt ds):ss) = do ss' <- f ss; return $ hsParen (HsLet ds ss')
    f (HsQualifier e:ss) = do ss' <- f ss; return $ hsParen (HsIf e ss' (HsList []))
    f ((HsGenerator srcLoc pat e):ss) | isLazyPat pat, Just exp' <- g ss = do
        return $ hsParen $ HsVar v_map `app` HsLambda srcLoc [pat] exp' `app` e
    f ((HsGenerator srcLoc pat e):HsQualifier q:ss) | isLazyPat pat, Just exp' <- g ss = do
        npvar <- newName
        return $ hsApp (HsVar v_foldr)  [HsLambda srcLoc [pat,HsPVar npvar] $
            hsIf q (hsApp (HsCon dc_Cons) [exp',HsVar npvar]) (HsVar npvar), HsList [],e]
    f ((HsGenerator srcLoc pat e):ss) | isLazyPat pat = do
        ss' <- f ss
        return $ hsParen $ HsVar v_concatMap `app`  HsLambda srcLoc [pat] ss' `app` e
    f ((HsGenerator srcLoc pat e):HsQualifier q:ss) | isFailablePat pat || Nothing == g ss = do
        ss' <- f ss
        let kase = HsLCase  [a1, a2 ]
            a1 =  HsAlt srcLoc pat (HsGuardedRhss [HsComp srcLoc [HsQualifier q] ss']) []
            a2 =  HsAlt srcLoc HsPWildCard (HsUnGuardedRhs $ HsList []) []
        return $ hsParen $ HsVar v_concatMap `app` kase `app`  e
    f ((HsGenerator srcLoc pat e):ss) | isFailablePat pat || Nothing == g ss = do
        ss' <- f ss
        let kase = HsLCase [a1, a2 ]
            a1 =  HsAlt srcLoc pat (HsUnGuardedRhs ss') []
            a2 =  HsAlt srcLoc HsPWildCard (HsUnGuardedRhs $ HsList []) []
        return $ hsParen $ HsVar v_concatMap `app` kase `app` e
    f ((HsGenerator srcLoc pat e):ss) = do
        let Just exp' = g ss
            kase = HsLCase [a1]
            a1 =  HsAlt srcLoc pat (HsUnGuardedRhs exp') []
        return $ hsParen $ HsVar v_map `app` kase `app` e
    g [] = return exp
    g (HsLetStmt ds:ss) = do
        e <- g ss
        return (hsParen (HsLet ds e))
    g _ = Nothing
    app x y = HsApp x (hsParen y)

-- patterns are
-- failable - strict and may fail to match
-- refutable or strict - may bottom out
-- irrefutable or lazy - match no matter what
-- simple, a wildcard or variable
-- failable is a subset of refutable

isFailablePat p | isStrictPat p = f (openPat p) where
    f (HsPTuple ps) = any isFailablePat ps
    f (HsPUnboxedTuple ps) = any isFailablePat ps
    f (HsPBangPat (Located _ p)) = isFailablePat p
    f _ = True
isFailablePat _ = False

isSimplePat p = f (openPat p) where
    f HsPVar {} = True
    f HsPWildCard = True
    f _ = False

isLazyPat pat = not (isStrictPat pat)
isStrictPat p = f (openPat p) where
    f HsPVar {} = False
    f HsPWildCard = False
    f (HsPAsPat _ p) = isStrictPat p
    f (HsPParen p) = isStrictPat p
    f (HsPIrrPat p) = False -- isStrictPat p  -- TODO irrefutable patterns
    f _ = True

openPat (HsPParen p) = openPat p
openPat (HsPNeg p) = openPat p
openPat (HsPAsPat _ p) = openPat p
openPat (HsPTypeSig _ p _) = openPat p
openPat (HsPInfixApp a n b) = HsPApp n [a,b]
openPat p = p

hsApp e es = hsParen $ foldl HsApp (hsParen e) (map hsParen es)
hsIf e a b = hsParen $ HsIf e a b

hsExpError :: (MonadWarn m,MonadSrcLoc m) => String -> m HsExp
hsExpError hsExpString = do
    addWarn InvalidExp hsExpString
    hsExpSrcLoc <- getSrcLoc
    return HsError { hsExpErrorType = HsErrorInvalidExp, .. }

doToComp :: (MonadWarn m,MonadSetSrcLoc m) => [HsStmt] ->  m HsComp
doToComp ss = f (reverse ss) where
    bad s = do
        hsCompBody <- hsExpError s
        hsCompSrcLoc <- getSrcLoc
        return $ HsComp { hsCompStmts = [], .. }
    f [] = bad "A 'do' block must not be empty."
    f (HsGenerator srcLoc _ _:_) = withSrcLoc srcLoc $ bad "The last statement in a 'do' block must be an expression."
    f (HsLetStmt {}:_) = bad "The last statement in a 'do' block must be an expression."
    f (HsQualifier e:rs) = g e rs
    g e (HsLetStmt ds:rs) = g (HsLet ds e) rs
    g hsCompBody rs = do
        hsCompSrcLoc <- getSrcLoc
        return HsComp { hsCompStmts = reverse rs, .. }

doCompToExp
    :: Name      -- ^ bind  (>>=) to use
    -> Name      -- ^ bind_ (>>) to use
    -> Name      -- ^ fail to use
    -> HsComp
    -> HsExp
doCompToExp f_bind f_bind_ f_fail HsComp { .. } = f hsCompStmts where
    f [] = hsCompBody
    f (HsQualifier e:ss) = HsInfixApp (hsParen e) (HsVar f_bind_) (f ss)
    f ((HsGenerator srcLoc pat e):ss) | isSimplePat pat =
        HsInfixApp (hsParen e) (HsVar f_bind) (HsLambda srcLoc [pat] (f ss))
    f ((HsGenerator srcLoc pat e):ss) =
        let kase = HsLCase [a1, a2 ]
            a1 =  HsAlt srcLoc pat (HsUnGuardedRhs (f ss)) []
            a2 =  HsAlt srcLoc HsPWildCard (HsUnGuardedRhs (HsApp (HsVar f_fail) (HsLit $ HsString $ show srcLoc ++ " pattern match failure in do block"))) []
        in HsInfixApp (hsParen e) (HsVar f_bind) kase  where
    f (HsLetStmt decls:ss) = HsLet decls (f ss)
