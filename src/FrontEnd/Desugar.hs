    -- various desugaring routines
--
-- The general desugaring routine creates selectors for data
-- constructors with named fields, changes all pattern bindings
-- into 'simple' pattern bindings, and adds failure cases to lambda
-- expressions which have failable patterns

module FrontEnd.Desugar (doToExp, listCompToExp, desugarHsModule, desugarHsStmt) where

import Control.Monad.State

import FrontEnd.HsSyn
import FrontEnd.SrcLoc
import FrontEnd.Syn.Traverse
import Name.Name
import Name.Names

type PatState = Int

getUnique = do
    n <- get
    put (n + 1)
    return n

type PatSM = State PatState

instance MonadSrcLoc PatSM where
instance MonadSetSrcLoc PatSM where
    withSrcLoc _ a = a

-- a new (unique) name introduced in pattern selector functions
newPatVarName :: HsName
newPatVarName = nameName $ toName Val "patvar@0"

{-
 this function replaces all constructor-pattern bindings in a module with
 function calls

 ie:

 (x, y) = head $ zip "abc" [1,2,3]

 becomes

 x = (\(a, _) -> a) rhs1
 y = (\(_, a) -> a) rhs1
 rhs1 = head $ zip "abc" [1,2,3]
-}

-- first argument is imported synonyms

desugarHsModule :: HsModule -> HsModule
desugarHsModule m = hsModuleDecls_s ds' m where
    (ds', _) = runState (dsm (hsModuleDecls m)) (0::Int)
    dsm ds = fmap concat $ mapM desugarDecl ds

desugarHsStmt :: Monad m => HsStmt -> m HsStmt
desugarHsStmt s = return $ fst $ runState (desugarStmt s) (0::Int)

desugarDecl :: HsDecl -> PatSM [HsDecl]
desugarDecl (HsFunBind matches) = do
    newMatches <- mapM desugarMatch matches
    return [HsFunBind newMatches]

-- variable pattern bindings remain unchanged
desugarDecl pb@(HsPatBind sloc (HsPVar n) rhs wheres) = do
    newRhs <- desugarRhs rhs
    newWheres <- mapM desugarDecl wheres
    return [HsPatBind sloc (HsPVar n) newRhs (concat newWheres)]

desugarDecl pb@(HsPatBind sloc pat rhs wheres) = do
    rhs <- desugarRhs rhs
    unique <- getUnique
    let newRhsName = toName Val ("patrhs@" ++ show unique)
    newWheres <- mapM desugarDecl wheres
    let newTopDeclForRhs
               = HsPatBind sloc (HsPVar newRhsName) rhs (concat newWheres)
    let newBinds = genBindsForPat pat sloc newRhsName
    newBinds <- mapM desugarDecl newBinds
    return (newTopDeclForRhs : concat newBinds)

desugarDecl (HsClassDecl sloc qualtype decls) = do
    newDecls <- mapM desugarDecl decls
    return [HsClassDecl sloc qualtype (concat newDecls)]

desugarDecl (HsInstDecl sloc qualtype decls) = do
    newDecls <- mapM desugarDecl decls
    return [HsInstDecl sloc qualtype (concat newDecls)]

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
genBindsForPat pat sloc rhsName
   = [HsPatBind sloc (HsPVar patName) (HsUnGuardedRhs (HsApp selector (HsVar rhsName))) [] |  (patName, selector) <- selFuns]
   where
   selFuns = getPatSelFuns sloc pat

-- generate selector functions for each of the variables that
-- are bound in a pattern

getPatSelFuns :: SrcLoc -> HsPat -> [(HsName, (HsExp))]
getPatSelFuns sloc pat = [(varName, HsParen (HsLambda sloc [HsPVar newPatVarName] (kase (replaceVarNamesInPat varName pat)))) | varName <- getNamesFromHsPat pat] where
    kase p =  HsCase (HsVar newPatVarName) [a1, a2 ] where
       a1 =  HsAlt sloc p (HsUnGuardedRhs (HsVar newPatVarName)) []
       a2 =  HsAlt sloc HsPWildCard (HsUnGuardedRhs (HsApp (HsVar (toName Val "error")) (HsLit $ HsString $ show sloc ++ " failed pattern match"))) []

-- replaces all occurrences of a name with a new variable
-- and every other name with underscore

replaceVarNamesInPat :: HsName -> HsPat -> HsPat
replaceVarNamesInPat name p = f name p where
    f name1 (HsPVar (toName Val -> name2))
       | name1 == name2 = HsPVar $ newPatVarName
       | otherwise = HsPWildCard
    f _ p@(HsPLit _) = p
    f name (HsPNeg pat) = HsPNeg $ f name pat
    f name (HsPInfixApp pat1 conName pat2) = HsPInfixApp (f name pat1) conName (f name pat2)
    f name (HsPApp conName pats) = HsPApp conName (map (f name) pats)
    f name (HsPTuple pats) = HsPTuple (map (f name) pats)
    f name (HsPUnboxedTuple pats) = HsPUnboxedTuple (map (f name) pats)
    f name (HsPList pats) = HsPList (map (f name) pats)
    f name (HsPParen pat) = HsPParen (f name pat)
    f name (HsPRec conName fields) = HsPRec conName [ HsPFieldPat fname (f name pat)
                                                      | HsPFieldPat fname pat <- fields ]
    f name (HsPAsPat asName pat)
       | name == asName = HsPAsPat newPatVarName (f name pat)
       | otherwise = f name pat
    f name HsPWildCard = HsPWildCard
    f name (HsPIrrPat pat) = HsPIrrPat $ fmap (f name) pat
    f name (HsPBangPat pat) = HsPBangPat $ fmap (f name) pat
    f name p = error $ "f: " ++ show (name,p)

desugarRhs :: (HsRhs) -> PatSM (HsRhs)
desugarRhs  = traverseHsRhsHsExp desugarExp

desugarExp :: (HsExp) -> PatSM (HsExp)
desugarExp (HsLambda sloc pats e)
    | all isSimplePat pats  = do
        newE <- desugarExp e
        return (HsLambda sloc pats newE)
desugarExp (HsLambda sloc pats e) = z where
    z = do
        ps <- mapM f pats
        let (xs,zs) = unzip ps
        e' <- (ne e $ concat zs)
        return (HsLambda sloc (map HsPVar xs) e')
    ne e [] = desugarExp e
    ne e ((n,p):zs) =  do
        e' <- ne e zs
        let a1 =  HsAlt sloc p (HsUnGuardedRhs e') []
            a2 =  HsAlt sloc HsPWildCard (HsUnGuardedRhs (HsError { hsExpSrcLoc = sloc, hsExpErrorType = HsErrorPatternFailure, hsExpString = show sloc ++ " failed pattern match in lambda" })) []
        return $ HsCase (HsVar n) [a1, a2 ]

    f (HsPVar x) = return (x,[])
    f (HsPAsPat n p) = return (n,[(n,p)])
    f p = do
        unique <- getUnique
        let n = nameName $ toName Val ("lambind@" ++ show unique)
        return (n,[(n,p)])
desugarExp (HsLet decls e) = do
        newDecls <- mapM desugarDecl decls
        newE <- desugarExp e
        return (HsLet (concat newDecls) newE)
desugarExp (HsCase e alts) = do
        newE <- desugarExp e
        newAlts <- mapM desugarAlt alts
        return (HsCase newE newAlts)
desugarExp (HsDo stmts) = HsDo `liftM` mapM desugarStmt stmts
desugarExp (HsListComp e stmts) = do
        e <- desugarExp e
        stmts <- mapM desugarStmt stmts
        return (HsListComp e stmts)
desugarExp e = traverseHsExp desugarExp e

desugarAlt :: (HsAlt) -> PatSM (HsAlt)
desugarAlt (HsAlt sloc pat gAlts wheres) = do
        newGAlts <- desugarRhs gAlts
        newWheres <- mapM desugarDecl wheres
        return (HsAlt sloc pat newGAlts (concat newWheres))

desugarStmt :: (HsStmt) -> PatSM (HsStmt)
desugarStmt (HsGenerator srcLoc pat e) = do
        newE <- desugarExp e
        return (HsGenerator srcLoc pat newE)
desugarStmt (HsQualifier e) = do
        newE <- desugarExp e
        return (HsQualifier newE)
desugarStmt (HsLetStmt decls) = do
        newDecls <- mapM desugarDecl decls
        return (HsLetStmt $ concat newDecls)

doToExp :: Monad m
    => m HsName    -- ^ name generator
    -> HsName      -- ^ bind (>>=) to use
    -> HsName      -- ^ bind_ (>>) to use
    -> HsName      -- ^ fail to use
    -> [HsStmt]
    -> m HsExp
doToExp newName f_bind f_bind_ f_fail ss = hsParen `liftM` f ss where
    f [] = fail "doToExp: empty statements in do notation"
    f [HsQualifier e] = return e
    f [gen@(HsGenerator srcLoc _pat _e)] = fail $ "doToExp: last expression n do notation is a generator (srcLoc):" ++ show srcLoc
    f [letst@(HsLetStmt _decls)] = fail $ "doToExp: last expression n do notation is a let statement"
    f (HsQualifier e:ss) = do
        ss <- f ss
        return $ HsInfixApp (hsParen e) (HsVar f_bind_) (hsParen ss)
    f ((HsGenerator _srcLoc pat e):ss) | isSimplePat pat = do
        ss <- f ss
        return $ HsInfixApp (hsParen e) (HsVar f_bind) (HsLambda _srcLoc [pat] ss)
    f ((HsGenerator srcLoc pat e):ss) = do
        npvar <- newName
        ss <- f ss
        let kase = HsCase (HsVar npvar) [a1, a2 ]
            a1 =  HsAlt srcLoc pat (HsUnGuardedRhs ss) []
            a2 =  HsAlt srcLoc HsPWildCard (HsUnGuardedRhs (HsApp (HsVar f_fail) (HsLit $ HsString $ show srcLoc ++ " failed pattern match in do"))) []
        return $ HsInfixApp (hsParen e) (HsVar f_bind) (HsLambda srcLoc [HsPVar npvar] kase)  where
    f (HsLetStmt decls:ss) = do
        ss <- f ss
        return $ HsLet decls ss

hsApp e es = hsParen $ foldl HsApp (hsParen e) (map hsParen es)
hsIf e a b = hsParen $ HsIf e a b

listCompToExp :: Monad m => m HsName -> HsExp -> [HsStmt] -> m HsExp
listCompToExp newName exp ss = hsParen `liftM` f ss where
    f [] = return $ HsList [exp]
    f (gen:HsQualifier q1:HsQualifier q2:ss)  = f (gen:HsQualifier (hsApp (HsVar v_and) [q1,q2]):ss)
    f ((HsLetStmt ds):ss) = do ss' <- f ss; return $ hsParen (HsLet ds ss')
    f (HsQualifier e:ss) = do ss' <- f ss; return $ hsParen (HsIf e ss' (HsList []))
    f ((HsGenerator srcLoc pat e):ss) | isLazyPat pat, Just exp' <- g ss = do
        return $ hsParen $ HsVar v_map `app` HsLambda srcLoc [pat] exp' `app` e
    --f ((HsGenerator srcLoc pat e):[HsQualifier q]) | isHsPVar pat = hsParen $ HsApp (HsApp (HsVar f_filter)  (hsParen $ HsLambda srcLoc [pat] q) ) e
    f ((HsGenerator srcLoc pat e):HsQualifier q:ss) | isLazyPat pat, Just exp' <- g ss = do
        npvar <- newName
        return $ hsApp (HsVar v_foldr)  [HsLambda srcLoc [pat,HsPVar npvar] $ hsIf q (hsApp (HsCon dc_Cons) [exp',HsVar npvar]) (HsVar npvar), HsList [],e]
    f ((HsGenerator srcLoc pat e):ss) | isLazyPat pat = do
        ss' <- f ss
        return $ hsParen $ HsVar v_concatMap `app`  HsLambda srcLoc [pat] ss' `app` e
    f ((HsGenerator srcLoc pat e):HsQualifier q:ss) | isFailablePat pat || Nothing == g ss = do
        npvar <- newName
        ss' <- f ss
        let kase = HsCase (HsVar npvar) [a1, a2 ]
            a1 =  HsAlt srcLoc pat (HsGuardedRhss [HsGuardedRhs srcLoc q ss']) []
            a2 =  HsAlt srcLoc HsPWildCard (HsUnGuardedRhs $ HsList []) []
        return $ hsParen $ HsVar v_concatMap `app`  HsLambda srcLoc [HsPVar npvar] kase `app`  e
    f ((HsGenerator srcLoc pat e):ss) | isFailablePat pat || Nothing == g ss = do
        npvar <- newName
        ss' <- f ss
        let kase = HsCase (HsVar npvar) [a1, a2 ]
            a1 =  HsAlt srcLoc pat (HsUnGuardedRhs ss') []
            a2 =  HsAlt srcLoc HsPWildCard (HsUnGuardedRhs $ HsList []) []
        return $ hsParen $ HsVar v_concatMap `app` HsLambda srcLoc [HsPVar npvar] kase `app` e
    f ((HsGenerator srcLoc pat e):ss) = do
        npvar <- newName
        let Just exp' = g ss
            kase = HsCase (HsVar npvar) [a1 ]
            a1 =  HsAlt srcLoc pat (HsUnGuardedRhs exp') []
        return $ hsParen $ HsVar v_map `app` HsLambda srcLoc [HsPVar npvar] kase `app` e
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
    f (HsPIrrPat p) = False -- isStrictPat p  -- TODO irrefutable patterns
    f _ = True

openPat (HsPParen p) = openPat p
openPat (HsPNeg p) = openPat p
openPat (HsPAsPat _ p) = openPat p
openPat (HsPTypeSig _ p _) = openPat p
openPat (HsPInfixApp a n b) = HsPApp n [a,b]
openPat p = p
