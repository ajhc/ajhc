    -- various desugaring routines
--
-- The general desugaring routine creates selectors for data
-- constructors with named fields, changes all pattern bindings
-- into 'simple' pattern bindings, and adds failure cases to lambda
-- expressions which have failable patterns

module FrontEnd.Desugar (desugarHsModule, desugarHsStmt) where

import FrontEnd.HsSyn
import FrontEnd.SrcLoc
import FrontEnd.Syn.Traverse
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
desugarHsModule m = hsModuleDecls_s ds' m where
    (ds', _) = runUniq 0 (dsm (hsModuleDecls m)) -- (0::Int)
    dsm ds = fmap concat $ mapM desugarDecl ds
--desugarHsModule m = m

desugarHsStmt :: Monad m => HsStmt -> m HsStmt
desugarHsStmt s = return $ fst $ runUniq 0 (desugarStmt s)

desugarDecl :: HsDecl -> PatSM [HsDecl]
desugarDecl (HsFunBind matches) = do
    newMatches <- mapM desugarMatch matches
    return [HsFunBind newMatches]

--desugarDecl pb@(HsPatBind sloc p rhs wheres) = do
--    newRhs <- desugarRhs rhs
--    newWheres <- mapM desugarDecl wheres
--    return [HsPatBind sloc p newRhs (concat newWheres)]

--variable pattern bindings remain unchanged
desugarDecl HsPatBind { hsDeclPat = hsDeclPat@HsPVar {}, .. } = do
    hsDeclRhs <- desugarRhs hsDeclRhs
    hsDeclDecls <- concat <$> mapM desugarDecl hsDeclDecls
    return [HsPatBind { .. }]
--desugarDecl (HsPatBind sloc pat rhs wheres) = do
desugarDecl HsPatBind { .. } = do
    hsDeclRhs <- desugarRhs hsDeclRhs
    hsDeclDecls <- concat <$> mapM desugarDecl hsDeclDecls
    unique <- newUniq
    let newRhsName = toName Val ("rhs@" ++ show unique)
    let newBinds = genBindsForPat hsDeclPat hsDeclSrcLoc newRhsName
    newBinds <- concat <$> mapM desugarDecl newBinds
    let newTopDeclForRhs = HsPatBind { hsDeclPat = HsPVar newRhsName, .. }
    return (newTopDeclForRhs : newBinds)
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
genBindsForPat pat sloc rhs = ans where
    ans = [HsPatBind sloc (HsPVar pn) (HsUnGuardedRhs selector) [] | (pn, selector) <- selFuns]
    selFuns = getPatSelFuns sloc pat rhs

-- generate selector functions for each of the variables that
-- are bound in a pattern

getPatSelFuns :: SrcLoc -> HsPat -> Name -> [(Name, HsExp)]
getPatSelFuns sloc pat rhsvar = ans where
    --ans = [(v, HsParen (HsLambda sloc [HsPVar newPatVarName] (kase (replaceVarNamesInPat v pat)))) | v <- getNamesFromHsPat pat, nameType v == Val]
    ans = [(v, kase (replaceVarNamesInPat v pat)) | v <- getNamesFromHsPat pat, nameType v == Val]
    kase p =  HsCase (HsVar rhsvar) [a1, a2 ] where
       a1 =  HsAlt sloc p (HsUnGuardedRhs (HsVar newPatVarName)) []
       a2 =  HsAlt sloc HsPWildCard (HsUnGuardedRhs (HsError { hsExpSrcLoc = sloc, hsExpErrorType = HsErrorPatternFailure, hsExpString = show sloc ++ " failed pattern match" })) []
       --a2 =  HsAlt sloc HsPWildCard (HsUnGuardedRhs (HsApp (HsVar (toName Val ("error"::String))) (HsLit $ HsString $ show sloc ++ " failed pattern match"))) []

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
desugarExp (HsLambda sloc pats e)
    | all isSimplePat pats  = do
        newE <- desugarExp e
        return (HsLambda sloc pats newE)
desugarExp (HsLambda sloc pats e) = do
        ps <- mapM f pats
        let (xs,zs) = unzip ps
        e' <- (ne e $ concat zs)
        return (HsLambda sloc (map HsPVar xs) e')
    where
    ne e [] = desugarExp e
    ne e ((n,p):zs) =  do
        e' <- ne e zs
        let a1 =  HsAlt sloc p (HsUnGuardedRhs e') []
            a2 =  HsAlt sloc HsPWildCard (HsUnGuardedRhs (HsError { hsExpSrcLoc = sloc, hsExpErrorType = HsErrorPatternFailure, hsExpString = show sloc ++ " failed pattern match in lambda" })) []
        return $ HsCase (HsVar n) [a1, a2 ]

    f (HsPVar x) = return (x,[])
    f (HsPAsPat n p) = return (n,[(n,p)])
    f p = do
        unique <- newUniq
        let n = toName Val ("lambind@" ++ show unique)
        return (n,[(n,p)])
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

isSimplePat p = f (openPat p) where
    f HsPVar {} = True
    f HsPWildCard = True
    f _ = False

openPat (HsPParen p) = openPat p
openPat (HsPNeg p) = openPat p
openPat (HsPAsPat _ p) = openPat p
openPat (HsPTypeSig _ p _) = openPat p
openPat (HsPInfixApp a n b) = HsPApp n [a,b]
openPat p = p
