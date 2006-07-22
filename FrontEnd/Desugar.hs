{-------------------------------------------------------------------------------

        Copyright:              The Hatchet Team (see file Contributors)

        Module:                 Desugar

        Description:            Desugaring of the abstract syntax.

                                The main tasks implemented by this module are:
                                        - pattern bindings are converted
                                          into "simple" pattern bindings
                                          (x, y, z) = foo
                                             becomes
                                          newVal = foo
                                          x = (\(a, _, _) -> a) newVal
                                          y = (\(_, a, _) -> a) newVal
                                          z = (\(_, _, a) -> a) newVal
                                        - do notation is converted into
                                          expression form, using (>>) and
                                          (>>=)
                                        - type synonyms are removed

        Primary Authors:        Bernie Pope

        Notes:                  See the file License for license information

                                According to the Haskell report a pattern
                                binding is called "simple" if it consists only
                                of a single variable - thus we convert all
                                pattern bindings to simple bindings.

-------------------------------------------------------------------------------}

-- Type synonyms are no longer handled here. only 'local' desugaring is done.
-- Does this module need to exist?

module FrontEnd.Desugar ( doToExp, desugarHsModule, desugarHsStmt, desugarHsExp) where

import Control.Monad.State

import FrontEnd.SrcLoc
import GenUtil
import HsSyn
import Name.Name
import Name.Names
import Name.VConsts
import Options
import FrontEnd.Syn.Traverse
import qualified FlagOpts as FO
import FrontEnd.SrcLoc

removeSynonymsFromType _ t = t
removeSynsFromSig _ t = t

-- (unique int, list of type synoyms)
type PatState = (Int, [HsDecl])

getUnique = do
    n <- readUnique
    incUnique
    return n

readUnique :: PatSM Int
readUnique = do
        state <- readPatSM
        return (fst state)

readSyns :: PatSM [HsDecl]
readSyns = do
        state <- readPatSM
        return (snd state)


incUnique :: PatSM ()
incUnique = updatePatSM (\(u, s) -> (u + 1, s))

type PatSM = State PatState

instance MonadSrcLoc PatSM where
instance MonadSetSrcLoc PatSM where
    withSrcLoc _ a = a


{------------------------------------------------------------------------------}

readPatSM = get
updatePatSM = modify
runPatSM = flip runState


-- a new (unique) name introduced in pattern selector functions
newPatVarName :: HsName
newPatVarName = nameName $ toName Val "patvar@0"


remSynsSig :: HsDecl -> PatSM HsDecl
remSynsSig sig
   = do
        syns <- readSyns
        let newSig = removeSynsFromSig syns sig
        return newSig

remSynsType :: HsType -> PatSM HsType
remSynsType t
   = do
        syns <- readSyns
        let newType = removeSynonymsFromType syns t
        return newType


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
    (ds', _) = runPatSM (0::Int, undefined) $ dsm (hsModuleDecls m)
    dsm ds = fmap concat $ mapM desugarDecl ds

desugarHsStmt :: Monad m => HsStmt -> m HsStmt
desugarHsStmt s = return $ fst $ runPatSM (0::Int, undefined) $ desugarStmt s

desugarHsExp :: Monad m => HsExp -> m HsExp
desugarHsExp s = return $ fst $ runPatSM (0::Int, undefined) $ desugarExp s


desugarDecl :: HsDecl -> PatSM [HsDecl]
desugarDecl (HsForeignDecl a b c qt) = do
    qt <- remSynsQualType qt
    return [HsForeignDecl a b c qt]
desugarDecl (HsForeignExport a b c qt) = do
    qt <- remSynsQualType qt
    return [HsForeignExport a b c qt]
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
    let newRhsName = nameName $ toName Val ("patrhs@" ++ show unique)
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
    newQualType <- remSynsQualType qualtype
    newDecls <- mapM desugarDecl decls
    return [HsInstDecl sloc newQualType (concat newDecls)]

desugarDecl sig@(HsTypeSig _sloc _names _qualType) = do
    newSig <- remSynsSig sig
    return [newSig]


desugarDecl (HsDataDecl sloc cntxt name args condecls derives) = do
        --newConDecls <- mapM remSynsFromCondecl condecls
        newConDecls <- return condecls
        ds <- deriveInstances sloc name args newConDecls derives
        ss <- createSelectors sloc newConDecls
        return $ (HsDataDecl sloc cntxt name args newConDecls derives):(ds ++ ss)

desugarDecl (HsNewTypeDecl sloc cntxt name args condecl derives) = do
        --newConDecl <- remSynsFromCondecl condecl
        newConDecl <- return condecl
        ds <- deriveInstances sloc name args [newConDecl] derives
        ss <- createSelectors sloc [newConDecl]
        return $ (HsNewTypeDecl sloc cntxt name args newConDecl derives):(ds ++ ss)

desugarDecl anyOtherDecl = return [anyOtherDecl]



createSelectors _sloc ds = ans where
    ds' :: [(HsName,[(HsName,HsBangType)])]
    ds' = [ (c,[(n,t) | (ns,t) <- rs , n <- ns ]) | HsRecDecl { hsConDeclName = c, hsConDeclRecArg = rs } <- ds ]
    ns = sortGroupUnderF fst $ concatMap f ds' -- [  | (c,nts) <- ds' ]
    f ::  (HsName,[(HsName,HsBangType)]) -> [ (HsName, (HsName,Int,Int)) ]
    f (c,nts) = [ (n,(c,i,length nts)) | (n,_) <- nts | i <- [0..]]
    ans = return $  map g ns
    g (n,cs) = HsFunBind (map f cs ++ [els]) where
        f (_,(c,i,l)) = HsMatch _sloc n [pat c i l] (HsUnGuardedRhs (HsVar var)) []
        pat c i l = HsPApp c [ if p == i then HsPVar var else HsPWildCard | p <- [0 .. l - 1]]
        els = HsMatch _sloc n [HsPWildCard] (HsUnGuardedRhs (HsApp (HsVar err) (HsLit (HsString (show n))))) []

    var = nameName $ toName Val "x"
    err = nameName $ toUnqualified $ v_error


deriveInstances :: Monad m => SrcLoc -> HsName -> [HsName] -> [HsConDecl] -> [HsName] -> m [HsDecl]
deriveInstances sloc name args cons ds = return []


desugarMatch :: (HsMatch) -> PatSM (HsMatch)
desugarMatch (HsMatch sloc funName pats rhs wheres)
   = do
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
getPatSelFuns sloc pat = [(varName, HsParen (HsLambda sloc [HsPVar newPatVarName] (kase (replaceVarNamesInPat varName pat)))) | varName <- patVarNames pat] where
    kase p =  HsCase (HsVar newPatVarName) [a1, a2 ] where
       a1 =  HsAlt sloc p (HsUnGuardedRhs (HsVar newPatVarName)) []
       a2 =  HsAlt sloc HsPWildCard (HsUnGuardedRhs (HsApp (HsVar (UnQual $ HsIdent "error")) (HsLit $ HsString $ show sloc ++ " failed pattern match"))) []


--getPatSelFuns sloc pat = [(varName, HsParen (HsLambda sloc [replaceVarNamesInPat varName pat] (HsVar newPatVarName))) | varName <- patVarNames pat]
-- returns the names of variables bound in a pattern
-- XXX bjpop: do as patterns work properly?
patVarNames :: HsPat -> [HsName]
patVarNames (HsPVar name) = [name]
patVarNames (HsPLit _) = []
patVarNames (HsPNeg pat) = patVarNames pat
patVarNames (HsPInfixApp pat1 conName pat2)
   = patVarNames pat1 ++ patVarNames pat2
patVarNames (HsPApp conName pats)
   = concatMap patVarNames pats
patVarNames (HsPTuple pats)
   = concatMap patVarNames pats
patVarNames (HsPList pats)
   = concatMap patVarNames pats
patVarNames (HsPParen pat)
   = patVarNames pat
patVarNames (HsPRec _ _) = error "patVarNames (HsPRec _ _): not implemented "
patVarNames (HsPAsPat asName pat)
   = asName : patVarNames pat
patVarNames HsPWildCard = []
patVarNames (HsPIrrPat pat)
   = patVarNames pat
patVarNames e = error $ "patVarNames: " ++ show e

-- replaces all occurrences of a name with a new variable
-- and every other name with underscore

replaceVarNamesInPat :: HsName -> HsPat -> HsPat

replaceVarNamesInPat name1 (HsPVar name2)
   | name1 == name2 = HsPVar $ newPatVarName
   | otherwise = HsPWildCard
replaceVarNamesInPat _ p@(HsPLit _) = p
replaceVarNamesInPat name (HsPNeg pat)
   = HsPNeg $ replaceVarNamesInPat name pat
replaceVarNamesInPat name (HsPInfixApp pat1 conName pat2)
   = HsPInfixApp (replaceVarNamesInPat name pat1) conName (replaceVarNamesInPat name pat2)
replaceVarNamesInPat name (HsPApp conName pats)
   = HsPApp conName (map (replaceVarNamesInPat name) pats)
replaceVarNamesInPat name (HsPTuple pats)
   = HsPTuple (map (replaceVarNamesInPat name) pats)
replaceVarNamesInPat name (HsPList pats)
   = HsPList (map (replaceVarNamesInPat name) pats)
replaceVarNamesInPat name (HsPParen pat)
   = HsPParen (replaceVarNamesInPat name pat)
replaceVarNamesInPat name (HsPRec _ _)
   = error  "replaceVarNamesInPat name (HsPRec _ _): not implemented"
replaceVarNamesInPat name (HsPAsPat asName pat)
   | name == asName = HsPAsPat newPatVarName (replaceVarNamesInPat name pat)
   | otherwise = replaceVarNamesInPat name pat
replaceVarNamesInPat name HsPWildCard = HsPWildCard
replaceVarNamesInPat name (HsPIrrPat pat)
   = HsPIrrPat $ replaceVarNamesInPat name pat
replaceVarNamesInPat name p = error $ "replaceVarNamesInPat: " ++ show (name,p)


desugarRhs :: (HsRhs) -> PatSM (HsRhs)
desugarRhs (HsUnGuardedRhs e)
   = do
        newE <- desugarExp e
        return (HsUnGuardedRhs newE)

desugarRhs (HsGuardedRhss gRhss)
   = do
        newRhss <- mapM desugarGRhs gRhss
        return (HsGuardedRhss newRhss)

desugarGRhs :: HsGuardedRhs -> PatSM (HsGuardedRhs)
desugarGRhs (HsGuardedRhs sloc e1 e2)
   = do
        newE1 <- desugarExp e1
        newE2 <- desugarExp e2
        return (HsGuardedRhs sloc newE1 newE2)

desugarExp :: (HsExp) -> PatSM (HsExp)
desugarExp (HsLambda sloc pats e)
    | all isHsPVar pats = do
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
            a2 =  HsAlt sloc HsPWildCard (HsUnGuardedRhs (HsApp (HsVar (nameName $ toUnqualified v_error)) (HsLit $ HsString $ show sloc ++ " failed pattern match in lambda"))) []
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
desugarExp (HsDo stmts) = do
        newStmts <- mapM desugarStmt stmts
        return (doToExp newStmts)
desugarExp (HsListComp e stmts) = do
        newE <- desugarExp e
        newStmts <- mapM desugarStmt stmts
        return (listCompToExp newE newStmts)
desugarExp (HsExpTypeSig sloc e qualType) = do
        e' <- desugarExp e
        newQualType <- remSynsQualType qualType
        return (HsExpTypeSig sloc e' newQualType)
desugarExp e = traverseHsExp desugarExp e



desugarAlt :: (HsAlt) -> PatSM (HsAlt)

desugarAlt (HsAlt sloc pat gAlts wheres) = do
        newGAlts <- desugarGAlts gAlts
        newWheres <- mapM desugarDecl wheres
        return (HsAlt sloc pat newGAlts (concat newWheres))

desugarGAlts :: (HsRhs) -> PatSM (HsRhs)

desugarGAlts (HsUnGuardedRhs e) = do
        newE <- desugarExp e
        return (HsUnGuardedRhs newE)

desugarGAlts (HsGuardedRhss gAlts) = do
        newGAlts <- mapM desugarGuardedAlt gAlts
        return (HsGuardedRhss newGAlts)

desugarGuardedAlt :: (HsGuardedRhs) -> PatSM (HsGuardedRhs)

desugarGuardedAlt (HsGuardedRhs sloc e1 e2) = do
        newE1 <- desugarExp e1
        newE2 <- desugarExp e2
        return (HsGuardedRhs sloc newE1 newE2)

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


remSynsQualType :: HsQualType -> PatSM HsQualType
remSynsQualType qualtype
   = case qualtype of
        HsQualType cntxt t
           -> do
                 newT <- remSynsType t
                 return (HsQualType cntxt newT)

--------------------------------------------------------------------------------

-- desugar the do-notation

-- flatten out do notation into an expression
-- involving ">>" and ">>="
-- TODO -  THIS IS BROKEN


doToExp :: [HsStmt] -> HsExp

f_bind = nameName $ toUnqualified (func_bind sFuncNames)
f_bind_ = nameName $ toUnqualified (func_bind_ sFuncNames)
f_concatMap = nameName $ toUnqualified v_concatMap
f_map = nameName $ toUnqualified v_map
f_foldr = nameName $ toUnqualified v_foldr
f_fail = nameName $ toUnqualified v_fail
f_filter = nameName $ toUnqualified v_filter
f_and = nameName $ toUnqualified v_and
con_cons = nameName $ toUnqualified dc_Cons

doToExp [] = error "doToExp: empty statements in do notation"
doToExp [HsQualifier e] = e
doToExp [gen@(HsGenerator srcLoc _pat _e)]
   = error $ "doToExp: last expression n do notation is a generator (srcLoc):" ++ show srcLoc
doToExp [letst@(HsLetStmt _decls)]
   = error $ "doToExp: last expression n do notation is a let statement"
doToExp ((HsQualifier e):ss)
   = HsInfixApp (hsParen e) (HsVar f_bind_) (hsParen $ doToExp ss)
doToExp ((HsGenerator _srcLoc pat@(HsPVar {}) e):ss)
   = HsInfixApp (hsParen e) (HsVar f_bind) (HsLambda _srcLoc [pat] (doToExp ss))
doToExp ((HsGenerator srcLoc pat e):ss) = HsInfixApp (hsParen e) (HsVar f_bind) (HsLambda srcLoc [HsPVar newPatVarName] kase)  where
   kase = HsCase (HsVar newPatVarName) [a1, a2 ]
   a1 =  HsAlt srcLoc pat (HsUnGuardedRhs (doToExp ss)) []
   a2 =  HsAlt srcLoc HsPWildCard (HsUnGuardedRhs (HsApp (HsVar f_fail) (HsLit $ HsString $ show srcLoc ++ " failed pattern match in do"))) []
doToExp ((HsLetStmt decls):ss)
   = HsLet decls (doToExp ss)

hsApp e es = hsParen $ foldl HsApp (hsParen e) (map hsParen es)
hsIf e a b = hsParen $ HsIf e a b
patVar = HsVar newPatVarName

listCompToExp :: HsExp -> [HsStmt] -> HsExp
listCompToExp exp ss = hsParen (f ss) where
    f [] = HsList [exp]
    f (gen:HsQualifier q1:HsQualifier q2:ss)  = f (gen:HsQualifier (hsApp (HsVar f_and) [q1,q2]):ss)
    f ((HsLetStmt ds):ss) = hsParen (HsLet ds (f ss))
    f (HsQualifier e:ss) = hsParen (HsIf e (f ss) (HsList []))
    f ((HsGenerator srcLoc pat e):ss) | isHsPVar pat, Just exp' <- g ss = hsParen $ HsApp (HsApp (HsVar f_map)  (hsParen $ HsLambda srcLoc [pat] exp')) e
    --f ((HsGenerator srcLoc pat e):[HsQualifier q]) | isHsPVar pat = hsParen $ HsApp (HsApp (HsVar f_filter)  (hsParen $ HsLambda srcLoc [pat] q) ) e
    f ((HsGenerator srcLoc pat e):HsQualifier q:ss) | isHsPVar pat, Just exp' <- g ss =  hsApp (HsVar f_foldr)  [HsLambda srcLoc [pat,HsPVar newPatVarName] $ hsIf q (hsApp (HsCon con_cons) [exp',patVar]) (HsVar newPatVarName), HsList [],e]
    f ((HsGenerator srcLoc pat e):ss) | isHsPVar pat = hsParen $ HsApp (HsApp (HsVar f_concatMap)  (hsParen $ HsLambda srcLoc [pat] (f ss))) e
    f ((HsGenerator srcLoc pat e):HsQualifier q:ss) | isFailablePat pat || Nothing == (g ss) = hsParen $ HsApp (HsApp (HsVar f_concatMap)  (hsParen $ HsLambda srcLoc [HsPVar newPatVarName] kase)) e where
        kase = HsCase (HsVar newPatVarName) [a1, a2 ]
        a1 =  HsAlt srcLoc pat (HsGuardedRhss [HsGuardedRhs srcLoc q (f ss)]) []
        a2 =  HsAlt srcLoc HsPWildCard (HsUnGuardedRhs $ HsList []) []
    f ((HsGenerator srcLoc pat e):ss) | isFailablePat pat || Nothing == (g ss) = hsParen $ HsApp (HsApp (HsVar f_concatMap)  (hsParen $ HsLambda srcLoc [HsPVar newPatVarName] kase)) e where
        kase = HsCase (HsVar newPatVarName) [a1, a2 ]
        a1 =  HsAlt srcLoc pat (HsUnGuardedRhs (f ss)) []
        a2 =  HsAlt srcLoc HsPWildCard (HsUnGuardedRhs $ HsList []) []
    f ((HsGenerator srcLoc pat e):ss)  = hsParen $ HsApp (HsApp (HsVar f_map)  (hsParen $ HsLambda srcLoc [HsPVar newPatVarName] kase)) e where
        Just exp' = g ss
        kase = HsCase (HsVar newPatVarName) [a1 ]
        a1 =  HsAlt srcLoc pat (HsUnGuardedRhs exp') []
    g [] = return exp
    g (HsLetStmt ds:ss) = do
        e <- g ss
        return (hsParen (HsLet ds e))
    g _ = Nothing

-- patterns are
-- failable - may fail to match
-- refutable - may bottom out
-- irrefutable - match no matter what
-- failable is a subset of refutable


isFailablePat p | isStrictPat p = f (openPat p) where
    f (HsPTuple ps) = any isFailablePat ps
    f _ = True
isFailablePat _ = False

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



