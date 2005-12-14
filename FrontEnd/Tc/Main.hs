module FrontEnd.Tc.Main (tiExpr, makeProgram ) where

import Control.Monad.Error
import Control.Monad.Writer
import List((\\), intersect, union)
import qualified Data.Map as Map
import qualified Text.PrettyPrint.HughesPJ as PPrint

import Class(ClassHierarchy, entails, split, topDefaults, splitReduce)
import Control.Monad.Reader
import DeclsDepends(getDeclDeps)
import DependAnalysis(getBindGroups)
import Diagnostic
import Doc.PPrint as PPrint
import FrontEnd.Desugar(doToExp)
import FrontEnd.KindInfer(KindEnv,hsQualTypeToSigma)
import FrontEnd.Tc.Monad
import FrontEnd.Tc.Type
import FrontEnd.Tc.Unify
import FrontEnd.SrcLoc
import FrontEnd.Utils(getDeclName)
import GenUtil
import HsPretty
import HsSyn
import Name.Name
import Name.Names
import Name.VConsts



type Expl = (Sigma, HsDecl)
-- TODO: this is different than the "Typing Haskell in Haskell" paper
-- we do not further sub-divide the implicitly typed declarations in
-- a binding group.
type BindGroup = ([Expl], [HsDecl])


tcApps e as typ = do
    bs <- sequence [ newBox Star | _ <- as ]
    e' <- tcExpr e (foldr fn typ bs)
    as' <- sequence [ tcExprPoly a r | r <- bs | a <- as ]
    return (e',as')


tcApp e1 e2 typ = do
    (e1,[e2]) <- tcApps e1 [e2] typ
    return (e1,e2)

tiExprPoly,tcExprPoly ::  HsExp -> Type ->  Tc HsExp

tcExprPoly e t = do
    t <- findType t
    tiExprPoly e t

tiExprPoly e t@TMetaVar {} = tcExpr e t   -- GEN2
tiExprPoly e t = do                   -- GEN1
    (_,t) <- skolomize t
    tcExpr e t

tiExpr,tcExpr ::  HsExp -> Type ->  Tc HsExp

tcExpr e t = do
    t <- findType t
    tiExpr e t

-- TODO should subsume for rank-n
tiExpr (HsVar v) typ = do
    sc <- lookupName (toName Val v)
    sc <- freshSigma sc
--    sc <- freshInstance sc
    sc `subsumes` typ
    return (HsVar v)

tiExpr (HsCon conName) typ = do
    sc <- lookupName (toName DataConstructor conName)
    sc <- freshSigma sc
 --   sc <- freshInstance sc
    sc `subsumes` typ
    return (HsCon conName)

tiExpr (HsLit l) typ = do
    t <- tiLit l
    t `subsumes` typ
    return (HsLit l)

tiExpr (HsAsPat n e) typ = do
    e <- tcExpr e typ
    addToCollectedEnv (Map.singleton (toName Val n) typ)
    return (HsAsPat n e)

-- comb LET-S and VAR
tiExpr expr@(HsExpTypeSig sloc e qt) typ =  withContext (locMsg sloc "in the annotated expression" $ render $ ppHsExp expr) $ do
    kt <- getKindEnv
    s <- hsQualTypeToSigma kt qt
    s `subsumes` typ
    e' <- tcExprPoly e s
    --s'@(TForall _ (ps :=> r)) <- freshSigma s
    --addPreds ps
    --e' <- tiExpr e r
    --s' `subsumes` typ
    return (HsExpTypeSig sloc e' qt)


tiExpr (HsLeftSection e1 e2) typ = do
    (e1,e2) <- tcApp e1 e2 typ
    return (HsLeftSection e1 e2)

-- I know this looks weird but it appears to be correct
-- e1 :: b
-- e2 :: a -> b -> c
-- e1 e2 :: a -> c

-- (: [])  \x -> x : []   `fn`

tiExpr (HsRightSection e1 e2) typ = do
    arg <- newBox Star
    arg2 <- newBox Star
    e2 <- tiExpr e2 (arg `fn` (arg2 `fn` typ))
    e1 <- tiExpr e1 arg2
    return (HsRightSection e1 e2)



tiExpr expr@HsApp {} typ = withContext (makeMsg "in the application" $ render $ ppHsExp $ backToApp h as) $ do
    (h,as) <- tcApps h as typ
    return $ backToApp h as
    where
    backToApp h as = foldl HsApp h as
    (h,as) = fromHsApp expr
    fromHsApp t = f t [] where
        f (HsApp a b) rs = f a (b:rs)
        f t rs = (t,rs)

tiExpr expr@(HsInfixApp e1 e2 e3) typ = withContext (makeMsg "in the infix application" $ render $ ppHsExp expr) $ do
    (e2',[e1',e3']) <- tcApps e2 [e1,e3] typ
    return (HsInfixApp e1' e2' e3')

-- we need to fix the type to to be in the class
-- cNum, just for cases such as:
-- foo = \x -> -x

tiExpr expr@(HsNegApp e) typ = withContext (makeMsg "in the negative expression" $ render $ ppHsExp expr) $ do
        e <- tcExpr e typ
        addPreds [IsIn class_Num typ]
        return (HsNegApp e)


-- ABS1
tiExpr expr@(HsLambda sloc ps e) typ = withContext (locSimple sloc $ "in the lambda expression\n   \\" ++ show ps ++ " -> ...") $ do
    let lam (p:ps) e (TMetaVar mv) rs = do -- ABS2
            b1 <- newBox Star
            b2 <- newBox Star
            r <- lam (p:ps) e (b1 `fn` b2) rs
            varBind mv (b1 `fn` b2)
            return r
        lam (p:ps) e (TArrow s1' s2') rs = do -- ABS1
            box <- newBox Star
            s1' `boxyMatch` box
            (p',env) <- tiPat p box
            localEnv env $ do
                lamPoly ps e s2' (p':rs)  -- TODO poly
        lam [] e typ rs = do
            e' <- tcExpr e typ
            return (HsLambda sloc (reverse rs) e')
        lam _ _ _ _ = fail "lambda type mismatch"
        lamPoly ps e s@TBox {} rs = lam ps e s rs
        lamPoly ps e s rs = do
            (_,s) <- skolomize s
            lam ps e s rs


    lam ps e typ []


tiExpr (HsIf e e1 e2) typ = withContext (simpleMsg $ "in the if expression\n   if " ++ show e ++ "...") $ do
    e <- tcExpr e tBool
    e1 <- tcExpr e1 typ
    e2 <- tcExpr e2 typ
    return (HsIf e e1 e2)

tiExpr tuple@(HsTuple exps@(_:_)) typ = withContext (makeMsg "in the tuple" $ render $ ppHsExp tuple) $ do
    (HsCon _,exps') <- tcApps (HsCon (toTuple (length exps))) exps typ
    return (HsTuple exps')


-- special case for the empty list
tiExpr (HsList []) typ = do
        v <- newBox Star
        (TAp tList v) `subsumes` typ
        return (HsList [])

-- non empty list
tiExpr expr@(HsList exps@(_:_)) typ = withContext (makeMsg "in the list " $ render $ ppHsExp expr) $ do
        --v <- newTVar Star
        v <- newBox Star
        exps' <- mapM (`tcExpr` v) exps
        (TAp tList v) `subsumes` typ
        return (HsList exps')

tiExpr (HsParen e) typ = tcExpr e typ

tiExpr (HsDo stmts) typ = do
        let newExp = doToExp stmts
        withContext (simpleMsg "in a do expression")
                    (tcExpr newExp typ)

tiExpr expr@(HsLet decls e) typ = withContext (makeMsg "in the let binding" $ render $ ppHsExp expr) $ do
    sigEnv <- getSigEnv
    liftIO $ print sigEnv
    let bgs = getFunDeclsBg sigEnv decls
        f (bg:bgs) rs = do
            (ds,env) <- tcBindGroup bg
            localEnv env $ f bgs (ds ++ rs)
        f [] rs = do
            e' <- tcExpr e typ
            return (HsLet rs e')
    f bgs []

tiExpr expr@(HsLet [HsPatBind sl (HsPVar x) (HsUnGuardedRhs u) []] t) typ = withContext (makeMsg "in the let binding" $ render $ ppHsExp expr) $ do
    ch <- getClassHierarchy
    tb <- newBox Star
    tb' <- newTVar Star
    (u',ds) <- listen $ localEnv (Map.singleton (toName Val x) tb') $ tcExpr u tb
    tb' `boxyMatch` tb
    ds :=> rr <- flattenType (ds :=> tb)
    let tvs = freeMetaVars rr
    --(ds,rs) <- (Class.split ch tvs ds)
    addPreds ds
    rr <- quantify tvs ds rr
    addToCollectedEnv $ (Map.singleton (toName Val x) rr)
    t' <- localEnv (Map.singleton (toName Val x) rr) $ do
        tcExpr t typ
    return (HsLet [HsPatBind sl (HsPVar x) (HsUnGuardedRhs u') []] t')

tiExpr (HsCase e alts) typ = withContext (simpleMsg $ "in the case expression\n   case " ++ show e ++ " of ...") $ do
    scrutinee <- newBox Star
    e' <- tcExpr e scrutinee
    alts' <- mapM (tcAlt scrutinee typ) alts
    return (HsCase e' alts')

tiExpr e typ = fail $ "tiExpr: not implemented for: " ++ show (e,typ)


-----------------------------------------------------------------------------

-- type check implicitly typed bindings


tcAlt ::  Sigma -> Sigma -> HsAlt -> Tc HsAlt

tcAlt scrutinee typ alt@(HsAlt sloc pat gAlts [])  = withContext (locMsg sloc "in the alternative" $ render $ ppHsAlt alt) $ do
    (pat',env) <- tcPat pat scrutinee
    localEnv env $ case gAlts of
        HsUnGuardedAlt e -> do
            e' <- tcExpr e typ
            return (HsAlt sloc pat' (HsUnGuardedAlt e') [])
        HsGuardedAlts as -> do
            gas <- mapM (tcGuardedAlt typ) as
            return (HsAlt sloc pat' (HsGuardedAlts gas) [])

tcGuardedAlt typ gAlt@(HsGuardedAlt sloc eGuard e) = withContext (locMsg sloc "in the guarded alternative" $ render $ ppGAlt gAlt) $ do
    g' <- tcExpr eGuard tBool
    e' <- tcExpr e typ
    return  (HsGuardedAlt sloc g' e')

tcGuardedRhs typ gAlt@(HsGuardedRhs sloc eGuard e) = withContext (locMsg sloc "in the guarded alternative" $ render $ ppHsGuardedRhs gAlt) $ do
    g' <- tcExpr eGuard tBool
    e' <- tcExpr e typ
    return  (HsGuardedRhs sloc g' e')

-- Typing Patterns

--tiPat :: HsPat -> TI ([Pred], Map.Map Name Scheme, Type)
tiPat,tcPat :: HsPat -> Type -> Tc (HsPat, Map.Map Name Sigma)

tcPat p typ = do
    typ <- findType typ
    tiPat p typ

tiPat (HsPVar i) typ = do
        --v <- newTVar Star
        (v) <- newBox Star
        v `subsumes` typ
        return (HsPVar i, Map.singleton (toName Val i) v)

tiPat (HsPLit l) typ = do
    t <- tiLit l
    t `subsumes` typ
    return (HsPLit l,Map.empty)

-- this is for negative literals only
-- so the pat must be a literal
-- it is safe not to make any predicates about
-- the pat, since the type checking of the literal
-- will do this for us
tiPat (HsPNeg pat) typ = tiPat pat typ

tiPat (HsPIrrPat p) typ = tiPat p typ
tiPat (HsPParen p) typ = tiPat p typ


-- TODO check that constructors are saturated
tiPat (HsPApp conName pats) typ = do
    bs <- sequence [ newBox Star | _ <- pats ]
    s <- lookupName (toName DataConstructor conName)
    s `subsumes` (foldr fn typ bs)
    pats' <- sequence [ tcPat a r | r <- bs | a <- pats ]
    return (HsPApp conName (fsts pats'), mconcat (snds pats'))

tiPat pl@(HsPList []) typ = do
    v <- newBox Star
    TAp tList v `subsumes` typ
    return (pl,mempty)


tiPat (HsPList pats@(_:_)) typ = do
    v <- newBox Star
    TAp tList v `subsumes` typ
    ps <- mapM (`tcPat` v) pats
    return (HsPList (fsts ps), mconcat (snds ps))

tiPat HsPWildCard typ = return (HsPWildCard, mempty)


tiPat (HsPAsPat i pat) typ = do
    (pat',env) <- tcPat pat typ
    addToCollectedEnv (Map.singleton (toName Val i) typ)
    return (HsPAsPat i pat', Map.insert (toName Val i) typ env)

tiPat (HsPInfixApp pLeft conName pRight) typ =  tiPat (HsPApp conName [pLeft,pRight]) typ

tiPat tuple@(HsPTuple pats) typ = tiPat (HsPApp (toTuple (length pats)) pats) typ

tcBindGroup :: BindGroup -> Tc ([HsDecl], TypeEnv)
tcBindGroup (es, is) = do
     let env1 = Map.fromList [(getDeclName decl, sc) | (sc,decl) <- es ]
     localEnv env1 $ do
         (impls, implEnv) <- tiImpls is
         localEnv implEnv $ do
             expls   <- mapM tiExpl es
             return (impls ++ fsts expls, mconcat (implEnv:env1:snds expls))

tiImpls ::  [HsDecl] -> Tc ([HsDecl], TypeEnv)
tiImpls [] = return ([],Map.empty)
tiImpls bs = withContext (locSimple (srcLoc bs) ("in the implicitly typed: " ++ (show (map getDeclName bs)))) $ do
    liftIO $ putStrLn $ "tiimpls " ++ show (map getDeclName bs)
    ss <- sequence [newTVar Star | _ <- bs]
    rs <- localEnv (Map.fromList [  (getDeclName d,s) | d <- bs | s <- ss]) $ sequence [ tcDecl d s | d <- bs | s <- ss ]
    nenv <- sequence [ flattenType s >>= generalize >>= return . (,) n | (n,s) <- Map.toAscList $ mconcat $ snds rs]
    addToCollectedEnv (Map.fromAscList nenv)
    return (fsts rs, Map.fromAscList nenv)

tcRhs :: HsRhs -> Sigma -> Tc HsRhs
tcRhs rhs typ = case rhs of
    HsUnGuardedRhs e -> do
        e' <- tcExpr e typ
        return (HsUnGuardedRhs e')
    HsGuardedRhss as -> do
        gas <- mapM (tcGuardedRhs typ) as
        return (HsGuardedRhss gas)


tcDecl ::  HsDecl -> Sigma -> Tc (HsDecl,TypeEnv)

tcDecl d@(HsForeignDecl _ _ _ n _) typ = do
    s <- lookupName (toName Val n)
    s `subsumes` typ
    return (d,mempty)



tcDecl decl@(HsPatBind sloc (HsPVar v) rhs []) typ = withContext (declDiagnostic decl) $ do
    case rhs of
        HsUnGuardedRhs e -> do
            e' <- tcExpr e typ
            return (HsPatBind sloc (HsPVar v) (HsUnGuardedRhs e') [], Map.singleton (toName Val v) typ)
        HsGuardedRhss as -> do
            gas <- mapM (tcGuardedRhs typ) as
            return (HsPatBind sloc (HsPVar v) (HsGuardedRhss gas) [], Map.singleton (toName Val v) typ)


tcDecl decl@(HsFunBind matches) typ = withContext (declDiagnostic decl) $ do
        matches' <- mapM (`tcMatch` typ) matches
        return (HsFunBind matches', Map.singleton (getDeclName decl) typ)

tcMatch ::  HsMatch -> Sigma -> Tc HsMatch
tcMatch (HsMatch sloc funName pats rhs []) typ = withContext (locMsg sloc "in" $ show funName) $ do
    let lam (p:ps) (TMetaVar mv) rs = do -- ABS2
            b1 <- newBox Star
            b2 <- newBox Star
            r <- lam (p:ps) (b1 `fn` b2) rs
            varBind mv (b1 `fn` b2)
            return r
        lam (p:ps) (TArrow s1' s2') rs = do -- ABS1
            box <- newBox Star
            s1' `boxyMatch` box
            (p',env) <- tiPat p box
            localEnv env $ do
                lamPoly ps s2' (p':rs)  -- TODO poly
        lam [] typ rs = do
            rhs <- tcRhs rhs typ
            return (HsMatch sloc funName (reverse rs) rhs [])
        lam _ _ _ = fail "lambda type mismatch"
        lamPoly ps s@TBox {} rs = lam ps s rs
        lamPoly ps s rs = do
            (_,s) <- skolomize s
            lam ps s rs
    lam pats typ []

declDiagnostic ::  (HsDecl) -> Diagnostic
declDiagnostic decl@(HsPatBind sloc (HsPVar {}) _ _) = locMsg sloc "in the declaration" $ render $ ppHsDecl decl
declDiagnostic decl@(HsPatBind sloc pat _ _) = locMsg sloc "in the pattern binding" $ render $ ppHsDecl decl
declDiagnostic decl@(HsFunBind matches) = locMsg (srcLoc decl) "in the function binding" $ render $ ppHsDecl decl

tiExpl ::  Expl -> Tc (HsDecl,TypeEnv)
tiExpl (sc, decl@HsForeignDecl {}) = do return (decl,Map.empty)
tiExpl (sc, decl) = withContext (locSimple (srcLoc decl) ("in the explicitly typed " ++  (render $ ppHsDecl decl))) $ do
    liftIO $ putStrLn $ "typing expl: " ++ show (getDeclName decl)
    addToCollectedEnv (Map.singleton (getDeclName decl) sc)
    (_,sc) <- skolomize sc
    tcDecl decl sc
    {-
       cHierarchy <- getClassHierarchy
       --(qs :=> t) <- -fmap snd $ freshInst sc
       let (qs :=> t) = unQuantify sc
       t <- flattenType t
       qs <- flattenType qs
       --liftIO $ putStrLn  $ show sc
       (ps, env') <- tiDeclTop env decl t
       --liftIO $ putStrLn  $ show ps
       ps <- flattenType ps

       --qs' <- flattenType qs
       --ps'' <- flattenType ps
       fs <- liftM tv (flattenType env)
       --qs' <- sequence [ flattenType y >>= return . IsIn x | IsIn x y <- qs]
       s          <- getSubst
       let qs'     = apply s qs
           t'      = apply s t
           ps'     = [ p | p <- apply s ps, not (entails cHierarchy qs' p) ]
       --    fs      = tv (apply s env)
           gs      = tv t' {- \\ fs  -} -- TODO fix this!
           sc'     = quantify gs (qs':=>t')
       -- (ds,rs) <- reduce cHierarchy fs gs ps'
       --liftIO $ putStrLn  $ show (gs,ps')
       (ds,rs,nsub) <- splitReduce cHierarchy fs gs ps'
       --liftIO $ putStrLn  $ show (ds,rs,nsub)
       sequence_ [ unify  (TVar tv) t | (tv,t) <- nsub ]
       --extSubst nsub
       --unify t' t
       --unify t t'
       if sc /= sc' then
           fail $ "signature too general for " ++ show (getDeclName decl) ++ "\n Given: " ++ show sc ++ "\n Infered: " ++ show sc'
        else if not (null rs) then
           fail $ "context too weak for "  ++ show (getDeclName decl) ++ "\nGiven: " ++ PPrint.render (pprint  sc) ++ "\nInfered: " ++ PPrint.render (pprint sc') ++"\nContext: " ++ PPrint.render (pprint  rs)
        else
           return (sc', ds,  env')
           --return (sc', ds, env')

-}
{-


tiExpr env expr@(HsLet decls e)
 = withContext
       (makeMsg "in the let binding" $ render $ ppHsExp expr) $
         do
         sigEnv <- getSigEnv
         let bgs = getFunDeclsBg sigEnv decls
         (ps, env1) <- tiSeq tiBindGroup env bgs
         (qs, env2, t) <- tiExpr (env1 `Map.union` env) e
         -- keep the let bound type assumptions in the environment
         return (ps ++ qs, env1 `Map.union` env2, t)


--------------------------------------------------------------------------------

tiStmts ::  TypeEnv -> [(HsStmt)] -> TI ([Pred], TypeEnv)

tiStmts = tiStmtsAcc [] Map.empty

tiStmtsAcc ::   [Pred] -> TypeEnv -> TypeEnv -> [(HsStmt)] -> TI ([Pred], TypeEnv)
tiStmtsAcc predAcc envAcc _ []
   = return (predAcc, envAcc)

tiStmtsAcc predAcc envAcc env (s:ss)
   = do
        (newPs, newEnv) <- tiStmt (envAcc `Map.union` env) s
        tiStmtsAcc (newPs ++ predAcc) (newEnv `Map.union` envAcc) env ss

tiStmt :: TypeEnv -> (HsStmt) -> TI ([Pred], TypeEnv)

-- with lists:
-- x <- xs
-- xs :: [a]
-- x :: a

tiStmt env expr@(HsGenerator srcLoc pat e)
   = withContext
        (locMsg srcLoc "in the generator " $ render $ ppHsStmt expr) $
        do
        (ePs, eEnv, eT) <- tiExpr env e
        (patPs, patEnv, patT) <- tiPat pat
        unify eT (TAp tList patT)
        return (ePs ++ patPs, eEnv `Map.union` patEnv)

tiStmt env stmt@(HsQualifier e)
   = withContext (makeMsg "in " $ render $ ppHsStmt stmt) $
        do
        (ePs, eEnv, eT) <- tiExpr env e
        unify eT tBool
        return (ePs, eEnv)

tiStmt env stmt@(HsLetStmt decls)
   = withContext
         (makeMsg "in let statement" $ render $ ppHsStmt stmt) $
         do
         sigEnv <- getSigEnv
         let bgs = getFunDeclsBg sigEnv decls
         tiSeq tiBindGroup env bgs

--------------------------------------------------------------------------------


-- NOTE: there's no need to do tiDecl with error contexts as the unification
--       doesn't happen until after this function is finished with
tiDecl ::  TypeEnv -> HsDecl -> TI ([Pred], TypeEnv, Type)

tiDecl env (HsForeignDecl _ _ _ n _) = do
    sigEnv <- getSigEnv
    let Just qt =  Map.lookup (toName Val n) sigEnv
    ((ps :=> t)) <- freshInst qt
    return (ps, env, t)

tiDecl env decl@(HsPatBind sloc pat rhs wheres) = withContext (declDiagnostic decl) $ do
        sigEnv <- getSigEnv
        let wheresBgs = getFunDeclsBg sigEnv wheres
        (ps, env1)     <- tiSeq tiBindGroup env wheresBgs
        (qs, env2, t)  <- tiRhs (env1 `Map.union` env) rhs
        return (ps ++ qs, env1 `Map.union` env2, t)


tiDecl env decl@(HsFunBind matches)  = withContext (declDiagnostic decl) $ do
        psEnvts <- mapM (tiMatch env) matches
        let ps' = concatMap fst3 psEnvts
        let ts'@(h':_) = map trd3 psEnvts
        let matchesEnv = Map.unions $ map snd3 psEnvts
        unifyList ts'  -- all matches must have the same type
        return (ps', matchesEnv, h')

--    where
--    matchLoc
--       = case matches of
--            [] -> bogusASrcLoc  -- this should never happen, there should be no empty match list
--            (m:_) -> case m of
--                        HsMatch sloc _name _pats _rhs _decls -> sloc


tiDeclTop ::  TypeEnv -> HsDecl -> Type -> TI ([Pred], TypeEnv)
tiDeclTop env decl t
   = do (ps,env,t') <- tiDecl env decl
        unify t t'
        return (ps, env)

--------------------------------------------------------------------------------

tiMatch ::  TypeEnv -> (HsMatch) -> TI ([Pred], TypeEnv, Type)
tiMatch env (HsMatch sloc funName pats rhs wheres)
   = withContext (locMsg sloc "in" $ show funName) $
     do
        -- pats must be done before wheres b/c variables bound in patterns
        -- may be referenced in the where clause
        (patsPs, patsEnv, patsTs) <- tiPats pats
        sigEnv <- getSigEnv
        let wheresBgs = getFunDeclsBg sigEnv wheres
        (wheresPs, wheresEnv) <- tiSeq tiBindGroup (patsEnv `Map.union` env) wheresBgs
        (rhsPs, rhsEnv, rhsT)   <- tiRhs (wheresEnv `Map.union` patsEnv `Map.union` env) rhs
        return (wheresPs++patsPs++rhsPs, patsEnv `Map.union` rhsEnv `Map.union` wheresEnv, foldr fn rhsT patsTs)  --Boba

-----------------------------------------------------------------------------


tiRhs env (HsUnGuardedRhs e)
   = tiExpr env e


tiRhs env (HsGuardedRhss rhss)
   = do
        psEnvTs <- mapM (tiGuardedRhs env) rhss
        let guardsPsEnvTs = map fst psEnvTs
        let rhsPsEnvTs    = map snd psEnvTs
        let guardPs    = concatMap fst3 guardsPsEnvTs
        let rhsPs      = concatMap fst3 rhsPsEnvTs
        let guardTs    = map trd3 guardsPsEnvTs
        let rhsTs@(h':_)      = map trd3 rhsPsEnvTs
        let guardEnv    = Map.unions $ map snd3 guardsPsEnvTs
        let rhsEnv      = Map.unions $ map snd3 rhsPsEnvTs
        unifyList (tBool:guardTs)                -- make sure these are all booleans
        unifyList rhsTs
        return (guardPs ++ rhsPs, guardEnv `Map.union` rhsEnv, h')


tiGuardedRhs ::  TypeEnv -> (HsGuardedRhs) -> TI (([Pred], TypeEnv, Type), ([Pred], TypeEnv, Type))
tiGuardedRhs env gRhs@(HsGuardedRhs sloc eGuard eRhs)
   = withContext (locMsg sloc "in the guarded right hand side" $ render $ ppHsGuardedRhs gRhs) $
     do
        (guardPs, guardEnv, guardT) <- tiExpr env eGuard
        unify tBool guardT
        (rhsPs, rhsEnv, rhsT)       <- tiExpr env eRhs
        return ((guardPs, guardEnv, guardT), (rhsPs, rhsEnv, rhsT))



-----------------------------------------------------------------------------

-- type check explicitly typed bindings



tiExpl ::  TypeEnv -> Expl -> TI (Scheme, [Pred], TypeEnv)
tiExpl env (sc, HsForeignDecl {}) = do
    return (sc,[],Map.empty)
tiExpl env (sc, decl) = withContext
       (locSimple (srcLoc decl) ("in the explicitly typed " ++  (render $ ppHsDecl decl))) $ do
       --liftIO $ putStrLn  $ render (ppHsDecl decl)
       cHierarchy <- getClassHierarchy
       --(qs :=> t) <- -fmap snd $ freshInst sc
       let (qs :=> t) = unQuantify sc
       t <- flattenType t
       qs <- flattenType qs
       --liftIO $ putStrLn  $ show sc
       (ps, env') <- tiDeclTop env decl t
       --liftIO $ putStrLn  $ show ps
       ps <- flattenType ps

       --qs' <- flattenType qs
       --ps'' <- flattenType ps
       fs <- liftM tv (flattenType env)
       --qs' <- sequence [ flattenType y >>= return . IsIn x | IsIn x y <- qs]
       s          <- getSubst
       let qs'     = apply s qs
           t'      = apply s t
           ps'     = [ p | p <- apply s ps, not (entails cHierarchy qs' p) ]
       --    fs      = tv (apply s env)
           gs      = tv t' {- \\ fs  -} -- TODO fix this!
           sc'     = quantify gs (qs':=>t')
       -- (ds,rs) <- reduce cHierarchy fs gs ps'
       --liftIO $ putStrLn  $ show (gs,ps')
       (ds,rs,nsub) <- splitReduce cHierarchy fs gs ps'
       --liftIO $ putStrLn  $ show (ds,rs,nsub)
       sequence_ [ unify  (TVar tv) t | (tv,t) <- nsub ]
       --extSubst nsub
       --unify t' t
       --unify t t'
       if sc /= sc' then
           fail $ "signature too general for " ++ show (getDeclName decl) ++ "\n Given: " ++ show sc ++ "\n Infered: " ++ show sc'
        else if not (null rs) then
           fail $ "context too weak for "  ++ show (getDeclName decl) ++ "\nGiven: " ++ PPrint.render (pprint  sc) ++ "\nInfered: " ++ PPrint.render (pprint sc') ++"\nContext: " ++ PPrint.render (pprint  rs)
        else
           return (sc', ds,  env')
           --return (sc', ds, env')

-----------------------------------------------------------------------------

-- type check implicitly typed bindings


restricted   :: [Impl] -> Bool
restricted bs
   = any isSimpleDecl bs
   where
   isSimpleDecl :: (HsDecl) -> Bool
   isSimpleDecl (HsPatBind _sloc _pat _rhs _wheres) = True
   isSimpleDecl _ = False

tiImpls ::  TypeEnv -> [Impl] -> TI ([Pred], TypeEnv)
tiImpls env [] = return ([],env)
tiImpls env bs = withContext (locSimple (srcLoc bs) ("in the implicitly typed: " ++ (show (map getDeclName bs)))) $ do
      --liftIO $ mapM (putStrLn .  render . ppHsDecl) bs
      cHierarchy <- getClassHierarchy
      ts <- mapM (\_ -> newTVar Star) bs
      let
          is      = getImplsNames bs
          scs     = map toScheme ts
          newEnv1 = Map.fromList $ zip is scs
          env'    = newEnv1 `Map.union` env
      pssEnvs <- sequence (zipWith (tiDeclTop env') bs ts)
      let pss  = map fst pssEnvs
      let envs = map snd pssEnvs
      s   <- getSubst
      ps' <- flattenType $ concat pss
      ts' <- flattenType ts
      fs <- liftM tv (flattenType env)
      --let ps'     = apply s (concat pss)
      --    ts'     = apply s ts
      --    fs      = tv (apply s env)
      let vss@(_:_)  = map tv ts'
          gs      = foldr1 union vss \\ fs
      -- (ds,rs) <- reduce cHierarchy fs (foldr1 intersect vss) ps'
      (ds,rs,nsub) <- splitReduce cHierarchy fs (foldr1 intersect vss) ps'
      sequence_ [ unify  (TVar tv) t | (tv,t) <- nsub ]
      -- extSubst nsub
      if restricted bs then
          let gs'  = gs \\ tv rs
              scs' = map (quantify gs' . ([]:=>)) ts'
              newEnv2 = Map.fromList $ zip is scs' -- map assumpToPair $ zipWith makeAssump is scs'
          in return (ds++rs,  (Map.unions envs) `Map.union` newEnv2)
        else
          let scs' = map (quantify gs . (rs:=>)) ts'
              newEnv3 = Map.fromList $ zip is scs' -- map assumpToPair $ zipWith makeAssump is scs'
          in return (ds,  (Map.unions envs) `Map.union` newEnv3)

getImplsNames :: [Impl] -> [Name]
getImplsNames impls = map getDeclName impls


-----------------------------------------------------------------------------



tiBindGroup env (es, is)
   = do
     modName <- getModName
     --let env1 = Map.fromList [assumpToPair $ getDeclName decl :>: sc | (sc,decl) <- es ]
     let env1 = Map.fromList [(getDeclName decl, sc) | (sc,decl) <- es ]
     (implPs, implEnv) <- tiImpls (env1 `Map.union` env) is
     explPsEnv   <- mapM (tiExpl (implEnv `Map.union` env1 `Map.union` env)) es
     let explPs = concat [ x | (_,x,_) <- explPsEnv]
     let explEnv = Map.unions $ [ x | (_,_,x) <- explPsEnv]
     --let env2 = Map.fromList [ assumpToPair (getDeclName decl :>: sc) | (sc,_,_) <- explPsEnv | (_,decl) <- es ]
     let env2 = Map.fromList [ (getDeclName decl,sc) | (sc,_,_) <- explPsEnv | (_,decl) <- es ]
     return (implPs ++ explPs, env2 `Map.union` explEnv `Map.union` implEnv)

tiSeq ti env []
 = return ([],Map.empty)
tiSeq ti env (bs:bss)
 = do (ps,env1)  <- ti env bs
      (qs,env2) <- tiSeq ti (env1 `Map.union` env) bss
      return (ps++qs, env2 `Map.union` env1)


-----------------------------------------------------------------------------


tiProgram ::  Module -> SigEnv -> KindEnv -> ClassHierarchy -> TypeEnv -> TypeEnv -> Program -> IO TypeEnv
tiProgram modName sEnv kt h dconsEnv env bgs = runTI dconsEnv h kt sEnv modName $
  do (ps, env1) <- tiSeq tiBindGroup env bgs
     s         <- getSubst
     ps <- flattenType ps
     ([], rs) <- split h [] (apply s ps)
     case topDefaults h rs of
       Right s' -> do
        env1' <- flattenType env1
        return $  apply  s'  env1'
       --Nothing -> return $  apply  s env1
       Left s -> fail $ show modName ++ s


--------------------------------------------------------------------------------


-}

-- Typing Literals

tiLit :: HsLiteral -> Tc Tau
tiLit (HsChar _) = return tChar
tiLit (HsInt _) = do
        --v <- newTVar Star
        (v) <- newBox Star
        addPreds [IsIn class_Num v]
        return v

tiLit (HsFrac _) = do
        --v <- newTVar Star
        (v) <- newBox Star
        addPreds [IsIn class_Fractional v]
        return v

tiLit (HsString _)  = return tString



--tiProgram = undefined

------------------------------------------
-- Binding analysis and program generation
------------------------------------------


-- create a Program structure from a list of decls and
-- type sigs. Type sigs are associated with corresponding
-- decls if they exist

getFunDeclsBg :: TypeEnv -> [HsDecl] -> [BindGroup]
getFunDeclsBg sigEnv decls = makeProgram sigEnv equationGroups where
   equationGroups :: [[HsDecl]]
   equationGroups = getBindGroups bindDecls (nameName . getDeclName) getDeclDeps
   --equationGroups = getBindGroups bindDecls (hsNameIdent_u (hsIdentString_u ("equationGroup" ++)) . getDeclName) getDeclDeps
   -- just make sure we only deal with bindDecls and not others
   bindDecls = collectBindDecls decls

-- | make a program from a set of binding groups
makeProgram :: TypeEnv -> [[HsDecl]] -> [BindGroup]
makeProgram sigEnv groups = map (makeBindGroup sigEnv ) groups


-- reunite decls with their signatures, if ever they had one

makeBindGroup :: TypeEnv -> [HsDecl] -> BindGroup
makeBindGroup sigEnv decls = (exps, impls) where
   (exps, impls) = makeBindGroup' sigEnv decls

makeBindGroup' _ [] = ([], [])
makeBindGroup' sigEnv (d:ds)
   = case Map.lookup funName sigEnv of
        Nothing -- no type sig for this equation
           -> (restExpls, d:restImpls)
        Just scheme  -- this one has a type sig
           -> ((scheme, d):restExpls, restImpls)
   where
   funName = getDeclName d
   (restExpls, restImpls) = makeBindGroup' sigEnv ds

collectBindDecls :: [HsDecl] ->  [HsDecl]
collectBindDecls = filter isBindDecl where
    isBindDecl :: HsDecl -> Bool
    isBindDecl HsPatBind {} = True
    isBindDecl HsFunBind {} = True
    isBindDecl _ = False


