module FrontEnd.Tc.Main (tiExpr, tiProgram, makeProgram ) where

import Control.Monad.Writer
import Data.Graph(stronglyConnComp, SCC(..))
import IO(hFlush,stdout)
import List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Text.PrettyPrint.HughesPJ as P

import Control.Monad.Reader
import DeclsDepends(getDeclDeps)
import Diagnostic
import Doc.DocLike
import Doc.PPrint as PPrint
import FrontEnd.Desugar(doToExp)
import FrontEnd.KindInfer
import FrontEnd.SrcLoc
import FrontEnd.Tc.Class
import FrontEnd.Tc.Monad hiding(listenPreds)
import FrontEnd.Tc.Type
import FrontEnd.Tc.Unify
import FrontEnd.Utils(getDeclName)
import GenUtil
import HsPretty
import HsSyn
import Name.Name
import Name.Names
import Name.VConsts
import Options
import Support.FreeVars
import qualified FlagDump as FD
import qualified FlagOpts as FO


listenPreds = listenSolvePreds

type Expl = (Sigma, HsDecl)
-- TODO: this is different than the "Typing Haskell in Haskell" paper
-- we do not further sub-divide the implicitly typed declarations in
-- a binding group.
type BindGroup = ([Expl], [Either HsDecl [HsDecl]])


tcKnownApp e coerce vname as typ = do
    sc <- lookupName vname
    let (_,_,rt) = fromType sc
    -- fall through if the type isn't arrowy enough (will produce type error)
    if (length . fst $ fromTArrow rt) < length as then tcApps' e as typ else do
    (ts,rt) <- freshInstance Sigma sc
    e' <- if coerce then doCoerce (ctAp ts) e else return e
    --addCoerce nname (ctAp ts)
    let f (TArrow x y) (a:as) = do
            a <- tcExprPoly a x
            y <- evalType y
            (as,fc) <- f y as
            return (a:as,fc)
        f lt [] = do
            fc <- lt `subsumes` typ
            return ([],fc)
    (nas,CTId) <- f rt as
    return (e',nas)

tcApps e@(HsVar v) as typ = do
    let vname = toName Val v
    --let nname = toName Val n
    when (dump FD.BoxySteps) $ liftIO $ putStrLn $ "tcApps: " ++ (show vname)
    rc <- asks tcRecursiveCalls
    -- fall through if this is a recursive call to oneself
    if (vname `Set.member` rc) then tcApps' e as typ else do
    tcKnownApp e True vname as typ

tcApps e@(HsAsPat n (HsCon v)) as typ = do
    let vname = toName DataConstructor v
    let nname = toName Val n
    when (dump FD.BoxySteps) $ liftIO $ putStrLn $ "tcApps: " ++ (show nname ++ "@" ++ show vname)
    addToCollectedEnv (Map.singleton (toName Val n) typ)
    tcKnownApp e False vname as typ

tcApps e as typ = tcApps' e as typ

-- the fall through case
tcApps' e as typ = do
    bs <- sequence [ newBox Star | _ <- as ]
    e' <- tcExpr e (foldr fn typ bs)
    as' <- sequence [ tcExprPoly a r | r <- bs | a <- as ]
    return (e',as')



tcApp e1 e2 typ = do
    (e1,[e2]) <- tcApps e1 [e2] typ
    return (e1,e2)

tiExprPoly,tcExprPoly ::  HsExp -> Type ->  Tc HsExp

tcExprPoly e t = do
    t <- evalType t
    tiExprPoly e t

tiExprPoly e t@TMetaVar {} = tcExpr e t   -- GEN2
tiExprPoly e t = do                   -- GEN1
    (ts,_,t) <- skolomize t
    e <- tcExpr e t
    doCoerce (ctAbs ts) e

doCoerce :: CoerceTerm -> HsExp -> Tc HsExp
doCoerce CTId e = return e
doCoerce ct e = do
    (e',n) <- wrapInAsPat e
    addCoerce n ct
    return e'

wrapInAsPat :: HsExp -> Tc (HsExp,Name)
wrapInAsPat e = do
    n <- newHsVar "As"
    return (HsAsPat (nameName n) e, n)

newHsVar ns = do
    nn <- newUniq
    return $ toName Val (ns ++ "@",show nn)



tiExpr,tcExpr ::  HsExp -> Type ->  Tc HsExp

tcExpr e t = do
    t <- evalType t
    e <- tiExpr e t
    --(_,False,_) <- unbox t
    return e

tiExpr (HsVar v) typ = do
    sc <- lookupName (toName Val v)
    f <- sc `subsumes` typ
    rc <- asks tcRecursiveCalls
    if (toName Val v `Set.member` rc) then do
        (e',n) <- wrapInAsPat (HsVar v)
        tell mempty { outKnots = [(n,toName Val v)] }
        return e'
      else do
        doCoerce f (HsVar v)

tiExpr (HsCase e alts) typ = withContext (simpleMsg $ "in the case expression\n   case " ++ show e ++ " of ...") $ do
    scrutinee <- newBox Star
    e' <- tcExpr e scrutinee
    alts' <- mapM (tcAlt scrutinee typ) alts
    (ne,ap) <- wrapInAsPat (HsCase e' alts')
    addToCollectedEnv (Map.singleton ap typ)
    return ne


tiExpr (HsCon conName) typ = do
    sc <- lookupName (toName DataConstructor conName)
    sc `subsumes` typ
    return (HsCon conName)

tiExpr (HsLit l@(HsInt _)) typ = do
    t <- tiLit l
    t `subsumes` typ
    (ne,n) <- wrapInAsPat (HsLit l)
    addToCollectedEnv (Map.singleton n typ)
    return ne

tiExpr (HsLit l) typ = do
    t <- tiLit l
    t `subsumes` typ
    return (HsLit l)

tiExpr (HsAsPat n e) typ = do
    e <- tcExpr e typ
    --typ <- flattenType typ
    addToCollectedEnv (Map.singleton (toName Val n) typ)
    return (HsAsPat n e)

-- comb LET-S and VAR
tiExpr expr@(HsExpTypeSig sloc e qt) typ =  withContext (locMsg sloc "in the annotated expression" $ render $ ppHsExp expr) $ do
    kt <- getKindEnv
    s <- hsQualTypeToSigma kt qt
    s `subsumes` typ
    e' <- tcExpr e typ
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
    ret <- newBox Star
    e1 <- tcExpr e1 arg2
    e2 <- tcExpr e2 (arg `fn` (arg2 `fn` ret))
    (arg `fn` ret) `subsumes` typ
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
tiExpr expr@(HsLambda sloc ps e) typ = withContext (locSimple sloc $ "in the lambda expression\n   \\" ++ show (pprint ps:: P.Doc) ++ " -> ...") $ do
    let lam (p:ps) e (TMetaVar mv) rs = do -- ABS2
            withMetaVars mv [Star,Star] (\ [a,b] -> a `fn` b) $ \ [a,b] -> lam (p:ps) e (a `fn` b) rs
        lam (p:ps) e (TArrow s1' s2') rs = do -- ABS1
            --box <- newBox Star
            --s1' `boxyMatch` box
            (p',env) <- tcPat p s1'
            localEnv env $ do
                s2' <- evalType s2'
                lamPoly ps e s2' (p':rs)  -- TODO poly
        lam [] e typ rs = do
            e' <- tcExpr e typ
            return (HsLambda sloc (reverse rs) e')
        lam _ _ t _ = do
            t <- flattenType t
            fail $ "expected a -> b, found: " ++ prettyPrintType t
        lamPoly ps e s rs = do
            (ts,_,s) <- skolomize s
            e <- lam ps e s rs
            doCoerce (ctAbs ts) e
    lam ps e typ []


tiExpr (HsIf e e1 e2) typ = withContext (simpleMsg $ "in the if expression\n   if " ++ show e ++ "...") $ do
    e <- tcExpr e tBool
    e1 <- tcExpr e1 typ
    e2 <- tcExpr e2 typ
    return (HsIf e e1 e2)

tiExpr tuple@(HsTuple exps@(_:_)) typ = withContext (makeMsg "in the tuple" $ render $ ppHsExp tuple) $ do
    (_,exps') <- tcApps (HsCon (toTuple (length exps))) exps typ
    return (HsTuple exps')


-- special case for the empty list
tiExpr (HsList []) (TAp c v) | c == tList = do
    unBox v
    return (HsList [])

-- special case for the empty list
tiExpr (HsList []) typ = do
    v <- newVar Star
    let lt = TForAll [v] ([] :=> TAp tList (TVar v))
    lt `subsumes` typ
    return (HsList [])

-- non empty list
tiExpr expr@(HsList exps@(_:_)) (TAp tList' v) | tList == tList' = withContext (makeMsg "in the list " $ render $ ppHsExp expr) $ do
        exps' <- mapM (`tcExpr` v) exps
        return (HsList exps')

-- non empty list
tiExpr expr@(HsList exps@(_:_)) typ = withContext (makeMsg "in the list " $ render $ ppHsExp expr) $ do
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
    let bgs = getFunDeclsBg sigEnv decls
        f (bg:bgs) rs = do
            (ds,env) <- tcBindGroup bg
            localEnv env $ f bgs (ds ++ rs)
        f [] rs = do
            e' <- tcExpr e typ
            return (HsLet rs e')
    f bgs []



tiExpr e typ = fail $ "tiExpr: not implemented for: " ++ show (e,typ)

tcWheres :: [HsDecl] -> Tc ([HsDecl],TypeEnv)
tcWheres decls = do
    sigEnv <- getSigEnv
    let bgs = getFunDeclsBg sigEnv decls
        f (bg:bgs) rs cenv  = do
            (ds,env) <- tcBindGroup bg
            localEnv env $ f bgs (ds ++ rs) (env `mappend` cenv)
        f [] rs cenv = return (rs,cenv)
    f bgs [] mempty

-----------------------------------------------------------------------------

-- type check implicitly typed bindings


tcAlt ::  Sigma -> Sigma -> HsAlt -> Tc HsAlt

tcAlt scrutinee typ alt@(HsAlt sloc pat gAlts wheres)  = withContext (locMsg sloc "in the alternative" $ render $ ppHsAlt alt) $ do
    scrutinee <- evalType scrutinee
    (pat',env) <- tcPat pat scrutinee
    localEnv env $ do
    (wheres', env) <- tcWheres wheres
    localEnv env $ case gAlts of
        HsUnGuardedRhs e -> do
            e' <- tcExpr e typ
            return (HsAlt sloc pat' (HsUnGuardedRhs e') wheres')
        HsGuardedRhss as -> do
            gas <- mapM (tcGuardedAlt typ) as
            return (HsAlt sloc pat' (HsGuardedRhss gas) wheres')

tcGuardedAlt typ gAlt@(HsGuardedRhs sloc eGuard e) = withContext (locMsg sloc "in the guarded alternative" $ render $ ppGAlt gAlt) $ do
    typ <- evalType typ
    g' <- tcExpr eGuard tBool
    e' <- tcExpr e typ
    return  (HsGuardedRhs sloc g' e')

tcGuardedRhs typ gAlt@(HsGuardedRhs sloc eGuard e) = withContext (locMsg sloc "in the guarded alternative" $ render $ ppHsGuardedRhs gAlt) $ do
    typ <- evalType typ
    g' <- tcExpr eGuard tBool
    e' <- tcExpr e typ
    return  (HsGuardedRhs sloc g' e')

-- Typing Patterns

tiPat,tcPat :: HsPat -> Type -> Tc (HsPat, Map.Map Name Sigma)

tcPat p typ = withContext (makeMsg "in the pattern: " $ render $ ppHsPat p) $ do
    typ <- evalType typ
    tiPat p typ

tiPat (HsPVar i) typ = do
        --v <- newMetaVar Tau Star
        --v `boxyMatch` typ
        --typ `subsumes` v
        typ' <- unBox typ
        addToCollectedEnv (Map.singleton (toName Val i) typ')
        return (HsPVar i, Map.singleton (toName Val i) typ')

tiPat pl@(HsPLit HsChar {}) typ = boxyMatch tChar typ >> return (pl,mempty)
tiPat pl@(HsPLit HsString {}) typ = boxyMatch tString typ >> return (pl,mempty)
tiPat pl@(HsPLit HsInt {}) typ = do
    unBox typ
    addPreds [IsIn class_Num typ]
    return (pl,mempty)
tiPat pl@(HsPLit HsFrac {}) typ = do
    unBox typ
    addPreds [IsIn class_Fractional typ]
    return (pl,mempty)

{-
tiPat (HsPLit l) typ = do
    t <- tiLit l
    typ `subsumes` t -- `boxyMatch` typ
    return (HsPLit l,Map.empty)
-}
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
    s <- lookupName (toName DataConstructor conName)
    nn <- deconstructorInstantiate s
    let f (p:pats) (a `TArrow` rs) (ps,env) = do
            (np,res) <- tiPat p a
            f pats rs (np:ps,env `mappend` res)
        f (p:pats) rs _ = do
            fail $ "constructor applied to too many arguments:" <+> show p <+> prettyPrintType rs
        f [] (_ `TArrow` _) _ = do
            fail "constructor not applied to enough arguments"
        f [] rs (ps,env) = do
            rs `subsumes` typ
            unBox typ
            return (HsPApp conName (reverse ps), env)
    f pats nn mempty
    --bs <- sequence [ newBox Star | _ <- pats ]
    --s `subsumes` (foldr fn typ bs)
    --pats' <- sequence [ tcPat a r | r <- bs | a <- pats ]
    --return (HsPApp conName (fsts pats'), mconcat (snds pats'))


tiPat pl@(HsPList []) (TAp t v) | t == tList = do
    unBox v
    return (pl,mempty)

tiPat pl@(HsPList []) typ = do
    v <- newBox Star
    --typ `subsumes` TAp tList v
    typ `boxyMatch` TAp tList v
    return (pl,mempty)

tiPat (HsPList pats@(_:_)) (TAp t v) | t == tList = do
    --v <- newBox Star
    --TAp tList v `boxyMatch` typ
    --typ `subsumes` TAp tList v
    ps <- mapM (`tcPat` v) pats
    return (HsPList (fsts ps), mconcat (snds ps))

tiPat (HsPList pats@(_:_)) typ = do
    v <- newBox Star
    --TAp tList v `boxyMatch` typ
    ps <- mapM (`tcPat` v) pats
    typ `boxyMatch` TAp tList v
    return (HsPList (fsts ps), mconcat (snds ps))

--tiPat HsPWildCard typ = unBox typ >> return (HsPWildCard, mempty)
tiPat HsPWildCard typ = do
    n <- newHsVar "Wild"
    typ' <- unBox typ
    addToCollectedEnv (Map.singleton n typ')
    return (HsPVar (nameName n), Map.singleton n typ')


tiPat (HsPAsPat i pat) typ = do
    (pat',env) <- tcPat pat typ
    addToCollectedEnv (Map.singleton (toName Val i) typ)
    return (HsPAsPat i pat', Map.insert (toName Val i) typ env)

tiPat (HsPInfixApp pLeft conName pRight) typ =  tiPat (HsPApp conName [pLeft,pRight]) typ

tiPat tuple@(HsPTuple pats) typ = tiPat (HsPApp (toTuple (length pats)) pats) typ

tiPat p _ = error $ "tiPat: " ++ show p

tcBindGroup :: BindGroup -> Tc ([HsDecl], TypeEnv)
tcBindGroup (es, is) = do
     let env1 = Map.fromList [(getDeclName decl, sc) | (sc,decl) <- es ]
     localEnv env1 $ do
         (impls, implEnv) <- tiImplGroups is
         localEnv implEnv $ do
             expls   <- mapM tiExpl es
             return (impls ++ fsts expls, mconcat (implEnv:env1:snds expls))

tiImplGroups :: [Either HsDecl [HsDecl]] -> Tc ([HsDecl], TypeEnv)
tiImplGroups [] = return ([],mempty)
tiImplGroups (Left x:xs) = do
    (d,te) <- tiNonRecImpl x
    (ds',te') <- localEnv te $ tiImplGroups xs
    return (d:ds', te `mappend` te')
tiImplGroups (Right x:xs) = do
    (ds,te) <- tiImpls x
    (ds',te') <- localEnv te $ tiImplGroups xs
    return (ds ++ ds', te `mappend` te')

tiNonRecImpl :: HsDecl -> Tc (HsDecl, TypeEnv)
tiNonRecImpl decl = withContext (locSimple (srcLoc decl) ("in the implicitly typed: " ++ show (getDeclName decl))) $ do
    when (dump FD.BoxySteps) $ liftIO $ putStrLn $ "*** tiimpls " ++ show (getDeclName decl)
    mv <- newMetaVar Sigma Star
    (res,ps) <- listenPreds $ tcDecl decl mv
    ps' <- flattenType ps
    mv' <- flattenType mv
    fs <- freeMetaVarsEnv
    let vss = Set.fromList $ freeMetaVars mv'
        gs = vss Set.\\ fs
    (mvs,ds,rs) <- splitReduce (Set.toList fs) (Set.toList vss) ps'
    addPreds ds
    sc' <- if restricted [decl] then do
        let gs' = gs Set.\\ Set.fromList (freeVars rs)
        addPreds rs
        quantify (Set.toList gs') [] mv'
     else quantify (Set.toList gs) rs mv'
    let f n s = do
        let (TForAll vs _) = toSigma s
        addCoerce n (ctAbs vs)
        when (dump FD.BoxySteps) $ liftIO $ putStrLn $ "*** " ++ show n ++ " :: " ++ prettyPrintType s
        return (n,s)
    (n,s) <- f (getDeclName decl) sc'
    let nenv = (Map.singleton n s)
    addToCollectedEnv nenv
    return (fst res, nenv)

tiImpls ::  [HsDecl] -> Tc ([HsDecl], TypeEnv)
tiImpls [] = return ([],Map.empty)
tiImpls bs = withContext (locSimple (srcLoc bs) ("in the recursive implicitly typed: " ++ (show (map getDeclName bs)))) $ do
    let names = map getDeclName bs
    when (dump FD.BoxySteps) $ liftIO $ putStrLn $ "*** tiimpls " ++ show names
    ts <- sequence [newMetaVar Tau Star | _ <- bs]
    (res,ps) <- listenPreds $
        local (tcRecursiveCalls_u (Set.union $ Set.fromList names)) $
            localEnv (Map.fromList [  (d,s) | d <- names | s <- ts]) $
                sequence [ tcDecl d s | d <- bs | s <- ts ]
    ps' <- flattenType ps
    ts' <- flattenType ts
    fs <- freeMetaVarsEnv
    let vss = map (Set.fromList . freeVars) ts'
        gs = (Set.unions vss) Set.\\ fs
    (mvs,ds,rs) <- splitReduce (Set.toList fs) (Set.toList $ foldr1 Set.intersection vss) ps'
    addPreds ds
    scs' <- if restricted bs then do
        let gs' = gs Set.\\ Set.fromList (freeVars rs)
        addPreds rs
        mapM (quantify (Set.toList gs') []) ts'
     else mapM (quantify (Set.toList gs) rs) ts'
    let f n s = do
        let (TForAll vs _) = toSigma s
        addCoerce n (ctAbs vs)
        when (dump FD.BoxySteps) $ liftIO $ putStrLn $ "*** " ++ show n ++ " :: " ++ prettyPrintType s
        return (n,s)
    nenv <- sequence [ f (getDeclName d) t  | (d,_) <- res | t <- scs' ]
    addToCollectedEnv (Map.fromList nenv)
    return (fsts res, Map.fromList nenv)

tcRhs :: HsRhs -> Sigma -> Tc HsRhs
tcRhs rhs typ = case rhs of
    HsUnGuardedRhs e -> do
        e' <- tcExpr e typ
        return (HsUnGuardedRhs e')
    HsGuardedRhss as -> do
        gas <- mapM (tcGuardedRhs typ) as
        return (HsGuardedRhss gas)

tcPragmaDecl spec@HsPragmaSpecialize { hsDeclSrcLoc = sloc, hsDeclName = n, hsDeclType = t } = do
    withContext (locMsg sloc "in the SPECIALIZE pragma" $ show n) ans where
    ans = do
        kt <- getKindEnv
        t <- hsTypeToType kt t
        let nn = toName Val n
        sc <- lookupName nn
        sc `subsumes` t
        addRule RuleSpec { ruleUniq = hsDeclUniq spec, ruleName = nn, ruleType = t, ruleSuper = hsDeclBool spec }
        return [spec]

tcPragmaDecl (HsPragmaRules rs) = do
    rs' <- mapM tcRule rs
    return [HsPragmaRules rs']


-- foreign decls are accumulated by tiExpl
tcPragmaDecl fd@(HsForeignDecl _ _ n qt) = do
    kt <- getKindEnv
    s <- hsQualTypeToSigma kt qt
    addToCollectedEnv (Map.singleton (toName Val n) s)
    return []

tcPragmaDecl fd@(HsForeignExport _ e n qt) = do
    kt <- getKindEnv
    s <- hsQualTypeToSigma kt qt
    addToCollectedEnv (Map.singleton (ffiExportName e) s)
    return []

tcPragmaDecl _ = return []

tcRule prule@HsRule { hsRuleUniq = uniq, hsRuleFreeVars = vs, hsRuleLeftExpr = e1, hsRuleRightExpr = e2, hsRuleSrcLoc = sloc } =
    withContext (locMsg sloc "in the RULES pragma" $ hsRuleString prule) ans where
        ans = do
            vs' <- mapM dv vs
            tr <- newBox Star
            let (vs,envs) = unzip vs'
            ch <- getClassHierarchy
            ((e1,rs1),(e2,rs2)) <- localEnv (mconcat envs) $ do
                    (e1,ps1) <- listenPreds (tcExpr e1 tr)
                    (e2,ps2) <- listenPreds (tcExpr e2 tr)
                    ([],rs1) <- splitPreds ch [] ps1
                    ([],rs2) <- splitPreds ch [] ps2
                    return ((e1,rs1),(e2,rs2))
            mapM_ unBox vs
            vs <- flattenType vs
            tr <- flattenType tr
            let mvs = snub $ concatMap freeMetaVars (tr:vs)
            nvs <- mapM (newVar . metaKind) mvs
            sequence_ [ varBind mv (TVar v) | v <- nvs |  mv <- mvs ]
            (rs1,rs2) <- flattenType (rs1,rs2)
            ch <- getClassHierarchy
            rs1 <- return $ simplify ch rs1
            rs2 <- return $ simplify ch rs2
            assertEntailment rs1 rs2
            return prule { hsRuleLeftExpr = e1, hsRuleRightExpr = e2 }
        dv (n,Nothing) = do
            v <- newMetaVar Tau Star
            let env = (Map.singleton (toName Val n) v)
            addToCollectedEnv env
            return (v,env)
        dv (n,Just t) = do
            kt <- getKindEnv
            tt <- hsTypeToType kt t
            let env = (Map.singleton (toName Val n) tt)
            addToCollectedEnv env
            return (tt,env)

tcDecl ::  HsDecl -> Sigma -> Tc (HsDecl,TypeEnv)

{-
tcDecl d@(HsForeignDecl _ _ _ n _) typ = do
    s <- lookupName (toName Val n)
    s `subsumes` typ
    return (d,mempty)
-}


tcDecl decl@(HsPatBind sloc (HsPVar v) rhs wheres) typ = withContext (declDiagnostic decl) $ do
    typ <- evalType typ
    (wheres', env) <- tcWheres wheres
    localEnv env $ do
    case rhs of
        HsUnGuardedRhs e -> do
            e' <- tcExpr e typ
            return (HsPatBind sloc (HsPVar v) (HsUnGuardedRhs e') wheres', Map.singleton (toName Val v) typ)
        HsGuardedRhss as -> do
            gas <- mapM (tcGuardedRhs typ) as
            return (HsPatBind sloc (HsPVar v) (HsGuardedRhss gas) wheres', Map.singleton (toName Val v) typ)


tcDecl decl@(HsFunBind matches) typ = withContext (declDiagnostic decl) $ do
    typ <- evalType typ
    matches' <- mapM (`tcMatch` typ) matches
    return (HsFunBind matches', Map.singleton (getDeclName decl) typ)

tcMatch ::  HsMatch -> Sigma -> Tc HsMatch
tcMatch (HsMatch sloc funName pats rhs wheres) typ = withContext (locMsg sloc "in" $ show funName) $ do
    let lam (p:ps) (TMetaVar mv) rs = do -- ABS2
            withMetaVars mv [Star,Star] (\ [a,b] -> a `fn` b) $ \ [a,b] -> lam (p:ps) (a `fn` b) rs
        lam (p:ps) ty@(TArrow s1' s2') rs = do -- ABS1
            (p',env) <- tcPat p s1'
            localEnv env $ do
                s2' <- evalType s2'
                lamPoly ps s2' (p':rs)
        lam [] typ rs = do
            (wheres', env) <- tcWheres wheres
            rhs <- localEnv env $ tcRhs rhs typ
            return (HsMatch sloc funName (reverse rs) rhs wheres')
        lam _ t _ = do
            t <- flattenType t
            fail $ "expected a -> b, found: " ++ prettyPrintType t
        lamPoly ps s@TMetaVar {} rs = lam ps s rs
        lamPoly ps s rs = do
            (_,_,s) <- skolomize s
            lam ps s rs
    typ <- evalType typ
    res <- lam pats typ []
    return res

declDiagnostic ::  (HsDecl) -> Diagnostic
declDiagnostic decl@(HsPatBind sloc (HsPVar {}) _ _) = locMsg sloc "in the declaration" $ render $ ppHsDecl decl
declDiagnostic decl@(HsPatBind sloc pat _ _) = locMsg sloc "in the pattern binding" $ render $ ppHsDecl decl
declDiagnostic decl@(HsFunBind matches) = locMsg (srcLoc decl) "in the function binding" $ render $ ppHsDecl decl

tiExpl ::  Expl -> Tc (HsDecl,TypeEnv)
tiExpl (sc, decl@HsForeignDecl {}) = do return (decl,Map.empty)
tiExpl (sc, decl@HsForeignExport {}) = do return (decl,Map.empty)
tiExpl (sc, decl) = withContext (locSimple (srcLoc decl) ("in the explicitly typed " ++  (render $ ppHsDecl decl))) $ do
    when (dump FD.BoxySteps) $ liftIO $ putStrLn $ "** typing expl: " ++ show (getDeclName decl) ++ " " ++ prettyPrintType sc
    sc <- evalFullType sc
    (vs,qs,typ) <- skolomize sc
    let sc' = (tForAll vs (qs :=> typ))
        mp = (Map.singleton (getDeclName decl) sc')
    addCoerce (getDeclName decl) (ctAbs vs)
    addToCollectedEnv mp
    (ret,ps) <- localEnv mp $ listenPreds (tcDecl decl typ)
    ps <- flattenType ps
    ch <- getClassHierarchy
    env <- freeMetaVarsEnv
    (_,ds,rs) <- splitReduce (Set.toList env) (freeMetaVarsPreds qs) ps
    assertEntailment qs (rs ++ ds)
    return ret

restricted   :: [HsDecl] -> Bool
restricted bs = fopts FO.MonomorphismRestriction && any isSimpleDecl bs where
   isSimpleDecl :: (HsDecl) -> Bool
   isSimpleDecl (HsPatBind _sloc _pat _rhs _wheres) = True
   isSimpleDecl _ = False

{-



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


-}

getBindGroupName (expl,impls) =  map getDeclName (snds expl ++ concat (rights impls) ++ lefts impls)


tiProgram ::  [BindGroup] -> [HsDecl] -> Tc [HsDecl]
tiProgram bgs es = ans where
    ans = do
        (r,ps) <- listenPreds $ f bgs [] mempty
        ps <- flattenType ps
        ch <- getClassHierarchy
        ([],rs) <- splitPreds ch [] ps
        topDefaults rs
        return r
        --ps <- return $ simplify ch ps
        --liftIO $ mapM_ (putStrLn.show) ps
        --return r
    f (bg:bgs) rs cenv  = do
        ((ds,env),ps) <- listenPreds (tcBindGroup bg)
        ch <- getClassHierarchy
        withContext (makeMsg "in the binding group:" $ show (getBindGroupName bg)) $ do
            ([],leftovers) <- splitPreds ch [] ps
            topDefaults leftovers
        liftIO $ do putChar '.'; hFlush stdout
        localEnv env $ f bgs (ds ++ rs) (env `mappend` cenv)
    f [] rs _cenv = do
        ch <- getClassHierarchy
        (pdecls,ps) <- listenPreds $ mapM tcPragmaDecl es
        withContext (makeMsg "in the pragmas:" $ "rules") $ do
            ([],leftovers) <- splitPreds ch [] ps
            topDefaults leftovers
        liftIO $ putStrLn "!"
        return (rs ++ concat pdecls)

-- Typing Literals

tiLit :: HsLiteral -> Tc Tau
tiLit (HsChar _) = return tChar
tiLit (HsInt _) = do
    v <- newVar Star
    return $ TForAll [v] ([IsIn class_Num (TVar v)] :=> TVar v)
    --(v) <- newBox Star
    --addPreds [IsIn class_Num v]
    --return v

tiLit (HsFrac _) = do
    v <- newVar Star
    return $ TForAll [v] ([IsIn class_Fractional (TVar v)] :=> TVar v)
    --    (v) <- newBox Star
    --    addPreds [IsIn class_Fractional v]
    --    return v

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
   bindDecls = collectBindDecls decls

getBindGroups :: Ord name =>
                 [node]           ->    -- List of nodes
                 (node -> name)   ->    -- Function to convert nodes to a unique name
                 (node -> [name]) ->    -- Function to return dependencies of this node
                 [[node]]               -- Bindgroups

getBindGroups ns fn fd = map f $ stronglyConnComp [ (n, fn n, fd n) | n <- ns] where
    f (AcyclicSCC x) = [x]
    f (CyclicSCC xs) = xs

-- | make a program from a set of binding groups
makeProgram :: TypeEnv -> [[HsDecl]] -> [BindGroup]
makeProgram sigEnv groups = map (makeBindGroup sigEnv ) groups


-- | reunite decls with their signatures, if ever they had one

makeBindGroup :: TypeEnv -> [HsDecl] -> BindGroup
makeBindGroup sigEnv decls = (exps, f impls) where
    (exps, impls) = makeBindGroup' sigEnv decls
    enames = map (nameName . getDeclName . snd) exps
    f xs = map g $ stronglyConnComp [ (x, nameName $ getDeclName x,[ d | d <- getDeclDeps x, d `notElem` enames]) |  x <- xs]
    g (AcyclicSCC x) = Left x
    g (CyclicSCC xs) = Right xs

makeBindGroup' _ [] = ([], [])
makeBindGroup' sigEnv (d:ds) = case Map.lookup funName sigEnv of
        Nothing -> (restExpls, d:restImpls)
        Just scheme -> ((scheme, d):restExpls, restImpls)
   where
   funName = getDeclName d
   (restExpls, restImpls) = makeBindGroup' sigEnv ds

collectBindDecls :: [HsDecl] ->  [HsDecl]
collectBindDecls = filter isBindDecl where
    isBindDecl :: HsDecl -> Bool
    isBindDecl HsPatBind {} = True
    isBindDecl HsFunBind {} = True
    isBindDecl _ = False


