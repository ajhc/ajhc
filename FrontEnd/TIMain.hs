{-------------------------------------------------------------------------------

        Copyright:              Mark Jones and The Hatchet Team
                                (see file Contributors)

        Module:                 TIMain

        Description:            The main components of the type inference
                                algorithm.

        Primary Authors:        Mark Jones, Bernie Pope and Bryn Humberstone

        Notes:                  See the file License for license information

                                Large parts of this module were derived from
                                the work of Mark Jones' "Typing Haskell in
                                Haskell", (http://www.cse.ogi.edu/~mpj/thih/)

-------------------------------------------------------------------------------}

module TIMain (tiProgram, makeProgram) where

import Control.Monad.Error
import List((\\), intersect, union)
import qualified Data.Map as Map
import qualified Text.PrettyPrint.HughesPJ as PPrint

import Class(ClassHierarchy, entails, split, topDefaults, splitReduce)
import DeclsDepends(getDeclDeps)
import DependAnalysis(getBindGroups)
import Diagnostic
import Doc.PPrint as PPrint
import FrontEnd.Desugar(doToExp)
import FrontEnd.KindInfer(KindEnv)
import FrontEnd.SrcLoc
import FrontEnd.Utils(getDeclName)
import HsPretty
import HsSyn
import Name.Name
import Name.Names
import Name.VConsts
import Options
import Representation
import TIMonad
import Type
import TypeSigs(SigEnv)


getSubst = return Map.empty

isBindDecl :: HsDecl -> Bool
isBindDecl HsPatBind {} = True
isBindDecl HsFunBind {} = True
isBindDecl _ = False

fst3 :: (a,b,c) -> a
fst3 (a,_,_) = a
snd3 :: (a,b,c) -> b
snd3 (_,b,_) = b
trd3 :: (a,b,c) -> c
trd3 (_,_,c) = c

strace _ s = s

-- a TypeEnv maps identifier names to type schemes
type TypeEnv = Map.Map Name Scheme
type Expl = (Scheme, HsDecl)
type Impl = HsDecl
-- this is different than the "Typing Haskell in Haskell" paper
-- we do not further sub-divide the implicitly typed declarations in
-- a binding group.
type BindGroup = ([Expl], [Impl])
type Program = [BindGroup]

instance Types a => Types (Name, a) where
   apply s (x, y) = (x, apply s y)
   tv (_, y) = tv y

instance Types TypeEnv where
   apply s = Map.map (\e -> apply s e)
   tv env = tv $ map snd $ Map.toList env


tiExpr ::  TypeEnv -> (HsExp) -> TI ([Pred], TypeEnv, Type)

tiExpr env (HsVar v) | Just sc <- Map.lookup (toName Val v) env = do
          (ty@(ps :=> t)) <- freshInst sc
          --addInstance ((v,n),ty)
          return (ps, Map.empty, t)
tiExpr env (HsVar v) = error $ "tiExpr: could not find type scheme for: " ++ show v ++ " " ++ show env

{-

 = do let sc = case lookupEnv v env of
                  Nothing -> error $ "tiExpr: could not find type scheme for: " ++
		                     show v ++ " " ++ showEnv env
                  Just scheme -> scheme
      (ty@(ps :=> t)) <- freshInst sc
      --addInstance ((v,n),ty)
      return (ps, Map.empty, t)
-}

tiExpr env (HsCon conName)
 = do
      sc <- dConScheme (toName DataConstructor conName)
      (ty@(ps :=> t)) <- freshInst sc
      --addInstance ((conName,n),ty)
      return (ps, Map.empty, t)

tiExpr _env (HsLit l)
 = do (ps,t) <- tiLit l
      return (ps, Map.empty, t)

tiExpr env (HsAsPat n e) = do
    (ps,nenv, t) <- tiExpr env e
    --let newAssump = makeAssump n $ toScheme t
    --let newEnv = addToEnv (assumpToPair newAssump) nenv
    let newEnv = Map.insert (toName Val n) (toScheme t) nenv
    return (ps, newEnv, t)


tiExpr env expr@(HsInfixApp e1 e2 e3)
 = withContext
       (makeMsg "in the infix application" $ render $ ppHsExp expr) $
       do
       (ps, env1, te1) <- tiExpr env e1
       (qs, env2, te2) <- tiExpr env e2
       (rs, env3, te3) <- tiExpr env e3
       tout      <- newTVar Star
       unify (te1 `fn` (te3 `fn` tout)) te2
       return (ps ++ qs ++ rs, env1 `Map.union` env2 `Map.union` env3, tout)

tiExpr env expr@(HsApp e1 e2)
 = withContext
      (makeMsg "in the application" $ render $ ppHsExp expr) $
      do
      (ps, env1, te1) <- tiExpr env e1
      (qs, env2, te2) <- tiExpr env e2
      t           <- newTVar Star
      unify (te2 `fn` t) te1
      return (ps++qs, env1 `Map.union` env2, t)

-- we need to fix the type to to be in the class
-- cNum, just for cases such as:
-- foo = \x -> -x

tiExpr env expr@(HsNegApp e)
 = withContext
      (makeMsg "in the negative expression" $ render $ ppHsExp expr) $
      do
        (ps, env1, te) <- tiExpr env e
        return (IsIn class_Num te : ps, env1, te)

tiExpr env expr@(HsLambda sloc pats e)
 = withContext
      (locSimple sloc $ "in the lambda expression\n   \\" ++ show pats ++ " -> ...") $
      do
        (ps, envP, ts) <- tiPats pats
        (qs, envE, t)  <- tiExpr (envP `Map.union` env) e
        return (ps++qs, envP `Map.union` envE, foldr fn t ts)  -- Boba

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

tiExpr env (HsIf e e1 e2)
 = withContext
      (simpleMsg $
      "in the conditional expression\n   if " ++ show e ++ "...") $
      do (ps, env1, t)   <- tiExpr env e
         unify t tBool
         (ps1, env2, t1) <- tiExpr env e1
         (ps2, env3, t2) <- tiExpr env e2
         unify t1 t2
         return (ps++ps1++ps2, env1 `Map.union` env2 `Map.union` env3, t1)


tiExpr env (HsCase e alts)
 = withContext
        (simpleMsg $
            "in the case expression\n   case " ++ show e ++ " of ...") $
        do
        (pse, env1, te)    <- tiExpr env e
        psastsAlts     <- mapM (tiAlt env) alts
        let pstsPats = map fst3 psastsAlts
        let psPats   = concatMap fst pstsPats
        let tsPats   = map snd pstsPats
        let pstsEs   = map trd3 psastsAlts
        let psEs     = concatMap fst pstsEs
        let tsEs@(htsEs:_)  = map snd pstsEs
        let envAlts  = Map.unions $ map snd3 psastsAlts
        unifyList (te:tsPats)
        unifyList tsEs
        -- the list of rhs alternatives must be non-empty
        -- so it is safe to call head here
        return (pse ++ psPats ++ psEs, env1 `Map.union` envAlts, htsEs)


tiExpr env (HsDo stmts)
   = do
        let newExp = doToExp stmts
        withContext (simpleMsg "in a do expression")
                    (tiExpr env newExp)

-- tuples can't be empty, () is not a tuple
tiExpr env tuple@(HsTuple exps@(_:_))
   = withContext
        (makeMsg "in the tuple" $ render $ ppHsExp tuple) $
        do
           psasts <- mapM (tiExpr env) exps
           let typeList = map trd3 psasts
           let preds = concatMap fst3 psasts
           let env1 = Map.unions $ map snd3 psasts
           return (preds, env1, tTTuple typeList)

-- special case for the empty list
tiExpr _env (HsList [])
   = do
        v <- newTVar Star
        return ([], Map.empty, TAp tList v)

-- non empty list
tiExpr env expr@(HsList exps@(_:_))
   = withContext (makeMsg "in the list " $ render $ ppHsExp expr) $
        do
        psasts <- mapM (tiExpr env) exps
        let typeList@(htl:_) = map trd3 psasts
        unifyList typeList
        let preds = concatMap fst3 psasts
        let env1 = Map.unions $ map snd3 psasts
        return (preds, env1, TAp tList htl)



tiExpr env (HsParen e) = tiExpr env e

-- e1 :: a -> b
-- e2 :: a
-- e1 e2 :: b

{- XXX: we don't push error contexts for some cases, e.g.
   HsLeftSection -}
tiExpr env (HsLeftSection e1 e2)
   = do
        (e1Ps, envE1, e1T) <- tiExpr env e1
        (e2Ps, envE2, e2T) <- tiExpr env e2
        tv          <- newTVar Star
        unify e1T (e2T `fn` tv)
        return (e1Ps ++ e2Ps, envE1 `Map.union` envE2, tv)


-- I know this looks weird but it appears to be correct
-- e1 :: b
-- e2 :: a -> b -> c
-- e1 e2 :: a -> c

tiExpr env (HsRightSection e1 e2)
   = do
        (e1Ps, envE1, e1T) <- tiExpr env e1
        (e2Ps, envE2, e2T) <- tiExpr env e2
        tv1         <- newTVar Star
        tv2         <- newTVar Star
        unify e2T (tv1 `fn` (e1T `fn` tv2))
        return (e1Ps ++ e2Ps, envE1 `Map.union` envE2, tv1 `fn` tv2)

tiExpr env (HsRecConstr _ _)
   = error $ "tiExpr env (HsRecConstr _ _): not implemented"

tiExpr env (HsRecUpdate _ _)
   = error $ "tiExpr env (HsRecUpdate _ _): not implemented"

tiExpr env (HsEnumFrom e)
   = do
        (ePs, envE, eT) <- tiExpr env e
        return (IsIn class_Enum eT : ePs, envE, TAp tList eT)

tiExpr env (HsEnumFromTo e1 e2)
   = do
        (e1Ps, e1Env, e1T) <- tiExpr env e1
        (e2Ps, e2Env, e2T) <- tiExpr env e2
        unify e1T e2T
        return (IsIn class_Enum e1T : IsIn class_Enum e2T : (e1Ps ++ e2Ps),
                e1Env `Map.union` e2Env,
                TAp tList e1T)

tiExpr env (HsEnumFromThen e1 e2)
   = do
        (e1Ps, e1Env, e1T) <- tiExpr env e1
        (e2Ps, e2Env, e2T) <- tiExpr env e2
        unify e1T e2T
        return (IsIn class_Enum e1T : IsIn class_Enum e2T : (e1Ps ++ e2Ps),
                e1Env `Map.union` e2Env,
                TAp tList e1T)

tiExpr env (HsEnumFromThenTo e1 e2 e3)
   = do
        (e1Ps, e1Env, e1T) <- tiExpr env e1
        (e2Ps, e2Env, e2T) <- tiExpr env e2
        (e3Ps, e3Env, e3T) <- tiExpr env e3
        unifyList [e1T,e2T,e3T]
        return (IsIn class_Enum e1T : IsIn class_Enum e2T : IsIn class_Enum e3T : (e1Ps ++ e2Ps ++ e3Ps),
                e1Env `Map.union` e2Env `Map.union` e3Env,
                TAp tList e1T)

tiExpr env (HsListComp e stmts)
   = do
        psEnv <- tiStmts env stmts
        let stmtsPs = fst psEnv
        let stmtsEnv = snd psEnv
        (ePs, eEnv, eT) <- tiExpr (stmtsEnv `Map.union` env) e
        return (stmtsPs ++ ePs, eEnv `Map.union` stmtsEnv, TAp tList eT)

-- This should be desugared already
-- e :: t   ----> let {v::t; v=e} in v
tiExpr env (HsExpTypeSig _sloc e qt)
   = error $ "tiExpr: unexpected sugared explicitly typed expression " ++ show e

tiExpr _env e
   = error $ "tiExpr: not implemented for: " ++ show e

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

tiAlt ::  TypeEnv -> (HsAlt) -> TI (([Pred], Type), TypeEnv, ([Pred], Type))

tiAlt env alt@(HsAlt sloc pat gAlts wheres)
   = withContext (locMsg sloc "in the alternative" $ render $ ppHsAlt alt) $
        do
        sigEnv <- getSigEnv
        let wheresBgs = getFunDeclsBg sigEnv wheres
        (psPat, envPat, patT) <- tiPat pat
        (wheresPs, wheresEnv) <- tiSeq tiBindGroup (envPat `Map.union` env) wheresBgs
        (psAlt, envAlt, tAlt) <- tiGuardedAlts (wheresEnv `Map.union` envPat  `Map.union` env) gAlts
        -- not sure about the use of wheresPs below
        return ((psPat, patT), envPat `Map.union` envAlt `Map.union` wheresEnv, (wheresPs ++ psAlt, tAlt)) --Boba


tiGuardedAlts env (HsUnGuardedAlt e)
   = tiExpr env e

-- basically the same as HsGuardedRhss
tiGuardedAlts env (HsGuardedAlts gAlts)
   = withContext (simpleMsg "in guarded alternatives") $
     do
        psEnvTs <- mapM (tiGuardedAlt env) gAlts
        let guardsPsEnvTs = map fst psEnvTs
        let rhsPsEnvTs    = map snd psEnvTs
        let guardPs    = concatMap fst3 guardsPsEnvTs
        let rhsPs      = concatMap fst3 rhsPsEnvTs
        let guardTs    = map trd3 guardsPsEnvTs
        let rhsTs@(h':_) = map trd3 rhsPsEnvTs
        let guardEnv   = Map.unions $ map snd3 guardsPsEnvTs
        let rhsEnv      = Map.unions $ map snd3 rhsPsEnvTs
        unifyList (tBool:guardTs)                -- make sure these are all booleans
        unifyList rhsTs
        return (guardPs ++ rhsPs, guardEnv `Map.union` rhsEnv, h')


-- basically the same as tiGuardedRhs
tiGuardedAlt ::  TypeEnv  -> (HsGuardedAlt) -> TI (([Pred], TypeEnv, Type), ([Pred], TypeEnv, Type))
tiGuardedAlt env gAlt@(HsGuardedAlt sloc eGuard eRhs)
   = withContext (locMsg sloc "in the guarded alternative" $ render $ ppGAlt gAlt) $
     do
        (guardPs, guardEnv, guardT) <- tiExpr env eGuard
        (rhsPs, rhsEnv, rhsT)     <- tiExpr env eRhs
        return ((guardPs, guardEnv, guardT), (rhsPs, rhsEnv, rhsT))


-----------------------------------------------------------------------------

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

declDiagnostic ::  (HsDecl) -> Diagnostic
declDiagnostic decl@(HsPatBind sloc (HsPVar {}) _ _) = locMsg sloc "in the declaration" $ render $ ppHsDecl decl
declDiagnostic decl@(HsPatBind sloc pat _ _) = locMsg sloc "in the pattern binding" $ render $ ppHsDecl decl
declDiagnostic decl@(HsFunBind matches) = locMsg (srcLoc decl) "in the function binding" $ render $ ppHsDecl decl
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

-- create a Program structure from a list of decls and
-- type sigs. Type sigs are associated with corresponding
-- decls if they exist

getFunDeclsBg :: SigEnv -> [HsDecl] -> Program
getFunDeclsBg sigEnv decls
   = makeProgram sigEnv equationGroups
   where
   equationGroups :: [[HsDecl]]
   equationGroups = getBindGroups bindDecls (nameName . getDeclName) getDeclDeps
   --equationGroups = getBindGroups bindDecls (hsNameIdent_u (hsIdentString_u ("equationGroup" ++)) . getDeclName) getDeclDeps
   -- just make sure we only deal with bindDecls and not others
   bindDecls = collectBindDecls decls

makeProgram :: SigEnv -> [[HsDecl]] -> Program
makeProgram sigEnv groups
   = map (makeBindGroup sigEnv ) groups


-- reunite decls with their signatures, if ever they had one

makeBindGroup :: SigEnv -> [HsDecl] -> BindGroup
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
collectBindDecls = filter isBindDecl

-----------------------------------------------------------------------------


tiProgram ::  Opt -> Module -> SigEnv -> KindEnv -> ClassHierarchy -> TypeEnv -> TypeEnv -> Program -> IO TypeEnv
tiProgram opt modName sEnv kt h dconsEnv env bgs = runTI opt dconsEnv h kt sEnv modName $
  do (ps, env1) <- tiSeq tiBindGroup env bgs
     s         <- getSubst
     ps <- flattenType ps
     ([], rs) <- split h [] (apply s ps)
     opt <- getOptions
     case withOptionsT opt $ topDefaults h rs of
        Right s' -> do
            env1' <- flattenType env1
            return $  apply  s'  env1'
        Left s -> fail $ show modName ++ s
        --Left _ -> do
        --    env1' <- flattenType env1
        --    return  env1'


--------------------------------------------------------------------------------

-- Typing Literals

tiLit            :: HsLiteral -> TI ([Pred],Type)
tiLit (HsChar _) = return ([], tChar)
tiLit (HsInt _)
   = do
        v <- newTVar Star
        return ([IsIn class_Num v], v)

tiLit (HsFrac _)
   = do
        v <- newTVar Star
        return ([IsIn class_Fractional v], v)

tiLit (HsString _)  = return ([], tString)

--------------------------------------------------------------------------------

-- Typing Patterns

tiPat :: HsPat -> TI ([Pred], Map.Map Name Scheme, Type)

tiPat (HsPVar i) = do
        v <- newTVar Star
        --let newAssump = assumpToPair $ makeAssump i (toScheme v)
        --let newAssump = (i,toScheme v)
        return ([], Map.singleton (toName Val i) (toScheme v), v)

tiPat (HsPLit l)
   = do
        (ps, t) <- tiLit l
        return (ps, Map.empty, t)

-- this is for negative literals only
-- so the pat must be a literal
-- it is safe not to make any predicates about
-- the pat, since the type checking of the literal
-- will do this for us
tiPat (HsPNeg pat)
   = tiPat pat

tiPat (HsPInfixApp pLeft conName pRight)
   = do
        (psLeft, envLeft, tLeft)    <- tiPat pLeft
        (psRight, envRight, tRight) <- tiPat pRight
        t'                         <- newTVar Star
        sc <- dConScheme (toName DataConstructor conName)
        (qs :=> t) <-  freshInst sc
        unify t (tLeft `fn` (tRight `fn` t'))
        return (psLeft ++ psRight, envLeft `Map.union` envRight, t')

tiPat (HsPApp conName pats)
   = do
        (ps,env,ts) <- tiPats pats
        t'         <- newTVar Star
        sc <- dConScheme (toName DataConstructor conName)
        (qs :=> t) <- freshInst sc
        unify t (foldr fn t' ts)
        return (ps++qs, env, t')

tiPat tuple@(HsPTuple pats)
   = do
        (ps, env, ts) <- tiPats pats
        return (ps, env, tTTuple ts)

tiPat (HsPList [])
   = do
        v <- newTVar Star
        return ([], Map.empty, TAp tList v)

tiPat (HsPList pats@(_:_))
   = do
        (ps, env, ts@(hts:_)) <- tiPats pats
        unifyList ts
        return (ps, env, TAp tList hts)

tiPat HsPWildCard
 = do v <- newTVar Star
      return ([], Map.empty, v)

tiPat (HsPAsPat i pat)
 = do (ps, env, t) <- tiPat pat
      --let newAssump = makeAssump i $ toScheme t
      --let newEnv = addToEnv (assumpToPair newAssump) env
      let newEnv = Map.insert  (toName Val i) (toScheme t) env
      return (ps, newEnv, t)

tiPat (HsPIrrPat p)
 = tiPat p

tiPat (HsPParen p)
 = tiPat p

tiPats :: [HsPat] -> TI ([Pred], Map.Map Name Scheme, [Type])
tiPats pats =
  do psEnvts <- mapM tiPat pats
     let ps = [ p | (ps,_,_) <- psEnvts, p<-ps ]
         env = Map.unions $ map snd3 psEnvts
         ts = [ t | (_,_,t) <- psEnvts ]
     return (ps, env, ts)
