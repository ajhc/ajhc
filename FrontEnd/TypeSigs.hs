{-------------------------------------------------------------------------------

        Copyright:              The Hatchet Team (see file Contributors)

        Module:                 TypeSigs

        Description:            Collects all the type signatures from a module

        Primary Authors:        Bernie Pope

        Notes:                  See the file License for license information

-------------------------------------------------------------------------------}

module TypeSigs (collectSigs,
                 collectSigEnv,
                 SigEnv,
                 listSigsToSigEnv) where


import Type(assumpToPair)
import FrontEnd.KindInfer(KindEnv)
import Representation(Scheme)
import TypeUtils(aHsTypeSigToAssumps)
import Name.Name

import HsSyn
import qualified Data.Map as Map

--------------------------------------------------------------------------------

collectSigEnv :: KindEnv -> HsStmt -> SigEnv
collectSigEnv kindInfo stmt = sigEnv where
    allTypeSigs = collectSigsFromStmt stmt
    sigEnv = listSigsToSigEnv kindInfo allTypeSigs

collectSigs :: [(HsDecl)] -> [(HsDecl)]
collectSigs ds = collectSigsFromDecls ds

collectSigsFromDecls :: [(HsDecl)] -> [(HsDecl)]

collectSigsFromDecls [] = []

collectSigsFromDecls (d@(HsTypeSig {}):ds) = d : collectSigsFromDecls ds

collectSigsFromDecls ((HsForeignDecl sl _ _ _ n qt):ds) = HsTypeSig sl [n] qt:collectSigsFromDecls ds

collectSigsFromDecls ((HsPatBind _ _ rhs wheres):ds)
   = collectSigsFromRhs rhs ++
     collectSigsFromDecls wheres ++
     collectSigsFromDecls ds

collectSigsFromDecls ((HsFunBind matches):ds)
   = concatMap collectSigsFromMatch matches ++
     collectSigsFromDecls ds

collectSigsFromDecls (_:ds)
   = collectSigsFromDecls ds

collectSigsFromMatch :: (HsMatch) -> [(HsDecl)]

collectSigsFromMatch (HsMatch _ _ _ rhs wheres)
   = collectSigsFromRhs rhs ++
     collectSigsFromDecls wheres

collectSigsFromRhs :: (HsRhs) -> [(HsDecl)]

collectSigsFromRhs (HsUnGuardedRhs e)
   = collectSigsFromExp e

collectSigsFromRhs (HsGuardedRhss rhss)
   = concatMap collectSigsFromGuardedRhs rhss

collectSigsFromGuardedRhs :: (HsGuardedRhs) -> [(HsDecl)]

collectSigsFromGuardedRhs (HsGuardedRhs _ e1 e2)
   = collectSigsFromExp e1 ++
     collectSigsFromExp e2

collectSigsFromExp :: (HsExp) -> [(HsDecl)]


collectSigsFromExp (HsVar {}) = []

collectSigsFromExp (HsCon {}) = []

collectSigsFromExp (HsLit {}) = []

collectSigsFromExp (HsInfixApp e1 e2 e3)
   = collectSigsFromExp e1 ++
     collectSigsFromExp e2 ++
     collectSigsFromExp e3

collectSigsFromExp (HsApp e1 e2)
   = collectSigsFromExp e1 ++
     collectSigsFromExp e2

collectSigsFromExp (HsNegApp e)
   = collectSigsFromExp e

collectSigsFromExp (HsLambda _sloc _ e)
   = collectSigsFromExp e

collectSigsFromExp (HsLet decls e)
   = collectSigsFromDecls decls ++
     collectSigsFromExp e

collectSigsFromExp (HsIf e1 e2 e3)
   = collectSigsFromExp e1 ++
     collectSigsFromExp e2 ++
     collectSigsFromExp e3

collectSigsFromExp (HsCase e alts)
   = collectSigsFromExp e ++
     concatMap collectSigsFromAlt alts

collectSigsFromExp (HsDo stmts)
   = concatMap collectSigsFromStmt stmts

collectSigsFromExp (HsTuple exps)
   = concatMap collectSigsFromExp exps

collectSigsFromExp (HsList exps)
   = concatMap collectSigsFromExp exps

collectSigsFromExp (HsParen e)
   = collectSigsFromExp e

collectSigsFromExp (HsLeftSection e1 e2)
   = collectSigsFromExp e1 ++
     collectSigsFromExp e2

collectSigsFromExp (HsRightSection e1 e2)
   = collectSigsFromExp e1 ++
     collectSigsFromExp e2

collectSigsFromExp (HsRecConstr _ fs) = concat [ collectSigsFromExp e | HsFieldUpdate _ e <- fs ]
--   = error "collectSigsFromExp (HsRecConstr _ _) not implemented yet"

collectSigsFromExp (HsRecUpdate e fs) =  concat $ collectSigsFromExp e:[ collectSigsFromExp e | HsFieldUpdate _ e <- fs ]
--   = error "collectSigsFromExp (HsRecUpdate _ _) not implemented yet"

collectSigsFromExp (HsEnumFrom e)
   = collectSigsFromExp e

collectSigsFromExp (HsEnumFromTo e1 e2)
   = collectSigsFromExp e1 ++
     collectSigsFromExp e2

collectSigsFromExp (HsEnumFromThen e1 e2)
   = collectSigsFromExp e1 ++
     collectSigsFromExp e2

collectSigsFromExp (HsEnumFromThenTo e1 e2 e3)
   = collectSigsFromExp e1 ++
     collectSigsFromExp e2 ++
     collectSigsFromExp e3

collectSigsFromExp (HsListComp e stmts)
   = collectSigsFromExp e ++
     concatMap collectSigsFromStmt stmts

collectSigsFromExp (HsExpTypeSig _ e _)
   = collectSigsFromExp e

collectSigsFromExp (HsAsPat _ e)
   = collectSigsFromExp e

collectSigsFromExp (HsWildCard _) = []

collectSigsFromExp (HsIrrPat e)
   = collectSigsFromExp e

collectSigsFromAlt :: (HsAlt) -> [(HsDecl)]

collectSigsFromAlt (HsAlt _ _ (HsUnGuardedRhs e) decls)
   = collectSigsFromExp e ++
     collectSigsFromDecls decls

collectSigsFromAlt (HsAlt _ _ (HsGuardedRhss alts) decls)
   = concatMap collectSigsFromGuardedAlt alts ++
     collectSigsFromDecls decls

collectSigsFromGuardedAlt :: (HsGuardedRhs) -> [(HsDecl)]

collectSigsFromGuardedAlt (HsGuardedRhs _ e1 e2)
   = collectSigsFromExp e1 ++
     collectSigsFromExp e2

collectSigsFromStmt :: (HsStmt) -> [(HsDecl)]

collectSigsFromStmt (HsGenerator _ _ e)
   = collectSigsFromExp e

collectSigsFromStmt (HsQualifier e)
   = collectSigsFromExp e

collectSigsFromStmt (HsLetStmt decls)
   = collectSigsFromDecls decls

--------------------------------------------------------------------------------

type SigEnv = Map.Map Name Scheme

listSigsToSigEnv :: KindEnv -> [HsDecl] -> SigEnv
listSigsToSigEnv kt sigs
   = Map.fromList $
        map assumpToPair $
        concatMap (aHsTypeSigToAssumps kt) sigs


