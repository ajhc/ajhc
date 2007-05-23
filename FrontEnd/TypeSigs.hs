{-------------------------------------------------------------------------------

        Copyright:              The Hatchet Team (see file Contributors)
        Module:                 TypeSigs
        Description:            Collects all the type signatures from a module
        Primary Authors:        Bernie Pope, John Meacham
        Notes:                  See the file License for license information

-------------------------------------------------------------------------------}

module TypeSigs (collectSigs,
                 collectSigEnv,
                 SigEnv,
                 listSigsToSigEnv) where

import Control.Monad.Identity
import Control.Monad.Writer
import qualified Data.Map as Map

import FrontEnd.KindInfer
import FrontEnd.Syn.Traverse
import FrontEnd.Tc.Type
import FrontEnd.SrcLoc
import HsSyn
import Name.Name

newtype SC a = SC (Writer [HsDecl] a)
    deriving(Monad)

fromSC :: SC () -> [HsDecl]
fromSC (SC m) = execWriter m

addSigs :: [HsDecl] -> SC ()
addSigs ds = SC $ tell ds

instance MonadSrcLoc SC where
instance MonadSetSrcLoc SC where
    withSrcLoc _ a = a


collectSigEnv :: KindEnv -> HsStmt -> SigEnv
collectSigEnv kindInfo stmt = sigEnv where
    allTypeSigs = collectSigsFromStmt stmt
    sigEnv = listSigsToSigEnv kindInfo allTypeSigs

collectSigs :: [(HsDecl)] -> [(HsDecl)]
collectSigs ds = collectSigsFromDecls ds

collectSigsFromDecls :: [(HsDecl)] -> [(HsDecl)]
collectSigsFromDecls [] = []
collectSigsFromDecls (d@(HsTypeSig {}):ds) = d : collectSigsFromDecls ds
collectSigsFromDecls ((HsForeignDecl   sl _ n qt):ds) = HsTypeSig sl [n] qt:collectSigsFromDecls ds
collectSigsFromDecls ((HsForeignExport sl _ n qt):ds) = HsTypeSig sl [n] qt:collectSigsFromDecls ds
collectSigsFromDecls ((HsPatBind _ _ rhs wheres):ds)
   = collectSigsFromRhs rhs ++
     collectSigsFromDecls wheres ++
     collectSigsFromDecls ds
collectSigsFromDecls ((HsFunBind matches):ds)
   = concatMap collectSigsFromMatch matches ++
     collectSigsFromDecls ds
collectSigsFromDecls (_:ds) = collectSigsFromDecls ds

collectSigsFromMatch :: (HsMatch) -> [(HsDecl)]
collectSigsFromMatch (HsMatch _ _ _ rhs wheres)
   = collectSigsFromRhs rhs ++
     collectSigsFromDecls wheres

collectSigsFromRhs :: (HsRhs) -> [(HsDecl)]
collectSigsFromRhs (HsUnGuardedRhs e) = collectSigsFromExp e
collectSigsFromRhs (HsGuardedRhss rhss) = concatMap collectSigsFromGuardedRhs rhss

collectSigsFromGuardedRhs :: (HsGuardedRhs) -> [(HsDecl)]
collectSigsFromGuardedRhs (HsGuardedRhs _ e1 e2)
   = collectSigsFromExp e1 ++
     collectSigsFromExp e2

collectSigsFromExp :: HsExp -> [HsDecl]
collectSigsFromExp e = fromSC (collectExp e)

collectExp :: HsExp -> SC ()
collectExp (HsLet decls e) = do
    addSigs (collectSigsFromDecls decls)
    collectExp e
collectExp (HsCase e alts) = do
    collectExp e
    addSigs $ concatMap collectSigsFromAlt alts
collectExp (HsDo stmts) = addSigs $ concatMap collectSigsFromStmt stmts
collectExp (HsListComp e stmts) = do
    collectExp e
    addSigs $ concatMap collectSigsFromStmt stmts
collectExp e =  traverseHsExp_ collectExp e

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
collectSigsFromStmt (HsGenerator _ _ e) = collectSigsFromExp e
collectSigsFromStmt (HsQualifier e) = collectSigsFromExp e
collectSigsFromStmt (HsLetStmt decls) = collectSigsFromDecls decls

--------------------------------------------------------------------------------

type SigEnv = Map.Map Name Type

listSigsToSigEnv :: KindEnv -> [HsDecl] -> SigEnv
listSigsToSigEnv kt sigs
   = Map.fromList $ concatMap (aHsTypeSigToAssumps kt) sigs

aHsTypeSigToAssumps :: KindEnv -> HsDecl -> [(Name,Type)]
aHsTypeSigToAssumps kt sig@(~(HsTypeSig _ names qualType)) = [ (toName Val n,typ) | n <- names] where
    Identity typ = hsQualTypeToSigma kt qualType

