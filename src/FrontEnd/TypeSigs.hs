module FrontEnd.TypeSigs (
    collectSigs,
    collectSigEnv,
    SigEnv,
    listSigsToSigEnv) where

import Control.Monad.Writer
import Util.Std
import qualified Data.Map as Map
import qualified Util.Seq as Seq

import FrontEnd.HsSyn
import FrontEnd.KindInfer
import FrontEnd.SrcLoc
import FrontEnd.Syn.Traverse
import FrontEnd.Tc.Type
import Name.Name

newtype SC a = SC (Writer (Seq.Seq HsDecl) a)
    deriving(Monad,Applicative,Functor)

fromSC :: SC a -> [HsDecl]
fromSC (SC m) = Seq.toList $ execWriter m

addSig :: HsDecl -> SC ()
addSig ds = SC $ tell $ Seq.singleton ds

instance MonadSrcLoc SC where
instance MonadSetSrcLoc SC where
    withSrcLoc' _ a = a

csigsOps :: HsOps SC
csigsOps = (hsOpsDefault csigsOps) { opHsDecl } where
    opHsDecl d@HsTypeSig {} = d <$ addSig d
    opHsDecl d@HsForeignDecl { .. } = d <$ addSig HsTypeSig { hsDeclNames = [hsDeclName], .. }
    opHsDecl d@HsForeignExport { .. } = d <$ addSig HsTypeSig { hsDeclNames = [hsDeclName], .. }
    opHsDecl d = traverseHsOps csigsOps d

collectSigEnv :: KindEnv -> HsStmt -> SigEnv
collectSigEnv kindInfo stmt = sigEnv where
    allTypeSigs = fromSC $ applyHsOps csigsOps stmt -- collectSigsFromStmt stmt
    sigEnv = listSigsToSigEnv kindInfo allTypeSigs

collectSigs :: [(HsDecl)] -> [(HsDecl)]
collectSigs ds = fromSC $ applyHsOps csigsOps ds -- collectSigsFromDecls ds

type SigEnv = Map.Map Name Type

listSigsToSigEnv :: KindEnv -> [HsDecl] -> SigEnv
listSigsToSigEnv kt sigs
   = Map.fromList $ concatMap (aHsTypeSigToAssumps kt) sigs

aHsTypeSigToAssumps :: KindEnv -> HsDecl -> [(Name,Type)]
aHsTypeSigToAssumps kt sig@(~(HsTypeSig _ names qualType)) = [ (toName Val n,typ) | n <- names] where
    Identity typ = hsQualTypeToSigma kt qualType
