module FrontEnd.TypeSyns(expandTypeSyns) where

import Control.Monad.State
import Control.Monad.Writer

import FrontEnd.HsSyn
import FrontEnd.SrcLoc hiding(srcLoc)
import FrontEnd.Syn.Traverse
import FrontEnd.TypeSynonyms
import FrontEnd.Warning

data ScopeState = ScopeState {
    errors   :: [Warning],
    srcLoc   :: !SrcLoc
    }

type ScopeSM = State ScopeState

instance MonadWarn ScopeSM where
    addWarning w = modify (\s -> s { errors = w: errors s})
instance MonadSrcLoc ScopeSM where
    getSrcLoc = gets srcLoc
instance MonadSetSrcLoc ScopeSM where
    withSrcLoc' sl a = modify (\s -> s { srcLoc = sl `mappend` srcLoc s}) >> a

expandTypeSyns :: (TraverseHsOps a,MonadWarn m) => TypeSynonyms -> a -> m a
expandTypeSyns syns m = mapM_ addWarning (errors fs) >> return rm where
    startState = ScopeState { .. } where
        errors   = []
        srcLoc   = bogusASrcLoc
    (rm, fs) = runState (applyHsOps ops m) startState
    ops = (hsOpsDefault ops) { opHsDecl, opHsType = removeSynonymsFromType syns } where
        opHsDecl td@HsTypeDecl {} = return td
        opHsDecl d = traverseHsOps ops d
