module Deriving.Util where

import Deriving.Type
import FrontEnd.Syn.Q
import FrontEnd.HsSyn
import Name.Names
import Util.Gen

mkInst :: Derive -> Module -> Data -> ClassName -> [HsDecl] -> Q HsDecl
mkInst sloc mod d cn ds = mkInstN 0 sloc mod d cn ds
-- mkInst hsDeclSrcLoc m D { .. } cn ms = do
--     tvars <- mapM (const (newVar $ Just m)) vars
--     let hsClassHead = cn
--         hsClassHeadArgs = [foldl HsTyApp (HsTyCon name) (snds tvars)]
--         hsClassHeadContext = [HsAsst cn [t] | t <- fsts tvars ]
--     let hsDeclClassHead = HsClassHead { .. }
--         hsDeclDecls = ms
--     return HsInstDecl { .. }

mkInstN :: Int -> Derive -> Module -> Data -> ClassName -> [HsDecl] -> Q HsDecl
-- for standalone deriving, we trust the context given and use it exactly.
-- errors will be caught by the type checker.
mkInstN num Derive { standAlone = True, .. } m D { .. } cn ms = do
    let hsDeclClassHead = deriveHead
        hsDeclDecls = ms
        hsDeclSrcLoc = deriveSrcLoc
    return HsInstDecl { hsDeclIsDerived = True, .. }

-- Attempt to derive appropriate context. this is not entirely correct at the
-- moment but works in the majority of cases, use standalone deriving to work
-- around issues.

mkInstN num Derive { deriveHead = HsClassHead { hsClassHead },.. } m D { .. } _cn ms = do
    tvars <- mapM (const (newVarN "t" (Just m))) (drop num vars)
    let hsClassHeadArgs = [foldl HsTyApp (HsTyCon name) (snds tvars)]
        hsClassHeadContext = [HsAsst hsClassHead [t] | t <- fsts tvars ]
        hsDeclSrcLoc = deriveSrcLoc
    let hsDeclClassHead = HsClassHead { .. }
        hsDeclDecls = ms
    return HsInstDecl { hsDeclIsDerived = True, .. }

mkPat :: Module -> Body -> Q (HsPat,[HsExp])
mkPat m Body { .. } = do
    pvars <- mapM (const (newVarN "p" $ Nothing)) types
    return (HsPApp constructor (snds pvars), map HsVar $ fsts pvars)

app2 e a b = HsApp (HsApp e a) b

funBind hsMatchSrcLoc hsMatchName hsMatchPats e = HsFunBind [HsMatch { .. }] where
    hsMatchRhs = HsUnGuardedRhs e
    hsMatchDecls = []
