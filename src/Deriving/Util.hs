module Deriving.Util where

import Deriving.Type
import FrontEnd.Syn.Q
import FrontEnd.HsSyn
import Name.Names
import Util.Gen

mkInst :: SrcLoc -> Module -> Data -> ClassName -> [HsDecl] -> Q HsDecl
mkInst sloc mod d cn ds = mkInstN 0 sloc mod d cn ds
-- mkInst hsDeclSrcLoc m D { .. } cn ms = do
--     tvars <- mapM (const (newVar $ Just m)) vars
--     let hsClassHead = cn
--         hsClassHeadArgs = [foldl HsTyApp (HsTyCon name) (snds tvars)]
--         hsClassHeadContext = [HsAsst cn [t] | t <- fsts tvars ]
--     let hsDeclClassHead = HsClassHead { .. }
--         hsDeclDecls = ms
--     return HsInstDecl { .. }

mkInstN :: Int -> SrcLoc -> Module -> Data -> ClassName -> [HsDecl] -> Q HsDecl
mkInstN num hsDeclSrcLoc m D { .. } cn ms = do
    tvars <- mapM (const (newVar $ Just m)) (drop num vars)
    let hsClassHead = cn
        hsClassHeadArgs = [foldl HsTyApp (HsTyCon name) (snds tvars)]
        hsClassHeadContext = [HsAsst cn [t] | t <- fsts tvars ]
    let hsDeclClassHead = HsClassHead { .. }
        hsDeclDecls = ms
    return HsInstDecl { hsDeclIsDerived = True, .. }

mkPat :: Module -> Body -> Q (HsPat,[HsExp])
mkPat m Body { .. } = do
    pvars <- mapM (const (newVar $ Just m)) types
    return (HsPApp constructor (snds pvars), map HsVar $ fsts pvars)

app2 e a b = HsApp (HsApp e a) b

funBind hsMatchSrcLoc hsMatchName hsMatchPats e = HsFunBind [HsMatch { .. }] where
    hsMatchRhs = HsUnGuardedRhs e
    hsMatchDecls = []
