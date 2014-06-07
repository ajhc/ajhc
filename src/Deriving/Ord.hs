module Deriving.Ord(deriveEq,deriveOrd,deriveBounded) where

import Deriving.Type
import Deriving.Util
import FrontEnd.HsSyn
import FrontEnd.Syn.Q
import Name.Names
import Util.Std

deriveEq :: Derive -> Module -> Data -> Q HsDecl
deriveEq der@Derive { deriveSrcLoc = hsMatchSrcLoc } mod d@D{ .. } = do
    t <- lift True
    f <- lift False
    let mkMatch b@Body { .. } = do
            pa <- mkPat mod b
            pb <- mkPat mod b
            let hsMatchRhs
                    | null types = HsUnGuardedRhs t
                    | otherwise = HsUnGuardedRhs $
                        foldr1 (app2 (HsVar v_and)) (zipWith (\x -> HsParen . app2 (HsVar v_equals) x) (snd pa) (snd pb))
            return HsMatch { hsMatchPats = [fst pa,fst pb], .. }
        hsMatchName = v_equals
        hsMatchDecls = []
        dfalse = HsMatch { hsMatchPats = [HsPWildCard, HsPWildCard], hsMatchRhs = HsUnGuardedRhs f, .. }
    bs <- mapM mkMatch body
    let eqfn = HsFunBind $  bs ++ if length body <= 1 then [] else [dfalse]
    mkInst der mod d class_Eq [eqfn]

deriveOrd :: Derive -> Module -> Data -> Q HsDecl
deriveOrd der@Derive { deriveSrcLoc = hsMatchSrcLoc } mod d@D{ .. } = do
    let mkMatch (nx,b@Body { .. }) (ny,b2) | nx == ny = do
            pa <- mkPat mod b
            pb <- mkPat mod b
            t <- lift True
            let hsMatchRhs
                    | null types = HsUnGuardedRhs eq
                    | otherwise = HsUnGuardedRhs $
                        foldr1 compCase (zipWith (\x -> HsParen . app2 (HsVar v_compare) x) (snd pa) (snd pb))
                compCase e c = HsCase e [hsAlt (pcon dc_LT) (HsCon dc_LT),hsAlt (pcon dc_GT) (HsCon dc_GT), hsAlt (pcon dc_EQ) c]

                eq = HsCon dc_EQ
                pcon v = HsPApp v []
                hsAlt p e = HsAlt hsMatchSrcLoc p (HsUnGuardedRhs e) []
            return HsMatch { hsMatchPats = [fst pa,fst pb], .. }
        mkMatch (nx,bx) (ny,by) = do
                (pa,_) <- mkPat mod bx
                (pb,_) <- mkPat mod by
                return $ HsMatch { hsMatchPats = [pa,pb],.. } where
                hsMatchRhs = HsUnGuardedRhs $ if nx < ny then HsCon dc_LT else HsCon dc_GT
        hsMatchName = v_compare
        hsMatchDecls = []
    eqfn <- HsFunBind <$> sequence [ mkMatch x y  | x <- zip [ 0 .. ] body, y <- zip [ 0 :: Int .. ] body ]
    mkInst der mod d class_Ord [eqfn]

deriveBounded :: Derive -> Module -> Data -> Q HsDecl
deriveBounded der@Derive{deriveSrcLoc = hsDeclSrcLoc} mod d@D{ .. } = do
    if null body then fail "cannot create bounded for nullary type" else do
    let lowb = head body
        highb = last body
    let mm Body { .. } n = HsPatBind { .. } where
            hsDeclPat = HsPVar n
            hsDeclDecls = []
            hsDeclRhs = HsUnGuardedRhs $
                foldl HsApp (HsCon constructor) (map (const $ HsVar n) types)
    mkInst der mod d class_Bounded [mm highb v_maxBound,mm lowb v_minBound]
