module Deriving.Traverse(deriveFunctor,deriveFoldable,deriveTraversable) where

import Data.Either
import Deriving.Type
import Deriving.Util
import FrontEnd.HsSyn
import FrontEnd.Syn.Q
import FrontEnd.Syn.Traverse()
import FrontEnd.Warning
import Name.Names
import Support.FreeVars
import Util.Std
import qualified Data.Set as Set

fsts = map fst
snds = map snd

isRight Right {} = True
isRight _ = False

deriveFunctor :: Derive -> Module -> Data -> Q HsDecl
deriveFunctor der@Derive {..} mod d@D { vars = [], .. } = do
    warn deriveSrcLoc InvalidDecl "Attempt to derive Functor for type without parameters"
    mkInstN 1 der mod d undefined []

deriveFunctor der@Derive {..} mod d@D { body = [], .. } = do
    (HsVar -> fe,fp) <- newVarN "x" Nothing
    let hsMatchSrcLoc = deriveSrcLoc
        hsMatchPats = [HsPWildCard,fp]
        hsMatchRhs = HsUnGuardedRhs fe
        f hsMatchName = HsMatch { .. }
        hsMatchDecls = []
    mkInstN 1 der mod d class_Functor [HsFunBind [f v_fmap],HsFunBind [f v_fmap_const]]

deriveFunctor der@Derive {..} mod d@D { vars = reverse -> (fv:_), .. } = do
    let mkMatch func b@Body { .. } = do
            (HsVar -> fe,fp) <- newVarN "f" Nothing
            (pa,es) <- mkPat mod b
            let dt (e,HsTyVar t) | t == fv, func == v_fmap = return $ HsApp fe e
                dt (e,HsTyVar t) | t == fv = return fe
                dt (e,HsTyApp x (HsTyVar t)) | t == fv, fv `Set.notMember` freeVars x = return $ app2 (HsVar func) fe e
                dt (e,t) | fv `Set.notMember` freeVars t = return e
                dt (e,t) = do
                    warn deriveSrcLoc InvalidDecl "Cannot Derive Functor class for type"
                    return e
            as <- mapM dt (zip es $ map hsBangType types)
            let hsMatchRhs
                    | null types = HsUnGuardedRhs (HsCon constructor)
                    | otherwise = HsUnGuardedRhs $ foldl1 HsApp (HsCon constructor:as)
                hsMatchName = func
                hsMatchDecls = []
            return HsMatch { hsMatchPats = [fp,pa], .. }
        hsMatchSrcLoc = deriveSrcLoc
    bs <- mapM (mkMatch v_fmap) body
    bsc <- mapM (mkMatch v_fmap_const) body
    mkInstN 1 der mod d class_Functor [HsFunBind bs,HsFunBind bsc]

deriveFoldable :: Derive -> Module -> Data -> Q HsDecl
deriveFoldable der@Derive {..} mod d@D { vars = [], .. } = do
    warn deriveSrcLoc InvalidDecl "Attempt to derive Foldable for type without parameters"
    mkInstN 1 der mod d undefined []
deriveFoldable der@Derive {..} mod d@D { body = [], .. } = do
 --   (HsVar -> fe,fp) <- newVarN "x" Nothing
    let hsMatchSrcLoc = deriveSrcLoc
        hsMatchPats = [HsPWildCard,HsPWildCard]
        hsMatchRhs = HsUnGuardedRhs $HsVar v_mempty
        hsMatchName = v_foldMap
        fun = HsMatch { .. }
        hsMatchDecls = []
    mkInstN 1 der mod d class_Functor [HsFunBind [fun]]
deriveFoldable der@Derive {..} mod d@D { vars = reverse -> (fv:_), .. } = do
    let mkMatch func b@Body { .. } = do
            (HsVar -> fe,fp) <- newVarN "f" Nothing
            (pa,es) <- mkPat mod b
            let dt (e,HsTyVar t) | t == fv, func == v_foldMap = return [HsApp fe e]
                dt (e,HsTyVar t) | t == fv = return [e]
                dt (e,HsTyApp x (HsTyVar t)) | t == fv, fv `Set.notMember` freeVars x =
                    if func == v_foldMap
                        then return [ app2 (HsVar func) fe e]
                        else return [ HsApp (HsVar func) e]
                dt (e,t) | fv `Set.notMember` freeVars t = return []
                dt (e,t) = do
                    warn deriveSrcLoc InvalidDecl "Cannot Derive Foldable class for type"
                    return [e]
            as <- concat <$> mapM dt (zip es $ map hsBangType types)
            let hsMatchRhs
                    | null as = HsUnGuardedRhs (HsVar v_mempty)
                    | otherwise = HsUnGuardedRhs $ foldr1 (app2 (HsVar v_mappend)) as
                hsMatchName = func
                hsMatchDecls = []
            return HsMatch { hsMatchPats = if func == v_foldMap then [fp,pa] else [pa], .. }
        hsMatchSrcLoc = deriveSrcLoc
    bs <- mapM (mkMatch v_foldMap) body
    bsc <- mapM (mkMatch v_fold) body
    mkInstN 1 der mod d class_Functor [HsFunBind bs,HsFunBind bsc]

deriveTraversable :: Derive -> Module -> Data -> Q HsDecl
deriveTraversable der@Derive {..} mod d@D { vars = [], .. } = do
    warn deriveSrcLoc InvalidDecl "Attempt to derive Traversable for type without parameters"
    mkInstN 1 der mod d undefined []
deriveTraversable der@Derive {..} mod d@D { body = [], .. } = do
--    (HsVar -> fe,fp) <- newVarN "x" Nothing
    let hsMatchSrcLoc = deriveSrcLoc
        hsMatchPats = [HsPWildCard,HsPWildCard]
        hsMatchRhs = HsUnGuardedRhs $HsVar v_mempty
        hsMatchName = v_foldMap
        fun = HsMatch { .. }
        hsMatchDecls = []
    mkInstN 1 der mod d class_Functor [HsFunBind [fun]]
deriveTraversable der@Derive {..} mod d@D { vars = reverse -> (fv:_), .. } = do
    let mkMatch func b@Body { .. } = do
            (HsVar -> fe,fp) <- newVarN "f" Nothing
            (pa,es) <- mkPat mod b
            let dt (e,HsTyVar t) | t == fv, func == v_traverse = return $ Right (HsApp fe e)
                dt (e,HsTyVar t) | t == fv = return $ Right e
                dt (e,HsTyApp x (HsTyVar t)) | t == fv, fv `Set.notMember` freeVars x = return . Right $
                    if func == v_traverse
                        then (app2 (HsVar func) fe e)
                        else (HsApp (HsVar func) e)
                dt (e,t) | fv `Set.notMember` freeVars t = return $ Left e
                dt (e,t) = do
                    warn deriveSrcLoc InvalidDecl "Cannot Derive Traversable class for type"
                    return $ Left e
            as <- mapM dt (zip es $ map hsBangType types)
            let pullLeft as = f as [] where
                    f (Left x:xs) rs = f xs (x:rs)
                    f xs rs = (reverse rs,xs)
                (rs',xs') = pullLeft as
                bval = foldl1 HsApp (HsCon constructor:rs')
                lstar = HsVar v_lstar
                hsMatchPats = if func == v_traverse then [fp,pa] else [pa]
                hsMatchName = func
                appxs bv  = foldl1 (app2 lstar) (app2 (HsVar v_fmap) bv x:[ x | Right x <- xs]) where
                    (Right x:xs) = xs'
            let scont rhs = return ans where
                    ans = HsMatch {  .. }
                    hsMatchRhs = HsUnGuardedRhs rhs
                    hsMatchDecls = []
            case () of
             () | null xs'  -> scont $ HsApp (HsVar v_pure) bval
                | all isRight xs' -> scont $ appxs bval
                | otherwise -> do
                    (gname,ge) <- newVarN "g" Nothing
                    vs <- replicateM (length xs') (newVarN "v" Nothing)
                    let hsMatchRhs = HsUnGuardedRhs $ appxs ge
                        hsMatchDecls = [HsFunBind [HsMatch { .. }]] where
                            hsMatchName = gname
                            hsMatchDecls = []
                            hsMatchPats = [ p | ((_,p),Right _) <- zip vs xs' ]
                            hsMatchRhs = HsUnGuardedRhs $ foldl1 HsApp (bval:zipWith f vs xs') where
                                f (_,_) (Left e) = e
                                f (n,_) Right {} = HsVar n
                    return HsMatch { .. }
        hsMatchSrcLoc = deriveSrcLoc
    bs <- mapM (mkMatch v_traverse) body
    bsc <- mapM (mkMatch v_sequenceA) body
    mkInstN 1 der mod d class_Functor [HsFunBind bs,HsFunBind bsc]
deriveTraversable der@Derive {..} mod d@D {  .. } = do
    mkInstN 1 der mod d undefined []
