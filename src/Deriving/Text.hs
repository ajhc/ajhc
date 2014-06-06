module Deriving.Text(deriveRead,deriveShow) where

import Deriving.Type
import Deriving.Util
import FrontEnd.HsSyn
import FrontEnd.Syn.Q
import FrontEnd.Warning
import Name.Names
import Util.Std

fsts = map fst
snds = map snd

deriveRead :: SrcLoc -> Module -> Data -> Q HsDecl
deriveRead sloc mod d@D { body = [] } = do
    mkInst sloc mod d class_Read [funBind sloc v_readsPrec [HsPWildCard, HsPWildCard] (HsList [])]
deriveRead sloc mod d@D { .. } = do
    let newv n = newVarN n Nothing
        gen a b = HsGenerator sloc (HsPTuple [a,b])
        hsCompSrcLoc = sloc
        con n = HsString $ getIdent n
        fst' (x,y :: HsExp) = x
    (HsVar -> de,dp) <- newv "prec"
    (HsVar -> inpute,inputp) <- newv "input"
    let f Body { types = [], .. } = do
            (HsVar -> reste,restp) <- newv "rest"
            let hsCompStmts = [gen (HsPLit $ con constructor) restp (HsApp (HsVar v_lex) inpute)]
                hsCompBody = pair (HsCon constructor) reste
            return $ HsListComp HsComp { .. }
            listCompToExp (fst' <$>  newv "lc") hsCompBody hsCompStmts
        f Body { .. } = do
            (ine,inp) <- newVarN "li" Nothing
            (fre,frp) <- newVarN "fr" Nothing
            vs <- replicateM (length types) (newVarN "v" Nothing)
            rests@((ie,ip):_) <- replicateM (length types) (newVarN "r" Nothing)
            let pats = snds rests
                nams = fsts rests
            let hsCompBody = pair (foldl1 HsApp (HsCon constructor:[ HsVar e | (e,_ :: HsPat) <- vs])) (HsVar fre)
                hsCompStmts = gen (HsPLit $ con constructor) (head pats) (HsApp (HsVar v_lex) $ HsVar ine):[ gen pv pr (greadsPrec ie)  | ((_,pv),pr,HsVar -> ie) <- zip3 vs (tail pats ++ [frp]) (nams ++ [fre])]
            e <- listCompToExp (fst' <$>  newv "lc") hsCompBody hsCompStmts
            return $ greadParen de (HsLambda sloc [inp] (HsListComp HsComp { .. })) inpute
            return $ greadParen de (HsLambda sloc [inp] e) inpute
    bodies <- mapM f body
    let readsPrec = funBind sloc v_readsPrec [dp, inputp] $ foldr1 gpp bodies
    mkInst sloc mod d class_Read [readsPrec]

pair a b = HsTuple [a,b]

app3 f a b c = HsApp (HsApp (HsApp f a) b) c
greadParen n e i = app3 (HsVar v_readParen) (hsParen $ app2 (HsVar v_gt) n (HsLit $ HsInt 9)) e i
greadsPrec n = HsApp (HsApp (HsVar v_readsPrec) (HsLit $ HsInt 10)) n
gpp a b =  app2 (HsVar v_cat) a b

deriveShow :: SrcLoc -> Module -> Data -> Q HsDecl
deriveShow sloc mod d@D{ body = [] } = do
    warn sloc InvalidDecl "Cannot derive Show for nullary datatype"
    mkInst sloc mod d class_Show []
deriveShow hsMatchSrcLoc mod d@D{ .. } = do
    let mkMatch b@Body { .. } = do
            (pa,es) <- mkPat mod b
            (HsVar -> ne,np) <- newVar (Just mod)
            let gid = getIdent constructor
            let (HsUnGuardedRhs -> hsMatchRhs)
                    | (f:fs) <- labels, (e:es) <- es = foldr1 gpar (gshowString (gid ++ " { " ++ getIdent f ++ " = ") `gdot` gshowsPrec e:(zipWith df fs es)) `gdot` gshowString " }"
                    | isOpLike constructor, [e1,e2] <- es = gshowParen ne $ gshowsPrec e1 `gcomp` (gshowString gid) `gcomp` gshowsPrec e2
                    | isOpLike constructor = gshowParen ne $ foldr1  gcomp (gshowString ("(" ++ gid ++ ")"):(map gshowsPrec es))
                    | null types = gshowString gid
                    | otherwise = gshowParen ne $ foldr1  gcomp (gshowString gid:(map gshowsPrec es))
                df lab e = gshowString (getIdent lab ++ " = ") `gdot` gshows e
            return HsMatch { hsMatchPats = [np,pa], .. }
        hsMatchName = v_showsPrec
        hsMatchDecls = []
    bs <- mapM mkMatch body
    mkInst hsMatchSrcLoc mod d class_Show [HsFunBind bs]

gsc c = HsLeftSection (HsCon dc_Cons)  (HsLit $ HsChar c)
gcomp a b =  app2 (HsVar v_Dot) a (app2 (HsVar v_Dot) (gsc ' ') b)
gpar a b =  app2 (HsVar v_Dot) a (app2 (HsVar v_Dot) (gshowString ", ") b)
gdot a b =  app2 (HsVar v_Dot) a b
gshowString t = HsApp (HsVar v_showString) (HsLit $ HsString t)
gshowParen n e = app2 (HsVar v_showParen) (hsParen $ app2 (HsVar v_geq) n (HsLit $ HsInt 10)) e
gshowsPrec n = HsApp (HsApp (HsVar v_showsPrec) (HsLit $ HsInt 10)) n
gshows n = HsApp (HsApp (HsVar v_showsPrec) (HsLit $ HsInt 0)) n

hsApp e es = foldl HsApp e es
listCompToExp :: Monad m => m HsName -> HsExp -> [HsStmt] -> m HsExp
listCompToExp newName exp ss = hsParen `liftM` f ss where
    f [] = return $ HsList [exp]
    f (gen:HsQualifier q1:HsQualifier q2:ss)  = f (gen:HsQualifier (hsApp (HsVar v_and) [q1,q2]):ss)
    f ((HsLetStmt ds):ss) = do ss' <- f ss; return $ hsParen (HsLet ds ss')
    f (HsQualifier e:ss) = do ss' <- f ss; return $ hsParen (HsIf e ss' (HsList []))
    f ((HsGenerator srcLoc pat e):ss) | isLazyPat pat, Just exp' <- g ss = do
        return $ hsParen $ HsVar v_map `app` HsLambda srcLoc [pat] exp' `app` e
    f ((HsGenerator srcLoc pat e):HsQualifier q:ss) | isLazyPat pat, Just exp' <- g ss = do
        npvar <- newName
        return $ hsApp (HsVar v_foldr)  [HsLambda srcLoc [pat,HsPVar npvar] $
            HsIf q (hsApp (HsCon dc_Cons) [exp',HsVar npvar]) (HsVar npvar), HsList [],e]
    f ((HsGenerator srcLoc pat e):ss) | isLazyPat pat = do
        ss' <- f ss
        return $ hsParen $ HsVar v_concatMap `app`  HsLambda srcLoc [pat] ss' `app` e
    f ((HsGenerator srcLoc pat e):HsQualifier q:ss) | isFailablePat pat || Nothing == g ss = do
        npvar <- newName
        ss' <- f ss
        let kase = HsCase (HsVar npvar) [a1, a2 ]
            a1 =  HsAlt srcLoc pat (HsGuardedRhss [HsComp srcLoc [HsQualifier q] ss']) []
            a2 =  HsAlt srcLoc HsPWildCard (HsUnGuardedRhs $ HsList []) []
        return $ hsParen $ HsVar v_concatMap `app`  HsLambda srcLoc [HsPVar npvar] kase `app`  e
    f ((HsGenerator srcLoc pat e):ss) | isFailablePat pat || Nothing == g ss = do
        npvar <- newName
        ss' <- f ss
        let kase = HsCase (HsVar npvar) [a1, a2 ]
            a1 =  HsAlt srcLoc pat (HsUnGuardedRhs ss') []
            a2 =  HsAlt srcLoc HsPWildCard (HsUnGuardedRhs $ HsList []) []
        return $ hsParen $ HsVar v_concatMap `app` HsLambda srcLoc [HsPVar npvar] kase `app` e
    f ((HsGenerator srcLoc pat e):ss) = do
        npvar <- newName
        let Just exp' = g ss
            kase = HsCase (HsVar npvar) [a1 ]
            a1 =  HsAlt srcLoc pat (HsUnGuardedRhs exp') []
        return $ hsParen $ HsVar v_map `app` HsLambda srcLoc [HsPVar npvar] kase `app` e
    g [] = return exp
    g (HsLetStmt ds:ss) = do
        e <- g ss
        return (hsParen (HsLet ds e))
    g _ = Nothing
    app x y = HsApp x (hsParen y)

isFailablePat p | isStrictPat p = f (openPat p) where
    f (HsPTuple ps) = any isFailablePat ps
    f (HsPUnboxedTuple ps) = any isFailablePat ps
    f (HsPBangPat (Located _ p)) = isFailablePat p
    f _ = True
isFailablePat _ = False

isLazyPat pat = not (isStrictPat pat)
isStrictPat p = f (openPat p) where
    f HsPVar {} = False
    f HsPWildCard = False
    f (HsPAsPat _ p) = isStrictPat p
    f (HsPParen p) = isStrictPat p
    f (HsPIrrPat p) = False -- isStrictPat p  -- TODO irrefutable patterns
    f _ = True

openPat (HsPParen p) = openPat p
openPat (HsPNeg p) = openPat p
openPat (HsPAsPat _ p) = openPat p
openPat (HsPTypeSig _ p _) = openPat p
openPat (HsPInfixApp a n b) = HsPApp n [a,b]
openPat p = p
