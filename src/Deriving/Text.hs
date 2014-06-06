module Deriving.Text(deriveRead,deriveShow) where

import Deriving.Type
import Deriving.Util
import FrontEnd.HsSyn
import FrontEnd.Syn.Q
import Name.Names

deriveRead :: SrcLoc -> Module -> Data -> Q HsDecl
deriveRead hsDeclSrcLoc m d@D { .. } = do
    mkInst hsDeclSrcLoc m d class_Read []

deriveShow :: SrcLoc -> Module -> Data -> Q HsDecl
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

    {-
deriveShow :: SrcLoc -> Module -> Data -> Q HsDecl
deriveShow hsMatchSrcLoc m d@D { .. } = do
    (HsVar -> ne,np) <- newVar (Just m)
    let sprec = HsFunBind [HsMatch { .. }] where
            hsMatchName = v_showsPrec

    mkInst hsDeclSrcLoc m d class_Show [sprec]

showfn = instanceSkeleton (q class_Show) [(makeShow,empty)]

makeShow :: IFunction
makeShow (Body{constructor=constructor,labels=labels,types=types})
	| null types = fnName <+> fsep [headfn,showString constructor]
	| null labels = fnName <+> fsep [headfn,bodyStart, body]   -- datatype
	| otherwise = fnName <+> fsep[headfn,bodyStart,recordBody] -- record
	where
	fnName = text (u v_showsPrec)
	headfn = fsep [char 'd',(pattern constructor types),equals]
	bodyStart = fsep [text (q v_showParen),parens $ fsep [text "d",text (q v_geq),text "10"]]
	body = parens . fsep $ sepWith s (c : b)
	recordBody = parens $ fsep [c,comp,showString " {",comp,
				    fsep (sepWith s' b'),comp,showChar '}']
	c = showString constructor
	b = map (\x -> fsep[text (q v_showsPrec), text "10", x]) (varNames types)
	b' = zipWith (\x l -> fsep [showString l, comp, showString " = ", comp, x])
			            b (map getIdent labels)
	s = fsep [comp,showChar ' ', comp]
	s' = fsep [comp,showChar ',',comp]
	showChar c = fsep [text (q v_showChar), text ('\'':c:"\'")]
	showString s | isOpLike s = fsep [text (q v_showString), doubleQuotes $ char '(' <> text s <> char ')']
	showString s = fsep [text (q v_showString), doubleQuotes $ text s]
	comp = text (q v_compose)
isOpLike n  = x `elem` "!#$%&*+./<=>?@\\^-~:|" where
    (x:_) = n
    -}
