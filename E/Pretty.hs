module E.Pretty(ePretty, ePrettyNEx, ePrettyEx, ePrettyN, prettyE, render ) where

import Char
import qualified Data.Map as Map

import Atom(intToAtom,fromAtom)
import Doc.Attr
import Doc.DocLike
import Doc.PPrint
import Doc.Pretty
import E.E
import E.FreeVars()
import Support.FreeVars
import Name.Name
import Name.Names
import Options
import qualified Doc.Chars as UC
import qualified FlagDump as FD
import Support.Unparse
import Name.VConsts

-----------------
-- Pretty Print E
-----------------


instance PPrint Doc E where
    pprint x = ePretty x



instance DocLike d => PPrint d TVr where
    pprint TVr { tvrIdent = i }  = prettyI i



isTup ('(':',':xs) | (a,")") <- span (== ',') xs = return (length a + 2)
isTup _ = fail "Not tuple"


render :: Doc -> String
render doc =  displayS (renderPretty 0.95 (optColumns options)  doc) ""

data PrettyOpt = PrettyOpt {
    optExpanded :: Bool,
    optColors :: Bool,
    optNames :: Int -> Doc
    }

prettyOpt = PrettyOpt { optExpanded = False, optColors = True, optNames = pName Map.empty }



ePretty e = ePrettyN Map.empty e
ePrettyEx e = (eDoc e prettyOpt { optExpanded = True})
ePrettyN m e = (eDoc e prettyOpt { optNames = pName m})
ePrettyNEx m e = (eDoc e prettyOpt { optExpanded = True, optNames = pName m})
prettyE :: E -> String
prettyE e = render $ ePrettyN Map.empty e
prettyENameEx m e = render $ ePrettyNEx m e


pName m i = case Map.lookup i m of
    Nothing -> text $ 'x':show i
    Just n -> case nameType n of
        TypeVal -> text $ UC.uArrow ++ show n
        _ -> text $ show n




prettyTvr t = ( (eDoc (EVar t)) prettyOpt)


attr = if dump FD.Html then html else ansi

bold :: Doc -> Doc
bold = attrBold (attr oob)

--bold doc = oob attrBold <> doc <> oob attrClear


--color :: Int -> Doc -> Doc
--color 1 doc = oob (attr [1]) <> doc <> oob (attr [0])
--color c doc = oob (attr [c]) <> doc <> oob (attr [39])




prettyI 0 = (char '_')
prettyI i | Just x <- intToAtom i  = (text $ show  $ (fromAtom x :: Name))
prettyI i = (text $ 'x':show i)

rawType s  = ELit (LitCons (toName RawType s) [] eHash)

eDoc e PrettyOpt {optExpanded = optExpanded, optColors = colors, optNames = optNames} = unparse (prettye e) where
    expanded = optExpanded || dump FD.EVerbose
    retOp x = col "lightgreen" x

    bold' = if colors then bold else id
    bc = bold' . char
    col n x = if colors then  (attrColor (attr oob) n x) else x

    keyword x = bold' (text x)
    symbol x = atom (bold' x)

    --atomize (x,_) = (x,Atom)

    prettylit :: (a -> Unparse Doc) -> Lit a E -> Unparse Doc
    prettylit pbind (LitInt c t) | t == tChar = atom $ (col "blue" (text (show $ chr $ fromIntegral  c)))
    prettylit pbind (LitInt i _) = atom $ (col "blue" (text $ show i))
    prettylit pbind (LitCons n [] t) | t == rawType "tag#" = atom $ (col "blue" (text $ show n))
    prettylit pbind (LitCons s es _) | Just n <- isTup (snd $ (snd $ fromName s :: (String,String))), n == length es = atom $ tupled (map (unparse . pbind) es)
    prettylit pbind (LitCons s es _) | Just n <- fromUnboxedNameTuple s, n == length es = atom $ encloseSep (text "(# ") (text " #)") (text ", ") (map (unparse . pbind) es)
    prettylit pbind (LitCons n [a,b] _) | vCons == n  = (pbind a) `cons` (pbind b)
    prettylit pbind (LitCons n [e] _) | toName TypeConstructor ("Prelude","[]") == n = atom   (char '[' <> unparse (pbind e)  <> char ']')
    prettylit pbind (LitCons s es _) | not expanded = foldl app  (atom $ text (snd $ fromName s)) ( map pbind es)
    prettylit pbind (LitCons s es t) = foldl app (atom (text (snd $ fromName s))) ( map pbind es) `inhabit` prettye t

    inhabit = bop (N,-2) $ retOp UC.coloncolon
    arr = bop (R,0) $ retOp (space <> UC.rArrow <> space)
    dot = bop (R,-1) $ retOp (char '.')
    app = bop (L,100) (text " ")
    cons = bop (R,5) (text ":")

    prettytvr TVr { tvrIdent = i, tvrType =  t, tvrInfo = nfo} | dump FD.EInfo = atom (prettyI i <> tshow nfo)  `inhabit` prettye t
    prettytvr TVr { tvrIdent = i, tvrType =  t} = atom (prettyI i) `inhabit` prettye t

    prettye me =  case me of
        e | Just s <- toString e -> atom $ col "blue" (text $ show s)
        e | Just xs <- toList e -> atom $ list (map (unparse . prettye) xs)
        (EAp a b) -> (prettye a) `app` (prettye b)
        (ELam (TVr {tvrIdent =  i, tvrType =  z}) e) | z == eStar ->  (pop (retOp UC.lAmbda) (atom $ prettyI i)) `dot` prettye e
        (ELam t e) ->  (pop (retOp UC.lambda) (atomize $ prettytvr t)) `dot` prettye e
        (EPi (TVr { tvrIdent = 0, tvrType =  e1}) e)  -> prettye e1 `arr` prettye e
        (EPi (TVr { tvrIdent = j, tvrType =  e1}) e) | j `notElem` freeVars e -> prettye e1 `arr` prettye e
        (EPi (TVr { tvrIdent = i, tvrType =  z}) e) | z == eStar ->  (pop (retOp UC.forall) (atom $ prettyI i)) `dot` prettye e
        (EPi t e) ->  (pop (retOp UC.pI) (atomize $ prettytvr t)) `dot` prettye e
        --(EVar tvr) | expanded -> prettytvr tvr
        (EVar (TVr { tvrIdent = i })) -> atom $ prettyI i
        Unknown -> symbol (char  '?')
        ESort EStar -> symbol UC.star
        ESort EBox -> symbol UC.box
        ESort EHash -> symbol (text "#")
        (ELit l) -> prettylit prettye l
        (ELetRec bg e) -> fixitize (L,(-10)) $ atom $ let
            bg' = map ((<> bc ';') . unparse . prettydecl ) bg
            e' = unparse  (prettye e)
            in group ( nest 4  ( keyword "let" </> (align $ sep bg') </> (keyword "in" <+> e')) )
        ec@(ECase { eCaseScrutinee = e, eCaseAlts = alts }) -> fixitize ((L,(-10))) $ atom $ let
            e' = unparse $ prettye e
            alts' = map  ((<> bc ';') . prettyalt b) alts ++ dcase
            b = eCaseBind ec
            dcase = maybe [] ( (:[]) . pdef b) (eCaseDefault ec)
            in  group ( nest 4 ( keyword "case" <+> e' <+> keyword "of" <$>  (align $ sep (alts'))) )
        (EPrim s es t) -> atom (angles  (unparse $ foldl app (atom (pprint s)) ( map prettye es) `inhabit` prettye t) )
        (EError s t) -> atom $ angles ( UC.bottom <> char ':' <> text s <>  UC.coloncolon <> unparse (prettye t) )

    prettyalt b (Alt l e) = nest 4 $ fill 10 ((unparse (prettylit prettytvr l)) <+>  UC.rArrow </> (unparse (prettye e)))
    pdef b e =  unparse (prettytvr b) <+>  UC.rArrow <+> unparse (prettye e) <> bc ';'


    prettydecl (t,e) =  atom $ nest 4 $ unparse (prettytvr t) <+> retOp (char '=') </> unparse (prettye e)

