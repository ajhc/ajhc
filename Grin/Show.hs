module Grin.Show(prettyFun,prettyVal,prettyExp,printGrin) where

import Grin.Grin
import Doc.Pretty
import Doc.PPrint
import Doc.DocLike
import Atom
import Char
import VConsts
import ANSI
import Grin.Val
import Number
import Monad
import CharIO
import E.Pretty(render)

instance PPrint Doc Val   where
    pprint v = prettyVal v

instance PPrint Doc Exp   where
    pprint v = prettyExp empty v

pVar v | v == unit = empty
pVar v  = pVal v <+> operator "<- " 

pVar' v  = pVal v <+> operator "<- " 


color :: Int -> Doc -> Doc
color 1 doc = oob (attr [1]) <> doc <> oob (attr [0])
color c doc = oob (attr [c]) <> doc <> oob (attr [39])

operator = color 1 . text
keyword = color 1 . text 
tag = text
func = color 92 . text
prim = color 91 . text
--func = text
--tag = color 92 . text

prettyVal = pVal

isComplex (_ :>>= _) = True
isComplex _ = False
{-# NOINLINE prettyExp #-}
prettyExp vl (e1 :>>= v :-> e2) | isComplex e1 = align $ ((pVar' v) <> (prettyExp empty e1)) <$> prettyExp vl e2
prettyExp vl (e1 :>>= v :-> e2) = align (prettyExp (pVar v) e1 <$> prettyExp vl e2)
prettyExp vl (Return v) = vl <> keyword "return" <+> pVal v
prettyExp vl (Store v) = vl <> keyword "store" <+> pVal v
prettyExp vl (Fetch v) = vl <> keyword "fetch" <+> pVal v
prettyExp vl (Error s _) = vl <> keyword "error" <+> tshow s
prettyExp vl (App t [v]) | t == funcEval = vl <> keyword "eval" <+> pVal v
prettyExp vl (App t [a,b]) | t == funcApply = vl <> keyword "apply" <+> pVal a <+> pVal b
prettyExp vl (App a vs)  = vl <> func (fromAtom a) <+> hsep (map pVal vs)
prettyExp vl (Prim Primitive { primName = nm } vs)  = vl <> prim (fromAtom nm) <+> hsep (map pVal vs)
prettyExp vl (Update x y) = vl <> keyword "update" <+> pVal x <+> pVal y
prettyExp vl (Cast x _) = vl <> keyword "cast" <+> pVal x
prettyExp vl (Case v vs) = vl <> keyword "case" <+> pVal v <+> keyword "of" <$> indent 2 (vsep (map f vs)) where
    f (v :-> e) = pVal v <+> operator "->" <+> keyword "do" <$> indent 2 (prettyExp empty e)

pVal s | Just st <- fromVal s = text $ show (st::String)
pVal (NodeC t []) = parens $ tag (fromAtom t)
pVal (NodeC t vs) = parens $ tag (fromAtom t) <+> hsep (map pVal vs)
pVal (NodeV (V i) vs) = parens $ char 't' <> tshow i <+> hsep (map pVal vs)
pVal (Tag t) = tag (fromAtom t)
pVal (Var (V i) t)
    | TyPtr _ <- t = char 'p' <> tshow i
    | TyNode <- t = char 'n' <> tshow i
    | t == Ty cChar = char 'c' <> tshow i
    | t == tIntzh  = char 'i' <> tshow i
    | Ty _ <- t  = char 'l' <> tshow i
    | TyTag <- t  = char 't' <> tshow i
pVal (Var (V i) _) = char 'v' <> tshow i
pVal (Lit i t) | t == tCharzh, i >= 0x20 && i < 0x7f, Just x <- toIntegral i = tshow (chr x)
pVal (Lit i _)  = tshow i
--pVal Unit = text "()"
pVal (Tup xs)  = tupled $ map pVal xs
pVal (Const v) = char '&' <> pVal v 
pVal (Addr _) = text "<ref>"

instance DocLike d => PPrint d Var where
    pprint (V i) = text $ 'v':show i
--pv (V 0) = char '_'
--pv (V i) = char 'v' <> tshow i


prettyFun :: (Atom,Lam) -> Doc
prettyFun (n,(Tup as :-> e)) = func (fromAtom n) <+> hsep (map pVal as) <+> operator "=" <+> keyword "do" <$> indent 2 (prettyExp empty e)


printGrin :: Grin -> IO ()
printGrin Grin { grinFunctions = ds', grinCafs = cafs } = do
    when (not $ null cafs) $ do 
        putErrLn "-- Cafs"
        mapM_ (putErrLn) $ map (\(x,y) -> show x ++ " = " ++  render (prettyVal y))  cafs 
    putErrLn "-- Functions"
    mapM_ (putErrLn . render) $ map prettyFun ds'

