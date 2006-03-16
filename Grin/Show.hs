module Grin.Show(prettyFun,prettyVal,prettyExp,printGrin,render) where

import Char
import Monad
import qualified Data.Set as Set

import Atom
import CharIO
import Doc.Attr
import Doc.DocLike
import Doc.PPrint
import Doc.Pretty
import Grin.Grin
import Grin.Val
import Name.VConsts
import Number
import Options
import qualified FlagDump as FD



instance DocLike d => PPrint d Val   where
    pprint v = prettyVal v


instance PPrint Doc Exp   where
    pprint v = prettyExp empty v

pVar v | v == unit = empty
pVar v  = prettyVal v <+> operator "<- "

pVar' v  = prettyVal v <+> operator "<- "

attr = if dump FD.Html then html else ansi

bold :: Doc -> Doc
bold = attrBold (attr oob)
color n x = attrColor (attr oob) n x

--color :: Int -> Doc -> Doc
--color 1 doc = oob (attr [1]) <> doc <> oob (attr [0])
--color c doc = oob (attr [c]) <> doc <> oob (attr [39])

operator = bold . text
keyword = bold . text
tag x = text x
func = color "lightgreen" . text
prim = color "red" . text
--func = text
--tag = color 92 . text


isComplex (_ :>>= _) = True
isComplex _ = False
{-# NOINLINE prettyExp #-}
prettyExp vl (e1 :>>= v :-> e2) | isComplex e1 = align $ ((pVar' v) <> (prettyExp empty e1)) <$> prettyExp vl e2
prettyExp vl (e1 :>>= v :-> e2) = align (prettyExp (pVar v) e1 <$> prettyExp vl e2)
prettyExp vl (Return v) = vl <> keyword "return" <+> prettyVal v
prettyExp vl (Store v) = vl <> keyword "store" <+> prettyVal v
prettyExp vl (Fetch v) = vl <> keyword "fetch" <+> prettyVal v
prettyExp vl (Error "" _) = vl <> prim "exitFailure"
prettyExp vl (Error s _) = vl <> keyword "error" <+> tshow s
prettyExp vl (App t [v] _) | t == funcEval = vl <> keyword "eval" <+> prettyVal v
prettyExp vl (App t [a,b] _) | t == funcApply = vl <> keyword "apply" <+> prettyVal a <+> prettyVal b
prettyExp vl (App a vs _)  = vl <> func (fromAtom a) <+> hsep (map prettyVal vs)
prettyExp vl (Prim Primitive { primName = nm } vs)  = vl <> prim (fromAtom nm) <+> hsep (map prettyVal vs)
prettyExp vl (Update x y) = vl <> keyword "update" <+> prettyVal x <+> prettyVal y
prettyExp vl (Cast x _) = vl <> keyword "cast" <+> prettyVal x
prettyExp vl (Case v vs) = vl <> keyword "case" <+> prettyVal v <+> keyword "of" <$> indent 2 (vsep (map f vs)) where
    f (v :-> e) = prettyVal v <+> operator "->" <+> keyword "do" <$> indent 2 (prettyExp empty e)

prettyVal :: DocLike d => Val -> d
prettyVal s | Just [] <- valToList s = text "[]"
prettyVal s | Just st <- fromVal s = text $ show (st::String)
prettyVal s | Just vs <- valToList s = list $ map prettyVal vs
prettyVal (NodeC t []) = parens $ tag (fromAtom t)
prettyVal (NodeC t vs) = parens $ tag (fromAtom t) <+> hsep (map prettyVal vs)
prettyVal (NodeV (V i) vs) = parens $ char 't' <> tshow i <+> hsep (map prettyVal vs)
prettyVal (Tag t) = tag (fromAtom t)
prettyVal (Var (V i) t)
    | TyPtr _ <- t = char 'p' <> tshow i
    | TyNode <- t = char 'n' <> tshow i
    | t == tCharzh = char 'c' <> tshow i
    | t == tIntzh  = char 'i' <> tshow i
    | Ty _ <- t  = char 'l' <> tshow i
    | TyTag <- t  = char 't' <> tshow i
prettyVal (Var (V i) _) = char 'v' <> tshow i
prettyVal (Lit i t) | t == tCharzh, Just x <- toIntegral i = tshow (chr x)
prettyVal (Lit i _)  = tshow i
prettyVal (Tup xs)  = tupled $ map prettyVal xs
prettyVal (Const v) = char '&' <> prettyVal v
prettyVal (Addr _) = text "<ref>"
prettyVal (ValPrim aprim xs ty) = tshow aprim <> tupled (map tshow xs)

instance DocLike d => PPrint d Var where
    pprint (V i) = text $ 'v':show i
--pv (V 0) = char '_'
--pv (V i) = char 'v' <> tshow i


prettyFun :: (Atom,Lam) -> Doc
prettyFun (n,(Tup as :-> e)) = func (fromAtom n) <+> hsep (map prettyVal as) <+> operator "=" <+> keyword "do" <$> indent 2 (prettyExp empty e)

render :: Doc -> String
render doc =  displayS (renderPretty 0.95 (optColumns options)  doc) ""

printGrin :: Grin -> IO ()
printGrin Grin { grinFunctions = ds', grinCafs = cafs } = do
    when (not $ null cafs) $ do
        putErrLn "-- Cafs"
        mapM_ (putErrLn) $ map (\(x,y) -> show x ++ " = " ++  render (prettyVal y))  cafs
    putErrLn "-- Functions"
    mapM_ (putErrLn . render) $ map prettyFun ds'

showSome xs = f 7 xs [] where
    f 0 _ xs = reverse ("...":xs)
    f _ [] xs = reverse xs
    f n (x:xs) rs = f (n - 1) xs (x:rs)

instance Show Item where
    show (BasicValue ty) = "<" ++ show ty ++ ">"
    show (HeapValue hv) = braces $ hcat $ punctuate "," (showSome $ map show (Set.toList hv))
    show (NodeValue hv) = braces $ hcat $ punctuate "," (showSome $ map show (Set.toList hv))
    show (TupledValue xs) = tupled (map show xs)

instance Show NodeValue where
    show (NV t as) = parens $ hsep (show t:map show as)

instance Show HeapValue where
    show (HV _ (Right v)) = prettyVal (Const v)
    show (HV n (Left (ht,_))) = show ht ++ "-" ++ show n

instance Show HeapType where
    show Constant = "C"
    show SharedEval = "Es"
    show UnsharedEval = "Eu"
    show Reference = "Ref"
    show RecursiveThunk = "Rt"
