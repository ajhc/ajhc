module Grin.Show(
    prettyFun,
    prettyVal,
    prettyExp,
    printGrin,
    hPrintGrin,
    graphGrin,
    render
    ) where

import Char
import Control.Monad.Writer
import Data.Maybe
import IO
import Monad
import qualified Data.Map as Map
import qualified Data.Set as Set

import Atom
import CharIO
import Data.Graph.Inductive.Basic(elfilter)
import Data.Graph.Inductive.Graph(mkGraph,nmap)
import Data.Graph.Inductive.Tree
import Doc.Attr
import Doc.DocLike
import Doc.PPrint
import Doc.Pretty
import Grin.Grin
import Grin.Noodle
import Grin.Val
import Name.VConsts
import Number
import Options
import Support.FreeVars
import Support.Tuple
import Util.Graphviz
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

isOneLine (_ :>>= _) = False
isOneLine Case {} = False
isOneLine Let {} = False
isOneLine MkCont {} = False
isOneLine _ = True

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
prettyExp vl (Case v vs) = vl <> keyword "case" <+> prettyVal v <+> keyword "of" <$> indent 2 (vsep (map f vs)) where
    f (v :-> e) | isOneLine e = prettyVal v <+> operator "->" <+> prettyExp empty e
    f (v :-> e) = prettyVal v <+> operator "->" <+> keyword "do" <$> indent 2 (prettyExp empty e)
prettyExp vl NewRegion { expLam = (r :-> body)} = vl <> keyword "region" <+> text "\\" <> prettyVal r <+> text "-> do" <$> indent 2 (prettyExp empty body)
--prettyExp vl MkCont { expCont = (r :-> body) } = vl <> keyword "continuation" <+> text "\\" <> prettyVal r <+> text "-> do" <$> indent 2 (prettyExp empty body)
prettyExp vl Let { expDefs = defs, expBody = body } = vl <> keyword "let" <$> indent 4 (vsep $ map f defs) <$> text " in" <$> indent 2 (prettyExp empty body) where
    f FuncDef { funcDefName = name, funcDefBody = as :-> body } = func (show name) <+> hsep (map prettyVal $ fromTuple as) <+> operator "=" <+> keyword "do" <$> indent 2 (prettyExp empty body)
prettyExp vl Alloc { expValue = val, expCount = Lit n _, expRegion = r }| n == 1 = vl <> keyword "alloc" <+> prettyVal val <+> text "at" <+> prettyVal r
prettyExp vl Alloc { expValue = val, expCount = count, expRegion = r } = vl <> keyword "alloc" <+> prettyVal val <> text "[" <> prettyVal count <> text "]" <+> text "at" <+> prettyVal r
prettyExp vl Call { expValue = Item t (TyCall fun _ _), expArgs = vs, expJump = jump } | fun `elem` [Function,LocalFunction] =  vl <> f jump  <+> func (fromAtom t) <+> hsep (map prettyVal vs) where
    f True = text "jump to"
    f False = text "call"
prettyExp vl Call { expValue = Var v (TyCall fun _ _), expArgs = vs, expJump = jump}  =  vl <> f jump fun  <+> color "lightgreen" (pprint v) <+> hsep (map prettyVal vs) where
    f False Continuation = text "cut to"
    f False Function = text "call"
    f True Function = text "jump to"
    f False Closure = text "enter"
    f True Closure = text "jump into"
prettyExp vl Call { expValue = ValPrim ap [] (TyCall Primitive' _ _), expArgs = vs } = vl <> prim (tshow ap) <+> hsep (map prettyVal vs)

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
prettyVal (Lit i t) | t == Ty (toAtom "wchar_t"), Just x <- toIntegral i = tshow (chr x)
prettyVal (Lit i _)  = tshow i
prettyVal (Tup xs)  = tupled $ map prettyVal xs
prettyVal (Const v) = char '&' <> prettyVal v
prettyVal (Addr _) = text "<ref>"
prettyVal (ValUnknown ty) = text "?::" <> tshow ty
prettyVal (Item a  ty) = tshow a <> text "::" <> tshow ty
prettyVal (ValPrim aprim xs ty) = pprint aprim <> tupled (map tshow xs)

instance DocLike d => PPrint d Var where
    pprint (V i) = text $ 'v':show i
--pv (V 0) = char '_'
--pv (V i) = char 'v' <> tshow i


prettyFun :: (Atom,Lam) -> Doc
prettyFun (n,(Tup as :-> e)) = func (fromAtom n) <+> hsep (map prettyVal as) <+> operator "=" <+> keyword "do" <$> indent 2 (prettyExp empty e)

render :: Doc -> String
render doc =  displayS (renderPretty 0.95 (optColumns options)  doc) ""

printGrin :: Grin -> IO ()
printGrin grin = hPrintGrin stderr grin

hPrintGrin :: Handle -> Grin -> IO ()
hPrintGrin handle grin@Grin { grinCafs = cafs } = do
    when (not $ null cafs) $ do
        hPutStrLn handle "-- Cafs"
        mapM_ (hPutStrLn handle) $ map (\(x,y) -> show x ++ " = " ++  render (prettyVal y))  cafs
    hPutStrLn handle "-- Functions"
    mapM_ (hPutStrLn handle . render) $ map prettyFun (grinFuncs grin)

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


{-# NOINLINE graphGrin #-}

graphGrin :: Grin -> String
graphGrin grin = graphviz' gr [] fnode fedge  where
    nodes = zip [0..] (grinFuncs grin)
    nodeMap = Map.fromList [ (y,x) | (x,(y,_)) <- nodes]
    gr :: Gr (Atom,Lam) CallType
    gr =   mkGraph nodes [ (n,n2,tc) | (n,(_,_ :-> l)) <- nodes, (tc,fv) <- Set.toList (freeVars l), n2 <- Map.lookup fv nodeMap ]
    fnode :: (Atom,Lam) -> [(String,String)]
    fnode (x,_ :-> e) = [("label",show x)]
        ++ (if hasError e then [("color","red")] else [])
        ++ (if x `elem` grinEntryPointNames grin then [("shape","box")] else [])
    fedge :: CallType -> [(String,String)]
    fedge TailCall = []
    fedge StandardCall = [("style","dotted")]

hasError x = isNothing (hasError' x)
hasError' Error {} = Nothing
hasError' e = mapExpExp hasError' e


data CallType = TailCall | StandardCall
    deriving(Ord,Show,Eq)

instance FreeVars Exp (Set.Set (CallType,Atom)) where
    freeVars (a :>>= _ :-> b) = freeVars b `Set.union` Set.map (\ (_ :: CallType,y) -> (StandardCall, y)) (freeVars a)
    freeVars (App a _ _) = Set.singleton (TailCall,a)
    freeVars e = execWriter $ mapExpExp (\e -> tell (freeVars e) >> return e) e


