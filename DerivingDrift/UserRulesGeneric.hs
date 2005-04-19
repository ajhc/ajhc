
module UserRulesGeneric(userRulesGeneric) where

-- import StandardRules
import DerivingDrift.RuleUtils
import List(intersperse)


userRulesGeneric :: [RuleDef]
userRulesGeneric =  [
    ("ATermConvertible", atermfn, "Representation", "encode terms in the ATerm format", Nothing),
    ("Typeable", typeablefn, "General", "derive Typeable for Dynamic", Nothing),
    ("Term", dyntermfn, "Generics","Strafunski representation via Dynamic", Nothing),
    ("HFoldable", hfoldfn, "Generics", "Strafunski hfoldr", Nothing),
    ("Observable", observablefn, "Debugging", "HOOD observable", Nothing)
    ]



-- useful helper things

addPrime doc = doc <> (text "'")

ppCons cv c = mkpattern (constructor c) (types c) cv

namesupply   = [text [x,y] | x <- ['a' .. 'z'], 
                             y <- ['a' .. 'z'] ++ ['A' .. 'Z']]
mknss []     _  = []
mknss (c:cs) ns =
  let (thisns,rest) = splitAt (length (types c)) ns
  in thisns: mknss cs rest 

mkpattern :: Constructor -> [a] -> [Doc] -> Doc
mkpattern c l ns =
  if null l then text c
  else parens (hsep (text c : take (length l) ns))

instanceheader cls dat =
  let fv     = vars dat
      tycon  = name dat
      ctx    = map (\v-> text cls <+> text v)
      parenSpace = parens . hcat . sepWith space
  in
  hsep [ text "instance"
       , opt fv (\v -> parenList (ctx v) <+> text "=>")
       , text cls
       , opt1 (texts (tycon: fv)) parenSpace id
       , text "where"
       ]

doublequote str
  = "\""++str++"\""

mkList :: [Doc] -> Doc
mkList xs = text "[" <> hcat (punctuate comma xs) <> text "]"

typeablefn :: Data -> Doc
typeablefn  dat  
  = tcname <+> equals <+> text "mkTyCon" <+> text (doublequote $ name dat) $$
    instanceheader "Typeable" dat $$ block (
	[ text "typeOf x = mkAppTy"  <+> 
	  tcname <+> 
	  text "[" <+> hcat (sepWith comma (map getV' (vars dat))) <+> text "]" $$ 
	  wheres ]) 
    where
      tcname = text ("_tc_" ++ (name dat)  ++ "Tc")
      wheres = where_decls (map getV (vars dat))
      tpe    = text (name dat) <+> hcat (sepWith space (map text (vars dat)))
      getV' var
        = text "typeOf" <+> parens (text "get" <> text var <+> text "x")
      getV var 
        = text "get" <> text var <+> text "::" <+> tpe <+> text "->" <+> text var $$
          text "get" <> text var <+> equals <+> text "undefined"

where_decls [] = empty 
where_decls ds = text "  where" $$ block ds

dyntermfn :: Data -> Doc
dyntermfn dat = instanceheader "Term" dat $$ block [ 
    text "explode (x::"<>a<>text ") = TermRep (toDyn x, f x, g x) where", block (
	zipWith f cvs cs ++ zipWith g cvs cs
	)] where
	    f cv c = text "f" <+> ppCons cv c <+> equals <+> mkList (map (text "explode" <+>) $ vrs c cv)
	    g cv c = text "g" <+> ppCons underscores c <+> text "xs" <+> 
--		text "|" <+> mkList (vrs c cv) <+> text "<- TermRep.fArgs xs" <+> equals <+> text "toDyn" <+> parens (parens (text (constructor c) <+> hsep (map h (vrs c cv))) <> text "::a" ) 
		equals <+> text "case TermRep.fArgs xs of" <+> mkList (vrs c cv) <+> text "->" <+> text "toDyn" <+> parens (parens (text (constructor c) <+> hsep (map h (vrs c cv))) <> text "::"<>a<>text "" ) <> text " ; _ -> error \"Term explosion error.\""
	    h n = parens $ text "TermRep.fDyn" <+> n
	    cvs = mknss cs namesupply
	    cs = body dat
	    vrs c cv = take (length (types c)) cv
	    underscores = repeat $ text "_"
	    a = text (name dat) <+> hcat (sepWith space (map text (vars dat)))
    

-- begin observable 

observablefn :: Data -> Doc
observablefn  dat = 
  let cs  = body dat
      cvs = mknss cs namesupply
  in
  instanceheader "Observable" dat $$ 
  block (zipWith observefn cvs cs)

observefn cv c = 
    text "observer" <+> ppCons cv c <+> text "= send"  <+> text (doublequote (constructor c)) <+> parens (text "return" <+> text (constructor c) <+> hsep (map f (take (length (types c)) cv))) where
    f n = text "<<" <+> n






-- begin of ATermConvertible derivation 
-- Author: Joost.Visser@cwi.nl

atermfn dat
  = instanceSkeleton "ATermConvertible" 
      [ (makeToATerm (name dat),defaultToATerm)
      , (makeFromATerm (name dat),defaultFromATerm (name dat))
      ] 
      dat

makeToATerm name body
  = let cvs = head (mknss [body] namesupply)
    in text "toATerm" <+> 
       ppCons cvs body <+>
       text "=" <+>
       text "(AAppl" <+>
       text (doublequote (constructor body)) <+>
       text "[" <+> 
       hcat (intersperse (text ",") (map childToATerm cvs)) <+> 
       text "])"
defaultToATerm
  = empty
childToATerm v
  = text "toATerm" <+> v

makeFromATerm name body
  = let cvs = head (mknss [body] namesupply)
    in text "fromATerm" <+> 
       text "(AAppl" <+>
       text (doublequote (constructor body)) <+>
       text "[" <+> 
       hcat (intersperse (text ",") cvs) <+> 
       text "])" <+>
       text "=" <+> text "let" <+>
       vcat (map childFromATerm cvs) <+>
       text "in" <+>
       ppCons (map addPrime cvs) body
defaultFromATerm name
  = hsep $ texts ["fromATerm", "u", "=", "fromATermError", (doublequote name), "u"]
childFromATerm v
  = (addPrime v) <+> text "=" <+> text "fromATerm" <+> v

-- end of ATermConvertible derivation

-- begin of HFoldable derivation 
-- Author: Joost Visser and Ralf Laemmel

hfoldfn dat
  = instanceSkeleton "HFoldable" 
      [ (make_hfoldr (name dat), default_hfoldr),
        (make_conof (name dat), default_conof)
      ] 
      dat

make_hfoldr name body
  = let cvs = head (mknss [body] namesupply)
    in text "hfoldr'" <+> 
       text "alg" <+>
       ppCons cvs body <+>
       text "=" <+>
       foldl (\rest var -> text "hcons alg" <+> var  <+> parens rest) 
             (text "hnil alg" <+> text (constructor body))
             cvs 

default_hfoldr
  = empty
  
make_conof name body
  = let cvs = head (mknss [body] namesupply)
    in text "conOf" <+> 
       ppCons cvs body <+>
       text "=" <+>
       text (doublequote (constructor body))

default_conof
  = empty


