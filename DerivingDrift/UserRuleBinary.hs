-- stub module to add your own rules.
module UserRuleBinary (userRulesBinary) where

import List (nub,intersperse)
import DerivingDrift.RuleUtils -- useful to have a look at this too

userRulesBinary = [
    ("Binary", userRuleBinary, "Binary", "efficient binary encoding of terms", Nothing)
    ]

{- datatype that rules manipulate :-


data Data = D {	name :: Name,			 -- type's name
			constraints :: [(Class,Var)], 
			vars :: [Var],		 -- Parameters
			body :: [Body],
			derives :: [Class],	 -- derived classes
			statement :: Statement}  -- type of statement
	   | Directive				 --|
	   | TypeName Name			 --| used by derive (ignore)
		deriving (Eq,Show) 

data Body = Body { constructor :: Constructor,
		    labels :: [Name], -- [] for a non-record datatype.
		    types :: [Type]} deriving (Eq,Show) 

data Statement = DataStmt | NewTypeStmt deriving (Eq,Show)

type Name = String
type Var = String
type Class = String
type Constructor = String

type Rule = (Tag, Data->Doc)

-}


-- useful helper things
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




-- begin here for Binary derivation


userRuleBinary dat = 
  let cs  = body dat
      cvs = mknss cs namesupply
      k   = (ceiling . logBase 2 . realToFrac . length) cs
  in
  instanceheader "Binary" dat $$
  block (  zipWith3 (putfn k) [0..] cvs cs
        ++ getfn k [0..] cvs cs
        :  getFfn k [0..] cvs cs
        :  zipWith (sizefn k) cvs cs
        )

putfn k n cv c =
  text "put bh" <+> ppCons cv c <+> text "= do" $$
  nest 8 (
    text "pos <- putBits bh" <+> text (show k) <+> text (show n) $$
    vcat (map (text "put bh" <+>) cv) $$
    text "return pos"
  )

ppCons cv c = mkpattern (constructor c) (types c) cv

getfn k ns cvs cs =
  text "get bh = do" $$
  nest 8 (
    text "h <- getBits bh" <+> text (show k) $$
    text "case h of" $$
    nest 2 ( vcat $
      zipWith3 (\n vs c-> text (show n) <+> text "-> do" $$
                          nest 6 (
                            vcat (map (\v-> v <+> text "<-" <+> text "get bh") vs) $$
                            text "return" <+> ppCons vs c
                          ))
               ns cvs cs
    )
  )

getFfn k ns cvs cs =
  text "getF bh p =" <+>
  nest 8 (
    text "let (h,p1) = getBitsF bh 1 p in" $$
    text "case h of" $$
    nest 2 ( vcat $
      zipWith3 (\n vs c-> text (show n) <+> text "->" <+>
                          parens (cons c <> text ",p1") <+>
                          hsep (map (\_-> text "<< getF bh") vs))
               ns cvs cs
    )
  )
  where cons =  text . constructor

sizefn k [] c =
  text "sizeOf" <+> ppCons [] c <+> text "=" <+> text (show k)
sizefn k cv c =
  text "sizeOf" <+> ppCons cv c <+> text "=" <+> text (show k) <+> text "+" <+>
  hsep (intersperse (text "+") (map (text "sizeOf" <+>) cv))


-- end of binary derivation

