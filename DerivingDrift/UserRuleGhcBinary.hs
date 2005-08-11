-- stub module to add your own rules.
module UserRuleGhcBinary (userRulesGhcBinary) where

import List (nub,intersperse)
import DerivingDrift.RuleUtils -- useful to have a look at this too

userRulesGhcBinary = [
    ("GhcBinary", userRuleGhcBinary, "Binary", "byte sized binary encoding of terms", Nothing)
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


userRuleGhcBinary dat =
  let cs  = body dat
      cvs = mknss cs namesupply
      --k   = (ceiling . logBase 256 . realToFrac . length) cs
      k = length cs
  in
  instanceheader "Binary" dat $$
  block (  zipWith3 (putfn k) [0..] cvs cs
        ++ [getfn k [0..] cvs cs]
        )

putfn 1 _ [] c =
    text "put_ _" <+> ppCons [] c <+> text "= return ()"
putfn 1 _ cv c =
  text "put_ bh" <+> ppCons cv c <+> text "= do" $$
  nest 8 (
    vcat (map (text "put_ bh" <+>) cv)
  )
putfn _ n cv c =
  text "put_ bh" <+> ppCons cv c <+> text "= do" $$
  nest 8 (
    text "putByte bh" <+> text (show n) $$
    vcat (map (text "put_ bh" <+>) cv) -- $$
    --text "return pos"
  )

ppCons cv c = mkpattern (constructor c) (types c) cv

getfn _ _ [[]] [c] =
    text "return" <+> ppCons [] c
getfn _ _ [vs] [c] =
  text "get bh = do" $$
    vcat (map (\v-> v <+> text "<-" <+> text "get bh") vs) $$
    text "return" <+> ppCons vs c
getfn _ ns cvs cs =
  text "get bh = do" $$
  nest 8 (
    text "h <- getByte bh"  $$
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



-- end of binary derivation

