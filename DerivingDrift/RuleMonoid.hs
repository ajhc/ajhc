-- stub module to add your own rules.
module RuleMonoid (rules) where

import List 
import DerivingDrift.RuleUtils 

rules = [
    ("Monoid", userRuleMonoid, "Generics", "derive reasonable Data.Monoid implementation", Nothing)
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

mkpattern :: Constructor -> [Doc] -> Doc
mkpattern c ns =
  if null ns then text c
  else parens (hsep (text c :  ns))

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


userRuleMonoid dat@D{name = name, vars = vars, body=[body] } = ins where
    ins = instanceheader "Monoid" dat $$ 
        block [me, ma]
    me, ma :: Doc
    me = text "mempty" <+> equals <+> text (constructor body) <+> hsep (replicate lt (text "mempty"))     
    ma = text "mappend" <+> mkpattern c (varNames ty) <+> mkpattern c (varNames' ty) <+> equals <+> text c <+> hcat (zipWith f (varNames ty) (varNames' ty))
    f a b = parens $ text "mappend"  <+> a <+> b
    c = constructor body
    ty = types body
    lt = length (types body)
userRuleMonoid D{name = name } = text "--" <+> text name <> text ": Cannot derive Monoid from type"


