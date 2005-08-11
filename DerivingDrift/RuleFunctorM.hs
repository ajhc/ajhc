-- stub module to add your own rules.
module RuleFunctorM (rules) where

import List
import DerivingDrift.RuleUtils

rules = [
    ("FunctorM", userRuleFunctorM, "Generics", "derive reasonable fmapM implementation", Nothing),
    ("RMapM", userRuleRMapM, "Generics", "derive reasonable rmapM implementation", Nothing)
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

{-

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

-}



-- begin here for Binary derivation


userRuleFunctorM D{name = name, vars = [] } = text "--" <+> text name <> text ": Cannot derive FunctorM without type variables"
userRuleFunctorM D{name = name, vars = vars, body=body } = ins where
    (tt:rt') = reverse vars
    rt = reverse rt'
    fn = if null rt then text name else parens (text name <+> hsep (map text rt))
    ins = text "instance" <+> text "FunctorM" <+> fn <+> text "where" $$ block fs
    fs = map f' $ body
    f' Body{constructor=constructor, types=types} = text "fmapM" <+> text "f" <+> pattern constructor types <+> equals <+> text "do" <+> hcat (map g (zip types vnt)) <+> text "return $" <+> text constructor <+> hsep vnt where
        vnt = varNames types
        g (t,n) | not (has t) = empty
        g (Var t,n) | t == tt = n <+> lArrow <+> text "f" <+> n <> semicolon
        g (List (Var t),n) | t == tt = n <+> lArrow <+> text "mapM" <+> f <+> n <> semicolon
        g (List t,n)  = n <+> lArrow <+> text "mapM" <+> lf t <+> n <> semicolon  where
            lf t = parens $ text "\\x ->" <+> text "do" <+> g (t,x) <+> text "return" <+> x
        g (LApply t [],n) = g (t,n)
        g (LApply t ts,n) | last ts == Var tt = n <+> lArrow <+> text "fmapM" <+> f <+> n <> semicolon
        g (Tuple ts,n) = n <+> lArrow <+> (parens $ text "do" <+> tuple (varNames ts) <+> lArrow <+> text "return" <+> n <> semicolon  <+> hcat (map g (zip ts (varNames ts))) <> text "return" <+> tuple (varNames ts)) <> semicolon
        g _ = empty
    has (Var t) | t == tt = True
    has (List t) = has t
    has (Arrow a b) = has a || has b
    has (LApply t ts) = any has (t:ts)
    has (Tuple ts) = any has (ts)
    has _ = False

userRuleRMapM D{name = name, vars = vars, body=body } = ins where
    --(tt:rt') = reverse vars
    tt = if null vars then Con name else LApply (Con name) (map Var vars)
    rt = vars
    fn = if null rt then text name else parens (text name <+> hsep (map text rt))
    ins = text "instance" <+> text "RMapM" <+> fn <+> text "where" $$ block fs
    fs = map f' $ body
    f' Body{constructor=constructor, types=types} = text "rmapM" <+> text "f" <+> pattern constructor types <+> equals <+> text "do" <+> hcat (map g (zip types vnt)) <+> text "return $" <+> text constructor <+> hsep vnt where
        vnt = varNames types
        g (t,n) | not (has t) = empty
        g ( t,n) | t == tt = n <+> lArrow <+> text "f" <+> n <> semicolon
        g (List (t),n) | t == tt = n <+> lArrow <+> text "mapM" <+> f <+> n <> semicolon
        g (List t,n)  = n <+> lArrow <+> text "mapM" <+> lf t <+> n <> semicolon  where
            lf t = parens $ text "\\x ->" <+> text "do" <+> g (t,x) <+> text "return" <+> x
        g (LApply t [],n) = g (t,n)
        g (LApply t ts,n) | last ts ==  tt = n <+> lArrow <+> text "fmapM" <+> f <+> n <> semicolon
        g (Tuple ts,n) = n <+> lArrow <+> (parens $ text "do" <+> tuple (varNames ts) <+> lArrow <+> text "return" <+> n <> semicolon  <+> hcat (map g (zip ts (varNames ts))) <> text "return" <+> tuple (varNames ts)) <> semicolon
        g _ = empty
    has t | t == tt = True
    has (List t) = has t
    has (Arrow a b) = has a || has b
    has (LApply t ts) = any has (t:ts)
    has (Tuple ts) = any has (ts)
    has _ = False

