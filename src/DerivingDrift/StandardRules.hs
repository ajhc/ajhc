module DerivingDrift.StandardRules (standardRules) where

import DerivingDrift.RuleUtils
import Name.Prim
import Name.Name
import qualified Data.Map as Map


--- Add Rules Below Here ----------------------------------------------------

standardRules :: Map.Map Name.Name.Name (Data -> Doc)
standardRules = Map.fromList [
    (class_Eq,eqfn),
    (class_Ord,ordfn),
    (class_Enum,enumfn),
    (class_Show,showfn),
    (class_Read,readfn),
    (class_Bounded,boundedfn)]



------------------------------------------------------------------------------
-- Rules for the derivable Prelude Classes

-- Eq

eqfn = instanceSkeleton "Eq" [(makeEq,defaultEq)]

makeEq :: IFunction
makeEq (Body{constructor=constructor,types=types})
	| null types = hsep $ texts [constructor,"==",constructor, "=", "True"]
	| otherwise = let
	v = varNames types
	v' = varNames' types
	d x = parens . hsep $ text constructor : x
	head = [ text "==", d v', text "="]
	body = sepWith (text "&&") $
		zipWith (\x y -> (x <+> text "==" <+> y)) v v'
	in d v <+> fsep (head ++  body)

defaultEq = hsep $ texts ["_", "==", "_", "=" ,"False"]

----------------------------------------------------------------------

-- Ord

ordfn d = let
   ifn = [f c c'
		| c <- zip (body d) [1 :: Int ..]
		, c' <- zip (body d) [1 :: Int ..]]
   cmp n n' = show $  compare n n'
   f (b,n) (b',n')
	| null (types b) = text "compare" <+>
		   fsep [text (constructor b),
			 pattern (constructor b') (types b')
			, char '=', text $ cmp n n' ]
	| otherwise = let
		      head  = fsep [l,r, char '=']
		      l = pattern (constructor b) (types b)
		      r = pattern' (constructor b') (types b')
		      one x y = fsep [text "compare",x,y]
		      list [x] [y] = one x y
		      list xs ys = fsep [text "foldl", parens fn, text "EQ",
			           bracketList (zipWith one xs ys)]
		      fn = fsep $ texts  ["\\x y", "->", "if", "x", "==","EQ",
			   "then", "compare", "y", "EQ", "else", "x"]
		in if constructor b == constructor b' then
		    text "compare" <+> fsep [head,
			     list (varNames $ types b) (varNames' $ types b')]
		   else  text "compare" <+> fsep [head,text (cmp n n')]
    in simpleInstance "Ord" d <+> text "where" $$ block ifn


----------------------------------------------------------------------

-- Show & Read
-- 	won't work for infix constructors
-- 	(and anyway, neither does the parser currently)
--
-- Show

showfn = instanceSkeleton "Show" [(makeShow,empty)]

makeShow :: IFunction
makeShow (Body{constructor=constructor,labels=labels,types=types})
	| null types = fnName <+> fsep [headfn,showString constructor]
	| null labels = fnName <+> fsep [headfn,bodyStart, body]   -- datatype
	| otherwise = fnName <+> fsep[headfn,bodyStart,recordBody] -- record
	where
	fnName = text "showsPrec"
	headfn = fsep [char 'd',(pattern constructor types),equals]
	bodyStart = fsep [text "showParen",parens (text "d >= 10")]
	body = parens . fsep $ sepWith s (c : b)
	recordBody = parens $ fsep [c,comp,showChar '{',comp,
				    fsep (sepWith s' b'),comp,showChar '}']
	c = showString constructor
	b = map (\x -> fsep[text "showsPrec", text "10", x]) (varNames types)
	b' = zipWith (\x l -> fsep[showString l,comp,showChar '=',comp,x])
			            b labels
	s = fsep [comp,showChar ' ', comp]
	s' = fsep [comp,showChar ',',comp]
	showChar c = fsep [text "showChar", text ('\'':c:"\'")]
	showString s = fsep[ text "showString", doubleQuotes $ text s]
	comp = char '.'

-- Read

readfn d = simpleInstance "Read" d <+> text "where" $$ readsPrecFn d

readsPrecFn d = let
	fnName = text "readsPrec"
	bodies = vcat $ sepWith (text "++") (map makeRead (body d))
	in nest 4 $ fnName <+> fsep[char 'd', text "input", equals,bodies]

makeRead :: IFunction
makeRead (Body{constructor=constructor,labels=labels,types=types})
	| null types = fsep [read0,text "input"]
	| null labels = fsep [headfn,read,text "input"]
	| otherwise = fsep [headfn,readRecord, text "input"]
	where
	headfn = fsep [text "readParen", parens (text "d > 9")]
	read0 = lambda $ listComp (result rest) [lexConstr rest]
	read = lambda . listComp (result rest)
		     $ lexConstr ip : ( map f (init vars) )
			++ final (last vars)
        f v = fsep [tup v ip, from,readsPrec, ip]
	final v = [fsep[tup v rest,from,readsPrec,ip]]
	readRecord = let
		f lab v = [
			fsep [tup (text $ show lab) ip,lex],
			fsep [tup (text $ show "=") ip,lex],
			fsep [tup v ip ,from,readsPrec,ip]]
		openB = fsep [tup (text $ show "{") ip,lex]
		closeB = fsep [tup (text $ show "}") rest,lex]
		comma = [fsep [tup (text $ show ",") ip,lex]]
		in lambda . listComp (result rest)
			$ lexConstr ip : openB
			: (concat . sepWith comma) (zipWith f labels vars)
			 ++ [closeB]
	lambda x = parens ( fsep [text "\\",ip,text "->",x])
	listComp x ~(l:ll) = brackets . fsep . sepWith comma $
				((fsep[x, char '|', l]) : ll)
	result x = tup (pattern constructor vars) x
	lexConstr x = fsep [tup (text $ show constructor) x, lex]
	-- nifty little bits of syntax
	vars = varNames types
	ip = text "inp"
	rest = text "rest"
	tup x y = parens $ fsep [x, char ',',y]
	lex = fsep[from,text "lex",ip]
	readsPrec = fsep [text "readsPrec",text "10"]
	from = text "<-"

----------------------------------------------------------------------

-- Enum -- a lot of this code should be provided as default instances,
-- 	 but currently isn't

enumfn d = let
	fromE = fromEnumFn d
	toE = toEnumFn d
	eFrom = enumFromFn d
	in if any (not . null . types) (body d)
	   then commentLine $ text "Warning -- can't derive Enum for"
				<+> text (name d)
	   else simpleInstance "Enum" d <+> text "where"
		$$ block (fromE ++ toE ++ [eFrom,enumFromThenFn])

fromEnumFn :: Data -> [Doc]
fromEnumFn (D{body=body}) = map f (zip body [0:: Int ..])
	where
	f (Body{constructor=constructor},n) = text "fromEnum" <+> (fsep $
		texts [constructor , "=", show n])

toEnumFn :: Data -> [Doc]
toEnumFn (D{body=body}) = map f (zip body [0 :: Int ..])
	where
	f (Body{constructor=constructor},n) = text "toEnum" <+> (fsep $
		texts [show n , "=", constructor])

enumFromFn :: Data -> Doc
enumFromFn D{body=body} = let
	conList = bracketList . texts . map constructor $ body
	bodydoc = fsep [char 'e', char '=', text "drop",
		parens (text "fromEnum" <+> char 'e'), conList]
	in text "enumFrom" <+> bodydoc

enumFromThenFn ::  Doc
enumFromThenFn = let
	wrapper = fsep $ texts ["i","j","=","enumFromThen\'","i","j","(",
		 "enumFrom", "i", ")"]
	eq1 = text "enumFromThen\'" <+> fsep (texts ["_","_","[]","=","[]"])
	eq2 = text "enumFromThen\'" <+> fsep ( texts ["i","j","(x:xs)","=",
		"let","d","=","fromEnum","j","-","fromEnum","i","in",
		"x",":","enumFromThen\'","i","j","(","drop","(d-1)","xs",")"])
	in text "enumFromThen" <+> wrapper $$ block [text "where",eq1,eq2]

----------------------------------------------------------------------

-- Bounded - as if anyone uses this one :-) ..

boundedfn d@D{name=name,body=body,derives=derives}
	| all (null . types) body  = boundedEnum d
	| singleton body = boundedSingle d
       | otherwise = commentLine $ text "Warning -- can't derive Bounded for"
			<+> text name

boundedEnum d@D{body=body} = let f = constructor . head $ body
			         l = constructor . last $ body
	in simpleInstance "Bounded" d <+> text "where" $$ block [
		hsep (texts[ "minBound","=",f]),
		hsep (texts[ "maxBound","=",l])]

boundedSingle d@D{body=body} = let f = head $ body
	in simpleInstance "Bounded" d <+> text "where" $$ block [
		hsep . texts $ [ "minBound","=",constructor f] ++
			replicate (length (types f)) "minBound",
		hsep . texts $ [ "maxBound","=",constructor f] ++
			replicate (length (types f)) "maxBound"]

singleton [x] = True
singleton _ = False

