module DerivingDrift.StandardRules (standardRules) where

import DerivingDrift.RuleUtils
import List
import GenUtil

tshow = text . show

--- Add Rules Below Here ----------------------------------------------------

standardRules :: [RuleDef]
standardRules = [("test",dattest, "Utility", "output raw data for testing", Nothing),
		  ("update",updatefn, "Utility","for label 'foo' provides 'foo_u' to update it and foo_s to set it", Nothing ),
		  ("is",isfn, "Utility", "provides isFoo for each constructor", Nothing),
		  ("get",getfn, "Utility", "for label 'foo' provide foo_g to get it", Nothing),
	          ("from",fromfn, "Utility", "provides fromFoo for each constructor", Nothing),
		  ("has",hasfn, "Utility", "hasfoo for record types", Nothing),
		  ("un",unfn, "Utility", "provides unFoo for unary constructors", Nothing),
		  ("NFData",nffn, "General","provides 'rnf' to reduce to normal form (deepSeq)", Nothing ),
		  ("Eq",eqfn, "Prelude","", Nothing),
		  ("Ord",ordfn, "Prelude", "", Nothing),
		  ("Enum",enumfn, "Prelude", "", Nothing),
		  ("Show",showfn, "Prelude", "", Nothing),
		  ("Read",readfn, "Prelude", "", Nothing),
		  ("Bounded",boundedfn, "Prelude", "", Nothing)]

-----------------------------------------------------------------------------
-- NFData - This class provides 'rnf' to reduce to normal form.
-- This has a default for non-constructed datatypes
-- Assume that base cases have been defined for lists, functions, and
-- (arbitrary) tuples - makeRnf produces a function which applies rnf to
-- each of the combined types in each constructor of the datatype. (If
-- this isn't very clear, just look at the code to figure out what happens)

nffn = instanceSkeleton "NFData" [(makeRnf,empty)]

makeRnf :: IFunction
makeRnf (Body{constructor=constructor,types=types})
	| null types = text "rnf" <+>
		fsep [pattern constructor [],equals,text "()"]
	| otherwise = let
   vars = varNames types
   head = [pattern constructor vars, equals]
   body =  sepWith (text "`seq`") . map (text "rnf" <+>) $ vars
       in  text "rnf" <+> fsep (head ++  body)


-----------------------------------------------------------------------------
-- Forming 'update' functions for each label in a record
--
-- for a datatype G, where label has type G -> a
-- the corresponding update fn has type (a -> a) -> G -> G
-- The update fn has the same name as the label with _u appended

-- an example of what we want to generate
-- 	--> foo_u f d{foo}=d{foo = f foo}
--
-- labels can be common to more than one constructor in a type. -- this
-- is a problem, and the reason why a sort is used.

updatefn :: Data -> Doc
updatefn d@(D{body=body,name=name})
	| hasRecord d = vcat (updates ++ sets)
	| otherwise = commentLine $
	text "Warning - can't derive `update' functions for non-record type: "
	<+> text name
	where
    nc = length body
    labs = gf $ sort . concatMap f $ body
    updates = map genup labs --  $$  hsep [text (n ++ "_u"), char '_', char 'x', equals, char 'x']
    sets = map genset . nub . map fst $ labs
    f :: Body -> [(Name,Constructor)]
    f (Body{constructor=constructor,labels=labels}) = zip (filter (not . null) labels ) (repeat constructor)
    gf ts = map (\ts -> (fst (head ts), snds ts)) (groupBy (\(a,_) (b,_) -> a == b) (sort ts))

    genup :: (Name,[Constructor]) -> Doc
    genup (n,cs) = vcat (map up cs) $$ up' where
        up c = hsep [text (n ++ "_u") , char 'f'
            , char 'r' <> char '@' <> text c <> braces (text n <+> text " = x")
            , equals , char 'r' <> braces (hsep [text n, text "= f x"])]
        up' | nc > length cs = hsep [text (n ++ "_u"), char '_', char 'x', equals, char 'x']
            | otherwise =  empty

    -- while we're at it, may as well define a set function too...
    genset :: Name -> Doc
    genset n = hsep [text (n ++ "_s v = "), text (n ++ "_u"), text " (const v)"]

getfn :: Data -> Doc
getfn d@(D{body=body,name=name})
	| hasRecord d = vcat (updates ++ sets)
	| otherwise = commentLine $
	text "Warning - can't derive `get' functions for non-record type: "
	<+> text name
	where
    nc = length body
    labs = gf $ sort . concatMap f $ body
    updates = map genup labs
    sets = map genset . nub . map fst $ labs
    f :: Body -> [(Name,Constructor)]
    f (Body{constructor=constructor,labels=labels}) = zip (filter (not . null) labels ) (repeat constructor)
    gf ts = map (\ts -> (fst (head ts), snds ts)) (groupBy (\(a,_) (b,_) -> a == b) (sort ts))

    genup :: (Name,[Constructor]) -> Doc
    genup (n,cs) = vcat (map up cs) $$ up' where
        fn = n ++ "_g"
        up c = hsep [text fn
            , char 'r' <> char '@' <> text c <> braces (text n <+> text " = x")
            , equals , text "return x"]
        up' | nc > length cs = hsep [text fn, char '_',  equals, text "fail", tshow fn]
            | otherwise =  empty

    -- while we're at it, may as well define a set function too...
    genset :: Name -> Doc
    genset n = hsep [text (n ++ "_s v = "), text (n ++ "_u"), text " (const v)"]

----------------------------------------------------------------------
-- Similar rules to provide predicates for the presence of a constructor / label

isfn :: Data -> Doc
isfn (D{body=body}) =  vcat (map is body)
	where	
	is Body{constructor=constructor,types=types} = let
		fnName = text ("is" ++ constructor)
		fn = fnName <+>
			hsep [pattern_ constructor types,text "=",text "True"]
		defaultFn = fnName <+> hsep (texts ["_","=","False"])
		in fn $$ defaultFn

fromfn :: Data -> Doc
fromfn (D{body=body}) =  vcat (map from body) where	
    from Body{constructor=constructor,types=types} = fn $$ defaultFn where
            fnName = ("from" ++ constructor)
            fnName' = text fnName
            fn = fnName' <+>
                    hsep [pattern constructor types,text "=",text "return", tuple (varNames types) ]
            defaultFn = fnName' <+> hsep (texts ["_","=","fail",show fnName ])

hasfn :: Data -> Doc
hasfn d@(D{body=body,name=name})
	| hasRecord d = vcat [has l b | l <- labs, b <- body]
	| otherwise = commentLine $
	    text "Warning - can't derive `has' functions for non-record type:"
	    <+> text name
	where
	has lab Body{constructor=constructor,labels=labels} = let
		bool = text . show $ lab `elem` labels
		pattern = text (constructor ++ "{}")
		fnName = text ( "has" ++ lab)
		in fsep[fnName, pattern, text "=", bool]     	
	labs = nub . concatMap (labels) $  body
		

-- Function to make using newtypes a bit nicer.
-- for newtype N = T a , unN :: T -> a

unfn :: Data -> Doc
unfn (D{body=body,name=name,statement=statement}) | statement == DataStmt
	= commentLine
	  $ text "Warning - can't derive 'un' function for data declaration "
	  <+> text name
			      | otherwise
	= let fnName = text ("un" ++ name)
	      b = head body
	      pattern = parens $ text (constructor b) <+> text "a"
	      in fsep [fnName,pattern, equals, text "a"]


-----------------------------------------------------------------------------
-- A test rule for newtypes datastructures - just outputs
-- parsed information. Can put {-! global : Test !-} in an input file, and output
-- from the entire file should be generated.


dattest d =  commentBlock . vcat $
           [text (name d)
		, fsep . texts . map show $ constraints d
		, fsep . texts . map show $ vars d
	        , fsep . texts . map show $ body d
		, fsep . texts . map show $ derives d
		, text .  show $ statement d]


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
		| c <- zip (body d) [1 ..]
		, c' <- zip (body d) [1 ..]]
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
	listComp x (l:ll) = brackets . fsep . sepWith comma $
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
fromEnumFn (D{body=body}) = map f (zip body [0 ..])
	where
	f (Body{constructor=constructor},n) = text "fromEnum" <+> (fsep $
		texts [constructor , "=", show n])	
		
toEnumFn :: Data -> [Doc]
toEnumFn (D{body=body}) = map f (zip body [0 ..])
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

