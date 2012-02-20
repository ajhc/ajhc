module DerivingDrift.StandardRules (standardRules,driftResolvedNames) where

import DerivingDrift.RuleUtils
import Name.Prim
import Name.Name
import qualified Data.Map as Map

standardRules :: Map.Map Name.Name.Name (Data -> Doc)
standardRules = Map.fromList [
    (class_Eq,eqfn),
    (class_Ord,ordfn),
    (class_Enum,enumfn),
    (class_Show,showfn),
    (class_Read,readfn),
    (class_Bounded,boundedfn)]

-- this list is to be feeded to the renamer in the
-- renaming phase of the derived instances
driftResolvedNames :: [(Name.Name.Name,[Name.Name.Name])]
driftResolvedNames = map unkn stdCls ++ map self stdCls ++ map self stdVals
  where unkn n  = (toName UnknownType (q n), [n])
        self n  = (n,[n])
        stdCls  = Map.keys standardRules
        stdVals = [v_sub,v_compose,
                   dc_True,dc_False,v_and,
                   dc_EQ,v_equals,v_geq,v_gt,v_compare,
                   dc_Pair,
                   dc_EmptyList,dc_Cons,v_foldl,v_cat,v_drop,
                   v_showsPrec,v_showParen,v_showChar,v_showString,
                   v_readsPrec,v_readParen,v_lex,
                   v_fromEnum,v_toEnum,v_enumFrom,v_enumFromThen,
                   v_minBound,v_maxBound]

-- short for qualified and unqualified
q,u :: Name.Name.Name -> String
q = snd . fromName
u = getIdent

------------------------------------------------------------------------------
-- Rules for the derivable Prelude Classes

-- Eq

eqfn = instanceSkeleton (q class_Eq) [(makeEq,defaultEq)]

makeEq :: IFunction
makeEq (Body{constructor=constructor,types=types})
	| null types = hsep $ texts [constructor,u v_equals,constructor, "=", q dc_True]
	| otherwise = let
	v = varNames types
	v' = varNames' types
	d x = parens . hsep $ text constructor : x
	head = [ text (u v_equals), d v', text "="]
	body = sepWith (text $ q v_and) $
		zipWith (\x y -> (x <+> text (q v_equals) <+> y)) v v'
	in d v <+> fsep (head ++  body)

defaultEq = hsep $ texts ["_", u v_equals, "_", "=" ,q dc_False]

----------------------------------------------------------------------

-- Ord

ordfn d = let
   ifn = [f c c'
		| c <- zip (body d) [1 :: Int ..]
		, c' <- zip (body d) [1 :: Int ..]]
   cmp n n' = show $  compare n n'
   f (b,n) (b',n')
	| null (types b) = text (u v_compare) <+>
		   fsep [text (constructor b),
			 pattern (constructor b') (types b')
			, char '=', text $ cmp n n' ]
	| otherwise = let
		      head  = fsep [l,r, char '=']
		      l = pattern (constructor b) (types b)
		      r = pattern' (constructor b') (types b')
		      one x y = fsep [text (q v_compare),x,y]
		      list [x] [y] = one x y
		      list xs ys = fsep [text (q v_foldl), parens fn, text (q dc_EQ),
			           bracketList (zipWith one xs ys)]
		      fn = fsep $ texts  ["\\x y", "->", "if", "x", q v_equals, q dc_EQ,
			   "then", q v_compare, "y", q dc_EQ, "else", "x"]
		in if constructor b == constructor b' then
		    text (u v_compare) <+> fsep [head,
			     list (varNames $ types b) (varNames' $ types b')]
		   else  text (u v_compare) <+> fsep [head,text (cmp n n')]
    in simpleInstance (q class_Ord) d <+> text "where" $$ block ifn


----------------------------------------------------------------------

-- Show & Read
-- 	won't work for infix constructors
--
-- Show

showfn = instanceSkeleton (q class_Show) [(makeShow,empty)]

makeShow :: IFunction
makeShow (Body{constructor=constructor,labels=labels,types=types})
	| null types = fnName <+> fsep [headfn,showString constructor]
	| null labels = fnName <+> fsep [headfn,bodyStart, body]   -- datatype
	| otherwise = fnName <+> fsep[headfn,bodyStart,recordBody] -- record
	where
	fnName = text (u v_showsPrec)
	headfn = fsep [char 'd',(pattern constructor types),equals]
	bodyStart = fsep [text (q v_showParen),parens $ fsep [text "d",text (q v_geq),text "10"]]
	body = parens . fsep $ sepWith s (c : b)
	recordBody = parens $ fsep [c,comp,showChar '{',comp,
				    fsep (sepWith s' b'),comp,showChar '}']
	c = showString constructor
	b = map (\x -> fsep[text (q v_showsPrec), text "10", x]) (varNames types)
	b' = zipWith (\x l -> fsep [showString l, comp, showString " = ", comp, x])
			            b (map getIdent labels)
	s = fsep [comp,showChar ' ', comp]
	s' = fsep [comp,showChar ',',comp]
	showChar c = fsep [text (q v_showChar), text ('\'':c:"\'")]
	showString s = fsep [text (q v_showString), doubleQuotes $ text s]
	comp = text (q v_compose)

-- Read

readfn d = simpleInstance (q class_Read) d <+> text "where" $$ readsPrecFn d

readsPrecFn d = let
	fnName = text (u v_readsPrec)
	bodies = vcat $ sepWith (text $ q v_cat) (map makeRead (body d))
	in nest 4 $ fnName <+> fsep[char 'd', text "input", equals,bodies]

makeRead :: IFunction
makeRead (Body{constructor=constructor,labels=labels,types=types})
	| null types = fsep [read0,text "input"]
	| null labels = fsep [headfn,read,text "input"]
	| otherwise = fsep [headfn,readRecord, text "input"]
	where
	headfn = fsep [text (q v_readParen), parens (text $ unwords ["d",q v_gt,"9"])]
	read0 = lambda $ listComp (result rest) [lexConstr rest]
	read = lambda . listComp (result rest)
		     $ lexConstr ip : ( map f (init vars) )
			++ final (last vars)
        f v = fsep [tup v ip, from,readsPrec, ip]
	final v = [fsep[tup v rest,from,readsPrec,ip]]
	readRecord = let
		f lab v = [
			fsep [tup (tshow $ show (toUnqualified lab)) ip,lex],
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
	tup x y = parens $ fsep [x <> char ',', y]
	lex = fsep[from,text (q v_lex),ip]
	readsPrec = fsep [text (q v_readsPrec),text "10"]
	from = text "<-"


tshow x = text (show x)
----------------------------------------------------------------------

-- Enum -- a lot of this code should be provided as default instances,
-- 	 but currently isn't

enumfn d = let
	fromE = fromEnumFn d
	toE = toEnumFn d
	eFrom = enumFromFn d
	in if any (not . null . types) (body d)
	   then commentLine $ text "Warning -- can't derive Enum for"
				<+> text (getIdent $ name d)
	   else simpleInstance (q class_Enum) d <+> text "where"
		$$ block (fromE ++ toE ++ [eFrom,enumFromThenFn])

fromEnumFn :: Data -> [Doc]
fromEnumFn (D{body=body}) = map f (zip body [0:: Int ..])
	where
	f (Body{constructor=constructor},x) = text (u v_fromEnum) <+> (fsep $
		texts [constructor , "=", show x])

toEnumFn :: Data -> [Doc]
toEnumFn (D{body=body}) = map f (zip body [0 :: Int ..])
	where
	f (Body{constructor=constructor},x) = text (u v_toEnum) <+> (fsep $
		texts [show x , "=", constructor])

enumFromFn :: Data -> Doc
enumFromFn D{body=body} = let
	conList = bracketList . texts . map constructor $ body
	bodydoc = fsep [char 'e', char '=', text (q v_drop),
		parens (text (q v_fromEnum) <+> char 'e'), conList]
	in text (u v_enumFrom) <+> bodydoc

enumFromThenFn ::  Doc
enumFromThenFn = let
	wrapper = fsep $ texts ["i","j","=","enumFromThen\'","i","j","(",
		 q v_enumFrom, "i", ")"]
	eq1 = text "enumFromThen\'"
                <+> fsep (texts ["_","_",u dc_EmptyList,"=",u dc_EmptyList])
	eq2 = text "enumFromThen\'"
                <+> fsep (texts ["i","j","(x",u dc_Cons,"xs)","="])
                <+> fsep [hsep $ texts [
                           "let", "d", "=",
                                q v_fromEnum,"j",q v_sub,q v_fromEnum,"i"],
                          text "in" <+> fsep (texts [
                             "x",u dc_Cons,"enumFromThen\'","i","j","(", q v_drop]
                                ++ [parens . hsep $ texts ["d",q v_sub,"1"]]
                                ++ [text "xs",text ")"])]
	in text (q v_enumFromThen) <+> wrapper $$ block [text "where",eq1,eq2]

----------------------------------------------------------------------

-- Bounded - as if anyone uses this one :-) ..

boundedfn d@D{name=name,body=body,derives=derives}
	| all (null . types) body  = boundedEnum d
	| singleton body = boundedSingle d
       | otherwise = commentLine $ text "Warning -- can't derive Bounded for"
			<+> (text $ getIdent name)

boundedEnum d@D{body=body} = let f = constructor . head $ body
			         l = constructor . last $ body
	in simpleInstance (q class_Bounded) d <+> text "where" $$ block [
		hsep (texts [u v_minBound,"=",f]),
		hsep (texts [u v_maxBound,"=",l])]

boundedSingle d@D{body=body} = let f = head $ body
	in simpleInstance (q class_Bounded) d <+> text "where" $$ block [
		hsep . texts $ [u v_minBound,"=",constructor f] ++
			replicate (length (types f)) (q v_minBound),
		hsep . texts $ [u v_maxBound,"=",constructor f] ++
			replicate (length (types f)) (q v_maxBound)]

singleton [x] = True
singleton _ = False
