-- stub module to add your own rules.
module UserRuleXml (userRulesXml) where

import List (nub,sortBy)
import DerivingDrift.RuleUtils -- useful to have a look at this too

userRulesXml :: [RuleDef]
userRulesXml = [("Haskell2Xml", userRuleXml, "Representation", "encode terms as XML", Nothing)]

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

userRuleXml dat =
  let cs  = body dat
      cvs = mknss cs namesupply
  in
  instanceheader "Haskell2Xml" dat $$
  block (toHTfn cs cvs dat:
         ( text "fromContents (CElem (Elem constr [] cs):etc)" $$
           vcat (preorder cs (zipWith3 readsfn [0..] cvs cs))):
         zipWith3 showsfn [0..] cvs cs)

toHTfn cs cvs dat =
  let typ  = name dat
      fvs  = vars dat
      pats = concat (zipWith mkpat cvs cs)
  in
  text "toHType v =" $$
  nest 4 (
    text "Defined" <+>
    fsep [ text "\"" <> text typ <> text "\""
         , bracketList (map text fvs)
         , bracketList (zipWith toConstr cvs cs)
         ]
    ) $$
  if null pats then empty
  else nest 2 (text "where") $$
       nest 4 (vcat (map (<+> text "= v") pats)) $$
       nest 4 (vcat (map (simplest typ (zip cvs cs)) fvs))

namesupply   = [text [x,y] | x <- ['a' .. 'z'],
                             y <- ['a' .. 'z'] ++ ['A' .. 'Z']]

mknss []     _  = []
mknss (c:cs) ns =
  let (thisns,rest) = splitAt (length (types c)) ns
  in thisns: mknss cs rest

mkpat ns c =
  if null ns then []
  else [mypattern (constructor c) (types c) ns]


toConstr :: [Doc] -> Body -> Doc
toConstr ns c =
  let cn = constructor c
      ts = types c
      fvs = nub (concatMap deepvars ts)
  in
  text "Constr" <+>
  fsep [ text "\"" <> text cn <> text "\""
       , bracketList (map text fvs)
       , bracketList (map (\v-> text "toHType" <+> v) ns)
       ]

  where

    deepvars (Arrow t1 t2)  = []
    --deepvars (Apply t1 t2)  = deepvars t1 ++ deepvars t2
    deepvars (LApply c ts)  = concatMap deepvars ts
    deepvars (Var s)        = [s]
    deepvars (Con s)        = []
    deepvars (Tuple ts)     = concatMap deepvars ts
    deepvars (List t)       = deepvars t

--first [] fv = error ("cannot locate free type variable "++fv)
--first ((ns,c):cs) fv =
--  let npats = [ (n,pat) | (n,t) <- zip ns (types c)
--                        , (True,pat) <- [ find fv t ]
--              ]
--  in
--  if null npats then
--       first cs fv
--  else let (n,pat) = head npats
--       in parens pat <+> text "= toHType" <+> n
--
--  where
--
--    find :: String -> Type -> (Bool,Doc)
--    find v (Arrow t1 t2)  = (False,error "can't ShowXML for arrow type")
--    find v (Apply t1 t2)  = let (tf1,pat1) = find v t1
--                                (tf2,pat2) = find v t2
--                            in perhaps (tf1 || tf2)
--                                       (pat1 <+> snd (perhaps tf2 pat2))
--    find v (LApply c ts)  = let (_,cpat) = find v c
--                                tfpats = map (find v) ts
--                                (tfs,pats) = unzip tfpats
--                            in perhaps (or tfs)
--                                       (parens (cpat <+>
--                                                bracketList (map (snd.uncurry perhaps) tfpats)))
--    find v (Var s)        = perhaps (v==s) (text v)
--    find v (Con s)        = (False, text "Defined" <+>
--                                    text "\"" <> text s <> text "\"")
--    find v (Tuple ts)     = let tfpats = map (find v) ts
--                                (tfs,pats) = unzip tfpats
--                            in perhaps (or tfs)
--                                       (parens (text "Tuple" <+>
--                                                bracketList (map (snd.uncurry perhaps) tfpats)))
--    find v (List t)       = let (tf,pat) = find v t
--                            in perhaps tf (parens (text "List" <+> pat))
--    perhaps tf doc = if tf then (True,doc) else (False,text "_")

simplest typ cs fv =
  let npats = [ (depth,(n,pat)) | (ns,c) <- cs
                                , (n,t) <- zip ns (types c)
                                , (depth, pat) <- [ find fv t ]
              ]
      (_,(n,pat)) = foldl closest (Nothing,error "free tyvar not found") npats
  in
  parens pat <+> text "= toHType" <+> n

  where

    find :: String -> Type -> (Maybe Int,Doc)
    find v (Arrow t1 t2)  = (Nothing,error "can't derive Haskell2XML for arrow type")
--    find v (Apply t1 t2)  = let (d1,pat1) = find v t1
--                                (d2,pat2) = find v t2
--                            in perhaps (combine [d1,d2])
--                                       (pat1 <+> snd (perhaps d2 pat2))
    find v (LApply c ts)
        | c == (Con typ)  = (Nothing, text "_")
        | otherwise       = let (_,cpat)  = find v c
                                dpats     = map (find v) ts
                                (ds,pats) = unzip dpats
                            in perhaps (combine ds)
                                       (cpat <+>
                                        bracketList (map (snd.uncurry perhaps) dpats) <+>
                                        text "_")
    find v (Var s)        = perhaps (if v==s then Just 0 else Nothing) (text v)
    find v (Con s)        = (Nothing, text "Defined" <+>
                                      text "\"" <> text s <> text "\"")
    find v (Tuple ts)     = let dpats = map (find v) ts
                                (ds,pats) = unzip dpats
                            in perhaps (combine ds)
                                       (text "Tuple" <+>
                                        bracketList (map (snd.uncurry perhaps) dpats))
    find v (List t)       = let (d,pat) = find v t
                            in perhaps (inc d) (text "List" <+> parens pat)

    perhaps Nothing doc   = (Nothing, text "_")
    perhaps jn doc        = (jn,doc)
    combine ds   = let js = [ n | (Just n) <- ds ]
                   in if null js then Nothing else inc (Just (minimum js))
    inc Nothing  = Nothing
    inc (Just n) = Just (n+1)

    closest :: (Maybe Int,a) -> (Maybe Int,a) -> (Maybe Int,a)
    closest (Nothing,_)  b@(Just _,_) = b
    closest a@(Just n,_) b@(Just m,_) | n< m  = a
                                      | m<=n  = b
    closest a b = a



showsfn n ns cn =
  let cons = constructor cn
      typ  = types cn
      sc   = parens (text "showConstr" <+> text (show n) <+>
                     parens (text "toHType" <+> text "v"))
      cfn []  = text "[]"
      cfn [x] = parens (text "toContents" <+> x)
      cfn xs  = parens (text "concat" <+> bracketList (map (text "toContents" <+>) xs))
  in
  text "toContents" <+>
  text "v@" <> mypattern cons typ ns <+> text "=" $$
  nest 4 (text "[mkElemC" <+> sc <+> cfn ns <> text "]")

----
--  text "fromContents (CElem (Elem constr [] cs):etc)" $$
----
readsfn n ns cn =
  let cons   = text (constructor cn)
      typ    = types cn
      num    = length ns - 1
      str d  = text "\"" <> d <> text "\""
      trails = take num (map text [ ['c','s',y,z] | y <- ['0'..'9']
                                                  , z <- ['0'..'9'] ])
      cfn x  = parens (text "fromContents" <+> x)
      (init,[last]) = splitAt num ns
      something = parens (
                    text "\\" <> parenList [last, text "_"] <> text "->" <+>
                    parens (cons <+> hsep ns <> text "," <+> text "etc") )
      mkLambda (n,cv) z = parens (
                            text "\\" <> parenList [n,cv] <> text "->" <+>
                            fsep [z, cfn cv] )
  in
  nest 4 (
    text "|" <+> str cons <+> text "`isPrefixOf` constr =" $$
    nest 4 (
      if null ns then parenList [cons, text "etc"]
      else fsep [ foldr mkLambda something (zip init trails)
                , cfn (text "cs")]
    )
  )
  -- Constructors are matched with "isPrefixOf" rather than "=="
  -- because of parametric polymorphism.  For a datatype
  --        data A x = A | B x
  -- the XML tags will be <A>, <B-Int>, <B-Bool>, <B-Maybe-Char> etc.
  -- However prefix-matching presents a problem for types like
  --        data C = C | CD
  -- because (C `isPrefixOf`) matches both constructors.  The solution
  -- (implemented by "preorder") is to order the constructors such that
  -- <CD> is matched before <C>.

preorder cs =
    map snd . reverse . sortBy (\(a,_) (b,_)-> compare a b) . zip (map constructor cs)


--

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

mypattern :: Constructor -> [a] -> [Doc] -> Doc
mypattern c l ns =
  if null l then text c
  else parens (hsep (text c : take (length l) ns))
