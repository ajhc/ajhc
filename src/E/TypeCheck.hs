module E.TypeCheck(
    canBeBox,
    eAp,
    inferType,
    match,
    sortSortLike,
    sortKindLike,
    sortTermLike,
    sortTypeLike,
    typeInfer,
    typeInfer'
    ) where

import Control.Monad.Reader
import Control.Monad.Writer
import qualified Data.Map as Map

import Doc.DocLike
import Doc.PPrint
import Doc.Pretty
import E.E
import E.Eval(strong)
import E.Subst
import GenUtil
import Name.Id
import Name.Name
import Name.Names
import Support.CanType
import Util.ContextMonad
import Util.SetLike
import qualified Util.Seq as Seq
import {-# SOURCE #-} DataConstructors
import {-# SOURCE #-} E.Show

{-@Internals

# Jhc Core Type System

Jhc's core is based on a pure type system. A pure type system (also called a
PTS) is actually a parameterized set of type systems. Jhc's version is
described by the following.

    Sorts  = (*, !, **, #, (#), ##, □)
    Axioms = (*:**, #:##, !:**, **:□, ##:□)

    -- sort kind
    *   is the kind of boxed values
    !   is the kind of boxed strict values
    #   is the kind of unboxed values
    (#) is the kind of unboxed tuples
    -- sort superkind
    **  is the superkind of all boxed value
    ##  is the superkind of all unboxed values
    -- sort box
    □   superkinds inhabit this

    in addition there exist user defined kinds, which are always of supersort ##

The following Rules table shows what sort of abstractions are allowed, a rule
of the form (A,B,C) means you can have functions of things of sort A to things
of sort B and the result is something of sort C. _Function_ in this context
subsumes both term and type level abstractions.

Notice that functions are always boxed, but may be strict if they take an
unboxed tuple as an argument.  When a function is strict it means that it is
represented by a pointer to code directly, it cannot be a suspended value that
evaluates to a function.

These type system rules apply to lambda abstractions. It is possible that data
constructors might exist that cannot be given a type on their own with these
rules, even though when fully applied it has a well formed type. An example
would be unboxed tuples. This presents no difficulty as one concludes correctly
that it is a type error for these constructors to ever appear when not fully
saturated with arguments.

    as a shortcut we will use *# to mean every combination involving * and #, and so forth.
    for instance, (*#,*#,*) means the set (*,*,*) (#,*,*) (*,#,*) (#,#,*)

    Rules =
       (*#!,*#!,*)  -- functions from values to values are boxed and lazy
       (*#!,(#),*)  -- functions from values to unboxed tuples are boxed and lazy
       ((#),*#!,!)  -- functions from unboxed tuples to values are boxed and strict
       ((#),(#),!)  -- functions from unboxed tuples to unboxed tuples are boxed and strict
       (**,*,*)     -- may have a function from an unboxed type to a value
       (**,#,*)
       (**,!,*)
       (**,**,**)  -- we have functions from types to types
       (**,##,##)  -- Array__ a :: #

    The defining feature of boxed values is

    _|_ :: t iff t::*

    This PTS is functional but not injective

The PTS can be considered stratified into the following levels

    □                - sort box
    **,##,           - sort superkind
    *,#,(#),!        - sort kind
    Int,Bits32_,Char - sort type
    3,True,"bob"     - sort value

## On boxed kinds

The boxed kinds (* and !) represent types that have a uniform run time
representation. Due to this, functions may be written that are polymorphic in types of these kinds.
Hence the rules of the form (**,?,?), allowing taking types of boxed kinds as arguments.

the unboxed kind # is inhabited with types that have their own specific run
time representation. Hence you cannot write functions that are polymorphic in
unboxed types

## On sort box, the unboxed tuple, and friends

Although sort box does not appear in the code, it is useful from a theoretical
point of view to talk about certain types such as the types of unboxed tuples.
Unboxed tuples may have boxed and unboxed arguments, without sort box it would
be impossible to express this since it must be superkind polymorphic. sort box
allows one to express this as (in the case of the unboxed 2-tuple)

    ∀s1:□ ∀s2:□ ∀k1:s1 ∀k2:s2 ∀t1:k1 ∀t2:k2 . (# t1, t2 #)

However, although this is a valid typing of what it would mean if a unboxed
tuple were not fully applied, since we do not have any rules of form (##,?,?) or
(□,?,?) this type obviously does not typecheck. Which is what enforces the
invarient that unboxed tuples are always fully applied, and is also why we do
not need a code representation of sort box.

### Do we need a superbox?

You will notice that if you look at the axioms involving the sorts, you end up
with a disjoint graph

             □             - the box
            / \
          **   ##          - superkind
          /\     \
         *  !     #   (#)  - kind

This is simply due to the fact that nothing is polymorphic in unboxed tuples of
kind (#) so we never need to refer to any super-sorts of them. We can add sorts
(##),(□) and □□ to fill in the gaps, but since these sorts will never appear in
code or discourse, we will ignore them from now on.

               □□            - sort superbox
              /  \
             □    (□)        - sort box
            / \      \
          **   ##     (##)   - sort superkind
          /\     \    |
         *  !     #   (#)    - sort kind

-}

ptsAxioms :: Map.Map ESort ESort
ptsAxioms = Map.fromList [
    (EStar,EStarStar),
    (EBang,EStarStar),
    (EHash,EHashHash),
    (ETuple,EHashHash)
    ]

ptsRulesMap :: Map.Map (ESort,ESort) ESort
ptsRulesMap = Map.fromList [ ((a,b),c) | (as,bs,c) <- ptsRules, a <- as, b <- bs  ] where
    starHashBang = [EStar,EHash,EBang]
    ptsRules = [
        (starHashBang,ETuple:starHashBang,EStar),
        ([ETuple],ETuple:starHashBang,EBang),
        ([EStarStar],starHashBang,EStar),
        ([EStarStar],[EStarStar],EStarStar),
        ([EStarStar],[EHashHash],EHashHash)
        ]

canBeBox x | getType (getType x) == ESort EStarStar = True
canBeBox _ = False

tBox = mktBox eStar

monadicLookup key m = case Map.lookup key m of
    Just x  -> return x
    Nothing -> fail "Key not found"

-- Fast (and lazy, and perhaps unsafe) typeof
instance CanType E E where
    getType (ESort s) = ESort $ getType s
    getType (ELit l) = getType l
    getType (EVar v) =  getType v
    getType e@(EPi TVr { tvrType = a } b)
        | isUnknown typa || isUnknown typb = Unknown
        | otherwise = maybe (error $ "E.TypeCheck.getType: " ++ show (e,getType a,getType b)) ESort $ do
            ESort s1 <- return $ getType a
            ESort s2 <- return $ getType b
            monadicLookup (s1,s2) ptsRulesMap
        where typa = getType a; typb = getType b
    getType (EAp (ELit LitCons { litType = EPi tvr a }) b) = getType (subst tvr b a)
    getType (EAp (ELit lc@LitCons { litAliasFor = Just af }) b) = getType (foldl eAp af (litArgs lc ++ [b]))
    getType (EAp (EPi tvr a) b) = getType (subst tvr b a)
    getType e@(EAp a b) = ans where
        ans = if isUnknown typa then Unknown else if a == tBox || typa == tBox then tBox else (case a of
            (ELit LitCons {}) -> error $ "getType: application of type alias " ++ (render $ parens $ ePretty e)
            _ -> eAp typa b)
        typa = getType a
    getType (ELam (TVr { tvrIdent = x, tvrType =  a}) b) = EPi (tVr x a) (getType b)
    getType (ELetRec _ e) = getType e
    getType ECase {eCaseType = ty} = ty
    getType (EError _ e) = e
    getType (EPrim _ _ t) = t
    getType Unknown = Unknown

instance CanType ESort ESort where
    getType (ESortNamed _) = EHashHash
    getType s = case Map.lookup s ptsAxioms of
        Just s -> s
        Nothing -> error $ "getType: " ++ show s
instance CanType TVr E where
    getType = tvrType
instance CanType (Lit x t) t where
    getType l = litType l
instance CanType e t => CanType (Alt e) t where
    getType (Alt _ e) = getType e

sortSortLike (ESort s) = isEHashHash s || isEStarStar s
sortSortLike _ = False

sortKindLike (ESort s) =  not (isEHashHash s) && not (isEStarStar s)
sortKindLike e = sortSortLike (getType e)

sortTypeLike ESort {} = False
sortTypeLike e = sortKindLike (getType e)

sortTermLike ESort {} = False
sortTermLike e = sortTypeLike (getType e)

withContextDoc s a = withContext (render s) a

-- | Perform a full typecheck, evaluating type terms as necessary.

inferType :: ContextMonad String m => DataTable -> [(TVr,E)] -> E -> m E
inferType dataTable ds e = rfc e where
    inferType' ds e = inferType dataTable ds e
    prettyE = ePretty
    rfc e =  withContextDoc (text "fullCheck:" </> prettyE e) (fc e >>= strong')
    rfc' nds e = withContextDoc (text "fullCheck':" </> prettyE e) (inferType' nds e)
    strong' e = withContextDoc (parens $ text "Strong:" </> prettyE e) $ strong ds e
    fc s@(ESort _) = return $ getType s
    fc (ELit lc@LitCons {}) | let lc' = updateLit dataTable lc, litAliasFor lc /= litAliasFor lc' = fail $ "Alias not correct: " ++ show (lc, litAliasFor lc')
    fc (ELit LitCons { litName = n, litArgs = es, litType =  t}) | nameType n == TypeConstructor, Just _ <- fromUnboxedNameTuple n = do
        withContext ("Checking Unboxed Tuple: " ++ show n) $ do
        -- we omit kind checking for unboxed tuples
        valid t
        es' <- mapM rfc es
        strong' t
    fc e@(ELit LitCons { litName = n, litArgs = es, litType =  t}) = do
        withContext ("Checking Constructor: " ++ show e) $ do
        valid t
        es' <- mapM rfc es
        t' <- strong' t
        let sts = slotTypes dataTable n t
            les = length es
            lsts = length sts
        withContext ("Checking Args: " ++ show (sts,es')) $ do
        unless (les == lsts || (les < lsts && isEPi t')) $ do
            fail "constructor with wrong number of arguments"
        zipWithM_ eq sts es'
        return t'
    fc e@(ELit _) = let t = getType e in valid t >> return t
    fc (EVar (TVr { tvrIdent = eid })) | eid == emptyId = fail "variable with nothing!"
    fc (EVar (TVr { tvrType =  t})) = valid t >> strong' t
    fc (EPi (TVr { tvrIdent = n, tvrType =  at}) b) = do
        ESort a <- rfc at
        ESort b <- rfc' [ d | d@(v,_) <- ds, tvrIdent v /= n ] b
        liftM ESort $ monadicLookup (a,b) ptsRulesMap
        --valid at >> rfc' [ d | d@(v,_) <- ds, tvrIdent v /= n ] b
    --fc (ELam tvr@(TVr n at) b) = valid at >> rfc' [ d | d@(v,_) <- ds, tvrIdent v /= n ] b >>= \b' -> (strong' $ EPi tvr b')
    fc (ELam tvr@(TVr { tvrIdent = n, tvrType =  at}) b) = do
        withContext "Checking Lambda" $ do
        valid at
        b' <- withContext "Checking Lambda Body" $ rfc' [ d | d@(v,_) <- ds, tvrIdent v /= n ] b
        withContext "Checking lambda pi" $ strong' $ EPi tvr b'
    fc (EAp (EPi tvr e) b) = rfc (subst tvr b e)
    fc (EAp (ELit lc@LitCons { litAliasFor = Just af }) b) = rfc (EAp (foldl eAp af (litArgs lc)) b)
    fc (EAp a b) = do
        withContextDoc (text "EAp:" </> parens (prettyE a) </> parens (prettyE b)) $ do
            a' <- rfc a
            if a' == tBox then return tBox else strong' (eAp a' b)
    fc (ELetRec vs e) = do
        let ck (TVr { tvrIdent = eid },_) | eid == emptyId = fail "binding of empty var"
            ck (tv@(TVr { tvrType =  t}),e) = withContextDoc (hsep [text "Checking Let: ", parens (pprint tv),text  " = ", parens $ prettyE e ])  $ do
                when (getType t == eHash && not (isEPi t)) $ fail $ "Let binding unboxed value: " ++ show (tv,e)
                valid' nds t
                fceq nds e t
            nds = vs ++ ds
        mapM_ ck vs
        when (hasRepeatUnder (tvrIdent . fst) vs) $ fail "Repeat Variable in ELetRec"
        inferType' nds e
        --et <- inferType' nds e
        --strong nds et
    fc (EError _ e) = valid e >> (strong'  e)
    fc (EPrim _ ts t) = mapM_ valid ts >> valid t >> ( strong' t)
    fc ec@ECase { eCaseScrutinee = e@ELit {}, eCaseBind = b, eCaseAlts = as, eCaseType = dt } | sortTypeLike e = do   -- TODO - this is a hack to get around case of constants.
        withContext "Checking typelike pattern binding case" $ do
        et <- rfc e
        withContext "Checking typelike default binding" $ eq et (getType b)
        verifyPats (casePats ec)
        -- skip checking alternatives
        ps <- mapM (strong' . getType) $ casePats ec
        withContext "Checking typelike pattern equality" $  eqAll (et:ps)
        strong' dt
    fc ec@ECase {eCaseScrutinee = e, eCaseBind = b, eCaseAlts = as, eCaseType = dt } | sortTypeLike e  = do   -- TODO - we should substitute the tested for value into the default type.
        withContext "Checking typelike binding case" $ do
        et <- rfc e
        withContext "Checking typelike default binding" $ eq et (getType b)
        --dt <- rfc d
        --bs <- mapM rfc (caseBodies ec)  -- these should be specializations of dt
        withContext "Checking typelike alternatives" $ mapM_ (calt e) as
        --eqAll bs
        verifyPats (casePats ec)
        ps <- withContext "Getting pattern types" $ mapM (strong' . getType) $ casePats ec
        withContext "checking typelike pattern equality" $ eqAll (et:ps)
        withContext "Evaluating Case Type" $ strong' dt
    fc ec@ECase { eCaseScrutinee =e, eCaseBind = b } = do
        withContext "Checking plain case" $ do
        et <- rfc e
        withContext "Checking default binding" $ eq et (getType b)
        bs <- withContext "Checking case bodies" $ mapM rfc (caseBodies ec)
        ect <- strong' (eCaseType ec)
        withContext "Checking case bodies have equal types" $ eqAll (ect:bs)
        verifyPats (casePats ec)
        ps <- mapM (strong' . getType) $ casePats ec
        withContext "checking pattern equality" $ eqAll (et:ps)
        return ect
    fc Unknown = return Unknown
    fc e = failDoc $ text "what's this? " </> (prettyE e)
    calt (EVar v) (Alt l e) = do
        let nv =  followAliases undefined (patToLitEE l)
        rfc (subst' v nv e)
    calt _ (Alt _ e) = rfc e
    verifyPats xs = do
        mapM_ verifyPats' xs
        when (hasRepeatUnder litHead xs) $ fail "Duplicate case alternatives"

    verifyPats' LitCons { litArgs = xs } = when (hasRepeatUnder id (filter (/= emptyId) $ map tvrIdent xs)) $ fail "Case pattern is non-linear"
    verifyPats' _ = return ()

    eqAll ts = withContextDoc (text "eqAll" </> list (map prettyE ts)) $ foldl1M_ eq ts
    valid s = valid' ds s
    valid' nds ESort {} = return ()
    valid' nds s
        | Unknown <- s = return ()
        | otherwise =  withContextDoc (text "valid:" <+> prettyE s) (do t <- inferType' nds s;  valid' nds t)
    eq box t2 | boxCompat box t2 = return t2
    eq t1 box | boxCompat box t1 = return t1
   -- box == tBox, canBeBox t2 = return t2
   -- eq t1 box | box == tBox, canBeBox t1 = return t1
    eq Unknown t2 = return t2
    eq t1 Unknown = return t1
    eq t1 t2 = eq' ds t1 t2
    eq' nds t1 t2 = do
        e1 <- strong nds (t1)
        e2 <- strong nds (t2)
        case typesCompatable dataTable e1 e2 of
            Right () -> return (e1)
            Left s -> failDoc $ text "eq:" <+> align $ vcat [ text s, prettyE (e1), prettyE (e2) ]
    fceq nds e1 t2 = do
        withContextDoc (hsep [text "fceq:", align $ vcat [parens $ prettyE e1,  parens $ prettyE t2]]) $ do
        t1 <- inferType' nds e1
        eq' nds t1 t2
    boxCompat (ELit (LitCons { litName = n }))  t | Just e <- fromConjured modBox n =  e == getType t
    boxCompat _ _ = False

instance CanTypeCheck DataTable E E where
    typecheck dataTable e = case typeInfer'' dataTable [] e of
        Left ss -> fail $ "\n>>> internal error:\n" ++ unlines ss
        Right v -> return v

instance CanTypeCheck DataTable TVr E where
    typecheck dt tvr = do
        typecheck dt (getType tvr)
        return $ getType tvr

instance CanTypeCheck DataTable (Lit a E) E where
    typecheck  dt LitCons { litType = t } = typecheck dt t >> return t
    typecheck  dt LitInt  { litType = t } = typecheck dt t >> return t

-- TODO, types might be bound in scrutinization
instance CanTypeCheck DataTable (Alt E) E where
    typecheck dt (Alt l e) = typecheck dt l >> typecheck dt e

instance CanTypeCheck DataTable [(TVr,E)] [E] where
    typecheck dataTable ds = do mapM (typecheck dataTable) (snds ds)

-- | Determine type of term using full algorithm with substitutions. This
-- should be used instead of 'typ' when let-bound type variables exist or you
-- wish a more thorough checking of types.

typeInfer :: DataTable -> E -> E
typeInfer dataTable e = case typeInfer'' dataTable [] e of
    Left ss -> error $ "\n>>> internal error:\n" ++ unlines (tail ss)
    Right v -> v

typeInfer' :: DataTable -> [(TVr,E)] -> E -> E
typeInfer' dataTable ds e = case typeInfer'' dataTable ds e of
    Left ss -> error $ "\n>>> internal error:\n" ++ unlines (tail ss)
    Right v -> v

data TcEnv = TcEnv {
    tcDefns :: [(TVr,E)],
    tcContext :: [String],
    tcDataTable :: DataTable
    }
   {-! derive: update !-}

newtype Tc a = Tc (Reader TcEnv a)
    deriving(Monad,Functor,MonadReader TcEnv)

instance ContextMonad String Tc where
    withContext s = local (tcContext_u (s:))

tcE :: E -> Tc E
tcE e = rfc e where
    rfc e =  withContextDoc (text "tcE:" </> ePretty e) (fc e >>=  strong')
    strong' e = do
        ds <- asks tcDefns
        withContextDoc (text "tcE.strong:" </> ePretty e) $ strong ds e

    fc s@ESort {} = return $ getType s
    fc (ELit LitCons { litType = t }) = strong' t
    fc e@ELit {} = strong' (getType e)
    fc (EVar TVr { tvrIdent = eid }) | eid == emptyId = fail "variable with nothing!"
    fc (EVar TVr { tvrType =  t}) =  strong' t
    fc (EPi TVr { tvrIdent = n, tvrType = at} b) =  do
        ESort a <- rfc at
        ESort b <- local (tcDefns_u (\ds -> [ d | d@(v,_) <- ds, tvrIdent v /= n ])) $ rfc b
        liftM ESort $ monadicLookup (a,b) ptsRulesMap
    fc (ELam tvr@TVr { tvrIdent = n, tvrType =  at} b) = do
        at' <- strong' at
        b' <- local (tcDefns_u (\ds -> [ d | d@(v,_) <- ds, tvrIdent v /= n ])) $ rfc b
        return (EPi (tVr n at') b')
    fc (EAp (EPi tvr e) b) = do
        b <- strong' b
        rfc (subst tvr b e)
    fc (EAp (ELit lc@LitCons { litAliasFor = Just af }) b) = fc (EAp (foldl eAp af (litArgs lc)) b)
    fc (EAp a b) = do
        a' <- rfc a
        if a' == tBox then return tBox else strong' (eAp a' b)
    fc (ELetRec vs e) = local (tcDefns_u (vs ++)) $ rfc e
    fc (EError _ e) = strong' e
    fc (EPrim _ ts t) = strong' t
    fc ECase { eCaseType = ty } = do
        strong' ty
    fc Unknown = return Unknown
    fc e = failDoc $ text "what's this? " </> (ePretty e)

typeInfer'' :: ContextMonad String m => DataTable -> [(TVr,E)] -> E -> m E
typeInfer'' dataTable ds e = rfc e where
    inferType' ds e = typeInfer'' dataTable ds e
    rfc e =  withContextDoc (text "fullCheck':" </> ePretty e) (fc e >>= strong')
    rfc' nds e =  withContextDoc (text "fullCheck':" </> ePretty e) (inferType' nds e)
    strong' e = withContextDoc (text "Strong':" </> ePretty e) $ strong ds e
    fc s@ESort {} = return $ getType s
    fc (ELit LitCons { litType = t }) = strong' t
    fc e@ELit {} = strong' (getType e)
    fc (EVar TVr { tvrIdent = eid }) | eid == emptyId = fail "variable with nothing!"
    fc (EVar TVr { tvrType =  t}) =  strong' t
    fc (EPi TVr { tvrIdent = n, tvrType = at} b) =  do
        ESort a <- rfc at
        ESort b <- rfc' [ d | d@(v,_) <- ds, tvrIdent v /= n ] b
        liftM ESort $ monadicLookup (a,b) ptsRulesMap
    fc (ELam tvr@TVr { tvrIdent = n, tvrType =  at} b) = do
        at' <- strong' at
        b' <- rfc' [ d | d@(v,_) <- ds, tvrIdent v /= n ] b
        return (EPi (tVr n at') b')
    fc (EAp (EPi tvr e) b) = do
        b <- strong' b
        rfc (subst tvr b e)
    fc (EAp (ELit lc@LitCons { litAliasFor = Just af }) b) = fc (EAp (foldl eAp af (litArgs lc)) b)
    fc (EAp a b) = do
        a' <- rfc a
        if a' == tBox then return tBox else strong' (eAp a' b)
    fc (ELetRec vs e) = do
        let nds = vs ++ ds
        --et <- inferType' nds e
        --strong nds et
        inferType' nds e
    fc (EError _ e) = strong' e
    fc (EPrim _ ts t) = strong' t
    fc ECase { eCaseType = ty } = do
        strong' ty
    fc Unknown = return Unknown
    fc e = failDoc $ text "what's this? " </> (ePretty e)

-- | find substitution that will transform the left term into the right one,
-- only substituting for the vars in the list

match :: Monad m =>
    (Id -> Maybe E)      -- ^ function to look up values in the environment
    -> [TVr]              -- ^ vars which may be substituted
    -> E                  -- ^ pattern to match
    -> E                  -- ^ input expression
    -> m [(TVr,E)]
match lup vs = \e1 e2 -> liftM Seq.toList $ execWriterT (un e1 e2 etherealIds) where
    bvs :: IdSet
    bvs = fromList (map tvrIdent vs)

    un (EAp a b) (EAp a' b') c = do
        un a a' c
        un b b' c
    un (ELam va ea) (ELam vb eb) c = lam va ea vb eb c
    un (EPi va ea) (EPi vb eb) c = lam va ea vb eb c
    un (EPi va ea) (ELit LitCons { litName = ar, litArgs = [x,y], litType = lt}) c | ar == tc_Arrow = do
        un (tvrType va) x c
        un ea y c
    un (EPrim s xs t) (EPrim s' ys t') c | length xs == length ys = do
        sequence_ [ un x y c | x <- xs | y <- ys]
        un t t' c
    un (ESort x) (ESort y) c | x == y = return ()
    un (ELit (LitInt x t1))  (ELit (LitInt y t2)) c | x == y = un t1 t2 c
    un (ELit LitCons { litName = n, litArgs = xs, litType = t })  (ELit LitCons { litName = n', litArgs = ys, litType =  t'}) c | n == n' && length xs == length ys = do
        sequence_ [ un x y c | x <- xs | y <- ys]
        un t t' c

    un (EVar TVr { tvrIdent = i, tvrType =  t}) (EVar TVr {tvrIdent = j, tvrType =  u}) c | i == j = un t u c
    un (EVar TVr { tvrIdent = i, tvrType =  t}) (EVar TVr {tvrIdent = j, tvrType =  u}) c | isEtherealId i || isEtherealId j   = fail "Expressions don't match"
    un (EAp a b) (ELit lc@LitCons { litArgs = bas@(_:_), litType = t }) c = do
        let (al:as) = reverse bas
        un a (ELit lc { litArgs = reverse as, litType = ePi tvr { tvrType = getType al } t }) c
        un b al c
    un (EAp a b) (EPi TVr { tvrType = a1 } a2) c = do
        un a (ELit litCons { litArgs = [a1], litName = tc_Arrow, litType = EPi tvr { tvrType = getType a2 } (getType a1) }) c
        un b a2 c
    un (EVar tvr@TVr { tvrIdent = i, tvrType = t}) b c
        | i `member` bvs = tell (Seq.single (tvr,b))
        | otherwise = fail $ "Expressions do not unify: " ++ show tvr ++ show b
    un a (EVar tvr) c | Just b <- lup (tvrIdent tvr), not $ isEVar b = un a b c
    --un a b c | Just a' <- followAlias undefined a = un a' b c
    un a b c | Just b' <- followAlias undefined b = un a b' c

    un a b _ = fail $ "Expressions do not unify: " ++ show a ++ show b
    lam va ea vb eb (c:cs) = do
        un (tvrType va) (tvrType vb) (c:cs)
        un (subst va (EVar va { tvrIdent = c }) ea) (subst vb (EVar vb { tvrIdent = c }) eb) cs
