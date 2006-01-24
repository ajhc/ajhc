module E.TypeCheck(eAp, sortStarLike, sortTypeLike,  sortTermLike, inferType, typeInfer, typeInfer') where

import CanType
import {-# SOURCE #-} DataConstructors
import Doc.DocLike
import Doc.PPrint
import Doc.Pretty
import E.E
import E.Eval(strong)
import E.Pretty
import E.Subst
import GenUtil
import Util.ContextMonad
import Monad(when)



-- Fast (and lazy, and perhaps unsafe) typeof
typ ::  E -> E
typ (ESort EStar) =  eBox
typ (ESort EHash) =  eBox
typ (ESort EBox) = error "Box inhabits nowhere."
typ (ELit l) = getType l
typ (EVar v) =  getType v
typ (EPi _ b) = typ b
typ (EAp a b) = eAp (typ a) b
typ (ELam (TVr { tvrIdent = x, tvrType =  a}) b) = EPi (tVr x a) (typ b)
typ (ELetRec _ e) = typ e
typ (ECase {eCaseScrutinee = e, eCaseDefault = Just d}) | sortTypeLike e = typ d
typ (ECase {eCaseAlts = (x:_)}) = getType x
typ (ECase {eCaseDefault = Just e}) = typ e
typ (ECase _ _ [] Nothing) = error "empty case"
typ (EError _ e) = e
typ (EPrim _ _ t) = t
typ Unknown = Unknown


instance CanType E E where
    getType = typ
instance CanType TVr E where
    getType = tvrType
instance CanType (Lit x t) t where
    getType (LitInt _ t) = t
    getType (LitCons _ _ t) = t
instance CanType e t => CanType (Alt e) t where
    getType (Alt _ e) = getType e


sortStarLike e = e /= eBox && getType e == eBox
sortTypeLike e = e /= eBox && not (sortStarLike e) && sortStarLike (getType e)
sortTermLike e = e /= eBox && not (sortStarLike e) && not (sortTypeLike e) && sortTypeLike (getType e)




withContextDoc s a = withContext (render s) a

-- | Perform a full typecheck, evaluating type terms as necessary.

inferType :: ContextMonad String m => DataTable -> [(TVr,E)] -> E -> m E
inferType dataTable ds e = rfc e where
    inferType' ds e = inferType dataTable ds e
    prettyE = ePrettyEx
    rfc e =  withContextDoc (text "fullCheck:" </> prettyE e) (fc e >>=  strong')
    rfc' nds e =  withContextDoc (text "fullCheck':" </> prettyE e) (inferType' nds  e >>=  strong')
    strong' e = withContextDoc (text "Strong:" </> prettyE e) $ strong ds e
    fc s@(ESort _) = return $ typ s
    fc (ELit (LitCons _ es t)) = valid t >> mapM_ valid es >> (strong' t)
    fc e@(ELit _) = let t = typ e in valid t >> return t
    fc (EVar (TVr { tvrIdent = 0 })) = fail "variable with nothing!"
    fc (EVar (TVr { tvrType =  t})) = valid t >> strong' t
    fc (EPi (TVr { tvrIdent = n, tvrType =  at}) b) = valid at >> rfc' [ d | d@(v,_) <- ds, tvrNum v /= n ] b
    --fc (ELam tvr@(TVr n at) b) = valid at >> rfc' [ d | d@(v,_) <- ds, tvrNum v /= n ] b >>= \b' -> (strong' $ EPi tvr b')
    fc (ELam tvr@(TVr { tvrIdent = n, tvrType =  at}) b) = do
        valid at
        b' <- rfc' [ d | d@(v,_) <- ds, tvrNum v /= n ] b
        strong' $ EPi tvr b'
    --fc (EAp (EPi tvr e) b) = rfc (subst tvr b e)
    fc (EAp a b) = do
        a' <- rfc a
        case followAliases dataTable a' of
            (EPi tvr@(TVr { tvrType =  t}) v) -> do
                valid t
                withContextDoc (hsep [text "Application: ", parens $ prettyE a <> text "::" <> prettyE a', parens $ prettyE b]) $ fceq ds b t
                b' <- if sortStarLike t then strong' b else return b
                nt <- return (subst tvr b' v)
                valid nt
                return nt
            x -> fail $ "App: " ++ render (tupled [ePretty x,ePretty a, ePretty b])
    fc (ELetRec vs e) = do
        let ck (tv@(TVr { tvrType =  t}),e) = withContextDoc (hsep [text "Checking Let: ", parens (pprint tv),text  " = ", parens $ prettyE e ])  $ do
                when (getType t == eHash && not (isEPi t)) $ fail $ "Let binding unboxed value: " ++ show (tv,e)
                valid' nds t
                fceq nds e t
            nds = vs ++ ds
        mapM_ ck vs
        when (hasRepeatUnder (tvrNum . fst) vs) $ fail "Repeat Variable in ELetRec"
        et <- inferType' nds e
        strong nds et
    fc (EError _ e) = valid e >> (strong'  e)
    fc (EPrim _ ts t) = mapM_ valid ts >> valid t >> ( strong' t)
    fc ec@(ECase e@ELit {} b as (Just d)) | sortTypeLike e = do   -- TODO - this is a hack to get around case of constants.
        et <- rfc e
        eq et (getType b)
        dt <- rfc d
        verifyPats (casePats ec)
        -- skip checking alternatives
        ps <- mapM (strong' . getType) $ casePats ec
        eqAll (et:ps)
        return dt
    fc ec@(ECase e b as (Just d)) | sortTypeLike e  = do   -- TODO - we should substitute the tested for value into the default type.
        et <- rfc e
        eq et (getType b)
        dt <- rfc d
        --bs <- mapM rfc (caseBodies ec)  -- these should be specializations of dt
        mapM_ (calt e) as
        --eqAll bs
        verifyPats (casePats ec)
        ps <- mapM (strong' . getType) $ casePats ec
        eqAll (et:ps)
        return dt
    fc ec@(ECase e b _ _) = do
        et <- rfc e
        eq et (getType b)
        bs <- mapM rfc (caseBodies ec)
        eqAll bs
        verifyPats (casePats ec)
        ps <- mapM (strong' . getType) $ casePats ec
        eqAll (et:ps)
        return (head bs)
    fc e = failDoc $ text "what's this? " </> (prettyE e)
    calt (EVar v) (Alt l e) = do
        let nv =  patToLitEE l
        rfc (subst' v nv e)
    calt _ (Alt _ e) = rfc e
    verifyPats xs = do
        mapM_ verifyPats' xs
        when (hasRepeatUnder litHead xs) $ fail "Duplicate case alternatives"

    verifyPats' (LitCons _ xs _) = when (hasRepeatUnder id (filter (/= 0) $ map tvrNum xs)) $ fail "Case pattern is non-linear"
    verifyPats' _ = return ()



    {-
    fc (ECase _ []) = fail "Case with no alternatives"
    -- when checking typecases, we must check that the specialization of the default case matches each of the other alternatives.
    fc (ECase e alts) | sortTypeLike e = rfc e >>= \et -> mapM (cp et) alts >>= \as ->  return (last as) where
        cp et (PatLit l,e) = do
            withContextDoc (hsep [text "Case Pattern: ", parens $ prettyE et, parens $ prettyE (ELit l)]) $ eq et (getType  l)
            e' <- rfc e
            return (discardArgs (length es) e') where -- TODO - check these.
                es = case l of (LitCons _ es _) -> es ; _ -> []
        cp _ (PatWildCard,e') = rfc (eAp e' e)
    fc (ECase e alts) = rfc e >>= \et -> mapM (cp et) alts >>= \as@(a:_) -> eqAll  as >> return a where
        cp et (PatLit l,e) = do
            withContextDoc (hsep [text "Case Pattern: ", parens $ prettyE et, parens $ prettyE (ELit l)]) $ eq et (getType l)
            e' <- rfc e
            return (discardArgs (length es) e')  where -- TODO - check these.
                es = case l of (LitCons _ es _) -> es ; _ -> []
        cp _ (PatWildCard,e') = rfc (eAp e' e)
    -}
    eqAll ts = withContextDoc (text "eqAll" </> list (map prettyE ts)) $ foldl1M_ eq ts
    valid s = valid' ds s
    valid' nds s
        | s == eBox = return ()
        | Unknown <- s = fail "valid: Unknown"
        | otherwise =  withContextDoc (text "valid:" <+> prettyE e) (do t <- inferType' nds s;  valid' nds t)
    eq t1 t2 = eq' ds t1 t2
    eq' nds t1 t2 = do
        e1 <- strong nds t1
        e2 <- strong nds t2
        case typesCompatable dataTable e1 e2 of
            Right () -> return (followAliases dataTable e1)
            Left s -> failDoc $ hsep [text "eq:",text s, align $ vcat [ prettyE (e1),prettyE (e2) ]  ]

--        let x
--            --  | e1 == e2 = e1
--              | allShadow e1 == allShadow e2 = return e1
--            --  | e1 == tInt || e2 == tInt = return e1
--              | otherwise = failDoc $ hsep [text "eq:",{- tupled (map pprint $ fsts nds), -} tupled [ prettyE (e1),prettyE (e2) ], tupled [ prettyE (allShadow e1), prettyE (allShadow e2)] {- , tupled [ text (show e1), text (show e2)] -}  ]
--        x
--    -- | Check that the type of e1 is the same as t2 within the given context
    fceq nds e1 t2 = do
        withContextDoc (hsep [text "fceq:", align $ vcat [parens $ prettyE e1,  parens $ prettyE t2]]) $ do
        t1 <- inferType' nds e1
        eq' nds t1 t2
        --flip (eq' nds) t2 t
    --eq t1 t2 | Just _ <- E.Eval.unify (smplE t1) (smplE t2) = return (smplE t1)
    --eq t1 t2 | Just _ <- E.Eval.unify (smplE t2) (smplE t1) = return (smplE t1)
    --eq t1 t2 | smplE t1 == smplE t2 = return t1
    --eq t1 t2 | t1 == tInt || t2 == tInt = return t1  --TODO - hack.
    --eq t1 t2 | Left d <- E.Eval.unify (smplE t1) (smplE t2) = failDoc $ hsep [text "eq:", tupled [text d, prettyE (smplE t1),prettyE (smplE t2) ] ]
    --eq t1 t2  = failDoc $ hsep [text "eq:", tupled [ prettyE (smplE t1),prettyE (smplE t2) ] ]
    --valid s | s == eBox = return ()
    --valid Unknown = fail "valid: Unknown"
    --valid e = withContextDoc (text "valid:" <+> prettyE e) (rfc e >>= valid)


instance CanTypeCheck DataTable E E where
    typecheck dataTable e = case typeInfer'' dataTable [] e of
        Left ss -> fail $ "\n>>> internal error:\n" ++ unlines (tail ss)
        Right v -> return v

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

typeInfer'' :: ContextMonad String m => DataTable -> [(TVr,E)] -> E -> m E
typeInfer'' dataTable ds e = rfc e where
    inferType' ds e = typeInfer'' dataTable ds e
    prettyE = ePrettyEx
    rfc e =  withContextDoc (text "fullCheck:" </> prettyE e) (fc e >>=  strong')
    rfc' nds e =  withContextDoc (text "fullCheck:" </> prettyE e) (inferType' nds  e >>=  strong')
    strong' e = withContextDoc (text "Strong:" </> prettyE e) $ strong ds e
    fc s@(ESort _) = return $ typ s
    fc (ELit (LitCons _ es t)) = (strong' t)
    fc e@(ELit _) = strong' (typ e)
    fc (EVar (TVr { tvrIdent = 0 })) = fail "variable with nothing!"
    fc (EVar (TVr { tvrType =  t})) =  strong' t
    fc (EPi (TVr { tvrIdent = n, tvrType = at}) b) =  rfc' [ d | d@(v,_) <- ds, tvrNum v /= n ] b
    fc (ELam tvr@(TVr { tvrIdent = n, tvrType =  at}) b) = do
        at' <- strong' at
        b' <- rfc' [ d | d@(v,_) <- ds, tvrNum v /= n ] b
        return (EPi (tVr n at') b')
    --fc (EAp (EPi tvr e) b) = rfc (subst tvr b e)
    fc (EAp a b) = do
        a' <- rfc a
        case followAliases dataTable a' of
            (EPi tvr@(TVr { tvrType = t}) v) -> do
                --withContextDoc (hsep [text "Application: ", parens $ prettyE a <> text "::" <> prettyE a', parens $ prettyE b]) $ fceq ds b t
                b' <- if sortStarLike t then strong' b else return b
                return (subst tvr b' v)
            x -> fail $ "App: " ++ render (tupled [ePretty x,ePretty a, ePretty b])
    fc (ELetRec vs e) = do
        let nds = vs ++ ds
        et <- inferType' nds e
        strong nds et
    fc (EError _ e) = (strong'  e)
    fc (EPrim _ ts t) = ( strong' t)
    fc ec@(ECase e b as (Just d)) | sortTypeLike e  = do   -- TODO - we should substitute the tested for value into the default type.
        dt <- rfc' [ d | d@(v,_) <- ds, tvrNum b /= tvrNum v ] d
        return dt
    fc ec@(ECase e b _ _) = do
        --bs <- mapM rfc (caseBodies ec)
        --return (head bs)
        rfc (head $ caseBodies ec)
    fc e = failDoc $ text "what's this? " </> (prettyE e)



