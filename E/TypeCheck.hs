module E.TypeCheck(eAp, sortStarLike, sortTypeLike,  sortTermLike, inferType, typeInfer, typeInfer', match) where

import Monad(when,liftM)
import qualified Data.Set as Set
import Control.Monad.Writer
import Control.Monad.Reader

import Support.CanType
import {-# SOURCE #-} DataConstructors
import Doc.DocLike
import Doc.PPrint
import Doc.Pretty
import E.E
import E.Eval(strong)
import E.Show
import E.Subst
import GenUtil
import Util.ContextMonad
import qualified Util.Seq as Seq



-- Fast (and lazy, and perhaps unsafe) typeof
typ ::  E -> E
typ (ESort EStar) =  eBox
typ (ESort EHash) =  eBox
typ (ESort EBox) = error "Box inhabits nowhere."
typ (ELit l) = getType l
typ (EVar v) =  getType v
typ (EPi _ b) = typ b
typ (EAp (ELit LitCons { litType = EPi tvr a }) b) = getType (subst tvr b a)
typ e@(EAp (ELit LitCons { litType = ty }) b) | ty == eStar = eStar -- XXX functions might have unboxed return types in the future
typ e@(EAp (ELit LitCons {}) b) = error $ "getType: application of type alias " ++ (render $ ePretty e)
typ (EAp (EPi tvr a) b) = getType (subst tvr b a)
typ (EAp a b) = eAp (typ a) b
typ (ELam (TVr { tvrIdent = x, tvrType =  a}) b) = EPi (tVr x a) (typ b)
typ (ELetRec _ e) = typ e
typ ECase {eCaseType = ty} = ty
typ (EError _ e) = e
typ (EPrim _ _ t) = t
typ Unknown = Unknown


instance CanType E E where
    getType = typ
instance CanType TVr E where
    getType = tvrType
instance CanType (Lit x t) t where
    getType l = litType l
instance CanType e t => CanType (Alt e) t where
    getType (Alt _ e) = getType e


sortStarLike e = e /= eBox && getType e == eBox
sortTypeLike e = e /= eBox && not (sortStarLike e) && sortStarLike (getType e)
sortTermLike e = e /= eBox && not (sortStarLike e) && not (sortTypeLike e) && sortTypeLike (getType e)



simplifyAp :: Monad m => DataTable -> E -> E -> m E
simplifyAp dataTable (EAp a b) c = do
    na <- simplifyAp dataTable a b
    simplifyAp dataTable na c
simplifyAp _ (ELit lc@LitCons { litArgs = xs, litType = EPi tvr r }) b = do
        return (ELit lc { litArgs = (xs ++ [b]), litType = subst tvr b r })
simplifyAp dataTable a@ELit {} b = case followAliases dataTable a  of
        ELit {} -> fail $ "simplifyAp: " ++ render (tupled [ePretty a, ePretty b])
        (EPi tvr e) -> do
                return (subst tvr b e)
simplifyAp _ a b = fail $ "simplifyAp: " ++ render (tupled [ePretty a, ePretty b])


withContextDoc s a = withContext (render s) a

-- | Perform a full typecheck, evaluating type terms as necessary.

inferType :: ContextMonad String m => DataTable -> [(TVr,E)] -> E -> m E
inferType dataTable ds e = rfc e where
    inferType' ds e = inferType dataTable ds e
    prettyE = ePrettyEx
    rfc e =  withContextDoc (text "fullCheck:" </> prettyE e) (fc (followAliases dataTable e) >>=  strong')
    rfc' nds e = withContextDoc (text "fullCheck:" </> prettyE e) (inferType' nds (followAliases dataTable e) >>=  strong')
    strong' e = withContextDoc (text "Strong:" </> prettyE e) $ strong ds (followAliases dataTable e)
    fc s@(ESort _) = return $ typ s
    fc (ELit LitCons { litArgs = es, litType =  t}) = valid t >> mapM_ valid es >> (strong' t)
    fc e@(ELit _) = let t = typ e in valid t >> return t
    fc (EVar (TVr { tvrIdent = 0 })) = fail "variable with nothing!"
    fc (EVar (TVr { tvrType =  t})) = valid t >> strong' t
    fc (EPi (TVr { tvrIdent = n, tvrType =  at}) b) = valid at >> rfc' [ d | d@(v,_) <- ds, tvrNum v /= n ] b
    --fc (ELam tvr@(TVr n at) b) = valid at >> rfc' [ d | d@(v,_) <- ds, tvrNum v /= n ] b >>= \b' -> (strong' $ EPi tvr b')
    fc (ELam tvr@(TVr { tvrIdent = n, tvrType =  at}) b) = do
        valid at
        b' <- rfc' [ d | d@(v,_) <- ds, tvrNum v /= n ] b
        strong' $ EPi tvr b'
    fc (EAp (EPi tvr e) b) = rfc (subst tvr b e)
    fc (EAp a b) = do
        withContextDoc (text "EAp:" </> parens (prettyE a) </> parens (prettyE b)) $ do
            a' <- rfc a
            return $ eAp (followAliases dataTable a') b
        {-
        case followAliases dataTable a' of
            (EPi tvr@(TVr { tvrType =  t}) v) -> do
                valid t
                withContextDoc (hsep [text "Application: ", parens $ prettyE a <> text "::" <> prettyE a', parens $ prettyE b]) $ fceq ds b t
                b' <- if sortStarLike t then strong' b else return b
                nt <- return (subst tvr b' v)
                valid nt
                return nt
            x -> fail $ "App: " ++ render (tupled [ePretty x,ePretty a, ePretty a', ePretty b])
            -}
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
    fc ec@ECase { eCaseScrutinee = e@ELit {}, eCaseBind = b, eCaseAlts = as, eCaseDefault =  (Just d) } | sortTypeLike e = do   -- TODO - this is a hack to get around case of constants.
        et <- rfc e
        eq et (getType b)
        dt <- rfc d
        verifyPats (casePats ec)
        -- skip checking alternatives
        ps <- mapM (strong' . getType) $ casePats ec
        eqAll (et:ps)
        return dt
    fc ec@ECase {eCaseScrutinee = e, eCaseBind = b, eCaseAlts = as, eCaseType = dt } | sortTypeLike e  = do   -- TODO - we should substitute the tested for value into the default type.
        et <- rfc e
        eq et (getType b)
        --dt <- rfc d
        --bs <- mapM rfc (caseBodies ec)  -- these should be specializations of dt
        mapM_ (calt e) as
        --eqAll bs
        verifyPats (casePats ec)
        ps <- mapM (strong' . getType) $ casePats ec
        eqAll (et:ps)
        strong' dt
    fc ec@ECase { eCaseScrutinee =e, eCaseBind = b } = do
        et <- rfc e
        eq et (getType b)
        bs <- mapM rfc (caseBodies ec)
        eqAll bs
        verifyPats (casePats ec)
        ps <- mapM (strong' . getType) $ casePats ec
        eqAll (et:ps)
        return (head bs)
    fc Unknown = return Unknown
    fc e = failDoc $ text "what's this? " </> (prettyE e)
    calt (EVar v) (Alt l e) = do
        let nv =  patToLitEE l
        rfc (subst' v nv e)
    calt _ (Alt _ e) = rfc e
    verifyPats xs = do
        mapM_ verifyPats' xs
        when (hasRepeatUnder litHead xs) $ fail "Duplicate case alternatives"

    verifyPats' LitCons { litArgs = xs } = when (hasRepeatUnder id (filter (/= 0) $ map tvrNum xs)) $ fail "Case pattern is non-linear"
    verifyPats' _ = return ()

    eqAll ts = withContextDoc (text "eqAll" </> list (map prettyE ts)) $ foldl1M_ eq ts
    valid s = valid' ds s
    valid' nds s
        | s == eBox = return ()
        | Unknown <- s = return ()
        | otherwise =  withContextDoc (text "valid:" <+> prettyE e) (do t <- inferType' nds s;  valid' nds t)
    eq t1 t2 = eq' ds t1 t2
    eq' nds t1 t2 = do
        e1 <- strong nds (followAliases dataTable t1)
        e2 <- strong nds (followAliases dataTable t2)
        case typesCompatable dataTable e1 e2 of
            Right () -> return (followAliases dataTable e1)
            Left s -> failDoc $ hsep [text "eq:",text s, align $ vcat [ prettyE (e1),prettyE (e2) ]  ]
    fceq nds e1 t2 = do
        withContextDoc (hsep [text "fceq:", align $ vcat [parens $ prettyE e1,  parens $ prettyE t2]]) $ do
        t1 <- inferType' nds e1
        eq' nds t1 t2


instance CanTypeCheck DataTable E E where
    typecheck dataTable e = case typeInfer'' dataTable [] e of
        Left ss -> fail $ "\n>>> internal error:\n" ++ unlines (tail ss)
        Right v -> return v

instance CanTypeCheck DataTable TVr E where
    typecheck _ tvr = return $ getType tvr

instance CanTypeCheck DataTable (Lit a E) E where
    typecheck _ l = return $ getType l

-- TODO, types might be bound in scrutinization
instance CanTypeCheck DataTable (Alt E) E where
    typecheck dt (Alt _ e) = typecheck dt e

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
    tcContext :: [String],
    tcDataTable :: DataTable
    }
   {-! derive: update !-}

newtype Tc a = Tc (Reader TcEnv a)
    deriving(Monad,Functor,MonadReader TcEnv)

instance ContextMonad String Tc where
    withContext s = local (tcContext_u (s:))

--tcE :: E -> Tc E
--tcE e = rfc e where

typeInfer'' :: ContextMonad String m => DataTable -> [(TVr,E)] -> E -> m E
typeInfer'' dataTable ds e = rfc e where
    inferType' ds e = typeInfer'' dataTable ds e
    prettyE = ePrettyEx
    rfc e =  withContextDoc (text "fullCheck':" </> prettyE e) (fc (followAliases dataTable e) >>=  strong')
    rfc' nds e =  withContextDoc (text "fullCheck':" </> prettyE e) (inferType' nds  (followAliases dataTable e) >>=  strong')
    strong' e = withContextDoc (text "Strong':" </> prettyE e) $ strong ds (followAliases dataTable e)
    fc s@ESort {} = return $ getType s
    fc (ELit LitCons { litType = t }) = strong' t
    fc e@ELit {} = strong' (getType e)
    fc (EVar TVr { tvrIdent = 0 }) = fail "variable with nothing!"
    fc (EVar TVr { tvrType =  t}) =  strong' t
    fc (EPi TVr { tvrIdent = n, tvrType = at} b) =  rfc' [ d | d@(v,_) <- ds, tvrNum v /= n ] b
    fc (ELam tvr@TVr { tvrIdent = n, tvrType =  at} b) = do
        at' <- strong' at
        b' <- rfc' [ d | d@(v,_) <- ds, tvrNum v /= n ] b
        return (EPi (tVr n at') b')
    fc (EAp (EPi tvr e) b) = do
        b <- strong' b
        rfc (subst tvr b e)
    fc (EAp a b) = do
        a' <- rfc a
        strong' (eAp (followAliases dataTable a') b)
    fc (ELetRec vs e) = do
        let nds = vs ++ ds
        et <- inferType' nds e
        strong nds et
    fc (EError _ e) = strong' e
    fc (EPrim _ ts t) = strong' t
    fc ECase { eCaseType = ty } = do
        strong' ty
    fc Unknown = return Unknown
    fc e = failDoc $ text "what's this? " </> (prettyE e)



-- | find substitution that will transform the left term into the right one,
-- only substituting for the vars in the list

match :: Monad m =>
    (Id -> Maybe E)      -- ^ function to look up values in the environment
    -> [TVr]              -- ^ vars which may be substituted
    -> E                  -- ^ pattern to match
    -> E                  -- ^ input expression
    -> m [(TVr,E)]
match lup vs = \e1 e2 -> liftM Seq.toList $ execWriterT (un e1 e2 () (-2::Int)) where
    bvs = Set.fromList (map tvrIdent vs)

    un _ _ _ c | c `seq` False = undefined
    un (EAp a b) (EAp a' b') mm c = do
        un a a' mm c
        un b b' mm c
    un (ELam va ea) (ELam vb eb) mm c = lam va ea vb eb mm c
    un (EPi va ea) (EPi vb eb) mm c = lam va ea vb eb mm c
    un (EPrim s xs t) (EPrim s' ys t') mm c | length xs == length ys = do
        sequence_ [ un x y mm c | x <- xs | y <- ys]
        un t t' mm c
    un (ESort x) (ESort y) mm c | x == y = return ()
    un (ELit (LitInt x t1))  (ELit (LitInt y t2)) mm c | x == y = un t1 t2 mm c
    un (ELit LitCons { litName = n, litArgs = xs, litType = t })  (ELit LitCons { litName = n', litArgs = ys, litType =  t'}) mm c | n == n' && length xs == length ys = do
        sequence_ [ un x y mm c | x <- xs | y <- ys]
        un t t' mm c

    un (EVar TVr { tvrIdent = i, tvrType =  t}) (EVar TVr {tvrIdent = j, tvrType =  u}) mm c | i == j = un t u mm c
    un (EVar TVr { tvrIdent = i, tvrType =  t}) (EVar TVr {tvrIdent = j, tvrType =  u}) mm c | i < 0 || j < 0  = fail "Expressions don't match"
    un (EVar tvr@TVr { tvrIdent = i, tvrType = t}) b mm c
        | i `Set.member` bvs = tell (Seq.single (tvr,b))
        | otherwise = fail $ "Expressions do not unify: " ++ show tvr ++ show b
    un a (EVar tvr) mm c | Just b <- lup (tvrIdent tvr), not $ isEVar b = un a b mm c

    un a b _ _ = fail $ "Expressions do not unify: " ++ show a ++ show b
    lam va ea vb eb mm c = do
        un (tvrType va) (tvrType vb) mm c
        un (subst va (EVar va { tvrIdent = c }) ea) (subst vb (EVar vb { tvrIdent = c }) eb) mm (c - 2)

