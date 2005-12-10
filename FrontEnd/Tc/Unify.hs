
module FrontEnd.Tc.Unify(subsumes,boxyMatch) where

import Control.Monad.Writer

import Doc.PPrint
import Doc.DocLike
import FrontEnd.Tc.Type
import FrontEnd.Tc.Monad
import GenUtil

pretty vv = prettyPrintType vv
ppretty vv = parens (pretty vv)

subsumes :: Sigma' -> Sigma' -> Tc ()
subsumes s1 s2 = do
    s1 <- findType s1
    s2 <- findType s2
    liftIO $ putStrLn $ "subsumes: " <> ppretty s1 <+> ppretty s2
    sub s1 s2
   where
    -- SBOXY
    sub tb@TBox {} b = boxyMatch tb b

    sub (TArrow a b) t | Just t <- extractMetaTV t = do
        a' <- newTVar (kind a)
        b' <- newTVar (kind b)
        varBind t (TArrow a' b')
        (TArrow a b) `subsumes` (TArrow a' b')
    sub (TAp a b) t | Just t <- extractMetaTV t = do
        a' <- newTVar (kind a)
        b' <- newTVar (kind b)
        varBind t (TAp a' b')
        (TAp a b) `subsumes` (TAp a' b')
    sub t (TArrow a b) | Just t <- extractMetaTV t = do
        a' <- newTVar (kind a)
        b' <- newTVar (kind b)
        varBind t (TArrow a' b')
        (TArrow a' b') `subsumes` (TArrow a b)
    sub t (TAp a b) | Just t <- extractMetaTV t = do
        a' <- newTVar (kind a)
        b' <- newTVar (kind b)
        varBind t (TAp a' b')
        (TAp a' b') `subsumes` (TAp a b)

    -- SKOL needs to be after SBOXY
    sub s1 fa@TForAll {} = do
        (_,r2) <- skolomize fa
        --r1 <- freshInstance fa
        s1 `subsumes` r2

    -- SPEC
    sub s1@(TForAll as (_ :=> _))  r2 | isRho' r2 = do
        --r1' <- boxyInstantiate s1
        (bs,r1') <- boxySpec s1
        r1' `subsumes` r2
        let f (_,bs) = do
            bs' <- sequence [ openBox b >>= findType | ~TBox { typeBox = b } <- bs]
            unifyList bs'
        mapM_ f bs
        --bs <- mapM (const $ newBox Star) as
        --inst (Map.fromList $ zip (map tyvarAtom as) (snds bs)) r1 `subsumes` r2

    -- CON (??)
    sub s1 s2 | (TCon _,_) <- fromTAp s1 = s1 `boxyMatch` s2

    -- F1
    sub (TArrow s1 s2) (TArrow s3 s4) = do
        boxyMatch s3 s1
        s2 `subsumes` s4
    -- F2
    sub t@(TArrow s1 s2) (TBox { typeBox = box}) = do
        (oa,a) <- newBox (kind s1)
        (ob,b) <- newBox (kind s2)
        subsumes t (a `fn` b)
        na <- oa
        nb <- ob
        fillBox box (na `fn` nb)

    -- BMONO & MONO
    sub a b | isTau a = case b of
        (TBox {typeBox = b}) -> fillBox b a
        _ | isTau b -> unify a b -- TODO verify? fail $ "taus don't match in MONO" ++ show (a,b)
        _ -> do
            fail $ "subsumes failure: "  <> ppretty s1 <+> ppretty s2

    sub a b = fail $ "subsumes failure: " <> ppretty s1 <+> ppretty s2


boxyMatch :: Sigma' -> Sigma' -> Tc ()
boxyMatch s1 s2 = do
    s1 <- findType s1
    s2 <- findType s2
    liftIO $ putStrLn $ "boxyMatch: " <> ppretty s1 <+> ppretty s2
    b <- bm s1 s2
    if b then do
        liftIO $ putStrLn $ "boxyMatch: " <> ppretty s2 <+> ppretty s1
        b' <- bm s2 s1
        when b' $  fail $ "boxyMatch failure: " <> ppretty s1 <+> ppretty s2
     else return ()
   where
    -- BBEQ
    bm TBox { typeKind = k1, typeBox = ba} TBox { typeKind = k2, typeBox = bb} = do
        when (k1 /= k2) $ error "boxyMatch kinds"
        tt <- newTVar k1
        fillBox ba tt
        fillBox bb tt
        return False

    -- AEQ1
    bm (TArrow s1 s2) TBox { typeBox = box} = do
        (ra,a) <- newBox Star
        (rb,b) <- newBox Star
        boxyMatch (s1 `fn` s2) (a `fn` b)
        x <- ra
        y <- rb
        fillBox box (x `fn` y)
        return False

    -- AEQ2
    bm (TArrow s1 s2) (TArrow s3 s4) = do
        boxyMatch s1 s3
        boxyMatch s2 s4
        return False

    -- CEQ1

    bm a (TBox { typeBox = box }) | (TCon ca,as) <- fromTAp a = do
        bs <- mapM (newBox . kind) as
        a `boxyMatch` foldl TAp (TCon ca) (snds bs)
        bs <- sequence $ fsts bs
        fillBox box (foldl TAp (TCon ca) bs)
        return False


    -- CEQ2

    bm a b | (TCon ca,as) <- fromTAp a, (TCon cb,bs) <- fromTAp b = case ca == cb of
        -- False -> fail $ "constructor mismatch: " ++ show (a,b)
        False -> unificationError a b
        True | length as == length bs -> sequence_ [boxyMatch x y | x <- as | y <- bs] >> return False
        -- _ ->   fail $ "constructor args mismatch: " ++ show (a,b)
        _ -> unificationError a b


    -- SEQ1
    bm (TForAll vs (ps :=> t)) (TBox { typeKind = k, typeBox = box }) = do
        (ra,a) <- newBox k
        boxyMatch t a
        a <- ra >>= findType
        fillBox box (TForAll vs (ps :=> a))
        return False

    -- SEQ2

    bm (TForAll vs (ps :=> t)) (TForAll vs' (ps' :=> t')) = fail "SEQ2"
    -- >> do
    --    (ra,a) <- newBox Star
    --    boxyMatch t a
    --    a <- ra
     --   fillBox box (TForAll vs (ps :=> a))
     --   return False

    bm (TArrow a b) t | Just t <- extractMetaTV t = do
        a' <- newTVar (kind a)
        b' <- newTVar (kind b)
        varBind t (TArrow a' b')
        (TArrow a b) `boxyMatch` (TArrow a' b')
        return False
    bm (TAp a b) t | Just t <- extractMetaTV t = do
        a' <- newTVar (kind a)
        b' <- newTVar (kind b)
        varBind t (TAp a' b')
        (TAp a b) `boxyMatch` (TAp a' b')
        return False

    -- XXX app
    bm (TAp a b) (TAp c d) = do
        a `boxyMatch` c
        b `boxyMatch` d
        return False

    -- MEQ1 MEQ2  SYM
    bm a b | isTau a = case b of
        (TBox { typeBox = b}) -> fillBox b a >> return False
        _ | isTau b -> unify a b >> return False -- TODO, verify? fail $ "taus don't match in MEQ[12]" ++ show (a,b)
          | otherwise -> return True
    bm _ _ = return True


unify      :: Tau -> Tau -> Tc ()
unify t1 t2 = do
    t1' <- findType t1
    t2' <- findType t2
    mgu t1' t2'

mgu (TAp l r) (TAp l' r')
   = do s1 <- unify l l'
        s2 <- unify r r'
        return ()
mgu (TArrow l r) (TArrow l' r')
   = do s1 <- unify l l'
        s2 <- unify r r'
        return ()
mgu (TVar u) t | isMetaTV u  = varBind u t
mgu t (TVar u) | isMetaTV u  = varBind u t
mgu (TVar a) (TVar b) | a == b = return ()
mgu c1@(TCon tc1) c2@(TCon tc2)
           | tc1==tc2 = return ()
           -- | otherwise = fail $ "mgu: Constructors don't match:" ++ show (c1,c2)
           | otherwise = unificationError c1 c2
mgu TForAll {} _ = error "attempt to unify TForall"
mgu _ TForAll {} = error "attempt to unify TForall"
mgu _ TBox {} = error "attempt to unify TBox"
mgu TBox {} _ = error "attempt to unify TBox"
mgu t1 t2  = unificationError t1 t2

unifyList :: [Type] -> Tc ()
unifyList (t1:t2:ts) = unify t1 t2 >> unifyList (t2:ts)
unifyList _ = return ()
