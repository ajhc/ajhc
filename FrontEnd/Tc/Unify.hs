
module FrontEnd.Tc.Unify(subsumes,boxyMatch) where

import Control.Monad.Writer

import Doc.PPrint
import Doc.DocLike
import FrontEnd.Tc.Type
import FrontEnd.Tc.Monad
import GenUtil

pretty vv = prettyPrintType vv
ppretty vv = parens (pretty vv)
    {-
aquireType :: Sigma' -> Tc Sigma'
aquireType s = do
    s <- findType s
    case s of
        TBox { typeBox = box } -> do
            r <- openBox box
            case r of
                Just t -> aquireType t
                Nothing -> return s
        _ -> return s
 -}

subsumes :: Sigma' -> Sigma' -> Tc ()
subsumes s1 s2 = do
    s1 <- findType s1
    s2 <- findType s2
    liftIO $ putStrLn $ "subsumes: " <> ppretty s1 <+> ppretty s2
    sub s1 s2
   where
    -- SBOXY
    sub tb@TMetaVar {} b = boxyMatch tb b

{-
    sub (TArrow a b) t | Just t <- extractMetaVar t = do
        a' <- newTVar (kind a)
        b' <- newTVar (kind b)
        varBind t (TArrow a' b')
        (TArrow a b) `subsumes` (TArrow a' b')
    sub (TAp a b) t | Just t <- extractMetaVar t = do
        a' <- newTVar (kind a)
        b' <- newTVar (kind b)
        varBind t (TAp a' b')
        (TAp a b) `subsumes` (TAp a' b')
    sub t (TArrow a b) | Just t <- extractMetaVar t = do
        a' <- newTVar (kind a)
        b' <- newTVar (kind b)
        varBind t (TArrow a' b')
        (TArrow a' b') `subsumes` (TArrow a b)
    sub t (TAp a b) | Just t <- extractMetaVar t = do
        a' <- newTVar (kind a)
        b' <- newTVar (kind b)
        varBind t (TAp a' b')
        (TAp a' b') `subsumes` (TAp a b)
-}
    -- SKOL needs to be after SBOXY
    sub s1 fa@TForAll {} = do
        (_,r2) <- skolomize fa
        --r1 <- freshInstance fa
        s1 `subsumes` r2

    -- SPEC
    sub s1@(TForAll as (_ :=> _))  r2  = do   -- isRho' r2
        r1' <- boxyInstantiate s1
        --(bs,r1') <- boxySpec s1
        r1' `subsumes` r2
--        let f (_,bs) = do
--            bs' <- sequence [ openBox b >>= findType | ~TBox { typeBox = b } <- bs]
--            unifyList bs'
--        mapM_ f bs
        --bs <- mapM (const $ newBox Star) as
        --inst (Map.fromList $ zip (map tyvarAtom as) (snds bs)) r1 `subsumes` r2

    -- CON (??)
    sub s1 s2 | (TCon _,_) <- fromTAp s1 = s1 `boxyMatch` s2

    -- F1
    sub (TArrow s1 s2) (TArrow s3 s4) = do
        boxyMatch s3 s1
        s2 `subsumes` s4
    -- F2
    sub t@(TArrow s1 s2) (TMetaVar mv) = do
        a <- newMetaVar (metaType mv) (kind s1)
        b <- newMetaVar (metaType mv) (kind s2)
        varBind mv (a `fn` b)
        subsumes t (a `fn` b)
        --fillBox box (a `fn` b)

    -- BMONO & MONO
    sub a b | isTau a && isTau b = unify a b

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
    bm (TMetaVar v1) (TMetaVar v2) = do
        when (kind v1 /= kind v2) $ error "BBEQ boxyMatch kinds"
        -- create a new metavar to enforce tauness.
        tt <- newMetaVar Tau (kind v1)
        varBind v1 tt
        varBind v2 tt
        return False

    -- AEQ1
    bm (TArrow s1 s2) (TMetaVar mv) = do
        a <- newBox (kind s1)
        b <- newBox (kind s2)
        boxyMatch (s1 `fn` s2) (a `fn` b)
        varBind mv (a `fn` b)
        return False

    -- AEQ2
    bm (TArrow s1 s2) (TArrow s3 s4) = do
        boxyMatch s1 s3
        boxyMatch s2 s4
        return False

    -- CEQ1

    bm a (TMetaVar mv) | (TCon ca,as) <- fromTAp a = do
        bs <- mapM (newBox . kind) as
        a `boxyMatch` foldl TAp (TCon ca) bs
        varBind mv (foldl TAp (TCon ca) bs)
        return False


    -- CEQ2

    bm a b | (TCon ca,as) <- fromTAp a, (TCon cb,bs) <- fromTAp b = case ca == cb of
        -- False -> fail $ "constructor mismatch: " ++ show (a,b)
        False -> unificationError a b
        True | length as == length bs -> sequence_ [boxyMatch x y | x <- as | y <- bs] >> return False
        -- _ ->   fail $ "constructor args mismatch: " ++ show (a,b)
        _ -> unificationError a b


    -- SEQ1
    bm (TForAll vs (ps :=> t)) (TMetaVar mv) = do
        a <- newBox (kind mv)
        boxyMatch t a
        varBind mv (TForAll vs (ps :=> a))
        return False

    -- SEQ2

    bm (TForAll vs (ps :=> t)) (TForAll vs' (ps' :=> t')) = fail "SEQ2"
    -- >> do
    --    (ra,a) <- newBox Star
    --    boxyMatch t a
    --    a <- ra
     --   fillBox box (TForAll vs (ps :=> a))
     --   return False

    -- XXX app
    bm (TAp a b) (TAp c d) = do
        a `boxyMatch` c
        b `boxyMatch` d
        return False

    -- MEQ1 MEQ2  SYM
    bm a b
        | isTau a, TMetaVar mv <- b = varBind mv a >> return False
        | isTau a && isTau b = unify a b >> return False

{-
    bm (TArrow a b) t | Just t <- extractMetaVar t = do
        a' <- newTVar (kind a)
        b' <- newTVar (kind b)
        varBind t (TArrow a' b')
        (TArrow a b) `boxyMatch` (TArrow a' b')
        return False
    bm (TAp a b) t | Just t <- extractMetaVar t = do
        a' <- newTVar (kind a)
        b' <- newTVar (kind b)
        varBind t (TAp a' b')
        (TAp a b) `boxyMatch` (TAp a' b')
        return False
  -}

    bm _ _ = return True


unify      :: Tau -> Tau -> Tc ()
unify t1 t2 = do
    t1' <- findType t1
    t2' <- findType t2
    liftIO $ putStrLn $ "unify: " <> ppretty t1 <+> ppretty t2
    mgu t1' t2'

mgu (TAp l r) (TAp l' r')
   = do s1 <- unify l l'
        s2 <- unify r r'
        return ()
mgu (TArrow l r) (TArrow l' r')
   = do s1 <- unify l l'
        s2 <- unify r r'
        return ()
mgu (TMetaVar u) t = varBind u t
mgu t (TMetaVar u) = varBind u t
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
