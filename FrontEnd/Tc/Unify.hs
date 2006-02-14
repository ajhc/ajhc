
module FrontEnd.Tc.Unify(subsumes,boxyMatch) where

import Control.Monad.Writer

import Doc.PPrint
import Doc.DocLike
import FrontEnd.Tc.Type
import Support.CanType
import FrontEnd.Tc.Monad
import Options
import qualified FlagDump as FD
import GenUtil

pretty vv = prettyPrintType vv
ppretty vv = parens (pretty vv)

-- | this ensures the first argument is at least as polymorphic as the second
-- actual/offered <= expected
-- actual/offered `subsumes` expected

subsumes :: Sigma' -> Sigma' -> Tc ()
subsumes s1 s2 = do
    (s1,s2) <- if dump FD.BoxySteps then do
        (s1,_,_) <- unbox s1
        (s2,_,_) <- unbox s2
        return (s1,s2)
      else do
        s1 <- findType s1
        s2 <- findType s2
        return (s1,s2)
    printRule $ "subsumes: " <> ppretty s1 <+> ppretty s2
    sub s1 s2
   where
    -- SBOXY
    sub tb@(TMetaVar mv) b  = boxyMatch tb b

    -- SKOL needs to be after SBOXY
    sub s1 fa@TForAll {} = do
        printRule "SKOL"
        (_,_,r2) <- skolomize fa
        s1 `subsumes` r2

    -- SPEC
    sub s1@(TForAll as (_ :=> _))  r2 | isRho' r2 = do   -- isRho' r2
        printRule "SPEC"
        r1' <- boxyInstantiate s1
        r1' `subsumes` r2

    -- CON
    sub s1 s2 | (_,(_:_)) <- fromTAp s1 = s1 `boxyMatch` s2

   -- sub s1 (TMetaVar mv) | (t,ts@(_:_)) <- fromTAp s1 = do
   --     let ats = t:ts
   --     withMetaVars mv (map getaType ats) (map  (\ (t:ts) -> TArrow a b) $ \ [a,b] -> do
   --     subsumes t (a `fn` b)


    -- F1
    sub (TArrow s1 s2) (TArrow s3 s4) = do
        boxyMatch s3 s1
        s2 `subsumes` s4
    -- F2
    sub t@(TArrow s1 s2) (TMetaVar mv) = do
        withMetaVars mv [getType s1, getType s2] (\ [a,b] -> TArrow a b) $ \ [a,b] -> do
        subsumes t (a `fn` b)

    -- BMONO
    sub a (TMetaVar mv) | isTau a  = varBind mv a
    -- MONO
    sub a b | isTau a && isTau b = unify a b


    sub a b = fail $ "subsumes failure: " <> ppretty a <+> ppretty b

printRule :: String -> Tc ()
printRule s
    | dump FD.BoxySteps = liftIO $ putStrLn s
    | otherwise = return ()



boxyMatch :: Sigma' -> Sigma' -> Tc ()
boxyMatch s1 s2 = do
    (s1,s2) <- if dump FD.BoxySteps then do
        (s1,_,_) <- unbox s1
        (s2,_,_) <- unbox s2
        return (s1,s2)
      else do
        s1 <- findType s1
        s2 <- findType s2
        return (s1,s2)
    printRule $ "boxyMatch: " <> ppretty s1 <+> ppretty s2
    b <- bm s1 s2
    if b then do
        printRule "SYM"
        printRule $ "boxyMatch: " <> ppretty s2 <+> ppretty s1
        b' <- bm s2 s1
        when b' $  fail $ "boxyMatch failure: " <> ppretty s1 <+> ppretty s2
     else return ()
   where
    bm (TMetaVar v1) (TMetaVar v2) = do
        var_meets_var v1 v2
        return False

    -- AEQ1
    bm (TArrow s1 s2) (TMetaVar mv) = do
        printRule "AEQ1"
        withMetaVars mv [getType s1, getType s2] (\ [t1,t2] -> TArrow t1 t2) $ \ [t1,t2] ->
            boxyMatch s1 t1 >> boxyMatch s2 t2
        return False

    -- AEQ2
    bm (TArrow s1 s2) (TArrow s3 s4) = do
        printRule "AEQ2"
        boxyMatch s1 s3
        boxyMatch s2 s4
        return False



    -- CEQ1

    bm a (TMetaVar mv) | (TCon ca,as) <- fromTAp a = do
        withMetaVars mv (map getType as) (\ ts -> foldl TAp (TCon ca) ts) $ \ ts ->
            sequence_ [ boxyMatch a t | t <- ts | a <- as ]
        return False

    bm a (TMetaVar mv) | (x,xs@(_:_)) <- fromTAp a = do
        let xxs = x:xs
        withMetaVars mv (map getType xxs) (\ (t:ts) -> foldl TAp t ts) $ \ ts ->
            sequence_ [ boxyMatch a t | t <- ts | a <- xxs ]
        return False


    -- CEQ2

    bm a b | (TCon ca,as) <- fromTAp a, (TCon cb,bs) <- fromTAp b = case ca == cb of
        -- False -> fail $ "constructor mismatch: " ++ show (a,b)
        False -> unificationError a b
        True | length as == length bs -> do
            printRule "CEQ2"
            sequence_ [boxyMatch x y | x <- as | y <- bs] >> return False
        -- _ ->   fail $ "constructor args mismatch: " ++ show (a,b)
        _ -> unificationError a b




    -- SEQ1
    bm (TForAll vs (ps :=> tbody)) (TMetaVar mv) = do
        withMetaVars mv [getType mv] (\ [t] -> TForAll vs (ps :=> t))  $ \ [t] ->
            boxyMatch tbody t
        return False

    -- SEQ2

    bm (TForAll vs (ps :=> t)) (TForAll vs' (ps' :=> t')) = fail "SEQ2"
    --bm a (TMetaVar mv)  = do
     --   varBind mv a
     --   return False

    -- XXX app
 --   bm a b | (t1,as1@(_:_)) <- fromTAp a, (t2,as2) <- fromTAp b = case sameLength as1 as2 of
 --       False -> unificationError a b
 --       True -> do
 --           t1 `boxyMatch` t2
 --           sequence_ [boxyMatch x y | x <- as1 | y <- as2] >> return False
 --           printRule "XXX Apps"
 --           return False
    bm (TAp a b) (TAp c d) = do
        printRule "XXX App"
        a `boxyMatch` c
        b `boxyMatch` d
        return False

    -- MEQ1 MEQ2  SYM
    bm a b
        | isTau a, TMetaVar mv <- b = printRule "MEQ1" >> varBind mv a >> return False
        | isTau a && isTau b = printRule "MEQ2" >> unify a b >> return False
    bm _ _ = return True

var_meets_var :: MetaVar -> MetaVar -> Tc ()
var_meets_var tv1 tv2 = do
    when (getType tv1 /= getType tv2) $ error "BBEQ boxyMatch kinds"
    f tv1 tv2
    where
    f tv1 tv2 | tv1 == tv2 = return ()
    f tv1 tv2 | isBoxyMetaVar tv1 && isBoxyMetaVar tv2 = do
            printRule "BBEQ"
            tt <- newMetaVar Tau (getType tv1)
            varBind tv1 tt
            varBind tv2 tt
    f tv1 tv2 | isBoxyMetaVar tv1  = do
            printRule "BBEQ-L"
            varBind tv1 (TMetaVar tv2)
    f tv1 tv2 | isBoxyMetaVar tv2  = do
            printRule "BBEQ-R"
            varBind tv2 (TMetaVar tv1)
    f tv1 tv2  = do
            printRule "BBEQ-Tau"
            varBind tv2 (TMetaVar tv1)



unify      :: Tau -> Tau -> Tc ()
unify t1 t2 = do
    t1' <- findType t1
    t2' <- findType t2
    printRule $ "unify: " <> ppretty t1 <+> ppretty t2
    mgu t1' t2'

mgu (TAp l r) (TAp l' r')
   = do s1 <- unify l l'
        s2 <- unify r r'
        return ()
mgu (TArrow l r) (TArrow l' r')
   = do s1 <- unify l l'
        s2 <- unify r r'
        return ()
mgu (TMetaVar u) t | not $ isBoxyMetaVar u = varBind u t
mgu t (TMetaVar u) | not $ isBoxyMetaVar u = varBind u t
mgu (TVar a) (TVar b) | a == b = return ()
mgu c1@(TCon tc1) c2@(TCon tc2)
           | tc1==tc2 = return ()
           -- | otherwise = fail $ "mgu: Constructors don't match:" ++ show (c1,c2)
           | otherwise = unificationError c1 c2
mgu TForAll {} _ = error "attempt to unify TForall"
mgu _ TForAll {} = error "attempt to unify TForall"
mgu t1 t2  = unificationError t1 t2

unifyList :: [Type] -> Tc ()
unifyList (t1:t2:ts) = unify t1 t2 >> unifyList (t2:ts)
unifyList _ = return ()


-- This is used in pattern matching because it might be polymorphic, but also needs to match exactly
--subsumesPattern a b | isTau b = a `boxyMatch` b
--subsumes




