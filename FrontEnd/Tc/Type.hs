module FrontEnd.Tc.Type where

import Control.Monad.Trans
import Control.Monad.Writer
import Data.IORef
import qualified Data.Map as Map

import Representation
import FrontEnd.Tc.Monad
import GenUtil

type Box = IORef Type
type Sigma' = Sigma
type Tau' = Tau
type Rho' = Rho

type MetaTV = Tyvar
type SkolemTV = Tyvar
type BoundTV = Tyvar

openBox :: Box -> Tc Type
openBox x = liftIO $ readIORef x

fillBox :: Box -> Type -> Tc ()
fillBox x t = liftIO $ writeIORef x t

isTau :: Type -> Bool
isTau TForAll {} = False
isTau TBox {} = False
isTau (TAp a b) = isTau a && isTau b
isTau (TArrow a b) = isTau a && isTau b
isTau _ = True

isTau' :: Type -> Bool
isTau' TForAll {} = False
isTau' (TAp a b) = isTau a && isTau b
isTau' (TArrow a b) = isTau a && isTau b
isTau' _ = True


isRho' :: Type -> Bool
isRho' TForAll {} = False
isRho' _ = True

subsumes :: Sigma' -> Sigma' -> Tc ()
subsumes s1 s2 = do
    s1 <- findType s1
    s2 <- findType s2
    sub s1 s2
   where
    -- SBOXY
    sub (TBox k a) b = boxyMatch (TBox k a) b

    -- SKOL needs to be after SBOXY
    sub s1 fa@TForAll {} = do
        r1 <- freshInstance fa
        s1 `subsumes` r1

    -- SPEC
    sub (TForAll as (_ :=> r1))  r2 | isRho' r2 = do
        bs <- mapM (const $ newBox Star) as
        inst (Map.fromList $ zip (map tyvarAtom as) (snds bs)) r1 `subsumes` r2

    -- CON (??)
    sub s1@TAp {} s2 = s1 `boxyMatch` s2

    -- F1
    sub (TArrow s1 s2) (TArrow s3 s4) = do
        boxyMatch s3 s1
        subsumes s2 s4
    -- F2
    sub t@(TArrow _ _) (TBox _ box) = do
        (oa,a) <- newBox Star
        (ob,b) <- newBox Star
        subsumes t (a `fn` b)
        na <- oa
        nb <- ob
        fillBox box (na `fn` nb)

    -- BMONO & MONO
    sub a b | isTau a = case b of
        (TBox _ b) -> fillBox b a
        _ | isTau b -> unify a b -- TODO verify? fail $ "taus don't match in MONO" ++ show (a,b)
        _ -> fail $ "subsumes: " ++ show (a,b)

    sub a b = fail $ "subsumes: " ++ show (a,b)


boxyMatch :: Sigma' -> Sigma' -> Tc ()
boxyMatch s1 s2 = do
    s1 <- findType s1
    s2 <- findType s2
    b <- bm s1 s2
    if b then do
        b' <- bm s2 s1
        when b' $  fail $ "boxyMatch failure: " ++ show (s1,s2)
     else return ()
   where
    -- BBEQ
    bm (TBox k1 ba) (TBox k2 bb) = do
        when (k1 /= k2) $ error "boxyMatch kinds"
        tt <- newTVar k1
        fillBox ba tt
        fillBox bb tt
        return False

    -- AEQ1
    bm (TArrow s1 s2) (TBox _ box) = do
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

    bm a (TBox _ box) | (TCon ca,as) <- fromTAp a = do
        bs <- mapM (const $ newBox Star) as
        sequence_ [boxyMatch x y | x <- as | y <- snds bs]
        bs <- sequence $ fsts bs
        fillBox box (foldl TAp (TCon ca) bs)
        return False


    -- CEQ2

    bm a b | (TCon ca,as) <- fromTAp a, (TCon cb,bs) <- fromTAp b = case ca == cb of
        False -> fail $ "constructor mismatch: " ++ show (a,b)
        True -> sequence_ [boxyMatch x y | x <- as | y <- bs] >> return False


    -- SEQ1
    bm (TForAll vs (ps :=> t)) (TBox _ box) = do
        (ra,a) <- newBox Star
        boxyMatch t a
        a <- ra
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


    -- MEQ1 MEQ2  SYM
    bm a b | isTau a = case b of
        (TBox _ b) -> fillBox b a >> return False
        _ | isTau b -> unify a b >> return False -- TODO, verify? fail $ "taus don't match in MEQ[12]" ++ show (a,b)
          | otherwise -> return True
    bm _ _ = return True


fromTAp t = f t [] where
    f (TAp a b) rs = f a (b:rs)
    f t rs = (t,reverse rs)
