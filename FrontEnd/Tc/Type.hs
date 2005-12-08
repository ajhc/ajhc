module FrontEnd.Tc.Type where

import Control.Monad.Trans
import Control.Monad.Writer
import Data.IORef
import qualified Data.Map as Map

import Representation
import FrontEnd.Tc.Monad
import GenUtil

type Box = IORef Type
type BoxySigma = Sigma

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

subsumes :: BoxySigma -> BoxySigma -> Tc ()
-- SBOXY
subsumes (TBox a) b = boxyMatch (TBox a) b

-- SKOL needs to be after SBOXY
subsumes s1 fa@TForAll {} = do
    ps :=> r1 <- freshInst fa
    tell ps
    s1 `subsumes` r1

-- SPEC
subsumes (TForAll as (_ :=> r1))  r2 | isRho' r2 = do
    bs <- mapM (const newBox) as
    inst (Map.fromList $ zip (map tyvarAtom as) (snds bs)) r1 `subsumes` r2

-- CON (??)
subsumes s1@TAp {} s2 = s1 `boxyMatch` s2

-- F1
subsumes (TArrow s1 s2) (TArrow s3 s4) = do
    boxyMatch s3 s1
    subsumes s2 s4
-- F2
subsumes t@(TArrow _ _) (TBox box) = do
    (oa,a) <- newBox
    (ob,b) <- newBox
    subsumes t (a `fn` b)
    na <- oa
    nb <- ob
    fillBox box (na `fn` nb)

-- BMONO & MONO
subsumes a b | isTau a = case b of
    (TBox b) -> fillBox b a
    _ | a == b -> return ()
      | isTau b -> unify a b -- TODO verify? fail $ "taus don't match in MONO" ++ show (a,b)

-- -- MONO
-- subsumes (TCon a) (TCon b) | a == b = return ()

boxyMatch :: BoxySigma -> BoxySigma -> Tc ()

-- BBEQ
boxyMatch (TBox ba) (TBox bb) = do
    tt <- newTVar Star -- is star always right?
    fillBox ba tt
    fillBox bb tt

-- AEQ1
boxyMatch (TArrow s1 s2) (TBox box) = do
    (ra,a) <- newBox
    (rb,b) <- newBox
    boxyMatch (s1 `fn` s2) (a `fn` b)
    x <- ra
    y <- rb
    fillBox box (x `fn` y)

-- AEQ2
boxyMatch (TArrow s1 s2) (TArrow s3 s4) = do
    boxyMatch s1 s3
    boxyMatch s2 s4

-- SEQ1 
--boxyMatch 

-- MEQ1 MEQ2
boxyMatch a b | isTau a = case b of
    (TBox b) -> fillBox b a
    _ | a == b -> return ()
      | isTau b -> unify a b -- TODO, verify? fail $ "taus don't match in MEQ[12]" ++ show (a,b)

-- SYM (careful!)
boxyMatch a b = boxyMatch b a
