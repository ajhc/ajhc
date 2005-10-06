module E.PrimOpt(primOpt,primOpt') where

import List
import Monad
import qualified Data.Map as Map

import Atom
import CanType
import C.Prims
import DataConstructors
import Data.Monoid
import Doc.DocLike
import Doc.PPrint
import E.E
import E.Values
import FreeVars
import GenUtil
import NameMonad
import PrimitiveOperators
import Stats


-- Some of these arn't optimizations, but rather important transformations.

primOpt dataTable stats e = do
    runStatIO stats (primOpt' dataTable e)

create_integralCast dataTable e t = ECase e (tVr 0 te) [Alt (LitCons cna [tvra] te) cc] Nothing  where
    te = getType e
    (vara:varb:_) = freeNames (freeVars (e,t))
    tvra =  tVr vara sta
    tvrb =  tVr varb stb
    Just (cna,sta,ta) = lookupCType' dataTable te
    Just (cnb,stb,tb) = lookupCType' dataTable t
    cc = if ta == tb then ELit (LitCons cnb [EVar tvra] t) else
        eStrictLet  tvrb (EPrim (APrim (CCast ta tb) mempty) [EVar tvra] stb)  (ELit (LitCons cnb [EVar tvrb] t))

unbox :: DataTable -> E -> Int -> (TVr -> E) -> E
unbox dataTable e vn wtd = ECase e (tVr 0 te) [Alt (LitCons cna [tvra] te) (wtd tvra)] Nothing where
    te = getType e
    tvra = tVr vn sta
    Just (cna,sta,ta) = lookupCType' dataTable te

intt = rawType "int"

rawMap = Map.fromList [ (rawType w,toAtom t) | (_,w,t) <- allCTypes]
typ_float = toAtom "float"

primOpt' dataTable  (EPrim (APrim s _) xs t) | Just n <- primopt s xs t = do
    mtick (toAtom $ "E.PrimOpt." ++ braces (pprint s) )
    primOpt' dataTable  n  where
        primopt (PrimPrim "seq") [x,y] _  = return $ prim_seq x y
        --primopt (PrimPrim "prim_op_aaB.==") [e,(ELit (LitInt x t)) ] rt = return $ eCase e [Alt (LitInt x t) (prim_unsafeCoerce vTrue rt)] (prim_unsafeCoerce vFalse rt)
        --primopt (PrimPrim "prim_op_aaB.==") [(ELit (LitInt x t)),e ] rt = return $ eCase e [Alt (LitInt x t) (prim_unsafeCoerce vTrue rt)] (prim_unsafeCoerce vFalse rt)
        primopt (Operator "==" [ta,tb] tr) [e,(ELit (LitInt x t))] rt | Map.lookup t rawMap /= Just typ_float = return $ eCase e [Alt (LitInt x t) (ELit (LitInt 1 intt)) ] (ELit (LitInt 0 intt))
        primopt (Operator "==" [ta,tb] tr) [(ELit (LitInt x t)),e] rt | Map.lookup t rawMap /= Just typ_float = return $ eCase e [Alt (LitInt x t) (ELit (LitInt 1 intt)) ] (ELit (LitInt 0 intt))
        primopt (Operator "-" [ta] tr) [ELit (LitInt x t)] rt | ta == tr && rt == t = return $ ELit (LitInt (negate x) t)

        {-
        primopt (PrimPrim "divide") [a,b] t = do
            (_,ta) <- lookupCType dataTable (typ a)
            (_,tb) <- lookupCType dataTable (typ b)
            (_,tr) <- lookupCType dataTable t
            unless (ta == tb && tb == tr) $ fail "bad divide"
            return $ EPrim (APrim (Operator "/" [ta,tb] tr) mempty) [a,b] t
        -}

        primopt (PrimPrim "divide") [a,b] t = ans where
            (vara:varb:varc:_) = freeNames (freeVars (a,b,t))
            Just (cna,sta,ta) = lookupCType' dataTable t
            ans = do
                (_,ta) <- lookupCType dataTable (getType a)
                (_,tb) <- lookupCType dataTable (getType b)
                (_,tr) <- lookupCType dataTable t
                unless (ta == tb && tb == tr) $ fail "bad divide"
                return $ unbox dataTable a vara $ \tvra ->
                    unbox dataTable b varb $ \tvrb ->
                        eStrictLet (tVr varc sta) (EPrim (APrim (Operator "/" [ta,ta] ta) mempty) [EVar tvra, EVar tvrb] sta) (ELit (LitCons cna [EVar (tVr varc sta)] t))

                --return $ EPrim (APrim (Operator "/" [ta,tb] tr) mempty) [a,b] t

        --primopt (PrimPrim pn) [] t | Just c <-  getPrefix "const." pn = do
        --    (_,ta) <- lookupCType dataTable t
        --    return $ EPrim (APrim (CConst c ta) mempty) [] t
        primopt (PrimPrim pn) [] t | Just c <-  getPrefix "const." pn = do
            (cn,st,ct) <- case lookupCType' dataTable t of
                Right x -> return x
                Left x -> error x
            let (var:_) = freeNames (freeVars t)
            return $ eStrictLet (tVr var st) (EPrim (APrim (CConst c ct) mempty) [] st) (ELit (LitCons cn [EVar $ tVr var st] t))


        primopt (PrimPrim "integralCast") [e] t = return $ create_integralCast dataTable e t
        --primopt (PrimPrim "integralCast") [e] t | Just (_,ta) <- lookupCType dataTable (typ e), Just (_,tb) <- lookupCType dataTable t =
        --    if ta == tb then return (prim_unsafeCoerce e t)  else return $ EPrim (APrim (CCast ta tb) mempty) [e] t
        primopt (PrimPrim "integralCast") es t = error $ "Invalid integralCast " ++ show (es,t)
        primopt (CCast _ _) [ELit (LitInt x _)] t = return $ ELit (LitInt x t)  -- TODO ensure constant fits
        --primopt (CCast x y) [e] t | x == y = return $ prim_unsafeCoerce e t
        primopt _ _ _ = fail "No primitive optimization to apply"
primOpt' _  x = return x



--primopt "primEqInt" [ELit (LitInt x _),ELit (LitInt y _) ] _ = return $ if x == y then vTrue else vFalse
--primopt "primEqChar" [ELit (LitInt x _),ELit (LitInt y _)] _ = return $ if x == y then vTrue else vFalse
--primopt "primEq" [ELit (LitInt x _),ELit (LitInt y _) ] _ = return $ if x == y then vTrue else vFalse
{-
--primopt (PrimPrim "seq") [x,y] _ | isWHNF x  = return y
primopt (PrimPrim "seq") [x,y] _  = return $ prim_seq x y
primopt (PrimPrim "ord") [ELit (LitInt x t)] _ | t == tChar = return $ ELit (LitInt x tInt)
primopt (PrimPrim "chr") [ELit (LitInt x t)] _ | t == tInt  = return $ ELit (LitInt x tChar)
--primopt "prim_op.==" [e,(ELit (LitInt x t)) ] _ = return $ eCase e [Alt (LitInt x t) vTrue] vFalse
--primopt "prim_op.==" [(ELit (LitInt x t)),e ] _ = return $ eCase e [Alt (LitInt x t) vTrue] vFalse
--primopt "prim_op.!=" [e,(ELit (LitInt x t)) ] _ = return $ eCase e [Alt (LitInt x t) vFalse] vTrue
--primopt "prim_op.!=" [(ELit (LitInt x t)),e ] _ = return $ eCase e [Alt (LitInt x t) vFalse] vTrue
primopt (PrimPrim "unsafeCoerce") [e'] t | Just (x,_) <- from_unsafeCoerce e' = return $ prim_unsafeCoerce x t
primopt (PrimPrim "unsafeCoerce") [EError err _] t  = return $ EError err t
primopt (PrimPrim "unsafeCoerce") [ELit (LitInt x _)] t  = return $ ELit (LitInt x t)
--primopt (PrimPrim "unsafeCoerce") [ELit (LitFrac x _)] t  = return $ ELit (LitFrac x t)
primopt (PrimPrim "unsafeCoerce") [ELit (LitCons x y _)] t  = return $ ELit (LitCons x y t)
primopt (PrimPrim "unsafeCoerce") [x] t | typ x == t = return x

primopt (PrimPrim "integralCast") [e'] t | Just (x,_) <- from_integralCast e' = return $ prim_integralCast x t
primopt (PrimPrim "integralCast") [EError err _] t  = return $ EError err t
primopt (PrimPrim "integralCast") [ELit (LitInt x _)] t  = return $ ELit (LitInt x t)
primopt (PrimPrim "integralCast") [ELit (LitCons x y _)] t  = return $ ELit (LitCons x y t)
primopt (PrimPrim "integralCast") [x] t | typ x == t = return x
primopt _ _ _ = fail "No primitive optimization to apply"

--primopt "unsafeCoerce" [ELetRec ds e] t  = return $ ELetRec ds (EPrim "unsafeCoerce" [e] t)
--primopt "unsafeCoerce" [ELetRec ds e] t  = return $ ELetRec ds (EPrim "unsafeCoerce" [e] t)

primOpt dataTable  stats (EPrim (APrim s _) xs t) | Just n <- primopt s xs t = do
    tick stats (toAtom $ "E.PrimOpt." ++ braces (pprint s) )
    primOpt dataTable stats n  where
        primopt (PrimPrim "seq") [x,y] _  = return $ prim_seq x y
        primopt (PrimPrim "prim_op_aaB.==") [e,(ELit (LitInt x t)) ] rt = return $ eCase e [Alt (LitInt x t) (prim_unsafeCoerce vTrue rt)] (prim_unsafeCoerce vFalse rt)
        primopt (PrimPrim "prim_op_aaB.==") [(ELit (LitInt x t)),e ] rt = return $ eCase e [Alt (LitInt x t) (prim_unsafeCoerce vTrue rt)] (prim_unsafeCoerce vFalse rt)
        --primopt "prim_op.!=" [e,(ELit (LitInt x t)) ] _ = return $ eCase e [Alt (LitInt x t) vFalse] vTrue
        --primopt "prim_op.!=" [(ELit (LitInt x t)),e ] _ = return $ eCase e [Alt (LitInt x t) vFalse] vTrue
        primopt (PrimPrim "unsafeCoerce") [e'] t | Just (x,_) <- from_unsafeCoerce e' = return $ prim_unsafeCoerce x t
        primopt (PrimPrim "unsafeCoerce") [EError err _] t  = return $ EError err t
        primopt (PrimPrim "unsafeCoerce") [ELit (LitInt x _)] t  = return $ ELit (LitInt x t)
        primopt (PrimPrim "unsafeCoerce") [ELit (LitCons x y _)] t  = return $ ELit (LitCons x y t)
        primopt (PrimPrim "unsafeCoerce") [x] t | typ x == t = return x

        primopt (PrimPrim "divide") [a,b] t = do
            (_,ta) <- lookupCType dataTable (typ a)
            (_,tb) <- lookupCType dataTable (typ b)
            (_,tr) <- lookupCType dataTable t
            unless (ta == tb && tb == tr) $ fail "bad divide"
            return $ EPrim (APrim (Operator "/" [ta,tb] tr) mempty) [a,b] t

        primopt (PrimPrim pn) [] t | Just c <-  getPrefix "const." pn = do
            (_,ta) <- lookupCType dataTable t
            return $ EPrim (APrim (CConst c ta) mempty) [] t


        primopt (PrimPrim "integralCast") [e] t | Just (_,ta) <- lookupCType dataTable (typ e), Just (_,tb) <- lookupCType dataTable t =
            if ta == tb then return (prim_unsafeCoerce e t)  else return $ EPrim (APrim (CCast ta tb) mempty) [e] t
        primopt (PrimPrim "integralCast") es t = error $ "Invalid integralCast " ++ show (es,t)
        primopt (CCast _ _) [ELit (LitInt x _)] t = return $ ELit (LitInt x t)  -- TODO ensure constant fits
        primopt (CCast x y) [e] t | x == y = return $ prim_unsafeCoerce e t

        --primopt (PrimPrim "integralCast") [e'] t | Just (x,_) <- from_integralCast e' = return $ prim_integralCast x t
        --primopt (PrimPrim "integralCast") [EError err _] t  = return $ EError err t
        --primopt (PrimPrim "integralCast") [ELit (LitInt x _)] t  = return $ ELit (LitInt x t)
        --primopt (PrimPrim "integralCast") [ELit (LitCons x y _)] t  = return $ ELit (LitCons x y t)
        --primopt (PrimPrim "integralCast") [x] t | typ x == t = return x
        primopt _ _ _ = fail "No primitive optimization to apply"
primOpt _ _stats x = return x
-}
