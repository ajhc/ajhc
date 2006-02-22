module E.PrimOpt(primOpt,primOpt') where

import List
import Monad
import qualified Data.Map as Map

import Atom
import Support.CanType
import C.Prims
import DataConstructors
import Data.Monoid
import Doc.DocLike
import Doc.PPrint
import E.E
import E.Values
import Support.FreeVars
import GenUtil
import Util.NameMonad
import PrimitiveOperators
import Stats


-- Some of these arn't optimizations, but rather important transformations.

primOpt dataTable stats e = do
    runStatIO stats (primOpt' dataTable e)

create_integralCast dataTable e t = eCase e [Alt (LitCons cna [tvra] te) cc] Unknown  where
    te = getType e
    (vara:varb:_) = freeNames (freeVars (e,t))
    tvra =  tVr vara sta
    tvrb =  tVr varb stb
    Just (cna,sta,ta) = lookupCType' dataTable te
    Just (cnb,stb,tb) = lookupCType' dataTable t
    cc = if ta == tb then ELit (LitCons cnb [EVar tvra] t) else
        eStrictLet  tvrb (EPrim (APrim (CCast ta tb) mempty) [EVar tvra] stb)  (ELit (LitCons cnb [EVar tvrb] t))

unbox :: DataTable -> E -> Int -> (TVr -> E) -> E
unbox dataTable e vn wtd = eCase e  [Alt (LitCons cna [tvra] te) (wtd tvra)] Unknown where
    te = getType e
    tvra = tVr vn sta
    Just (cna,sta,ta) = lookupCType' dataTable te

intt = rawType "int"

rawMap = Map.fromList [ (rawType w,toAtom t) | (_,w,t) <- allCTypes]
typ_float = toAtom "float"

vars :: [E] -> [TVr]
vars ts = [ tVr n t | t <- ts | n <- [2,4 ..]]

primOpt' dataTable  (EPrim (APrim s _) xs t) | Just n <- primopt s xs t = do
    mtick (toAtom $ "E.PrimOpt." ++ braces (pprint s) )
    primOpt' dataTable  n  where
        primopt (PrimPrim "seq") [x,y] _  = return $ prim_seq x y
        primopt (Operator "==" [ta,tb] tr) [e,(ELit (LitInt x t))] rt | Map.lookup t rawMap /= Just typ_float = return $ eCase e [Alt (LitInt x t) (ELit (LitInt 1 intt)) ] (ELit (LitInt 0 intt))
        primopt (Operator "==" [ta,tb] tr) [(ELit (LitInt x t)),e] rt | Map.lookup t rawMap /= Just typ_float = return $ eCase e [Alt (LitInt x t) (ELit (LitInt 1 intt)) ] (ELit (LitInt 0 intt))
        primopt (Operator "-" [ta] tr) [ELit (LitInt x t)] rt | ta == tr && rt == t = return $ ELit (LitInt (negate x) t)
        primopt (PrimPrim "exitFailure__") [w] rt  = return $ EError "" rt
        primopt (PrimPrim "newRef__") [x,y] rt  = return $ EAp (EAp (ELam x' $ ELam y' $ eCaseTup' (EPrim (primPrim "newRef_") [EVar x',EVar y'] (ltTuple' [a,b])) [a',b'] (eTuple [EVar a',EVar b']) ) x) y where
            [x',y',a',b'] = vars [getType x,getType y,a,b]
            ELit (LitCons _ [a,b] (ESort EStar)) = rt
        primopt (PrimPrim "readRef__") [x,y] rt  = return $ EAp (EAp (ELam x' $ ELam y' $ eCaseTup' (EPrim (primPrim "readRef_") [EVar x',EVar y'] (ltTuple' [a,b])) [a',b'] (eTuple [EVar a',EVar b']) ) x) y where
            [x',y',a',b'] = vars [getType x,getType y,a,b]
            ELit (LitCons _ [a,b] (ESort EStar)) = rt
        primopt (PrimPrim "newHole__") [y] rt  = return $ EAp (ELam y' $ eCaseTup' (EPrim (primPrim "newHole_") [EVar y'] (ltTuple' [a,b])) [a',b'] (eTuple [EVar a',EVar b'])) y where
            [y',a',b'] = vars [getType y,a,b]
            ELit (LitCons _ [a,b] (ESort EStar)) = rt

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

        primopt (PrimPrim pn) [] t | Just c <-  getPrefix "const." pn = do
            (cn,st,ct) <- case lookupCType' dataTable t of
                Right x -> return x
                Left x -> error x
            let (var:_) = freeNames (freeVars t)
            return $ eStrictLet (tVr var st) (EPrim (APrim (CConst c ct) mempty) [] st) (ELit (LitCons cn [EVar $ tVr var st] t))
        primopt (PrimPrim "integralCast") [e] t = return $ create_integralCast dataTable e t
        primopt (PrimPrim "integralCast") es t = error $ "Invalid integralCast " ++ show (es,t)
        primopt (CCast _ _) [ELit (LitInt x _)] t = return $ ELit (LitInt x t)  -- TODO ensure constant fits
        primopt _ _ _ = fail "No primitive optimization to apply"
primOpt' _  x = return x



