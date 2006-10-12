module E.PrimOpt(
    primOpt',
    processPrimPrim
    ) where

import List
import Monad
import qualified Data.Map as Map

import Atom
import C.Prims
import DataConstructors
import Data.Monoid
import Doc.DocLike
import Doc.PPrint
import E.E
import E.Values
import GenUtil
import Name.Id
import Name.Names
import Name.VConsts
import PrimitiveOperators
import Stats
import Support.CanType
import Support.FreeVars
import Util.HasSize
import Util.NameMonad(genNames)
import Util.SetLike



create_integralCast dataTable e t = eCase e [Alt (litCons { litName = cna, litArgs = [tvra], litType = te }) cc] Unknown  where
    te = getType e
    (vara:varb:_) = freeNames (freeVars (e,t))
    tvra =  tVr vara sta
    tvrb =  tVr varb stb
    Just (cna,sta,ta) = lookupCType' dataTable te
    Just (cnb,stb,tb) = lookupCType' dataTable t
    cc = if ta == tb then ELit (litCons { litName = cnb, litArgs = [EVar tvra], litType = t }) else
        eStrictLet  tvrb (EPrim (APrim (CCast ta tb) mempty) [EVar tvra] stb)  (ELit (litCons { litName = cnb, litArgs = [EVar tvrb], litType = t }))

unbox :: DataTable -> E -> Int -> (TVr -> E) -> E
unbox dataTable e vn wtd = eCase e  [Alt (litCons { litName = cna, litArgs = [tvra], litType = te }) (wtd tvra)] Unknown where
    te = getType e
    tvra = tVr vn sta
    Just (cna,sta,ta) = lookupCType' dataTable te

intt = rawType "int"

rawMap = Map.fromList [ (rawType w,toAtom t) | (_,_,_,w,t) <- allCTypes]
typ_float = toAtom "float"

vars :: [E] -> [TVr]
vars ts = [ tVr n t | t <- ts | n <- [2,4 ..], n `notElem` fvs] where
    fvs = freeVars ts

iTrue = (ELit (LitInt 1 intt))
iFalse = (ELit (LitInt 0 intt))
isIntegral t = Map.lookup t rawMap /= Just typ_float

cextra Operator {} [] = ""
cextra Operator {} xs = '.':map f xs where
    f ELit {} = 'c'
    f _ = 'e'
cextra _ _ = ""

primOpt' dataTable  (EPrim (APrim s _) xs t) | Just n <- primopt s xs t = do
    mtick (toAtom $ "E.PrimOpt." ++ braces (pprint s) ++ cextra s xs )
    primOpt' dataTable  n  where

        -- constant operations
        primopt (Operator "+" [ta,tb] tr) [(ELit (LitInt l1 t1)),(ELit (LitInt l2 t2))] rt  = return $ (ELit (LitInt (l1 + l2) rt))
        primopt (Operator "-" [ta,tb] tr) [(ELit (LitInt l1 t1)),(ELit (LitInt l2 t2))] rt  = return $ (ELit (LitInt (l1 - l2) rt))
        primopt (Operator "*" [ta,tb] tr) [(ELit (LitInt l1 t1)),(ELit (LitInt l2 t2))] rt  = return $ (ELit (LitInt (l1 * l2) rt))
        primopt (Operator "==" [ta,tb] tr) [(ELit (LitInt l1 t1)),(ELit (LitInt l2 t2))] rt  = return $ if l1 == l2 then iTrue else iFalse
        primopt (Operator ">=" [ta,tb] tr) [(ELit (LitInt l1 t1)),(ELit (LitInt l2 t2))] rt  = return $ if l1 >= l2 then iTrue else iFalse
        primopt (Operator "<=" [ta,tb] tr) [(ELit (LitInt l1 t1)),(ELit (LitInt l2 t2))] rt  = return $ if l1 <= l2 then iTrue else iFalse
        primopt (Operator ">" [ta,tb] tr) [(ELit (LitInt l1 t1)),(ELit (LitInt l2 t2))] rt  = return $ if l1 > l2 then iTrue else iFalse
        primopt (Operator "<" [ta,tb] tr) [(ELit (LitInt l1 t1)),(ELit (LitInt l2 t2))] rt  = return $ if l1 < l2 then iTrue else iFalse
        primopt (Operator "-" [ta] tr) [ELit (LitInt x t)] rt | ta == tr && rt == t = return $ ELit (LitInt (negate x) t)
        -- compare of equals
        primopt (Operator "==" [ta,tb] tr) [e1,e2] rt | e1 == e2  = return iTrue
        primopt (Operator ">=" [ta,tb] tr) [e1,e2] rt | e1 == e2  = return iTrue
        primopt (Operator "<=" [ta,tb] tr) [e1,e2] rt | e1 == e2  = return iTrue
        primopt (Operator ">" [ta,tb] tr) [e1,e2] rt | e1 == e2  = return iFalse
        primopt (Operator "<" [ta,tb] tr) [e1,e2] rt | e1 == e2  = return iFalse
        -- x + 0 = x
        primopt (Operator "+" [ta,tb] tr) [e1,(ELit (LitInt 0 t))] rt  = return $ e1
        primopt (Operator "+" [ta,tb] tr) [(ELit (LitInt 0 t)),e1] rt  = return $ e1
        -- x * 0 = 0
        primopt (Operator "*" [ta,tb] tr) [_,(ELit (LitInt 0 t))] rt  = return $ (ELit (LitInt 0 t))
        primopt (Operator "*" [ta,tb] tr) [(ELit (LitInt 0 t)),_] rt  = return $ (ELit (LitInt 0 t))
        -- x * 1 = x
        primopt (Operator "*" [ta,tb] tr) [e1,(ELit (LitInt 1 t))] rt  = return $ e1
        primopt (Operator "*" [ta,tb] tr) [(ELit (LitInt 1 t)),e1] rt  = return $ e1
        -- x / 1 = x
        primopt (Operator "/" [ta,tb] tr) [e1,(ELit (LitInt 1 t))] rt  = return $ e1
        -- x / x = 1  - check for 0 / 0
        --primopt (Operator "/" [ta,tb] tr) [e1,e2] rt | e1 == e2  = return $ (ELit (LitInt 1 rt))
        -- 0 / x = 0  - check for 0 / 0
        --primopt (Operator "/" [ta,tb] tr) [(ELit (LitInt 0 t)),_] rt  = return $ (ELit (LitInt 0 t))
        -- x - 0 = x
        primopt (Operator "-" [ta,tb] tr) [e1,(ELit (LitInt 0 t))] rt  = return $ e1
        -- 0 - x = -x
        primopt (Operator "-" [ta,tb] tr) [(ELit (LitInt 0 t)),e1] rt  = return $ EPrim (APrim (Operator "-" [ta] tr) mempty) [e1] rt
        -- x << 0 = x, x >> 0 = x
        primopt (Operator "<<" [ta,tb] tr) [e1,(ELit (LitInt 0 t))] rt  = return $ e1
        primopt (Operator ">>" [ta,tb] tr) [e1,(ELit (LitInt 0 t))] rt  = return $ e1
        -- x % 1 = 0
        primopt (Operator "%" [ta,tb] tr) [e1,(ELit (LitInt 1 t))] rt  = return $ (ELit (LitInt 0 rt))
        -- x % x = 0 - check for 0 % 0
        --primopt (Operator "%" [ta,tb] tr) [e1,e2] rt | e1 == e2  = return $ (ELit (LitInt 0 rt))
        -- 0 % x = 0 - check for 0 % 0
        --primopt (Operator "%" [ta,tb] tr) [(ELit (LitInt 0 t)),_] rt  = return $ (ELit (LitInt 0 t))
        -- eq to case
        primopt (Operator "==" [ta,tb] tr) [e,(ELit (LitInt x t))] rt | isIntegral t  = return $ eCase e [Alt (LitInt x t) iTrue ] iFalse
        primopt (Operator "==" [ta,tb] tr) [(ELit (LitInt x t)),e] rt | isIntegral t = return $ eCase e [Alt (LitInt x t) iTrue ] iFalse
        -- cast of constant
        primopt (CCast _ _) [ELit (LitInt x _)] t = return $ ELit (LitInt x t)  -- TODO ensure constant fits
        primopt _ _ _ = fail "No primitive optimization to apply"
primOpt' _  x = return x


processPrimPrim dataTable o@(EPrim (APrim prim _) es t) = case primopt prim es t of
            Just e -> e
            Nothing -> o
        where
        primopt (PrimPrim "seq") [x,y] _  = return $ prim_seq x y
        primopt (PrimPrim "exitFailure__") [w] rt  = return $ EError "" rt
        primopt (PrimPrim "newRef__") [x,y] rt  = return $ EAp (EAp (ELam x' $ ELam y' $ eCaseTup' (EPrim (primPrim "newRef_") [EVar x',EVar y'] (ltTuple' [a,b])) [a',b'] (eTuple [EVar a',EVar b']) ) x) y where
            [x',y',a',b'] = vars [getType x,getType y,a,b]
            ELit LitCons { litArgs = [a,b], litType = ESort EStar } = rt
        primopt (PrimPrim "readRef__") [x,y] rt  = return $ EAp (EAp (ELam x' $ ELam y' $ eCaseTup' (EPrim (primPrim "readRef_") [EVar x',EVar y'] (ltTuple' [a,b])) [a',b'] (eTuple [EVar a',EVar b']) ) x) y where
            [x',y',a',b'] = vars [getType x,getType y,a,b]
            ELit LitCons { litArgs = [a,b], litType = ESort EStar } = rt
        primopt (PrimPrim "newHole__") [y] (ELit LitCons { litName = name, litArgs = [b], litType = ESort EStar }) | name == tc_IOResult =
            return $ eCaseTup' (EPrim (primPrim "newHole_") [y] (ltTuple' [tWorld__,b])) [a',b'] (eJustIO (EVar a') (EVar b')) where
                (a':b':_) = vars [tWorld__,b,y]

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
                        eStrictLet (tVr varc sta) (EPrim (APrim (Operator "/" [ta,ta] ta) mempty) [EVar tvra, EVar tvrb] sta) (ELit (litCons { litName = cna, litArgs = [EVar (tVr varc sta)], litType = t }))

        primopt (PrimPrim pn) [] t | Just c <-  getPrefix "const." pn = do
            (cn,st,ct) <- case lookupCType' dataTable t of
                Right x -> return x
                Left x -> error x
            let (var:_) = freeNames (freeVars t)
            return $ eStrictLet (tVr var st) (EPrim (APrim (CConst c ct) mempty) [] st) (ELit (litCons { litName = cn, litArgs = [EVar $ tVr var st], litType = t }))
        primopt (PrimPrim "integralCast") [e] t = return $ create_integralCast dataTable e t
        primopt (PrimPrim "integralCast") es t = error $ "Invalid integralCast " ++ show (es,t)
        primopt _ _ _ = fail "not a primopt we care about"


-- | Generate an infinite list of names not present in the given set.
freeNames :: IdSet -> [Id]
freeNames s  = filter (not . (`member` s)) (genNames (size s))


