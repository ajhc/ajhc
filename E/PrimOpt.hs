module E.PrimOpt(
    primOpt',
    processPrimPrim
    ) where

import List
import Monad
import Control.Monad.Fix
import Maybe
import qualified Data.Map as Map

import Atom
import PackedString
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


{-

The primitive operators provided which may be imported into code are

'seq' - evaluate first argument to WHNF, return second one
plus/divide/minus  - perform operation on primitive type
zero/one - the zero and one values for primitive types
const.<foo> - evaluates to the C constant <foo>
error.<err> - equivalent to 'error <err>'
exitFailure__ - abort program immediately with no message
integralCast - cast between primitive integral types with c semantics
increment/decrement - increment or decrement a primitive numeric type by 1

-}

create_integralCast dataTable e t = eCase e [Alt (litCons { litName = cna, litArgs = [tvra], litType = te }) cc] Unknown  where
    te = getType e
    (vara:varb:_) = newIds (freeVars (e,t))
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


processPrimPrim :: DataTable -> E -> E
processPrimPrim dataTable o@(EPrim (APrim (PrimPrim s) _) es orig_t) = maybe o id (primopt (unpackPS s) es (followAliases dataTable orig_t)) where
    binOps = [("divide","/"),("plus","+"),("minus","-"),("times","*"),("modulus","%")]

    primopt "seq" [x,y] _  = return $ prim_seq x y
    primopt "exitFailure__" [w] rt  = return $ EError "" rt
    primopt op [a,b] t | Just cop <- lookup op binOps = mdo
        (pa,(ta,sta)) <- extractPrimitive dataTable a
        (pb,(tb,stb)) <- extractPrimitive dataTable b
        (bp,(tr,str)) <- boxPrimitive dataTable
                (EPrim (APrim (Operator cop [ta,ta] tr) mempty) [pa, pb] str) t
        return bp
    primopt "equalsChar" [a,b] t = return (EPrim (APrim (Operator "==" ["HsChar","HsChar"] "int") mempty) [a,b] t)
    primopt "constPeekByte" [a] t = return (EPrim (APrim (Peek "uint8_t") mempty) [a] t)
    primopt "box" [a] t = return ans where
        Just (cna,sta,ta) = lookupCType' dataTable t
        ans = ELit litCons { litName = cna, litArgs = [a], litType = orig_t }
    primopt "unbox" [a] t = return ans where
        (vara:_) = newIds (freeVars (a,t,orig_t))
        ans = unbox dataTable a vara $ \tvra -> EVar tvra
    primopt op [a] t | Just o <- lookup op unop = do
        (pa,(ta,sta)) <- extractPrimitive dataTable a
        let tvra = tVr vn sta; (vn:_) = newIds (freeVars (a,t))
        (bp,(tr,str)) <- boxPrimitive dataTable (EVar tvra) t
        let res = EPrim (APrim (Operator o [ta,ta] tr) mempty) [pa, ELit (LitInt 1 sta)] str
        return $ eStrictLet tvra res bp
        where unop = [("increment","+"),("decrement","-")]
    primopt n [] t | Just num <- lookup n vs = mdo
        (res,(_,sta)) <- boxPrimitive dataTable (ELit (LitInt num sta)) t; return res
        where vs = [("zero",0),("one",1)]
    primopt pn [] t | Just c <-  getPrefix "const." pn = mdo
        (res,(ta,sta)) <- boxPrimitive dataTable (EPrim (APrim (CConst c ta) mempty) [] sta) t; return res
    primopt pn [] _ | Just c <-  getPrefix "error." pn = return (EError c orig_t)
    primopt "integralCast" [e] t = return $ create_integralCast dataTable e t
    primopt "integralCast" es t = error $ "Invalid integralCast " ++ show (es,t)
    primopt _ _ _ = fail "not a primopt we care about"
processPrimPrim _ e = e



