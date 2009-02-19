module E.PrimOpt(
    primOpt',
    processPrimPrim
    ) where

import List
import Monad
import Control.Monad.Fix()
import Maybe

import StringTable.Atom
import C.Prims
import C.Arch
import Cmm.OpEval
import DataConstructors
import Data.Monoid
import Doc.DocLike
import Doc.PPrint
import E.E
import E.Values
import GenUtil
import Name.Id
import Stats
import Support.CanType
import Support.FreeVars
import qualified Cmm.Op as Op


{-

The primitive operators provided which may be imported into code are

'seq' - evaluate first argument to WHNF, return second one
plus/divide/minus  - perform operation on primitive type
zero/one - the zero and one values for primitive types
const.<foo> - evaluates to the C constant <foo>
error.<err> - equivalent to 'error <err>'
exitFailure__ - abort program immediately with no message
increment/decrement - increment or decrement a primitive numeric type by 1

-}


unbox :: DataTable -> E -> Id -> (TVr -> E) -> E
unbox dataTable e vn wtd = eCase e  [Alt (litCons { litName = cna, litArgs = [tvra], litType = te }) (wtd tvra)] Unknown where
    te = getType e
    tvra = tVr vn sta
    Just (cna,sta,_ta) = lookupCType' dataTable te



cextra Op {} [] = ""
cextra Op {} xs = '.':map f xs where
    f ELit {} = 'c'
    f EPrim {} = 'p'
    f _ = 'e'
cextra _ _ = ""

primConv cop t1 t2 e rt = EPrim (APrim (Op (Op.ConvOp cop t1) t2) mempty) [e] rt

primOpt' dataTable  e@(EPrim (APrim s _) xs t) = do
    let primopt (Op (Op.BinOp bop t1 t2) tr) [e1,e2] rt = binOp bop t1 t2 tr e1 e2 rt
        primopt (Op (Op.ConvOp cop t1) t2) [ELit (LitInt n t)] rt = return $ ELit (LitInt (convNumber cop t1 t2 n) rt)
        primopt (Op (Op.ConvOp cop t1) t2) [e1] rt = case convOp cop t1 t2 of
            Nothing | getType e1 == rt -> return e1
            Just cop' | cop' /= cop -> return $ primConv cop' t1 t2 e1 rt
            _ -> fail "could noUnt apply conversion optimization"
        primopt (Op (Op.UnOp bop t1) tr) [e1] rt = unOp bop t1 tr e1 rt
        primopt _ _ _ = fail "No Primitive optimization to apply"
    case primopt s xs t of
        Just n -> do
            mtick (toAtom $ "E.PrimOpt." ++ braces (pprint s) ++ cextra s xs )
            primOpt' dataTable  n
        Nothing -> return e
primOpt' _ e = return e

instance Expression E E where
    toBool True = ELit lTruezh
    toBool False = ELit lFalsezh
    toConstant (ELit (LitInt n t)) = return (n,t)
    toConstant _ = Nothing
    equalsExpression e1 e2 = e1 == e2
    caseEquals scrut (n,t) e1 e2 = eCase scrut [Alt (LitInt n t) e1 ] e2
    toExpression n t = (ELit (LitInt n t))
    createBinOp bop t1 t2 tr e1 e2 str =
                EPrim (APrim Op { primCOp = Op.BinOp bop t1 t2, primRetTy = tr } mempty) [e1, e2] str
    createUnOp bop t1 tr e1 str =
                EPrim (APrim Op { primCOp = Op.UnOp bop t1, primRetTy = tr } mempty) [e1] str
    fromBinOp (EPrim (APrim Op { primCOp = Op.BinOp bop t1 t2, primRetTy = tr } mempty) [e1, e2] str) = Just (bop,t1,t2,tr,e1,e2,str)
    fromBinOp _ = Nothing
    fromUnOp (EPrim (APrim Op { primCOp = Op.UnOp bop t1, primRetTy = tr } mempty) [e1] str) = Just (bop,t1,tr,e1,str)
    fromUnOp _ = Nothing


{-

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
-}


processPrimPrim :: DataTable -> E -> E
processPrimPrim dataTable o@(EPrim (APrim (PrimPrim s) _) es orig_t) = maybe o id (primopt (fromAtom s) es (followAliases dataTable orig_t)) where
    primopt "seq" [x,y] _  = return $ prim_seq x y
    primopt "exitFailure__" [w] rt  = return $ EError "" rt
    primopt op [a,b] t | Just cop <- readM op = mdo
        (pa,(ta,sta)) <- extractPrimitive dataTable a
        (pb,(tb,stb)) <- extractPrimitive dataTable b
        (bp,(tr,str)) <- boxPrimitive dataTable
                (EPrim (APrim Op { primCOp = Op.BinOp cop (stringToOpTy ta) (stringToOpTy tb), primRetTy = (stringToOpTy tr) } mempty) [pa, pb] str) t
        return bp
    primopt op [a] t | Just cop <- readM op = mdo
        (pa,(ta,sta)) <- extractPrimitive dataTable a
        (bp,(tr,str)) <- boxPrimitive dataTable
                (EPrim (APrim Op { primCOp = Op.UnOp cop (stringToOpTy ta), primRetTy = (stringToOpTy tr) } mempty) [pa] str) t
        return bp
    primopt op [a] t | Just cop <- readM op = mdo
        (pa,(ta,sta)) <- extractPrimitive dataTable a
        (bp,(tr,str)) <- boxPrimitive dataTable
                (EPrim (APrim Op { primCOp = Op.ConvOp cop (stringToOpTy ta), primRetTy = (stringToOpTy tr) } mempty) [pa] str) t
        return bp
    primopt "constPeekByte" [a] t = return (EPrim (APrim (Peek Op.bits8) mempty) [a] t)
    primopt "box" [a] t = return ans where
        Just (cna,_sta,_ta) = lookupCType' dataTable t
        ans = ELit litCons { litName = cna, litArgs = [a], litType = orig_t }
    primopt "unbox" [a] t = return ans where
        (vara:_) = newIds (freeVars (a,t,orig_t))
        ans = unbox dataTable a vara $ \tvra -> EVar tvra
    primopt op [a] t | Just o <- lookup op unop = do
        (pa,(ta,sta)) <- extractPrimitive dataTable a
        let tvra = tVr vn sta; (vn:_) = newIds (freeVars (a,t))
        (bp,(tr,str)) <- boxPrimitive dataTable (EVar tvra) t
        let res = EPrim (APrim (Op (Op.BinOp o (stringToOpTy ta) (stringToOpTy ta)) (stringToOpTy tr)) mempty) [pa, ELit (LitInt 1 sta)] str
        return $ eStrictLet tvra res bp
        where unop = [("increment",Op.Add),("decrement",Op.Sub),("fincrement",Op.FAdd),("fdecrement",Op.FSub)]
    primopt n [] t | Just num <- lookup n vs = mdo
        (res,(_,sta)) <- boxPrimitive dataTable (ELit (LitInt num sta)) t; return res
        where vs = [("zero",0),("one",1)]
    primopt pn [a,w] t | Just c <- getPrefix "peek." pn      >>= Op.readTy = return (EPrim (APrim (Peek c) mempty) [w,a] t)
    primopt pn [a,v,w] t | Just c <- getPrefix "poke." pn    >>= Op.readTy = return (EPrim (APrim (Poke c) mempty) [w,a,v] t)
    primopt pn [v] t | Just c <- getPrefix "sizeOf." pn      >>= Op.readTy = return (EPrim (APrim (PrimTypeInfo c Op.bits32 PrimSizeOf) mempty) [] t)
    primopt pn [v] t | Just c <- getPrefix "alignmentOf." pn >>= Op.readTy = return (EPrim (APrim (PrimTypeInfo c Op.bits32 PrimAlignmentOf) mempty) [] t)
    primopt pn [v] t | Just c <- getPrefix "maxBound." pn    >>= Op.readTy = return (EPrim (APrim (PrimTypeInfo c c PrimMaxBound) mempty) [] t)
    primopt pn [v] t | Just c <- getPrefix "minBound." pn    >>= Op.readTy = return (EPrim (APrim (PrimTypeInfo c c PrimMinBound) mempty) [] t)
    primopt pn [v] t | Just c <- getPrefix "umaxBound." pn   >>= Op.readTy = return (EPrim (APrim (PrimTypeInfo c c PrimUMaxBound) mempty) [] t)
    primopt pn [] t | Just c <-  getPrefix "const.M_PI" pn = mdo
        (res,(ta,sta)) <- boxPrimitive dataTable (ELit (LitInt (realToFrac (pi :: Double)) sta)) t; return res
    primopt pn [] t | Just c <-  getPrefix "const." pn = mdo
        (res,(ta,sta)) <- boxPrimitive dataTable (EPrim (APrim (CConst c ta) mempty) [] sta) t; return res
    primopt pn [] _ | Just c <-  getPrefix "error." pn = return (EError c orig_t)
    primopt _ _ _ = fail "not a primopt we care about"
processPrimPrim _ e = e



