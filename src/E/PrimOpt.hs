{-# LANGUAGE RecursiveDo #-}
module E.PrimOpt(
    performPrimOpt,
    processPrimPrim
    ) where

import List
import Monad
import Control.Monad.Fix()
import Maybe

import StringTable.Atom
import C.Prims
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
import Cmm.Op(stringToOpTy)


{-@Extensions

# Foreign Primitives

In addition to foreign imports of external functions as described in the FFI
spec. Jhc supports 'primitive' imports that let you communicate primitives directly
to the compiler. In general, these should not be used other than in the implementation
of the standard libraries. They generally do little error checking as it is assumed you
know what you are doing if you use them. All haskell visible entities are
introduced via foreign declarations in jhc.

They all have the form

    foreign import primitive "specification" haskell_name :: type

where "specification" is one of the following

seq
: evaluate first argument to WHNF, then return the second argument

zero,one
: the values zero and one of any primitive type.

const.C_CONSTANT
: the text following const is directly inserted into the resulting C file

peek.TYPE
: the peek primitive for raw value TYPE

poke.TYPE
: the poke primitive for raw value TYPE

sizeOf.TYPE, alignmentOf.TYPE, minBound.TYPE, maxBound.TYPE, umaxBound.TYPE
: various properties of a given internal type.

error.MESSAGE
: results in an error with constant message MESSAGE.

constPeekByte
: peek of a constant value specialized to bytes, used internally by Jhc.String

box
: take an unboxed value and box it, the shape of the box is determined by the type at which this is imported

unbox
: take an boxed value and unbox it, the shape of the box is determined by the type at which this is imported

increment, decrement
: increment or decrement a numerical integral primitive value

fincrement, fdecrement
: increment or decrement a numerical floating point primitive value

exitFailure__
: abort the program immediately

C-- Primitive
: any C-- primitive may be imported in this manner.

-}



unbox :: DataTable -> E -> Id -> (TVr -> E) -> E
unbox dataTable e vn wtd = eCase e  [Alt (litCons { litName = cna, litArgs = [tvra], litType = te }) (wtd tvra)] Unknown where
    te = getType e
    tvra = tVr vn sta
    (ExtTypeBoxed cna sta _) = fromMaybe (error $ "lookupExtTypeInfo(unbox): " ++ show te) $ lookupExtTypeInfo dataTable te



-- | this creates a string representing the type of primitive optimization was
-- performed for bookkeeping purposes

cextra Op {} [] = ""
cextra Op {} xs = '.':map f xs where
    f ELit {} = 'c'
    f EPrim {} = 'p'
    f _ = 'e'
cextra _ _ = ""

primConv cop t1 t2 e rt = EPrim (APrim (Op (Op.ConvOp cop t1) t2) mempty) [e] rt


performPrimOpt (ELit lc@LitCons { litArgs = xs }) = do
    xs' <- mapM performPrimOpt xs
    primOpt' (ELit lc { litArgs = xs' })
performPrimOpt (EPrim ap xs t) = do
    xs' <- mapM performPrimOpt xs
    primOpt' (EPrim ap xs' t)
performPrimOpt e = return e


primOpt' e@(EPrim (APrim s _) xs t) = do
    let primopt (Op (Op.BinOp bop t1 t2) tr) [e1,e2] rt = binOp bop t1 t2 tr e1 e2 rt
        primopt (Op (Op.ConvOp cop t1) t2) [ELit (LitInt n t)] rt = return $ ELit (LitInt (convNumber cop t1 t2 n) rt)
        primopt (Op (Op.ConvOp cop t1) t2) [e1] rt = case convOp cop t1 t2 of
            Nothing | getType e1 == rt -> return e1
            Just cop' | cop' /= cop -> return $ primConv cop' t1 t2 e1 rt
            _ -> fail "couldn't apply conversion optimization"
        primopt (Op (Op.UnOp bop t1) tr) [e1] rt = unOp bop t1 tr e1 rt
        primopt _ _ _ = fail "No Primitive optimization to apply"
    case primopt s xs t of
        Just n -> do
            mtick (toAtom $ "E.PrimOpt." ++ braces (pprint s) ++ cextra s xs )
            primOpt' n
        Nothing -> return e
primOpt' e = return e

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


-- | this is called once after conversion to E on all primitives, it performs various
-- one time only transformations.

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
        (ExtTypeBoxed cna _ _) = fromMaybe (error $ "lookupExtTypeInfo(box): " ++ show t) $ lookupExtTypeInfo dataTable t
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
    primopt "options_target" [] t     = return (ELit (LitInt 0 t))
    primopt pn [] t | Just c <- getPrefix "options_" pn      = return (EPrim (APrim (CConst ("JHC_" ++ c) "int") mempty) [] t)
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



