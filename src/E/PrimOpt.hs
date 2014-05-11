{-# LANGUAGE RecursiveDo,ViewPatterns #-}
module E.PrimOpt(performPrimOpt) where

import Control.Monad.Fix()

import C.Prims
import Cmm.OpEval
import Doc.DocLike
import Doc.PPrint
import E.E
import E.Values
import Stats
import StringTable.Atom
import Support.CanType
import qualified Cmm.Op as Op

{-@Extensions

# Foreign Primitives

In addition to foreign imports of external functions as described in the FFI
spec. Jhc supports 'primitive' imports that let you communicate primitives
directly to the compiler. In general, these should not be used other than in the
implementation of the standard libraries. They generally do little error
checking as it is assumed you know what you are doing if you use them. All
haskell visible entities are introduced via foreign declarations in jhc.

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

-- | this creates a string representing the type of primitive optimization was
-- performed for bookkeeping purposes

primConv cop t1 t2 e rt = EPrim (Op (Op.ConvOp cop t1) t2) [e] rt

performPrimOpt (ELit lc@LitCons { litArgs = xs }) = do
    xs' <- mapM performPrimOpt xs
    primOpt' (ELit lc { litArgs = xs' })
performPrimOpt (EPrim ap xs t) = do
    xs' <- mapM performPrimOpt xs
    primOpt' (EPrim ap xs' t)
performPrimOpt e = return e

primOpt' e@(EPrim s xs t) = do
    let primopt (Op (Op.BinOp bop t1 t2) tr) [e1,e2] rt =
            binOp bop t1 t2 tr e1 e2 rt
        primopt (Op (Op.ConvOp cop t1) t2) [ELit (LitInt n t)] rt =
            return $ ELit (LitInt (convNumber cop t1 t2 n) rt)
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

cextra Op {} [] = ""
cextra Op {} xs = '.':map f xs where
    f ELit {} = 'c'
    f EPrim {} = 'p'
    f _ = 'e'
cextra _ _ = ""

instance Expression E E where
    toBool True = ELit lTruezh
    toBool False = ELit lFalsezh
    toConstant (ELit (LitInt n t)) = return (n,t)
    toConstant _ = Nothing
    equalsExpression e1 e2 = e1 == e2
    caseEquals scrut (n,t) e1 e2 = eCase scrut [Alt (LitInt n t) e1 ] e2
    toExpression n t = (ELit (LitInt n t))
    createBinOp bop t1 t2 tr e1 e2 str =
                EPrim Op { primCOp = Op.BinOp bop t1 t2,
                                  primRetTy = tr } [e1, e2] str
    createUnOp bop t1 tr e1 str =
                EPrim Op { primCOp = Op.UnOp bop t1,
                                  primRetTy = tr } [e1] str
    fromBinOp (EPrim Op { primCOp = Op.BinOp bop t1 t2,
                                 primRetTy = tr } [e1, e2] str) =
                                     Just (bop,t1,t2,tr,e1,e2,str)
    fromBinOp _ = Nothing
    fromUnOp (EPrim Op {
        primCOp = Op.UnOp bop t1,
        primRetTy = tr } [e1] str) = Just (bop,t1,tr,e1,str)
    fromUnOp _ = Nothing
