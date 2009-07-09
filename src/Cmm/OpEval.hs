module Cmm.OpEval(
    Expression(..),
    convOp,
    convNumber,
    convCombine,
    binOp,
    binOp',
    unOp
    ) where

import Cmm.Number
import Cmm.Op
import Control.Monad
import Maybe
import qualified Data.Map as Map


class Expression t e | e -> t where
    toConstant :: e -> Maybe (Number,t)
    toExpression :: Number -> t -> e
    toBool :: Bool -> e
    createBinOp :: BinOp -> Ty -> Ty -> Ty -> e -> e -> t -> e
    createUnOp  :: UnOp -> Ty -> Ty -> e -> t -> e
    fromUnOp :: e -> Maybe (UnOp,Ty,Ty,e,t)
    fromBinOp :: e -> Maybe (BinOp,Ty,Ty,Ty,e,e,t)
    caseEquals :: e -> (Number,t) -> e -> e -> e

    equalsExpression :: e -> e -> Bool

    toConstant _ = Nothing
    fromBinOp _ = Nothing
    fromUnOp _ = Nothing
    equalsExpression _ _ = False

TyBool `tyLte` _ = True
TyBits (Bits x) _ `tyLte` TyBits (Bits y) _ = x <= y
_ `tyLte` TyBits (BitsArch BitsMax) _ = True
TyBits (Bits x) _ `tyLte` TyBits (BitsArch BitsPtr) _ = x <= 32
x `tyLte` y  = x == y
x `tyEq` y = (x `tyLte` y) && (y `tyLte` x)

{-
x `tyLt` y = (x `tyLte` y) && not (y `tyLte` x)
x `tyGt` y = y `tyLt` x
x `tyGte` y = y `tyLte` x
-}

convOp :: ConvOp -> Ty -> Ty -> Maybe ConvOp
convOp F2I _ _ = Just F2I
convOp I2F _ _ = Just I2F
convOp F2U _ _ = Just F2U
convOp U2F _ _ = Just U2F
convOp _ t1 t2 | t1 == t2 = Nothing
convOp U2U t1 t2 | t2 `tyLte` t1 = Just Lobits
convOp I2I t1 t2 | t2 `tyLte` t1 = Just Lobits
convOp U2U t1 t2 | t1 `tyLte` t2 = Just Zx
convOp I2I t1 t2 | t1 `tyLte` t2 = Just Sx
convOp n _ _ = Just n

convNumber :: ConvOp -> Ty -> Ty -> Number -> Number
convNumber _ _ _ n = n
{-
convNumber :: ConvOp -> Val -> Ty -> Val
convNumber F2I (Val _ (ValFloat f)) ty = Val ty (fromInteger $ truncate f)
convNumber F2U (Val _ (ValFloat f)) ty = Val ty (if f < 0 then 0 else fromInteger $ truncate f)
convNumber I2F (Val _ (ValInteger f)) ty = Val ty (ValFloat $ fromInteger f)
convNumber U2F (Val _ (ValInteger f)) ty = Val ty (ValFloat $ fromInteger f)
convNumber _ (Val _ v) ty  = (Val ty v)
-}

convCombine :: Ty -> ConvOp -> Ty -> ConvOp -> Ty -> Maybe ConvOp
convCombine _ c1 _ c2 _ | c1 `elem` [F2I,I2F,U2F,F2U] || c2 `elem` [F2I,I2F,U2F,F2U] = Nothing
convCombine _ c1 t2 c2 t3 | tyEq t2 t3 && c1 == c2 = Just c2
convCombine _ _ _ _ _ = Nothing

binOp :: Expression t e => BinOp -> Ty -> Ty -> Ty -> e -> e -> t -> Maybe e
-- evaluate expressions at compile time if we can
binOp bop t1 t2 tr e1 e2 str | Just (v1,t1) <- toConstant e1, Just (v2,t2) <- toConstant e2 = f bop v1 v2 where
    f Add v1 v2 = return $ toExpression (v1 + v2) str
    f Sub v1 v2 = return $ toExpression (v1 - v2) str
    f Mul v1 v2 = return $ toExpression (v1 * v2) str
    f op v1 v2 | v2 /= 0, isJust ans = ans where
        ans = case op of
            Div  -> return $ toExpression (v1 `div` v2) str
            Mod  -> return $ toExpression (v1 `mod` v2) str
            Quot -> return $ toExpression (v1 `quot` v2) str
            Rem  -> return $ toExpression (v1 `rem` v2) str
            UDiv -> return $ toExpression (v1 `div` v2) str
            UMod -> return $ toExpression (v1 `mod` v2) str
            FDiv -> return $ toExpression (v1 / v2) str
            _ -> Nothing
    f FMul v1 v2 = return $ toExpression (v1 * v2) str
    f FAdd v1 v2 = return $ toExpression (v1 + v2) str
    f FSub v1 v2 = return $ toExpression (v1 - v2) str
    f FPwr v1 v2 = return $ toExpression (realToFrac (realToFrac v1 ** realToFrac v2 :: Double)) str

    f op v1 v2 | Just v <- Map.lookup op ops = return $ toBool (v1 `v` v2) where
        ops = Map.fromList [(Lt,(<)), (Gt,(>)), (Lte,(<=)), (Gte,(>=)),
               (FLt,(<)), (FGt,(>)), (FLte,(<=)), (FGte,(>=)), (Eq,(==)),(NEq,(/=))]

    f op v1 v2 | Just v <- Map.lookup op ops, v1 >= 0 && v2 >= 0 = return $ toBool (v1 `v` v2) where
        ops = Map.fromList [(ULt,(<)), (UGt,(>)), (ULte,(<=)), (UGte,(>=))]
    f _ _ _ =  Nothing
-- we normalize ops such that constants are always on the left side
binOp bop t1 t2 tr e1 e2 str | Just _ <- toConstant e2, Just bop' <- commuteBinOp bop = Just $ createBinOp bop' t2 t1 tr e2 e1 str
binOp bop t1 t2 tr e1 e2 str = f bop e1 e2 where
    zero = toExpression 0 str
    one = toExpression 1 str
    true = toBool True
    false = toBool False


    f op e1 e2 | Just (v,_) <- toConstant e2 = ans v where
        ans 0 = case op of
            Shr  -> return e1
            Shra -> return e1
            Shl  -> return e1
            Rotl -> return e1
            Rotr -> return e1
            Sub  -> return e1
            FSub -> return e1
            FPwr -> return one
            _ -> Nothing
        ans 1 = case op of
            Div -> return e1
            Mod -> return zero
            UDiv -> return e1
            UMod -> return zero
            Quot -> return e1
            Rem  -> return zero
            FPwr -> return e1
            FDiv -> return e1
            Mul  -> return e1
            FMul  -> return e1
            _ -> Nothing
        ans _ = Nothing

    f op e1 e2 | Just (v,t1) <- toConstant e1 = eans t1 v where
        eans t1 v1 = case op of
            Eq  -> return $ caseEquals e2 (v1,t1) true false
            NEq -> return $ caseEquals e2 (v1,t1) false true
            _ -> ans t1 v1
        ans t1 0 = case op of
            Shr  -> return zero
            Shra -> return zero
            Shl  -> return zero
            Rotl -> return zero
            Rotr -> return zero
            And  -> return zero
            Or   -> return e2
            Xor  -> return e2
            Add  -> return e2
            Mul  -> return zero
            UGt  -> return false
            ULte -> return true
            FAdd -> return e2
            UGte -> return $ caseEquals e2 (0,t1) true false
            ULt  -> return $ caseEquals e2 (0,t1) false true
            _ -> Nothing
        ans t1 1 = case op of
            Mul  -> return e2
            FMul -> return e2
            UGt  -> return $ caseEquals e2 (0,t1) true false
            _ -> Nothing
        ans _ _ = Nothing

    f op e1 e2 | e1 `equalsExpression` e2, isJust ans = ans where
        ans = case op of
            Eq    -> return true
            NEq   -> return false
            Lte   -> return true
            Gte   -> return true
            Lt    -> return false
            Gt    -> return false
            ULte  -> return true
            UGte  -> return true
            ULt   -> return false
            UGt   -> return false
            Sub   -> return zero
            Xor   -> return zero
            And   -> return e1
            Or    -> return e1
            _ -> Nothing

    f bop e1 e2 | isAssociative bop, Just (bop',t1',t2',tr',e1',e2',str') <- fromBinOp e1, bop == bop' = Just $
        createBinOp bop tr tr tr e1' (createBinOp bop tr tr tr e2' e2 str) str
    f bop e1 e2 = Nothing -- return $ createBinOp bop t1 t2 tr e1 e2 str

binOp' :: Expression t e => BinOp -> Ty -> Ty -> Ty -> e -> e -> t -> e
binOp' bop t1 t2 tr e1 e2 str =  case binOp bop t1 t2 tr e1 e2 str of
    Just e -> e
    Nothing -> createBinOp bop t1 t2 tr e1 e2 str

unOp :: Expression t e => UnOp -> Ty -> Ty -> e -> t -> Maybe e
unOp op t1 tr e str | Just (v,t) <- toConstant e = f op v where
    f Neg v = return $ toExpression (negate v) str
    f FNeg v = return $ toExpression (negate v) str
    f FAbs v = return $ toExpression (abs v) str
    f Sin v = return $ toExpression (realToFrac $ sin (realToFrac v :: Double)) str
    f Cos v = return $ toExpression (realToFrac $ cos (realToFrac v :: Double)) str
    f Tan v = return $ toExpression (realToFrac $ tan (realToFrac v :: Double)) str
    f Sqrt v = return $ toExpression (realToFrac $ sqrt (realToFrac v :: Double)) str
    f _ _ = Nothing
unOp op t1 tr e str = Nothing

