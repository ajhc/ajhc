module C.OpEval where

import Number
import C.Op
import Control.Monad


class Expression t e | e -> t where
    toConstant :: e -> Maybe (Number,t)
    toExpression :: Number -> t -> e
    toBool :: Bool -> e
    toTy   :: t -> Ty
    createBinOp :: BinOp -> e -> e -> e
    fromBinOp :: e -> Maybe (BinOp,e,e)

    toConstant _ = Nothing
    fromBinOp _ = Nothing

TyBool `tyLte` _ = True
TyBits (Bits x) _ `tyLte` TyBits (Bits y) _ = x <= y
_ `tyLte` TyBits (BitsArch BitsMax) _ = True
TyBits (Bits x) _ `tyLte` TyBits (BitsArch BitsPtr) _ = x <= 32
x `tyLte` y  = x == y

x `tyLt` y = (x `tyLte` y) && not (y `tyLte` x)
x `tyGt` y = y `tyLt` x
x `tyGte` y = y `tyLte` x
x `tyEq` y = (x `tyLte` y) && (y `tyLte` x)

convOp :: ConvOp -> Ty -> Ty -> Maybe ConvOp
convOp F2I _ _ = Just F2I
convOp I2F _ _ = Just I2F
convOp _ t1 t2 | t1 == t2 = Nothing
convOp U2U t1 t2 | t2 `tyLt` t1 = Just Lobits
convOp I2I t1 t2 | t2 `tyLt` t1 = Just Lobits
convOp U2U t1 t2 | t2 `tyGt` t1 = Just Zx
convOp I2I t1 t2 | t2 `tyGt` t1 = Just Sx
convOp n _ _ = Just n

convNumber :: ConvOp -> Ty -> Ty -> Number -> Number
convNumber _ _ _ n = n

convCombine :: Ty -> ConvOp -> Ty -> ConvOp -> Ty -> Maybe ConvOp
convCombine _ c1 _ c2 _ | c1 `elem` [F2I,I2F] || c2 `elem` [F2I,I2F] = Nothing
convCombine _ c1 t2 c2 t3 | tyEq t2 t3 && c1 == c2 = Just c2
convCombine _ _ _ _ _ = Nothing

binOp :: Expression t e => BinOp -> e -> e -> Maybe e
-- evaluate expressions at compile time if we can
binOp bop e1 e2 | Just (v1,t1) <- toConstant e1, Just (v2,t2) <- toConstant e2 = f bop t1 v1 v2 where
    f Add t v1 v2 = return $ toExpression (v1 + v2) t
    f Sub t v1 v2 = return $ toExpression (v1 - v2) t
    f Mul t v1 v2 = return $ toExpression (v1 * v2) t
    f Div t v1 v2 | v2 /= 0 = return $ toExpression (v1 `div` v2) t
    f Mod t v1 v2 | v2 /= 0 = return $ toExpression (v1 `mod` v2) t
    f Quot t v1 v2 | v2 /= 0 = return $ toExpression (v1 `quot` v2) t
    f Rem t v1 v2 | v2 /= 0 = return $ toExpression (v1 `rem` v2) t
    f UDiv t v1 v2 | v2 /= 0 = return $ toExpression (v1 `div` v2) t
    f UMod t v1 v2 | v2 /= 0 = return $ toExpression (v1 `mod` v2) t
    f Eq t v1 v2 | v2 /= 0 = return $ toBool (v1 == v2)
    f NEq t v1 v2 | v2 /= 0 = return $ toBool (v1 /= v2)
    f _ _ _ _ =  Nothing
-- we normalize ops such that constants are always on the left side
binOp bop e1 e2 | Just _ <- toConstant e2, Just bop' <- commuteBinOp bop = binOp bop' e2 e1 `mplus` Just (createBinOp bop e2 e1)
binOp Add e1 e2 | Just (0,_) <- toConstant e1 = return e2
binOp Sub e1 e2 | Just (0,_) <- toConstant e2 = return e1
binOp Mul e1 e2 | Just (0,t1) <- toConstant e1 = return $ toExpression 0 t1
binOp Div e1 e2 | Just (1,_) <- toConstant e2 = return e1
binOp UDiv e1 e2 | Just (1,_) <- toConstant e2 = return e1
binOp Quot e1 e2 | Just (1,_) <- toConstant e2 = return e1
binOp bop e1 e2 = return $ createBinOp bop e1 e2


