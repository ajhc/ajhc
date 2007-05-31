module C.OpEval where

import Number
import C.Op
import Control.Monad


class Expression t e | e -> t where
    toConstant :: e -> Maybe (Number,t)
    toExpression :: Number -> t -> e
    toBool :: Bool -> e
    createBinOp :: BinOp -> Ty -> Ty -> Ty -> e -> e -> t -> e
    createUnOp  :: UnOp -> Ty -> Ty -> e -> t -> e
    fromUnOp :: e -> Maybe (UnOp,Ty,Ty,e,t)
    fromBinOp :: e -> Maybe (BinOp,Ty,Ty,Ty,e,e,t)

    toConstant _ = Nothing
    fromBinOp _ = Nothing
    fromUnOp _ = Nothing

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
convOp U2U t1 t2 | t2 `tyLte` t1 = Just Lobits
convOp I2I t1 t2 | t2 `tyLte` t1 = Just Lobits
convOp U2U t1 t2 | t1 `tyLte` t2 = Just Zx
convOp I2I t1 t2 | t1 `tyLte` t2 = Just Sx
convOp n _ _ = Just n

convNumber :: ConvOp -> Ty -> Ty -> Number -> Number
convNumber _ _ _ n = n

convCombine :: Ty -> ConvOp -> Ty -> ConvOp -> Ty -> Maybe ConvOp
convCombine _ c1 _ c2 _ | c1 `elem` [F2I,I2F] || c2 `elem` [F2I,I2F] = Nothing
convCombine _ c1 t2 c2 t3 | tyEq t2 t3 && c1 == c2 = Just c2
convCombine _ _ _ _ _ = Nothing

binOp :: Expression t e => BinOp -> Ty -> Ty -> Ty -> e -> e -> t -> Maybe e
-- evaluate expressions at compile time if we can
binOp bop t1 t2 tr e1 e2 str | Just (v1,t1) <- toConstant e1, Just (v2,t2) <- toConstant e2 = f bop v1 v2 where
    f Add v1 v2 = return $ toExpression (v1 + v2) str
    f Sub v1 v2 = return $ toExpression (v1 - v2) str
    f Mul v1 v2 = return $ toExpression (v1 * v2) str
    f Div v1 v2 | v2 /= 0 = return $ toExpression (v1 `div` v2) str
    f Mod v1 v2 | v2 /= 0 = return $ toExpression (v1 `mod` v2) str
    f Quot v1 v2 | v2 /= 0 = return $ toExpression (v1 `quot` v2) str
    f Rem v1 v2 | v2 /= 0 = return $ toExpression (v1 `rem` v2) str
    f UDiv v1 v2 | v2 /= 0 = return $ toExpression (v1 `div` v2) str
    f UMod v1 v2 | v2 /= 0 = return $ toExpression (v1 `mod` v2) str
    f Eq v1 v2 | v2 /= 0 = return $ toBool (v1 == v2)
    f NEq v1 v2 | v2 /= 0 = return $ toBool (v1 /= v2)
    f op v1 v2 | Just v <- lookup op ops = return $ toBool (v1 `v` v2) where
        ops = [(Lt,(<)), (Gt,(>)), (Lte,(<=)), (Gte,(>=)),
               (FLt,(<)), (FGt,(>)), (FLte,(<=)), (FGte,(>=))]
    f op v1 v2 | Just v <- lookup op ops, v1 >= 0 && v2 >= 0 = return $ toBool (v1 `v` v2) where
        ops = [(ULt,(<)), (UGt,(>)), (ULte,(<=)), (UGte,(>=))]
    f _ _ _ =  Nothing
-- we normalize ops such that constants are always on the left side
binOp bop t1 t2 tr e1 e2 str | Just _ <- toConstant e2, Just bop' <- commuteBinOp bop = binOp bop' t2 t1 tr e2 e1 str `mplus` Just (createBinOp bop' t2 t1 tr e2 e1 str)
binOp bop t1 t2 tr e1 e2 str = f bop e1 e2 where
    f Shr e1 e2 | Just (0,_) <- toConstant e2 = return e1
    f Shra e1 e2 | Just (0,_) <- toConstant e2 = return e1
    f Shl e1 e2 | Just (0,_) <- toConstant e2 = return e1
    f Rotl e1 e2 | Just (0,_) <- toConstant e2 = return e1
    f Rotr e1 e2 | Just (0,_) <- toConstant e2 = return e1

    f Add e1 e2 | Just (0,_) <- toConstant e1 = return e2
    f Sub e1 e2 | Just (0,_) <- toConstant e2 = return e1
    f Mul e1 e2 | Just (0,t1) <- toConstant e1 = return $ toExpression 0 str
    f Mul e1 e2 | Just (1,_) <- toConstant e1 = return e2
    f Div e1 e2 | Just (1,_) <- toConstant e2 = return e1
    f Mod e1 e2 | Just (1,_) <- toConstant e2 = return  $ toExpression 0 str
    f UDiv e1 e2 | Just (1,_) <- toConstant e2 = return e1
    f UMod e1 e2 | Just (1,_) <- toConstant e2 = return $ toExpression 0 str
    f Quot e1 e2 | Just (1,_) <- toConstant e2 = return e1
    f Rem e1 e2 | Just (1,_) <- toConstant e2 = return  $ toExpression 0 str
    f UGt e1 _ | Just (0,_) <- toConstant e1 = return $ toBool False
    f ULte e1 _ | Just (0,_) <- toConstant e1 = return $ toBool True
--    f bop e1 e2 | isAssociative bop, Just (bop',t1',t2',tr',e1',e2',str') <- fromBinOp e1, bop == bop' = let
--        g binop = binop bop e1' (binop
--        in g binOp `mplus` g (Just . createBinOp)
    f bop e1 e2 = Nothing -- return $ createBinOp bop t1 t2 tr e1 e2 str

{-
unOp :: Expression t e => UnOp -> Ty -> Ty -> e -> t -> Maybe e
-- evaluate expressions at compile time if we can
binOp bop t1 t2 tr e1 e2 str | Just (v1,t1) <- toConstant e1, Just (v2,t2) <- toConstant e2 = f bop v1 v2 where
    f Add v1 v2 = return $ toExpression (v1 + v2) str
    f Sub v1 v2 = return $ toExpression (v1 - v2) str
    f Mul v1 v2 = return $ toExpression (v1 * v2) str
    f Div v1 v2 | v2 /= 0 = return $ toExpression (v1 `div` v2) str
    f Mod v1 v2 | v2 /= 0 = return $ toExpression (v1 `mod` v2) str
    f Quot v1 v2 | v2 /= 0 = return $ toExpression (v1 `quot` v2) str
    f Rem v1 v2 | v2 /= 0 = return $ toExpression (v1 `rem` v2) str
    f UDiv v1 v2 | v2 /= 0 = return $ toExpression (v1 `div` v2) str
    f UMod v1 v2 | v2 /= 0 = return $ toExpression (v1 `mod` v2) str
    f Eq v1 v2 | v2 /= 0 = return $ toBool (v1 == v2)
    f NEq v1 v2 | v2 /= 0 = return $ toBool (v1 /= v2)
    f _ _ _ =  Nothing
-- we normalize ops such that constants are always on the left side
binOp bop t1 t2 tr e1 e2 str | Just _ <- toConstant e2, Just bop' <- commuteBinOp bop = binOp bop' t2 t1 tr e2 e1 str `mplus` Just (createBinOp bop t2 t1 tr e2 e1 str)
binOp bop t1 t2 tr e1 e2 str = f bop e1 e2 where
    f Add e1 e2 | Just (0,_) <- toConstant e1 = return e2
    f Sub e1 e2 | Just (0,_) <- toConstant e2 = return e1
    f Mul e1 e2 | Just (0,t1) <- toConstant e1 = return $ toExpression 0 str
    f Div e1 e2 | Just (1,_) <- toConstant e2 = return e1
    f UDiv e1 e2 | Just (1,_) <- toConstant e2 = return e1
    f Quot e1 e2 | Just (1,_) <- toConstant e2 = return e1
    f bop e1 e2 = return $ createBinOp bop t1 t2 tr e1 e2 str
  -}
