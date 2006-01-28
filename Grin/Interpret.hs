module Grin.Interpret(evaluate) where

import Atom
import Support.CanType
import Char
import CharIO
import Control.Monad.Identity
import C.Prims
import Data.IORef
import Data.Map as Map hiding(map)
import Data.Monoid
import Doc.DocLike
import Doc.Pretty
import GenUtil hiding(putErrLn,putErr)
import Grin.Grin
import Grin.Show
import Name.VConsts
import Options
import qualified FlagDump as FD
import qualified Stats

builtins = []
builtinMap = Map.fromList [ (x,y) | (x,y) <- builtins ]

createCafMap as = f vars [] >>= return . Map.fromList  where
    f [] xs = return xs
    f ((x,y):xs) ys = newIORef y >>= \y -> f xs ((x,Addr y):ys)
    vars = as

evaluate ::  Grin -> IO (Val,Stats.Stats)
evaluate Grin { grinTypeEnv = tyEnv, grinFunctions = ts, grinCafs = cafs } =  do
    stats <- Stats.new
    cafMap <- createCafMap cafs
    let f x = interpret stats tyEnv cafMap builtinMap (fromList  ts) x
        g (App t [l@Lit {}] _) | t == funcEval = return l
        g (App t [Const n] _) | t == funcEval = return n
        g e = f e >>= \x -> case x of
            NodeC t xs -> do
                xs <- mapM (g . gEval) xs
                return $ NodeC t xs
            z -> return z
    v <- g (App funcMain [] tyUnit)
    return (v,stats)

funcCalls = toAtom "Function Calls"
primCalls = toAtom "Primitive Calls"

prettyEnv env = vcat [ text ('v':show x) <+> text "->" <+> prettyVal y | (V x,y) <-  Map.toList env ]

interpret ::  Stats.Stats -> TyEnv -> Map Var Val -> Map Atom Builtin -> Map Atom Lam  -> Exp -> IO Val
interpret stats te cafMap primMap scMap e = f mempty e where
    f :: Map Var Val -> Exp -> IO Val
    f env (e1 :>>= (v :-> e2)) = do
        r <- f env e1
        be <- bind v r
        f (be `mappend` env) e2
    f env (App a xs ty) = do
        wdump FD.Steps $ do
            putErrLn $ render (prettyExp mempty $ App a xs' ty)
        Stats.tick stats funcCalls
        Stats.tick stats (toAtom $ "Function." ++ fromAtom a)
        case Map.lookup a scMap of
            Nothing -> error $ "Unknown App: " ++ show (App a xs' ty)
            Just ((Tup as :-> e)) -> f (Map.fromList (zip [ v | Var v _ <- as] xs')) e
      where xs' = map (le env) xs
    f env (Prim Primitive { primAPrim = APrim CCast {} _, primType = (_,t)} [x]) = return $ (Lit n t)
        where (Lit n _) = le env x
    f env (Prim Primitive { primAPrim = APrim Func { funcName = "putwchar" } _} [x]) = putChar (chr $ fromIntegral n) >> return unit
        where (Lit n _) = le env x
    f env (Prim Primitive { primAPrim = APrim Operator { primOp = "<=" } _, primType = (_,t)} [x,y]) = if x' <= y' then return (Lit 1 t) else return (Lit 0 t)
        where (Lit x' _) = le env x
              (Lit y' _) = le env y
    f env (Prim Primitive { primAPrim = APrim Operator { primOp = ">=" } _, primType = (_,t)} [x,y]) = if x' >= y' then return (Lit 1 t) else return (Lit 0 t)
        where (Lit x' _) = le env x
              (Lit y' _) = le env y
    f env (Prim Primitive { primAPrim = APrim Operator { primOp = ">" } _, primType = (_,t)} [x,y]) = if x' > y' then return (Lit 1 t) else return (Lit 0 t)
        where (Lit x' _) = le env x
              (Lit y' _) = le env y
    f env (Prim Primitive { primAPrim = APrim Operator { primOp = "<" } _, primType = (_,t)} [x,y]) = if x' < y' then return (Lit 1 t) else return (Lit 0 t)
        where (Lit x' _) = le env x
              (Lit y' _) = le env y
    f env (Prim Primitive { primAPrim = APrim Operator { primOp = "==" } _, primType = (_,t)} [x,y]) = if x' == y' then return (Lit 1 t) else return (Lit 0 t)
        where (Lit x' _) = le env x
              (Lit y' _) = le env y
    f env (Prim Primitive { primAPrim = APrim Operator { primOp = "+" } _, primType = (_,t)} [x,y]) = return (Lit (x' + y') t)
        where (Lit x' _) = le env x
              (Lit y' _) = le env y
    f env (Prim Primitive { primAPrim = APrim Operator { primOp = "/" } _, primType = (_,t)} [x,y]) = return (Lit (x' `div` y') t)
        where (Lit x' _) = le env x
              (Lit y' _) = le env y
    f env (Prim Primitive { primAPrim = APrim Operator { primOp = "%" } _, primType = (_,t)} [x,y]) = return (Lit (x' `mod` y') t)
        where (Lit x' _) = le env x
              (Lit y' _) = le env y
    f env (Prim Primitive { primAPrim = APrim Operator { primOp = "-" } _, primType = (_,t)} [x,y]) = return (Lit (x' - y') t)
        where (Lit x' _) = le env x
              (Lit y' _) = le env y
    f env (Prim Primitive { primAPrim = APrim Operator { primOp = "*" } _, primType = (_,t)} [x,y]) = return (Lit (x' * y') t)
        where (Lit x' _) = le env x
              (Lit y' _) = le env y
    f env (Prim Primitive { primAPrim = APrim Operator { primOp = "-" } _, primType = (_,t)} [x]) = return (Lit (negate x') t)
        where (Lit x' _) = le env x
    f env (Prim p xs) = do
        let a = primName p
            xs' = map (le env) xs
        wdump FD.Steps $ do
            putErrLn $ render (prettyExp mempty $ Prim p xs')
        Stats.tick stats primCalls
        Stats.tick stats (toAtom $ "Primitive." ++ fromAtom a)
        case Map.lookup a primMap of
            Nothing -> error $ "Unknown Primitive: " ++ show (Prim p xs')
            Just action -> do action xs'
    f env (Return v) = return (le env v)
    f env (Store v) = do
        Stats.tick stats (toAtom "Allocations Performed")
        fmap Addr $ newIORef (le env v)
    f env (Fetch x)
        | (Addr x) <- le env x = readIORef x
        | (Const x) <- le env x  = return x
    f env (Update x v) | (Addr x) <- le env x = do
        Stats.tick stats (toAtom "Updates Performed")
        (writeIORef x $! (le env v)) >> return unit
    f env (Update x v) | (Const x) <- le env x, x == le env v =  return unit
    f env (Update x v)  = fail $ "Bad update: " ++ show (le env x,le env v)
    f env (Cast v nt) | Lit i _ <- le env v = return (Lit i nt)
    f env (Error s t) = fail $ render $  tshow (s,t) <$> (prettyEnv env)
--    f env (Eval x)
--        | otherwise = f env $ App funcEval [x]
--        | Const x <- lx = doEval x
--        | (Addr ref) <- lx = do
--            v <- readIORef ref
--            nv <- doEval v
--            writeIORef ref nv
--            return nv
--        where
--            lx = le env x
--    f env (Apply x y)
--        | True =  f env $ App funcApply [x,y]
--        | False = doApply (le env x) (le env y)
    f env (Case v ps) = match (le env v) ps where
        match s ((p :-> e):ps) = case bind p s of
            Nothing -> match s ps
            Just env' -> f (env' `mappend` env) e
        match e [] = fail $ "end of match: " ++ show e <+> show env
    f env z = fail $ "cannot interpret: " ++ show (toList env,z)
    le env (Tup vs) = Tup (map (le env) vs)
    le env (NodeC t vs) = NodeC t (map (le env) vs)
    le env z@(NodeV t vs) = NodeC (lt t) (map (le env) vs)  where
        lt x = case Map.lookup x env of
            Just (Tag t) -> t
            z' -> error $ "Invalid tag variable in NodeV: " ++ show (z,z')
    le env z@(Var v _) = case Map.lookup v env `mplus` Map.lookup v cafMap of
        Just x -> le env x
        Nothing -> error $ "le" ++ show (z,env)
    le _ x = x

    doApply (NodeC t xs) y
        | n == (1::Int) = f mempty (App (toAtom $ 'f':rs) (xs ++ [y]) TyNode)  -- TODO, right?
        | n > 1 = return $ NodeC (toAtom $ 'P':show (n - 1) ++ "_" ++ rs) (xs ++ [y])
        where
        ('P':cs) = fromAtom t
        (n','_':rs) = span isDigit cs
        n = read n'
    doApply x y = error $ "doApply " ++ show (x,y)
    doEval x@(NodeC t xs)
        | 'P':_ <- t' = return x
        | 'T':_ <- t' = return x
        | 'C':_ <- t' = return x
        | 'F':rs <- t' = f mempty (App (toAtom $ 'f':rs) xs TyNode)  -- TODO, right?
        | 'B':rs <- t' = f mempty (App (toAtom $ 'b':rs) xs TyNode)  -- TODO, right?
        where
        t' = fromAtom t
    doEval x = error $ "doEval " ++ show x

    bind :: Monad m => Val -> Val -> m (Map Var Val)
    bind (Var (V 0) _) _ = return mempty
    bind (Var v _) r = return $ singleton v r
    bind (Lit i _) (Lit i' _) | i == i' = return mempty
    bind (Tup xs) (Tup ys) = liftM mconcat $ sequence $  zipWith bind xs ys
    bind (Tag i) (Tag i') | i == i' = return mempty
    bind (NodeV v vs) (NodeC t vs') = do
        be <- liftM mconcat $ sequence $  zipWith bind vs vs'
        return (be `mappend` singleton v (Tag t))
    bind (NodeC t vs) (NodeC t' vs') | t == t' = do
        liftM mconcat $ sequence $  zipWith bind vs vs'
    bind v r | getType v == getType r = fail $ "unbindable: "  ++ show (v,r,getType v,getType r)   -- check type to be sure
    bind x y = error $ "bad bind: " ++ show (x,y)



