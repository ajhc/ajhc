module Grin.Primitives(primSc, builtins) where

import Grin.Val
import Grin.Grin
import GenUtil
import Name.VConsts
import Atom
import Char(chr,ord)
import Options
import C.Prims

createPrim name as exp = sc where
    sc = (n,Tup as :-> exp)
    --tenv = (n,([ v | Var _ v <- as ],TyNode))
    n = toAtom $ "b" ++ name

--primAp = createPrim "ap" [p1,p2] (gEval p1 :>>= (n3 :-> gApply n3 p2))

primitive = Primitive { primRets = Nothing }

-- Eval all arguments
createPrim' name as exp = sc where
    sc = (n,Tup [ Var n (TyPtr TyNode) | Var n _ <- as ] :-> exp')
    n = toAtom $ "b" ++ name
    exp' = foldr g exp as where
        g v@(Var x TyNode) r = gEval (Var x (TyPtr TyNode)) :>>= (v :-> r)

litPrim :: Tag -> String -> Int -> Builtin -> ((Atom,Lam),(Atom,Builtin))
litPrim tag name nargs fn = (createPrim' name as (f as $ Prim prim as'), (n,fn)) where
    n = toAtom $ "@" ++ name
    var n = Var n (Ty tag)
    as =  [ Var n TyNode | n <- [ v1 .. V nargs ]]
    as' =  [ var n | n <- [ v1 .. V nargs ]]
    f [] e = e
    f ((a@(Var n _)):as) e = Return a :>>= NodeC tag [var n] :-> f as e
    f a b = error $ "litPrim: " ++ show (a,b)
    prim = primitive { primRets = Just [ toAtom $ "CPrelude." ++ show x | x <- [LT .. ]], primName = n, primType = (TyTup (replicate nargs (Ty tag)),TyNode) , primAPrim = primPrim (show n) }

intPrim :: Tag -> String -> Int -> Builtin -> ((Atom,Lam),(Atom,Builtin))
intPrim tag name nargs fn = (createPrim' name as (f as (Prim prim as')), (n,fn)) where
    n = toAtom $ "@" ++ name
    var n = Var (V n) (Ty tag)
    f [] e = e :>>= var 1 :-> Return (NodeC tag [var 1])
    f ((a@(Var (V n) _)):as) e = Return a :>>= NodeC tag [var n] :-> f as e
    as =  [ Var (V n) TyNode | n <- [ 1 .. nargs ]]
    as' =  [ var n | n <- [ 1 .. nargs ]]
    prim = primitive { primName = n, primType = (TyTup (replicate nargs (Ty tag)),Ty tag) }

eqPrim :: Tag -> String -> Int -> Builtin -> ((Atom,Lam),(Atom,Builtin))
eqPrim tag name nargs fn = (createPrim' name as (f as (Prim prim as')), (n,fn)) where
    n = toAtom $ "@" ++ name
    var n = Var (V n) (Ty tag)
    f [] e = e
    f ((a@(Var (V n) _)):as) e = Return a :>>= NodeC tag [var n] :-> f as e
    as =  [ Var (V n) TyNode | n <- [ 1 .. nargs ]]
    as' =  [ var n | n <- [ 1 .. nargs ]]
    prim = primitive { primRets = Just [toAtom "CPrelude.True", toAtom "CPrelude.False"],primName = n, primType = (TyTup (replicate nargs (Ty tag)),TyNode)  , primAPrim = primPrim (show n)}

intPrimN :: Tag -> String -> Int -> Int -> Builtin -> ((Atom,Lam),(Atom,Builtin))
intPrimN tag name nargs 2 fn = (createPrim' name as (f as (Prim prim as')), (n,fn)) where
    n = toAtom $ "@" ++ name
    var n = Var (V n) (Ty tag)
    f [] e =
        e :>>= Tup rs' :->
        Store (NodeC tag [rs' !! 0]) :>>= p1 :->
        Store (NodeC tag [rs' !! 1]) :>>= p2 :->
        Return (NodeC (toAtom "CPrelude.(,)") [p1,p2])
    f ((a@(Var (V n) _)):as) e = Return a :>>= NodeC tag [var n] :-> f as e
    as =  [ Var (V n) TyNode | n <- [ 1 .. nargs ]]
    as' =  [ var n | n <- [ 1 .. nargs ]]
    rs' =  [ var n | n <- [ 1 .. 2 ]]
    prim = primitive { primName = n, primType = (TyTup (replicate nargs (Ty tag)),TyTup (replicate 2 $ Ty tag))  , primAPrim = primPrim (show n)}

--primQuotRem = intPrimN cInt "primQuotRem" 2 2 f where
--    f [Lit x tx,Lit y ty] | tx == ty = return $  let (a,b) = (quotRem x y) in Tup [Lit a tx, Lit b tx]
--    f xs = error $ "primQuotRem: " ++ show xs

fromBoxedLit (NodeC _ [Lit n _]) = return n
fromBoxedLit _ = fail "not boxed lit"

--binaryOp name op = intPrim (toAtom "int") name 2 f where
--    f [Lit x tx,Lit y ty] | tx == ty = return $  Lit (x `op` y) tx
--    f xs = error $ "binaryOp: " ++ name ++ " " ++ show xs

primId name = createPrim' name [n1] (Return n1)

--primTimes = binaryOp "primTimes" (*)
--primPlus = binaryOp "primPlus" (+)
--primMinus = binaryOp "primMinus" (-)

{-
primPlus = sprim "primPlus" 2 f where
    f [NodeC tx [Lit x tx'],NodeC ty [Lit y ty']]
        | tx == ty && tx' == ty' = return $ NodeC tx [Lit (x + y) tx']
primMinus = sprim "primMinus" 2 f where
    f [NodeC tx [Lit x tx'],NodeC ty [Lit y ty']]
        | tx == ty && tx' == ty' = return $ NodeC tx [Lit (x - y) tx']
-}
--primNegate = intPrim cInt "primNegate" 1 f where
--    f [Lit x tx] = return $ Lit (negate x) tx

primEq x = eqPrim (toAtom $ 'C':x) ("primEq" ++ x)  2 f where
    f [Lit x tx,Lit y ty]
        | tx == ty  = return $ if x == y then vTrue else vFalse
primCompare x = litPrim (toAtom $ 'C':x) ("primCompare" ++ x) 2 f where
    f [Lit x tx',Lit y ty']
        |  tx' == ty' = return $ vOrdering (compare x y)
    f xs = error $ "primCompare: " ++ show xs


sprims = [
--    primPlus,
 --   primMinus,
--    primNegate,
--    primEq "Int",
--    primEq "Char",
--    primCompare "Int",
--    primCompare "Char",
--    primTimes,
    (primPutChar,primPutCharBuiltin),
    (primGetChar,primGetCharBuiltin),
    (primGetArgs,primGetArgsBuiltin)
--    primQuotRem
    ]

builtins = snds sprims
primSc = fsts sprims ++ [
--    primId "unsafeCoerce",
--    primId "primFromInteger",
--    primId "primToInteger",
--    (toAtom "bord", primLitCast (cChar,tCharzh) (cInt,tIntzh)),
--    (toAtom "bchr", primLitCast (cInt,tIntzh) (cChar,tCharzh)),
--    createPrim "error" [p1] (Error "error call" TyNode),
    --primAp,
--    primExit
    ]

primLitCast (c1,t1) (c2,t2) = (Tup [p1] :->
    gEval p1 :>>= NodeC c1 [Var v2 t1] :->
    Cast (Var v2 t1) t2 :>>= Var v3 t2 :->
    Return (NodeC c2 [Var v3 t2])
    )

primitivePutChar = primitive {
    primName = toAtom "@putChar",
    primAPrim = primPrim  "@putChar",
    primType = (TyTup [tCharzh],tyUnit)
    }
primitiveGetChar = primitive {
    primName = toAtom "@getChar",
    primAPrim = primPrim  "@getChar",
    primType = (TyTup [],tCharzh)
    }

primPutCharBuiltin = (primName primitivePutChar, \[Lit n _] -> putChar (chr n) >> return unit)
primGetCharBuiltin = (primName primitiveGetChar, \[] -> getChar >>= \c -> return (Lit (ord c) tCharzh))

primGetChar = (toAtom "bprimGetChar",
    Tup [p0] :->
    Prim primitiveGetChar [] :>>= c1 :->
    Store (NodeC cChar [c1]) :>>= p2 :->
    Return (NodeC (toAtom "CJhc.IO.JustIO") [p0, p2])
    )

primPutChar = (toAtom "bprimPutChar",
    Tup [p1,p0] :->
    gEval p1 :>>= NodeC cChar [c1] :->
    Prim primitivePutChar [c1] :>>= unit :->
    Return (NodeC (toAtom "CJhc.IO.JustIO") [p0, Const $ vUnit])
    )

--  unboxed varients are only different for certain types.

-- binOp :: (ToVal a, ToVal b, FromVal c) =>  ( a -> b -> c) -> ((Atom,Lam),(Atom,Builtin))
-- binOp fn =




primGetArgsBuiltin = (primName primitiveGetArgs, \[] -> return $ Const $ toVal (map toVal (optProgArgs options)))
primitiveGetArgs = primitive {
    primName = toAtom "@getArgs",
    primType = (TyTup [],TyPtr TyNode)
    }
primGetArgs = (toAtom "bprimGetArgs",
    Tup [p0] :->
    Prim primitiveGetArgs [] :>>= p1 :->
    Return (NodeC (toAtom "CJhc.IO.JustIO") [p0,p1])
    )

--primExitBuiltin = (primName primitiveExit, \[] -> return $ Const $ toVal (map toVal (optProgArgs options)))
--primitiveExit = Primitive {
--    primName = toAtom "@exit",
--    primType = (TyTup [Ty cInt],tyUnit)
--    }
primExit = (toAtom "bexit",
    Tup [p3,p1] :->
    gEval p3 :>>= NodeC cInt [Var v1 tIntzh] :->
    Error "exit" TyNode
    --Prim primitiveExit [Var 1 (Ty cInt)] :>>= unit :->
    --Return (NodeC (toAtom "CPrelude.IO.JustIO") [Const $ toVal ()])
    )

c1 = Var v1 tCharzh
--world__ = Const (NodeC (toAtom "CJhc.IO.World__") [])

{-
primIO name action as rt = (createPrim' name as exp ,action) where
    as' = [ Var | a <- as ++ [TyNode] | v <- [v1 ..]]
    exp = Prim Primitive { primName = pn, primType =
    pn = toAtom $ "@" ++ name
-}

