
{- This file is generated -}
module PrimitiveOperators(primitiveInsts,constantMethods,theMethods,allCTypes,ctypeMap) where

import Data.Monoid
import qualified Data.Map as Map

import Support.CanType
import C.Prims
import E.E
import E.TypeCheck()
import E.Values
import Name.Name
import Name.Names
import Representation
import Name.VConsts


create_integralCast c1 t1 c2 t2 e t = eCase e [Alt (LitCons c1 [tvra] te) cc] Unknown  where
    te = getType e
    ELit (LitCons n1 [] _) = t1
    ELit (LitCons n2 [] _) = t2
    tvra =  tVr 4 t1
    tvrb =  tVr 6 t2
    cc = if n1 == n2 then ELit (LitCons c2 [EVar tvra] t) else
        eStrictLet  tvrb (EPrim (APrim (CCast (show n1) (show n2)) mempty) [EVar tvra] t2)  (ELit (LitCons c2 [EVar tvrb] t))

create_integralCast_toInt c1 t1 e = create_integralCast c1 t1 dc_Int tIntzh e tInt
create_integralCast_toInteger c1 t1 e = create_integralCast c1 t1 dc_Integer tIntegerzh e tInteger
create_integralCast_fromInt c2 t2 e t = create_integralCast dc_Int tIntzh c2 t2 e t
create_integralCast_fromInteger c2 t2 e t = create_integralCast dc_Integer tIntegerzh c2 t2 e t

ctypeMap = Map.fromList [ (tc,v) | (_,tc,_,v,_) <- allCTypes ]

toTypeName x = parseName TypeConstructor x
toClassName x = parseName ClassName x

toInstName x = toName Val ("Instance@",'i':x)

unbox' e cn tvr wtd = eCase e [Alt (LitCons cn [tvr] te) wtd] Unknown where
    te = getType e

oper_aa op ct e = EPrim (APrim (Operator op [ct] ct) mempty) [e] (rawType ct)
oper_aaI op ct a b = EPrim (APrim (Operator op [ct,ct] "int") mempty) [a,b] intt
oper_aaa op ct a b = EPrim (APrim (Operator op [ct,ct] ct) mempty) [a,b] (rawType ct)
oper_aIa op ct a b = EPrim (APrim (Operator op [ct,"int"] ct) mempty) [a,b] (rawType ct)

intt = rawType "int"
zeroI =  LitInt 0 intt

op_aIa op ct cn t = ELam tvra' (ELam tvrb' (unbox' (EVar tvra') cn tvra (unbox' (EVar tvrb') cn tvrb wtd))) where
    tvra' = tVr 2 t
    tvrb' = tVr 4 tInt
    tvra = tVr 6 st
    tvrb = tVr 8 intt
    tvrc = tVr 10 st
    st = rawType ct
    wtd = eStrictLet tvrc (oper_aIa op ct (EVar tvra) (EVar tvrb)) (rebox (EVar tvrc))
    rebox x = ELit (LitCons cn [x] t)
op_aaa op ct cn t = ELam tvra' (ELam tvrb' (unbox' (EVar tvra') cn tvra (unbox' (EVar tvrb') cn tvrb wtd))) where
    tvra' = tVr 2 t
    tvrb' = tVr 4 t
    tvra = tVr 6 st
    tvrb = tVr 8 st
    tvrc = tVr 10 st
    st = rawType ct
    wtd = eStrictLet tvrc (oper_aaa op ct (EVar tvra) (EVar tvrb)) (rebox (EVar tvrc))
    rebox x = ELit (LitCons cn [x] t)
op_aa op ct cn t = ELam tvra' (unbox' (EVar tvra') cn tvra wtd) where
    tvra' = tVr 2 t
    tvra = tVr 6 st
    tvrc = tVr 10 st
    st = rawType ct
    wtd = eStrictLet tvrc (oper_aa op ct (EVar tvra)) (rebox (EVar tvrc))
    rebox x = ELit (LitCons cn [x] t)
op_aaI op ct cn t = ELam tvra' (ELam tvrb' (unbox' (EVar tvra') cn tvra (unbox' (EVar tvrb') cn tvrb wtd))) where
    tvra' = tVr 2 t
    tvrb' = tVr 4 t
    tvra = tVr 6 st
    tvrb = tVr 8 st
    tvrc = tVr 10 intt
    st = rawType ct
    wtd = eStrictLet tvrc (oper_aaI op ct (EVar tvra) (EVar tvrb)) (rebox (EVar tvrc))
    rebox x = ELit (LitCons dc_Int [x] t)

op_aaB op ct cn t = ELam tvra' (ELam tvrb' (unbox' (EVar tvra') cn tvra (unbox' (EVar tvrb') cn tvrb wtd))) where
    tvra' = tVr 2 t
    tvrb' = tVr 4 t
    tvra = tVr 6 st
    tvrb = tVr 8 st
    tvrc = tVr 10 intt
    st = rawType ct
    wtd = eStrictLet tvrc (oper_aaI op ct (EVar tvra) (EVar tvrb)) (caseof (EVar tvrc))
    caseof x = eCase x [Alt zeroI vFalse]  vTrue

--buildAbs v t = eIf (EPrim (primPrim "prim_op_aaB.<") [EVar v,(ELit (LitInt 0 t))] tBool) (EPrim (primPrim "prim_op_aa.-") [EVar v] t) (EVar v)
--build_abs ct cn v = unbox' v cn tvra (eCase (EPrim (APrim (Operator "<" [ct,ct] "int") mempty) [EVar tvra, zero] intt) [Alt zeroI (rebox $ EVar tvra)] (fs)) where
build_abs ct cn v = unbox' v cn tvra (eCase (oper_aaI "<" ct (EVar tvra) zero)  [Alt zeroI (rebox $ EVar tvra)] (fs)) where
    te = getType v
    tvra = tVr 2 st
    tvrb = tVr 4 st
    zero = ELit $ LitInt 0 st
    st = rawType ct
    intt =  rawType "int"
    fs = eStrictLet tvrb (oper_aa "-" ct (EVar tvra)) (rebox (EVar tvrb))
    rebox x = ELit (LitCons cn [x] te)

buildSignum v t = eCase (EVar v) [Alt (LitInt 0 t) (ELit (LitInt 0 t))] (eIf (EPrim (primPrim "prim_op_aaB.<") [EVar v,(ELit (LitInt 0 t))] tBool) (ELit (LitInt (-1) t)) (ELit (LitInt 1  t)))
build_signum ct cn v = unbox' v cn tvra (eCase (EVar tvra) [Alt zero (rebox (ELit zero))] (eCase (oper_aaI "<" ct (EVar tvra) (ELit zero)) [Alt zeroI (rebox one)] (rebox negativeOne))) where
    tvra = tVr 2 st
    te = getType v
    st = rawType ct
    zero :: Lit a E
    zero = LitInt 0 st
    one = ELit $ LitInt 1 st
    negativeOne = ELit $ LitInt (-1) st
    rebox x = ELit (LitCons cn [x] te)




buildPeek cn t p = ELam tvr $ ELam tvrCont $ ELam tvrWorld (unbox' (EVar tvr) dc_Addr tvr' rest)  where
    tvr = (tVr 2 (tPtr t))
    tvr' = tVr 4 (rawType "HsPtr")
    tvrWorld2 = tVr 258 tWorld__
    tvrWorld = tVr 256 tWorld__
    rtVar = tVr 260 (rawType p)
    rtVar' = tVr 262 t
    rest = eCaseTup' (EPrim (APrim (Peek p) mempty) [EVar tvrWorld, EVar tvr'] (ltTuple' [tWorld__,rawType p])) [tvrWorld2,rtVar] (eLet rtVar' (ELit $ LitCons cn [EVar rtVar] t) $ eJustIO (EVar tvrWorld2) (EVar rtVar') )


buildPoke cn t p = ELam ptr_tvr $ ELam v_tvr $ createIO_ $ (\tw -> unbox' (EVar ptr_tvr) dc_Addr ptr_tvr' $ unbox' (EVar v_tvr) cn v_tvr' $ EPrim (APrim (Poke p) mempty) [EVar tw, EVar ptr_tvr', EVar v_tvr'] tWorld__) where
    ptr_tvr =  (tVr 2 (tPtr t))
    v_tvr = tVr 4 t
    ptr_tvr' =  (tVr 6 (rawType "HsPtr"))
    v_tvr' = tVr 8 (rawType p)

toIO :: E -> E -> E
toIO t x = x

{-
buildPeek t p = ELam tvr $ createIO t (\tvrWorld -> EPrim (APrim (Peek p) mempty) [EVar tvrWorld,EVar tvr] (ltTuple [tWorld__,t]))  where
    tvr =  (tVr 2 (tPtr t))
buildPoke t p = ELam ptr_tvr $ ELam v_tvr $ createIO_ $ (\tw -> EPrim (APrim (Poke p) mempty) [EVar tw, EVar ptr_tvr, EVar v_tvr] tWorld__) where
    ptr_tvr =  (tVr 2 (tPtr t))
    v_tvr = tVr 4 t
--toIO t x = prim_unsafeCoerce x (tIO t)
-}

createIO t pv = toIO t (ELam tvrCont $ ELam tvrWorld $  eCaseTup  (pv tvrWorld) [tvrWorld2,rtVar] (eJustIO (EVar tvrWorld2) (EVar rtVar))) where
    tvrWorld2 = tVr 258 tWorld__
    tvrWorld = tVr 256 tWorld__
    rtVar = tVr 260 t
createIO_ pv = toIO tUnit (ELam tvrCont $ ELam tvrWorld $  eStrictLet tvrWorld2 (pv tvrWorld)  (eJustIO (EVar tvrWorld2) vUnit)) where
    tvrWorld2 = tVr 258 tWorld__
    tvrWorld = tVr 256 tWorld__


prim_number v t et@(ELit (LitCons cn' _ _)) = ELit (LitCons cn [ELit (LitInt v (rawType t))] et) where
    cn = toName DataConstructor $ nameName cn'
prim_number _ _ _ = error "prim_number: invalid arg"


prim_const s t et@(ELit (LitCons cn' _ _)) =  eStrictLet (tVr 2 st) (EPrim (APrim (CConst s t) mempty) [] st) (ELit (LitCons cn [EVar $ tVr 2 st] et)) where
    st = rawType t
    cn = toName DataConstructor $ nameName cn'
prim_const _ _ _ = error "prim_const: invalid arg"

prim_sizeof s = (ELit (LitCons dc_Int [rp] tInt)) where
    rp = (EPrim (APrim (PrimTypeInfo { primArgType = s, primRetType = "int", primTypeInfo = PrimSizeOf }) mempty) [] tIntzh)

v2_Int = tVr 2 tInt
v2_Integer = tVr 2 tInteger
v2 t = tVr 2 t

v0 t = tVr 0 t

{-# NOINLINE constantMethods #-}
{-# NOINLINE primitiveInsts #-}
{-# NOINLINE allCTypes #-}
{-# NOINLINE ctypeMap #-}

