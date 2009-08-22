
{- This file is generated -}
module PrimitiveOperators(
    primitiveInsts,
    constantMethods,
    create_uintegralCast_toInt,
    create_uintegralCast_fromInt,
    theMethods,
    r_bits_ptr_,
    r_bits_max_,
    r_bits32,
    allCTypes
    ) where

import Data.Monoid

import C.Prims
import E.E
import E.TypeCheck()
import E.Values
import FrontEnd.Tc.Type
import Name.Name
import Name.Prim
import Name.VConsts
import Support.CanType
import qualified Cmm.Op as Op
import Cmm.Op(stringToOpTy)
import Name.Id

rawType s = ELit litCons { litName = toName RawType s, litType = eHash }
nameToOpTy n = do RawType <- return $ nameType n; Op.readTy (show n)


create_integralCast conv c1 t1 c2 t2 e t = eCase e [Alt (litCons { litName = c1, litArgs = [tvra], litType = te }) cc] Unknown  where
    te = getType e
    ELit LitCons { litName = n1, litArgs = [] } = t1
    ELit LitCons { litName = n2, litArgs = [] } = t2
    Just n1' = nameToOpTy n1
    Just n2' = nameToOpTy n2
    tvra =  tVr va2 t1
    tvrb =  tVr va3 t2
    cc = if n1 == n2 then ELit (litCons { litName = c2, litArgs = [EVar tvra], litType = t }) else
        eStrictLet  tvrb (EPrim (APrim (Op (Op.ConvOp conv n1') n2') mempty) [EVar tvra] t2)  (ELit (litCons { litName = c2, litArgs = [EVar tvrb], litType = t }))

create_integralCast_toInt c1 t1 e = create_integralCast Op.I2I c1 t1 dc_Int tIntzh e tInt
create_integralCast_toInteger c1 t1 e = create_integralCast Op.Sx c1 t1 dc_Integer tIntegerzh e tInteger
create_integralCast_fromInt c2 t2 e t = create_integralCast Op.I2I dc_Int tIntzh c2 t2 e t
create_integralCast_fromInteger c2 t2 e t = create_integralCast Op.Lobits dc_Integer tIntegerzh c2 t2 e t

create_uintegralCast_toInt c1 t1 e = create_integralCast Op.U2U c1 t1 dc_Int tIntzh e tInt
create_uintegralCast_toInteger c1 t1 e = create_integralCast Op.Zx c1 t1 dc_Integer tIntegerzh e tInteger
create_uintegralCast_fromInt c2 t2 e t = create_integralCast Op.U2U dc_Int tIntzh c2 t2 e t
create_uintegralCast_fromInteger c2 t2 e t = create_integralCast Op.Lobits dc_Integer tIntegerzh c2 t2 e t

toClassName x = parseName ClassName x

toInstName x = toName Val ("Instance@",'i':x)

unbox' e cn tvr wtd = eCase e [Alt (litCons { litName = cn, litArgs = [tvr], litType = te }) wtd] Unknown where
    te = getType e

binOp op ca cb cr = APrim (Op (Op.BinOp op ca cb) cr) mempty

oper_aa op ct' e = EPrim (APrim (Op (Op.UnOp op ct) ct) mempty) [e] (rawType ct') where
    ct = stringToOpTy ct'
oper_aaB op ct' a b = EPrim (binOp op ct ct ot_int) [a,b] tBoolzh where
    ct = stringToOpTy ct'
oper_aaa op ct' a b = EPrim (binOp op ct ct ct) [a,b] (rawType ct') where
    ct = stringToOpTy ct'
oper_aIa op ct' a b = EPrim (binOp op ct ot_int ct) [a,b] (rawType ct') where
    ct = stringToOpTy ct'

--zeroI =  LitInt 0 intt

ot_int = stringToOpTy "bits32"

op_aIa op ct cn t = ELam tvra' (ELam tvrb' (unbox' (EVar tvra') cn tvra (unbox' (EVar tvrb') dc_Int tvrb wtd))) where
    tvra' = tVr va1 t
    tvrb' = tVr va2 tInt
    tvra = tVr va3 st
    tvrb = tVr va4 intt
    tvrc = tVr va5 st
    st = rawType ct
    intt = rawType "bits32"
    wtd = eStrictLet tvrc (oper_aIa op ct (EVar tvra) (EVar tvrb)) (rebox (EVar tvrc))
    rebox x = ELit (litCons { litName = cn, litArgs = [x], litType = t })
op_aaa op ct cn t = ELam tvra' (ELam tvrb' (unbox' (EVar tvra') cn tvra (unbox' (EVar tvrb') cn tvrb wtd))) where
    tvra' = tVr va1 t
    tvrb' = tVr va2 t
    tvra = tVr va3 st
    tvrb = tVr va4 st
    tvrc = tVr va5 st
    st = rawType ct
    wtd = eStrictLet tvrc (oper_aaa op ct (EVar tvra) (EVar tvrb)) (rebox (EVar tvrc))
    rebox x = ELit (litCons { litName = cn, litArgs = [x], litType = t })
op_aa op ct cn t = ELam tvra' (unbox' (EVar tvra') cn tvra wtd) where
    tvra' = tVr va1 t
    tvra = tVr va3 st
    tvrc = tVr va5 st
    st = rawType ct
    wtd = eStrictLet tvrc (oper_aa op ct (EVar tvra)) (rebox (EVar tvrc))
    rebox x = ELit (litCons { litName = cn, litArgs = [x], litType = t })
--op_aaI op ct cn t = ELam tvra' (ELam tvrb' (unbox' (EVar tvra') cn tvra (unbox' (EVar tvrb') cn tvrb wtd))) where
--    tvra' = tVr 2 t
--    tvrb' = tVr 4 t
--    tvra = tVr 6 st
--    tvrb = tVr 8 st
--    tvrc = tVr 10 intt
--    st = rawType ct
--    wtd = eStrictLet tvrc (oper_aaI op ct (EVar tvra) (EVar tvrb)) (rebox (EVar tvrc))
--    rebox x = ELit (litCons { litName = dc_Int, litArgs = [x], litType = t })

op_aaB op ct cn t = ELam tvra' (ELam tvrb' (unbox' (EVar tvra') cn tvra (unbox' (EVar tvrb') cn tvrb wtd))) where
    tvra' = tVr va1 t
    tvrb' = tVr va2 t
    tvra = tVr va3 st
    tvrb = tVr va4 st
    tvrc = tVr va5 tBoolzh
    st = rawType ct
    wtd = eStrictLet tvrc (oper_aaB op ct (EVar tvra) (EVar tvrb)) (ELit (litCons { litName = dc_Boolzh, litArgs = [EVar tvrc], litType = tBool }))  -- (caseof (EVar tvrc))
--    caseof x = eCase x [Alt zeroI vFalse]  vTrue

build_abs ct cn v = unbox' v cn tvra (eCase (oper_aaB Op.Lt ct (EVar tvra) zero)  [Alt lFalsezh (rebox $ EVar tvra), Alt lTruezh fs] Unknown) where
    te = getType v
    tvra = tVr va1 st
    tvrb = tVr va2 st
    zero = ELit $ LitInt 0 st
    st = rawType ct
    fs = eStrictLet tvrb (oper_aa Op.Neg ct (EVar tvra)) (rebox (EVar tvrb))
    rebox x = ELit (litCons { litName = cn, litArgs = [x], litType = te })

build_uabs ct cn v = v


build_usignum ct cn v = unbox' v cn tvra (eCase (EVar tvra) [Alt zero (rebox (ELit zero))] (rebox (ELit one))) where
    tvra = tVr va1 st
    te = getType v
    st = rawType ct
    zero :: Lit a E
    zero = LitInt 0 st
    one = LitInt 1 st
    rebox x = ELit (litCons { litName = cn, litArgs = [x], litType = te })

build_signum ct cn v = unbox' v cn tvra (eCase (EVar tvra) [Alt zero (rebox (ELit zero))] (eCase (oper_aaB Op.Lt ct (EVar tvra) (ELit zero)) [Alt lFalsezh (rebox one),Alt lTruezh (rebox negativeOne)] Unknown)) where
    tvra = tVr va1 st
    te = getType v
    st = rawType ct
    zero :: Lit a E
    zero = LitInt 0 st
    one = ELit $ LitInt 1 st
    negativeOne = ELit $ LitInt (-1) st
    rebox x = ELit (litCons { litName = cn, litArgs = [x], litType = te })


toIO :: E -> E -> E
toIO t x = x

{-
createIO t pv = toIO t (ELam tvrWorld $  eCaseTup  (pv tvrWorld) [tvrWorld2,rtVar] (eJustIO (EVar tvrWorld2) (EVar rtVar))) where
    tvrWorld2 = tVr 258 tWorld__
    tvrWorld = tVr 256 tWorld__
    rtVar = tVr 260 t
-}
createIO_ pv = toIO tUnit (ELam tvrWorld $  eStrictLet tvrWorld2 (pv tvrWorld)  (eJustIO (EVar tvrWorld2) vUnit)) where
    tvrWorld2 = tVr (anonymous 258) tWorld__
    tvrWorld = tVr (anonymous 256) tWorld__


prim_number cn v t et = ELit litCons { litName = cn, litArgs = [ELit (LitInt v t)], litType = et }

prim_minbound, prim_maxbound, prim_uminbound, prim_umaxbound :: Name -> E -> ExtType ->  E
prim_uminbound dc dt s = prim_number dc 0 (rawType s) dt
prim_umaxbound = prim_bound PrimUMaxBound
prim_maxbound = prim_bound PrimMaxBound
prim_minbound = prim_bound PrimMinBound

prim_bound pt dc dt s = (ELit (litCons { litName = dc, litArgs = [rp], litType = dt })) where
    rt = rawType s
    Just at = Op.readTy s
    rp | Just n <- primStaticTypeInfo at pt = (ELit (LitInt (fromInteger n) rt))
       | otherwise = EPrim (APrim (PrimTypeInfo { primArgTy = at, primRetTy = at, primTypeInfo = pt }) mempty) [] rt

prim_sizeof s = (ELit (litCons { litName = dc_Int, litArgs = [rp], litType = tInt })) where
    Just at = Op.readTy s
    rp | Just n <- primStaticTypeInfo at PrimSizeOf = (ELit (LitInt (fromInteger n) tIntzh))
       | otherwise = EPrim (APrim (PrimTypeInfo { primArgTy = stringToOpTy s, primRetTy = ot_int, primTypeInfo = PrimSizeOf }) mempty) [] tIntzh


v2_Int = tVr va1 tInt
v2_Integer = tVr va1 tInteger
v2 t = tVr va1 t

v0 t = tVr emptyId t

{-# NOINLINE constantMethods #-}
{-# NOINLINE primitiveInsts #-}
{-# NOINLINE allCTypes #-}

