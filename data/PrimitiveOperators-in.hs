
{- This file is generated -}
module PrimitiveOperators(primitiveInsts,constantMethods,theMethods,allCTypes) where

import Data.Monoid

import C.Arch
import C.Prims
import E.E
import E.TypeCheck()
import E.Values
import FrontEnd.Tc.Type
import Name.Name
import Name.Prim
import Name.VConsts
import Support.CanType
import qualified C.Op as Op


tPtr t = ELit (litCons { litName = tc_Ptr, litArgs = [t], litType = eStar, litAliasFor = Just (ELam tvr { tvrIdent = 2, tvrType = eStar} (ELit litCons { litName = tc_Addr, litType = eStar })) })

create_integralCast c1 t1 c2 t2 e t = eCase e [Alt (litCons { litName = c1, litArgs = [tvra], litType = te }) cc] Unknown  where
    te = getType e
    ELit LitCons { litName = n1, litArgs = [] } = t1
    ELit LitCons { litName = n2, litArgs = [] } = t2
    tvra =  tVr 4 t1
    tvrb =  tVr 6 t2
    cc = if n1 == n2 then ELit (litCons { litName = c2, litArgs = [EVar tvra], litType = t }) else
        eStrictLet  tvrb (EPrim (APrim (CCast (show n1) (show n2)) mempty) [EVar tvra] t2)  (ELit (litCons { litName = c2, litArgs = [EVar tvrb], litType = t }))

create_integralCast_toInt c1 t1 e = create_integralCast c1 t1 dc_Int tIntzh e tInt
create_integralCast_toInteger c1 t1 e = create_integralCast c1 t1 dc_Integer tIntegerzh e tInteger
create_integralCast_fromInt c2 t2 e t = create_integralCast dc_Int tIntzh c2 t2 e t
create_integralCast_fromInteger c2 t2 e t = create_integralCast dc_Integer tIntegerzh c2 t2 e t


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

op_aIa op ct cn t = ELam tvra' (ELam tvrb' (unbox' (EVar tvra') cn tvra (unbox' (EVar tvrb') cn tvrb wtd))) where
    tvra' = tVr 2 t
    tvrb' = tVr 4 tInt
    tvra = tVr 6 st
    tvrb = tVr 8 intt
    tvrc = tVr 10 st
    st = rawType ct
    intt = rawType "bits32"
    wtd = eStrictLet tvrc (oper_aIa op ct (EVar tvra) (EVar tvrb)) (rebox (EVar tvrc))
    rebox x = ELit (litCons { litName = cn, litArgs = [x], litType = t })
op_aaa op ct cn t = ELam tvra' (ELam tvrb' (unbox' (EVar tvra') cn tvra (unbox' (EVar tvrb') cn tvrb wtd))) where
    tvra' = tVr 2 t
    tvrb' = tVr 4 t
    tvra = tVr 6 st
    tvrb = tVr 8 st
    tvrc = tVr 10 st
    st = rawType ct
    wtd = eStrictLet tvrc (oper_aaa op ct (EVar tvra) (EVar tvrb)) (rebox (EVar tvrc))
    rebox x = ELit (litCons { litName = cn, litArgs = [x], litType = t })
op_aa op ct cn t = ELam tvra' (unbox' (EVar tvra') cn tvra wtd) where
    tvra' = tVr 2 t
    tvra = tVr 6 st
    tvrc = tVr 10 st
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
    tvra' = tVr 2 t
    tvrb' = tVr 4 t
    tvra = tVr 6 st
    tvrb = tVr 8 st
    tvrc = tVr 10 tBoolzh
    st = rawType ct
    wtd = eStrictLet tvrc (oper_aaB op ct (EVar tvra) (EVar tvrb)) (ELit (litCons { litName = dc_Boolzh, litArgs = [EVar tvrc], litType = tBool }))  -- (caseof (EVar tvrc))
--    caseof x = eCase x [Alt zeroI vFalse]  vTrue

build_abs ct cn v = unbox' v cn tvra (eCase (oper_aaB Op.Lt ct (EVar tvra) zero)  [Alt lFalsezh (rebox $ EVar tvra), Alt lTruezh fs] Unknown) where
    te = getType v
    tvra = tVr 2 st
    tvrb = tVr 4 st
    zero = ELit $ LitInt 0 st
    st = rawType ct
    fs = eStrictLet tvrb (oper_aa Op.Neg ct (EVar tvra)) (rebox (EVar tvrb))
    rebox x = ELit (litCons { litName = cn, litArgs = [x], litType = te })

build_uabs ct cn v = v

build_fabs ct cn v = unbox' v cn tvra (rebox (oper_aa Op.FAbs ct (EVar tvra))) where
    te = getType v
    tvra = tVr 2 st
    st = rawType ct
    rebox x = ELit (litCons { litName = cn, litArgs = [x], litType = te })

build_usignum ct cn v = unbox' v cn tvra (eCase (EVar tvra) [Alt zero (rebox (ELit zero))] (rebox (ELit one))) where
    tvra = tVr 2 st
    te = getType v
    st = rawType ct
    zero :: Lit a E
    zero = LitInt 0 st
    one = LitInt 1 st
    rebox x = ELit (litCons { litName = cn, litArgs = [x], litType = te })

build_signum ct cn v = unbox' v cn tvra (eCase (EVar tvra) [Alt zero (rebox (ELit zero))] (eCase (oper_aaB Op.Lt ct (EVar tvra) (ELit zero)) [Alt lFalsezh (rebox one),Alt lTruezh (rebox negativeOne)] Unknown)) where
    tvra = tVr 2 st
    te = getType v
    st = rawType ct
    zero :: Lit a E
    zero = LitInt 0 st
    one = ELit $ LitInt 1 st
    negativeOne = ELit $ LitInt (-1) st
    rebox x = ELit (litCons { litName = cn, litArgs = [x], litType = te })


build_fsignum ct cn v = unbox' v cn tvra (eCase (EVar tvra) [Alt zero (rebox (ELit zero))] (eCase (oper_aaB Op.FLt ct (EVar tvra) (ELit zero)) [Alt lFalsezh (rebox one),Alt lTruezh (rebox negativeOne)] Unknown)) where
    tvra = tVr 2 st
    te = getType v
    st = rawType ct
    zero :: Lit a E
    zero = LitInt 0 st
    one = ELit $ LitInt 1 st
    negativeOne = ELit $ LitInt (-1) st
    rebox x = ELit (litCons { litName = cn, litArgs = [x], litType = te })


buildPeek cn t p = ELam tvr $ ELam tvrWorld (unbox' (EVar tvr) dc_Addr tvr' rest)  where
    tvr = (tVr 2 (tPtr t))
    tvr' = tVr 4 (rawType "bits<ptr>")
    tvrWorld2 = tVr 258 tWorld__
    tvrWorld = tVr 256 tWorld__
    rtVar = tVr 260 (rawType p)
    rtVar' = tVr 262 t
    rest = eCaseTup' (EPrim (APrim (Peek (stringToOpTy p)) mempty) [EVar tvrWorld, EVar tvr'] (ltTuple' [tWorld__,rawType p])) [tvrWorld2,rtVar] (eLet rtVar' (ELit $ litCons { litName = cn, litArgs = [EVar rtVar], litType = t }) $ eJustIO (EVar tvrWorld2) (EVar rtVar') )


buildPoke cn t p = ELam ptr_tvr $ ELam v_tvr $ createIO_ $ (\tw -> unbox' (EVar ptr_tvr) dc_Addr ptr_tvr' $ unbox' (EVar v_tvr) cn v_tvr' $ EPrim (APrim (Poke (stringToOpTy p)) mempty) [EVar tw, EVar ptr_tvr', EVar v_tvr'] tWorld__) where
    ptr_tvr =  (tVr 2 (tPtr t))
    v_tvr = tVr 4 t
    ptr_tvr' =  (tVr 6 (rawType "bits<ptr>"))
    v_tvr' = tVr 8 (rawType p)

toIO :: E -> E -> E
toIO t x = x

{-
createIO t pv = toIO t (ELam tvrWorld $  eCaseTup  (pv tvrWorld) [tvrWorld2,rtVar] (eJustIO (EVar tvrWorld2) (EVar rtVar))) where
    tvrWorld2 = tVr 258 tWorld__
    tvrWorld = tVr 256 tWorld__
    rtVar = tVr 260 t
-}
createIO_ pv = toIO tUnit (ELam tvrWorld $  eStrictLet tvrWorld2 (pv tvrWorld)  (eJustIO (EVar tvrWorld2) vUnit)) where
    tvrWorld2 = tVr 258 tWorld__
    tvrWorld = tVr 256 tWorld__


prim_number cn v t et = ELit litCons { litName = cn, litArgs = [ELit (LitInt v t)], litType = et }
prim_const cn s st t et = ELit litCons { litName = cn, litArgs = [EPrim (APrim (CConst s t) mempty) [] st], litType = et }

prim_minbound, prim_maxbound :: Name -> E -> ExtType -> E -> E
prim_minbound dc dt s e = f s where
    f "HsChar" = boxup $ ELit $ LitInt 0 (rawType "bits32")
    f s | Just pt <- genericPrimitiveInfo s = boxup $ case primTypeIsSigned pt of
        False -> ELit $ LitInt 0 (rawType s)
        True -> ELit $ LitInt (negate $ 2 ^ (8 * primTypeSizeOf pt - 1)) (rawType s)
    f _ = e
    boxup a =  ELit litCons { litName = dc, litArgs = [a], litType = dt }
prim_maxbound dc dt s e = f s where
    f "HsChar" = boxup $ ELit $ LitInt 0x10ffff (rawType "bits32")
    f s | Just pt <- genericPrimitiveInfo s = boxup $ case primTypeIsSigned pt of
        False -> ELit $ LitInt (2 ^ (8 * primTypeSizeOf pt)) (rawType s)
        True -> ELit $ LitInt (2 ^ (8 * primTypeSizeOf pt - 1) - 1) (rawType s)
    f _ = e
    boxup a =  ELit litCons { litName = dc, litArgs = [a], litType = dt }


prim_sizeof s | Just pt <- genericPrimitiveInfo s = let
    rp = ELit $ LitInt (fromIntegral (primTypeSizeOf pt)) tIntzh
    in (ELit (litCons { litName = dc_Int, litArgs = [rp], litType = tInt }))
prim_sizeof s = (ELit (litCons { litName = dc_Int, litArgs = [rp], litType = tInt })) where
    rp = EPrim (APrim (PrimTypeInfo { primArgTy = stringToOpTy s, primRetTy = ot_int, primTypeInfo = PrimSizeOf }) mempty) [] tIntzh


v2_Int = tVr 2 tInt
v2_Integer = tVr 2 tInteger
v2 t = tVr 2 t

v0 t = tVr 0 t

{-# NOINLINE constantMethods #-}
{-# NOINLINE primitiveInsts #-}
{-# NOINLINE allCTypes #-}

