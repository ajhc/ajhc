module E.Values where

import CanType
import Char
import C.Prims
import E.E
import E.FreeVars()
import E.Subst
import E.TypeCheck
import FreeVars
import Name
import qualified Data.Set as Set
import Ratio
import VConsts
import Info.Types


eIf e a b = ECase { eCaseScrutinee = e, eCaseBind = (tVr 0 tBool),  eCaseAlts =  [Alt vTrue a,Alt vFalse b], eCaseDefault = Nothing }

eTuple []  = vUnit
eTuple [e] = e
eTuple es = ELit $ LitCons (toTuple (length es)) es (ltTuple ts) where
    ts = map getType es

eTuple' es = ELit $ LitCons (unboxedNameTuple DataConstructor (length es)) es (ltTuple' ts) where
    ts = map getType es

ltTuple ts = ELit $ LitCons (nameTuple TypeConstructor (length ts)) ts eStar
ltTuple' ts = ELit $ LitCons (unboxedNameTuple TypeConstructor (length ts)) ts eStar



class ToE a where
    toE :: a -> E
    typeE :: a -> E -- lazy in a

class ToEzh a where
    toEzh :: a -> E
    typeEzh :: a -> E

instance ToEzh Char where
    toEzh ch = ELit $ LitInt (fromIntegral $ fromEnum ch) tCharzh
    typeEzh _ = tCharzh

instance ToEzh Int where
    toEzh ch = ELit $ LitInt (fromIntegral  ch) tIntzh
    typeEzh _ = tIntzh

instance ToEzh Integer where
    toEzh ch = ELit $ LitInt (fromIntegral  ch) tIntegerzh
    typeEzh _ = tIntegerzh

instance ToE () where
    toE () = vUnit
    typeE _ = tUnit

instance ToE Bool where
    toE True = vTrue
    toE False = vFalse
    typeE _ = tBool


instance ToE Char where
    toE ch = ELit (LitCons dc_Char [toEzh ch] tChar)
    typeE _ = tChar

instance ToE Rational where
    toE rat = ELit (LitCons dc_Rational [toE (numerator rat), toE (denominator rat)] tRational)
    typeE _ = tRational

instance ToE Integer where
    toE ch = ELit (litCons DataConstructor ("Prelude","Integer") [toEzh ch] tInteger)
    typeE _ = tInteger

instance ToE Int where
    toE ch = ELit (litCons DataConstructor ("Prelude","Int") [toEzh ch] tInt)
    typeE _ = tInt

instance ToE a => ToE [a] where
    toE xs@[] = eNil (typeE xs)
    toE (x:xs) = eCons (toE x) (toE xs)
    typeE (_::[a]) = ELit (litCons TypeConstructor ("Prelude","[]") [typeE (undefined::a)] eStar)


--eInt x = ELit $ LitInt x tInt

eCons x xs = ELit $ LitCons vCons [x,xs] (getType xs)
eNil t = ELit $ LitCons vEmptyList [] t

eCaseTup e vs w = ECase e (tVr 0 (getType e)) [Alt (LitCons (toTuple (length vs)) vs (getType e)) w] Nothing
eCaseTup' e vs w = ECase e (tVr 0 (getType e)) [Alt (LitCons (unboxedNameTuple DataConstructor (length vs)) vs (getType e)) w] Nothing

eJustIO w x = ELit (LitCons dc_JustIO [w,x] (ELit (LitCons (toName TypeConstructor ("Jhc.IO","IOResult")) [getType x] eStar)))
tIO t = ELit (LitCons (toName TypeConstructor ("Jhc.IO", "IO")) [t] eStar)

eCase e alts Unknown = ECase { eCaseScrutinee = e, eCaseBind = (tVr 0 (getType e)), eCaseDefault = Nothing, eCaseAlts =  alts }
eCase e alts els = ECase { eCaseScrutinee = e, eCaseBind = (tVr 0 (getType e)), eCaseDefault = Just els, eCaseAlts =  alts }

-- | This takes care of types right away, it simplifies various other things to do it this way.
eLet :: TVr -> E -> E -> E
eLet TVr { tvrIdent = 0 } _ = id
eLet t@(TVr { tvrType =  ty}) e | sortStarLike ty && isAtomic e = subst t e
eLet t e = ELetRec [(t,e)]




isLifted x = sortTermLike x

-- Note: This does not treat lambdas as whnf
whnfOrBot :: E -> Bool
whnfOrBot (EError {}) = True
whnfOrBot (ELit (LitCons _ xs _)) = all isAtomic xs
whnfOrBot (EPi (TVr { tvrIdent =  j, tvrType =  x }) y) | not (j `Set.member` freeVars y) = isAtomic x && isAtomic y
whnfOrBot e = isAtomic e

safeToDup e = whnfOrBot e || isELam e || isEPi e

eStrictLet t@(TVr { tvrType =  ty }) v e | sortStarLike ty && isAtomic v = subst t v e
eStrictLet t v e = ECase v t [] (Just e)

prim_seq a b | isWHNF a = b
prim_seq a b = ECase a (tVr 0 (getType a)) [] (Just b)

prim_unsafeCoerce e t = p e' where
    (_,e',p) = unsafeCoerceOpt $ EPrim (primPrim "unsafeCoerce") [e] t
from_unsafeCoerce (EPrim (APrim (PrimPrim "unsafeCoerce") _) [e] t) = return (e,t)
from_unsafeCoerce _ = fail "Not unsafeCoerce primitive"

rawType s  = ELit (LitCons (toName RawType s) [] eStar)

unsafeCoerceOpt (EPrim (APrim (PrimPrim "unsafeCoerce") _) [e] t) = f (0::Int) e t where
    f n e t | Just (e',_) <- from_unsafeCoerce e = f (n + 1) e' t
    f n (ELetRec ds e) t = (n + 1, ELetRec ds (p e'),id) where
        (n,e',p) = f n e t
    f n (EError err _) t = (n,EError err t,id)
    f n (ELit (LitInt x _)) t = (n,ELit (LitInt x t),id)
    f n (ELit (LitCons x y _)) t = (n,ELit (LitCons x y t),id)
    f n e t | getType e == t = (n,e,id)
    f n e t = (n,e,flip prim_unsafeCoerce t)
unsafeCoerceOpt e = (0,e,id)

prim_integralCast e t = EPrim (primPrim "integralCast") [e] t
from_integralCast (EPrim (APrim (PrimPrim "integralCast") _) [e] t) = return (e,t)
from_integralCast _ = fail "Not integralCast primitive"

tPtr t = ELit (LitCons (toName TypeConstructor ("Foreign.Ptr","Ptr")) [t] eStar)

instance HasProperties TVr where
    setProperty prop tvr = tvrInfo_u (setProperty prop) tvr
    unsetProperty prop tvr = tvrInfo_u (unsetProperty prop) tvr
    getProperty prop tvr = getProperty prop (tvrInfo tvr)


