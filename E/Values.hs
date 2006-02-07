module E.Values where

import Char
import Control.Monad.Identity
import Data.Monoid
import List
import qualified Data.Map as Map
import qualified Data.Set as Set
import Ratio

import Support.CanType
import Support.Tuple
import C.Prims
import E.E
import E.FreeVars()
import E.Subst
import E.TypeCheck
import Support.FreeVars
import Info.Types
import Name.Name
import Name.Names
import Name.VConsts


instance Tuple E where
    tupleNil = vUnit
    tupleMany es = ELit $ LitCons (nameTuple DataConstructor (length es)) es (ltTuple ts) where
        ts = map getType es

eIf e a b = ECase { eCaseScrutinee = e, eCaseBind = (tVr 0 tBool),  eCaseAlts =  [Alt vTrue a,Alt vFalse b], eCaseDefault = Nothing }

eTuple :: [E] -> E
eTuple = tuple

eTuple' es = ELit $ LitCons (unboxedNameTuple DataConstructor (length es)) es (ltTuple' ts) where
    ts = map getType es

ltTuple ts = ELit $ LitCons (nameTuple TypeConstructor (length ts)) ts eStar
ltTuple' ts = ELit $ LitCons (unboxedNameTuple TypeConstructor (length ts)) ts eHash


unboxedTuple es =  LitCons (unboxedNameTuple DataConstructor (length es)) es (ltTuple' ts) where
    ts = map getType es

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
    toE ch = ELit (LitCons dc_Integer [toEzh ch] tInteger)
    typeE _ = tInteger

instance ToE Int where
    toE ch = ELit (LitCons dc_Int [toEzh ch] tInt)
    typeE _ = tInt

instance ToE a => ToE [a] where
    toE xs@[] = eNil (typeE xs)
    toE (x:xs) = eCons (toE x) (toE xs)
    typeE (_::[a]) = ELit (LitCons tc_List [typeE (undefined::a)] eStar)


--eInt x = ELit $ LitInt x tInt

eCons x xs = ELit $ LitCons vCons [x,xs] (getType xs)
eNil t = ELit $ LitCons vEmptyList [] t

eCaseTup e vs w = ECase e (tVr 0 (getType e)) [Alt (LitCons (nameTuple DataConstructor (length vs)) vs (getType e)) w] Nothing
eCaseTup' e vs w = ECase e (tVr 0 (getType e)) [Alt (LitCons (unboxedNameTuple DataConstructor (length vs)) vs (getType e)) w] Nothing

eJustIO w x = ELit (LitCons dc_JustIO [w,x] (ELit (LitCons tc_IOResult [getType x] eStar)))
tIO t = ELit (LitCons tc_IO [t] eStar)

eCase e alts Unknown = ECase { eCaseScrutinee = e, eCaseBind = (tVr 0 (getType e)), eCaseDefault = Nothing, eCaseAlts =  alts }
eCase e alts els = ECase { eCaseScrutinee = e, eCaseBind = (tVr 0 (getType e)), eCaseDefault = Just els, eCaseAlts =  alts }

-- | This takes care of types right away, it simplifies various other things to do it this way.
eLet :: TVr -> E -> E -> E
eLet TVr { tvrIdent = 0 } _ e' = e'
eLet t@(TVr { tvrType =  ty}) e e'
    | sortStarLike ty && isAtomic e = subst t e e'
    | sortStarLike ty = ELetRec [(t,e)] (typeSubst mempty (Map.singleton (tvrIdent t) e) e')
    | isUnboxed ty && isAtomic e = subst t e e'
    | isUnboxed ty  = eStrictLet t e e'
eLet t e e' = ELetRec [(t,e)] e'

-- | strict version of let, evaluates argument before assigning it.
eStrictLet t@(TVr { tvrType =  ty }) v e | sortStarLike ty  = eLet t v e
eStrictLet t v e = ECase v t [] (Just e)

substLet :: [(TVr,E)] -> E -> E
substLet ds e  = ans where
    (as,nas) = partition (isAtomic . snd) (filter ((/= 0) . tvrNum . fst) ds)
    tas = filter (sortStarLike . tvrType . fst) nas
    ans = eLetRec (as ++ nas) (typeSubst' (Map.fromList [ (n,e) | (TVr { tvrIdent = n },e) <- as]) (Map.fromList [ (n,e) | (TVr { tvrIdent = n },e) <- tas]) e)


substLet' :: [(TVr,E)] -> E -> E
substLet' ds' e  = ans where
    (hh,ds) = partition (isUnboxed . tvrType . fst) ds'
    nas = filter ((/= 0) . tvrNum . fst) ds
    tas = filter (sortStarLike . tvrType . fst) nas
    ans = case (nas,tas) of
        ([],_) -> hhh hh $ e
        (nas,[]) -> hhh hh $ ELetRec nas e
        _  -> let
                    f = typeSubst' mempty (Map.fromList [ (n,e) | (TVr { tvrIdent = n },e) <- tas])
                    nas' = [ (v,f e) | (v,e) <- nas]
               in hhh hh $ ELetRec nas' (f e)
    hhh [] e = e
    hhh ((h,v):hh) e = eLet h v (hhh hh e)

eLetRec = substLet'

-- | determine if term can contain _|_
isLifted :: E -> Bool
isLifted x = sortTermLike x && not (isUnboxed (getType x))

-- Note: This does not treat lambdas as whnf
whnfOrBot :: E -> Bool
whnfOrBot (EError {}) = True
whnfOrBot (ELit (LitCons _ xs _)) = all isAtomic xs
whnfOrBot (EPi (TVr { tvrIdent =  j, tvrType =  x }) y) | not (j `Set.member` freeVars y) = isAtomic x && isAtomic y
whnfOrBot e = isAtomic e

-- Determine if a type represents an unboxed value
isUnboxed :: E -> Bool
isUnboxed e@EPi {} = False
isUnboxed e = getType e == eHash

safeToDup ec@ECase {}
    | EVar _ <- eCaseScrutinee ec = all safeToDup (caseBodies ec)
    | EPrim p _ _ <- eCaseScrutinee ec, aprimIsCheap p = all safeToDup (caseBodies ec)
safeToDup (EPrim p _ _) = aprimIsCheap p
safeToDup e = whnfOrBot e || isELam e || isEPi e


tTag = rawType "tag#"
vTag n = ELit $ LitCons n [] tTag

prim_seq a b | isWHNF a = b
prim_seq a b = ECase a (tVr 0 (getType a)) [] (Just b)

prim_toTag e = f e where
    f (ELit (LitCons n _ _)) = vTag n
    f (ELit (LitInt {})) = error "toTag applied to integer"
    f (ELetRec ds e) = ELetRec ds (prim_toTag e)
    f (EError err _) = EError err tTag
    f ec@ECase {} = nx where
        Identity nx = caseBodiesMapM (return . prim_toTag) ec
    f e = EPrim (primPrim "toTag") [e] tTag

-- prim_fromTag e t = EPrim (primPrim "fromTag") [e] t

prim_unsafeCoerce e t = p e' where
    (_,e',p) = unsafeCoerceOpt $ EPrim p_unsafeCoerce [e] t
from_unsafeCoerce (EPrim (APrim (PrimPrim "unsafeCoerce") _) [e] t) = return (e,t)
from_unsafeCoerce _ = fail "Not unsafeCoerce primitive"

rawType s  = ELit (LitCons (toName RawType s) [] eHash)

unsafeCoerceOpt (EPrim (APrim (PrimPrim "unsafeCoerce") _) [e] t) = f (0::Int) e t where
    f n e t | Just (e',_) <- from_unsafeCoerce e = f (n + 1) e' t
    f n (ELetRec ds e) t = (n + 1, ELetRec ds (p e'),id) where
        (n,e',p) = f n e t
    f n (EError err _) t = (n,EError err t,id)
    f n (ELit (LitInt x _)) t = (n,ELit (LitInt x t),id)
    f n (ELit (LitCons x y _)) t = (n,ELit (LitCons x y t),id)
    f n ec@ECase {} t = (n,nx,id) where
        Identity nx = caseBodiesMapM (return . flip prim_unsafeCoerce t) ec
    f n e t | getType e == t = (n,e,id)
    f n e t = (n,e,flip prim_unsafeCoerce t)
unsafeCoerceOpt e = (0,e,id)

prim_integralCast e t = EPrim p_integralCast [e] t
from_integralCast (EPrim (APrim (PrimPrim "integralCast") _) [e] t) = return (e,t)
from_integralCast _ = fail "Not integralCast primitive"

instance HasProperties TVr where
    setProperty prop tvr = tvrInfo_u (setProperty prop) tvr
    unsetProperty prop tvr = tvrInfo_u (unsetProperty prop) tvr
    getProperty prop tvr = getProperty prop (tvrInfo tvr)


