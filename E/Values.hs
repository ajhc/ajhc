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
import qualified Info.Info as Info
import Support.FreeVars
import Info.Types
import Name.Name
import Name.Names
import Name.VConsts
import Util.SetLike


instance Tuple E where
    tupleNil = vUnit
    tupleMany es = ELit $ LitCons (nameTuple DataConstructor (length es)) es (ltTuple ts) where
        ts = map getType es

eIf e a b = ECase { eCaseScrutinee = e, eCaseType = getType a, eCaseBind = (tVr 0 tBool),  eCaseAlts =  [Alt vTrue a,Alt vFalse b], eCaseDefault = Nothing }

eTuple :: [E] -> E
eTuple = tuple

eTuple' es = ELit $ LitCons (unboxedNameTuple DataConstructor (length es)) es (ltTuple' ts) where
    ts = map getType es



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

emptyCase = ECase { eCaseDefault = Nothing, eCaseAlts = [], eCaseBind = error "emptyCase: bind", eCaseType = error "emptyCase: type", eCaseScrutinee = error "emptyCase: scrutinee" }

eCaseTup e vs w = emptyCase { eCaseScrutinee = e, eCaseBind =  (tVr 0 (getType e)), eCaseType = getType w, eCaseAlts =  [Alt (LitCons (nameTuple DataConstructor (length vs)) vs (getType e)) w] }
eCaseTup' e vs w = emptyCase { eCaseScrutinee = e, eCaseBind = (tVr 0 (getType e)), eCaseType = getType w, eCaseAlts =  [Alt (LitCons (unboxedNameTuple DataConstructor (length vs)) vs (getType e)) w] }

eJustIO w x = ELit (LitCons dc_JustIO [w,x] (ELit (LitCons tc_IOResult [getType x] eStar)))
tIO t = ELit (LitCons tc_IO [t] eStar)

eCase e alts@(alt:_) Unknown = emptyCase { eCaseScrutinee = e, eCaseBind = (tVr 0 (getType e)), eCaseType = getType alt,  eCaseAlts =  alts }
eCase e alts els = emptyCase { eCaseScrutinee = e, eCaseBind = (tVr 0 (getType e)), eCaseDefault = Just els, eCaseAlts =  alts, eCaseType = getType els }

-- | This takes care of types right away, it simplifies various other things to do it this way.
eLet :: TVr -> E -> E -> E
eLet TVr { tvrIdent = 0 } _ e' = e'
eLet t@(TVr { tvrType =  ty}) e e'
    | sortStarLike ty && isAtomic e = subst t e e'
    | sortStarLike ty = ELetRec [(t,e)] (typeSubst mempty (msingleton (tvrIdent t) e) e')
    | isUnboxed ty && isAtomic e = subst t e e'
    | isUnboxed ty  = eStrictLet t e e'
eLet t e e' = ELetRec [(t,e)] e'

-- | strict version of let, evaluates argument before assigning it.
eStrictLet t@(TVr { tvrType =  ty }) v e | sortStarLike ty  = eLet t v e
eStrictLet t v e = emptyCase { eCaseScrutinee = v, eCaseBind = t, eCaseDefault = Just e, eCaseType = getType e }

substLet :: [(TVr,E)] -> E -> E
substLet ds e  = ans where
    (as,nas) = partition (isAtomic . snd) (filter ((/= 0) . tvrIdent . fst) ds)
    tas = filter (sortStarLike . tvrType . fst) nas
    ans = eLetRec (as ++ nas) (typeSubst' (fromList [ (n,e) | (TVr { tvrIdent = n },e) <- as]) (fromList [ (n,e) | (TVr { tvrIdent = n },e) <- tas]) e)


substLet' :: [(TVr,E)] -> E -> E
substLet' ds' e  = ans where
    (hh,ds) = partition (isUnboxed . tvrType . fst) ds'
    nas = filter ((/= 0) . tvrIdent . fst) ds
    tas = filter (sortStarLike . tvrType . fst) nas
    ans = case (nas,tas) of
        ([],_) -> hhh hh $ e
        (nas,[]) -> hhh hh $ ELetRec nas e
        _  -> let
                    f = typeSubst' mempty (fromList [ (n,e) | (TVr { tvrIdent = n },e) <- tas])
                    nas' = [ (v,f e) | (v,e) <- nas]
               in hhh hh $ ELetRec nas' (f e)
    hhh [] e = e
    hhh ((h,v):hh) e = eLet h v (hhh hh e)

eLetRec = substLet'


vTag n = ELit $ LitCons n [] tTag

prim_seq a b | isWHNF a = b
prim_seq a b = emptyCase { eCaseScrutinee = a, eCaseBind =  (tVr 0 (getType a)), eCaseDefault = Just b, eCaseType = getType b }

prim_toTag e = f e where
    f (ELit (LitCons n _ _)) = vTag n
    f (ELit (LitInt {})) = error "toTag applied to integer"
    f (ELetRec ds e) = ELetRec ds (prim_toTag e)
    f (EError err _) = EError err tTag
    f ec@ECase {} = nx where
        Identity nx = caseBodiesMapM (return . prim_toTag) ec
    f e = EPrim p_toTag [e] tTag

-- prim_fromTag e t = EPrim (primPrim "fromTag") [e] t

prim_unsafeCoerce e t = p e' where
    (_,e',p) = unsafeCoerceOpt $ EPrim p_unsafeCoerce [e] t
from_unsafeCoerce (EPrim (APrim (PrimPrim "unsafeCoerce") _) [e] t) = return (e,t)
from_unsafeCoerce _ = fail "Not unsafeCoerce primitive"

rawType s = ELit (LitCons (toName RawType s) [] eHash)

tWorldzh = ELit (LitCons rt_Worldzh [] eHash)
tTag = ELit (LitCons rt_tag [] eHash)

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


instance HasProperties TVr where
    setProperty prop tvr = tvrInfo_u (setProperty prop) tvr
    unsetProperty prop tvr = tvrInfo_u (unsetProperty prop) tvr
    getProperty prop tvr = getProperty prop (tvrInfo tvr)


-- various routines used to classify expressions
-- many assume atomicity constraints are in place

-- | whether a value is a compile time constant
isFullyConst :: E -> Bool
isFullyConst (ELit (LitCons _ [] _)) = True
isFullyConst (ELit (LitCons _ xs _)) = all isFullyConst xs
isFullyConst ELit {} = True
isFullyConst (EPi (TVr { tvrType = t }) x) =  isFullyConst t && isFullyConst x
isFullyConst (EPrim (APrim p _) as _) = primIsConstant p && all isFullyConst as
isFullyConst _ = False


-- | whether a value may be used as an argument to an application, literal, or primitive
-- these may be duplicated with no code size or runtime penalty
isAtomic :: E -> Bool
isAtomic EVar {}  = True
isAtomic e | sortTypeLike e = True
isAtomic (EPrim (APrim (PrimPrim "drop__") _) [x,y] _) = isAtomic y
isAtomic e = isFullyConst e


-- | whether an expression is small enough that it can be duplicated without code size growing too much. (work may be repeated)
isSmall e | isAtomic e = True
isSmall ELit {} = True
isSmall EPrim {} = True
isSmall EError {} = True
isSmall e | (EVar _,xs) <- fromAp e = length xs <= 4
isSmall _ = False

-- | whether an expression may be duplicated or pushed inside a lambda without duplicating too much work

isCheap :: E -> Bool
isCheap x | isAtomic x = True
isCheap EError {} = True
isCheap ELit {} = True
isCheap EPi {} = True
isCheap ELam {} = True -- should exclude values dropped at compile time
isCheap (EPrim p _ _) = aprimIsCheap p
isCheap ec@ECase {} = isCheap (eCaseScrutinee ec) && all isCheap (caseBodies ec)
isCheap e | (EVar v,xs) <- fromAp e, Just (Arity n b) <- Info.lookup (tvrInfo v) =
        (length xs < n)  -- Partial applications are cheap
          || (b && length xs >= n) -- bottoming out routines are cheap
isCheap _ = False


-- | determine if term can contain _|_
isLifted :: E -> Bool
isLifted x = sortTermLike x && not (isUnboxed (getType x))

-- Note: This does not treat lambdas as whnf
whnfOrBot :: E -> Bool
whnfOrBot (EError {}) = True
whnfOrBot (ELit (LitCons _ xs _)) = all isAtomic xs
whnfOrBot (EPi (TVr { tvrIdent =  j, tvrType =  x }) y) | not (j `Set.member` freeVars y) = isAtomic x && isAtomic y
whnfOrBot ELam {} = True
whnfOrBot e | isAtomic e = True
whnfOrBot e | (EVar v,xs) <- fromAp e, Just (Arity n True) <- Info.lookup (tvrInfo v), length xs >= n = True
whnfOrBot _ = False

-- Determine if a type represents an unboxed value
isUnboxed :: E -> Bool
isUnboxed e@EPi {} = False
isUnboxed e = getType e == eHash

safeToDup ec@ECase {}
    | EVar _ <- eCaseScrutinee ec = all safeToDup (caseBodies ec)
    | EPrim p _ _ <- eCaseScrutinee ec, aprimIsCheap p = all safeToDup (caseBodies ec)
safeToDup (EPrim p _ _) = aprimIsCheap p
safeToDup e = whnfOrBot e || isELam e || isEPi e

