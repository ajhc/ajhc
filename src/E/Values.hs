module E.Values where

import Control.Monad.Identity
import Data.Monoid
import List
import Ratio

import C.Prims
import E.E
import E.FreeVars()
import E.Subst
import E.TypeCheck
import Info.Info(HasInfo(..))
import Info.Types
import Name.Id
import Name.Name
import Name.Names
import Name.VConsts
import Support.CanType
import Support.FreeVars
import Support.Tuple
import Util.SetLike
import qualified Info.Info as Info

instance Tuple E where
    tupleNil = vUnit
    tupleMany es = ELit litCons { litName = nameTuple DataConstructor (length es), litArgs = es, litType = ltTuple ts } where
        ts = map getType es

eTuple :: [E] -> E
eTuple = tuple

eTuple' es = ELit $ unboxedTuple es

unboxedTuple es =  litCons { litName = unboxedNameTuple DataConstructor (length es), litArgs = es, litType = ltTuple' ts } where
    ts = map getType es

unboxedUnit :: E
unboxedUnit =  ELit $ unboxedTuple []

unboxedTyUnit :: E
unboxedTyUnit = ltTuple' []

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
    toE ch = ELit (litCons { litName = dc_Char, litArgs = [toEzh ch], litType = tChar })
    typeE _ = tChar

instance ToE Rational where
    toE rat = ELit (litCons { litName = dc_Ratio, litArgs = [toE (numerator rat), toE (denominator rat)], litType = tRational })
    typeE _ = tRational

instance ToE Integer where
    toE ch = ELit (litCons { litName = dc_Integer, litArgs = [toEzh ch], litType = tInteger })
    typeE _ = tInteger

instance ToE Int where
    toE ch = ELit (litCons { litName = dc_Int, litArgs = [toEzh ch], litType = tInt })
    typeE _ = tInt

instance ToE a => ToE [a] where
    toE xs@[] = eNil (typeE xs)
    toE (x:xs) = eCons (toE x) (toE xs)
    typeE (_::[a]) = ELit (litCons { litName = tc_List, litArgs = [typeE (undefined::a)], litType = eStar })

--eInt x = ELit $ LitInt x tInt

eCons x xs = ELit $ litCons { litName = dc_Cons, litArgs = [x,xs], litType = getType xs }
eNil t = ELit $ litCons { litName = dc_EmptyList, litArgs = [], litType = t }

emptyCase = ECase {
    eCaseAllFV = mempty,
    eCaseDefault = Nothing,
    eCaseAlts = [],
    eCaseBind = error "emptyCase: bind",
    eCaseType = error "emptyCase: type",
    eCaseScrutinee = error "emptyCase: scrutinee"
    }

eCaseTup e vs w = caseUpdate emptyCase { eCaseScrutinee = e, eCaseBind =  (tVr emptyId (getType e)), eCaseType = getType w, eCaseAlts =  [Alt litCons { litName = nameTuple DataConstructor (length vs), litArgs = vs, litType = getType e } w] }
eCaseTup' e vs w = caseUpdate emptyCase { eCaseScrutinee = e, eCaseBind = (tVr emptyId (getType e)), eCaseType = getType w, eCaseAlts =  [Alt litCons { litName = unboxedNameTuple DataConstructor (length vs), litArgs = vs, litType = getType e} w] }

eJustIO w x = eTuple' [w,x] -- ELit litCons { litName = dc_JustIO, litArgs = [w,x], litType = ELit litCons { litName = tc_IOResult, litArgs = [getType x], litType = eStar } }
tIO t = ELit (litCons { litName = tc_IO, litArgs = [t], litType = eStar })

eCase e alts@(alt:_) Unknown = caseUpdate emptyCase { eCaseScrutinee = e, eCaseBind = (tVr emptyId (getType e)), eCaseType = getType alt,  eCaseAlts =  alts }
eCase e alts els = caseUpdate emptyCase { eCaseScrutinee = e, eCaseBind = (tVr emptyId (getType e)), eCaseDefault = Just els, eCaseAlts =  alts, eCaseType = getType els }

-- | This takes care of types right away, it simplifies various other things to do it this way.
eLet :: TVr -> E -> E -> E
eLet TVr { tvrIdent = eid } _ e' | eid == emptyId = e'
eLet t@(TVr { tvrType =  ty}) e e'
    | sortKindLike ty && isAtomic e = subst t e e'
    | sortKindLike ty = ELetRec [(t,e)] (typeSubst mempty (msingleton (tvrIdent t) e) e')
    | isUnboxed ty && isAtomic e = subst t e e'
    | isUnboxed ty  = eStrictLet t e e'
eLet t e e' = ELetRec [(t,e)] e'

-- | strict version of let, evaluates argument before assigning it.
eStrictLet t@(TVr { tvrType =  ty }) v e | sortKindLike ty  = eLet t v e
eStrictLet t v e = caseUpdate emptyCase { eCaseScrutinee = v, eCaseBind = t, eCaseDefault = Just e, eCaseType = getType e }

substLet :: [(TVr,E)] -> E -> E
substLet ds e  = ans where
    (as,nas) = partition (isAtomic . snd) (filter ((/= emptyId) . tvrIdent . fst) ds)
    tas = filter (sortKindLike . tvrType . fst) nas
    ans = eLetRec (as ++ nas) (typeSubst' (fromList [ (n,e) | (TVr { tvrIdent = n },e) <- as]) (fromList [ (n,e) | (TVr { tvrIdent = n },e) <- tas]) e)

substLet' :: [(TVr,E)] -> E -> E
substLet' ds' e  = ans where
    (hh,ds) = partition (isUnboxed . tvrType . fst) ds'
    nas = filter ((/= emptyId) . tvrIdent . fst) ds
    tas = filter (sortKindLike . tvrType . fst) nas
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

prim_seq a b | isWHNF a = b
prim_seq a b = caseUpdate emptyCase { eCaseScrutinee = a, eCaseBind =  (tVr emptyId (getType a)), eCaseDefault = Just b, eCaseType = getType b }

prim_unsafeCoerce e t = p e' where
    (_,e',p) = unsafeCoerceOpt $ EPrim p_unsafeCoerce [e] t
from_unsafeCoerce (EPrim pp [e] t) | pp == p_unsafeCoerce = return (e,t)
from_unsafeCoerce _ = fail "Not unsafeCoerce primitive"

--tWorldzh = ELit litCons { litName = tc_World__, litArgs = [], litType = eHash }
isState_ e = case e of
    ELit (LitCons { litName = name }) | name == tc_State_ -> True
    _ -> False

unsafeCoerceOpt (EPrim uc [e] t) | uc == p_unsafeCoerce = f (0::Int) e t where
    f n e t | Just (e',_) <- from_unsafeCoerce e = f (n + 1) e' t
    f n (ELetRec ds e) t = (n + 1, ELetRec ds (p e'),id) where
        (n,e',p) = f n e t
    f n (EError err _) t = (n,EError err t,id)
    f n (ELit (LitInt x _)) t = (n,ELit (LitInt x t),id)
    f n (ELit lc@LitCons {}) t = (n,ELit lc { litType = t },id)
    f n ec@ECase {} t = (n,caseUpdate nx { eCaseType = t },id) where
        Identity nx = caseBodiesMapM (return . flip prim_unsafeCoerce t) ec
    f n e t | getType e == t = (n,e,id)
    f n e t = (n,e,\z -> EPrim p_unsafeCoerce [z] t)
unsafeCoerceOpt e = (0,e,id)

instance HasInfo TVr where
    getInfo = tvrInfo
    modifyInfo = tvrInfo_u

-- various routines used to classify expressions
-- many assume atomicity constraints are in place

-- | whether a value is a compile time constant
isFullyConst :: E -> Bool
isFullyConst (ELit LitCons { litArgs = [] }) = True
isFullyConst (ELit LitCons { litArgs = xs }) = all isFullyConst xs
isFullyConst ELit {} = True
isFullyConst (EPi (TVr { tvrType = t }) x) =  isFullyConst t && isFullyConst x
isFullyConst (EPrim (APrim p _) as _) = primIsConstant p && all isFullyConst as
isFullyConst _ = False

-- | whether a value may be used as an argument to an application, literal, or primitive
-- these may be duplicated with no code size or runtime penalty
isAtomic :: E -> Bool
isAtomic EVar {}  = True
isAtomic e | sortTypeLike e = True
isAtomic (EPrim don [x,y] _) | don == p_dependingOn = isAtomic x
isAtomic e = isFullyConst e

-- | whether a type is "obviously" atomic. fast and lazy, doesn't recurse
-- True -> definitely atomic
-- False -> maybe atomic
isManifestAtomic :: E -> Bool
isManifestAtomic EVar {}  = True
isManifestAtomic (ELit LitInt {})  = True
isManifestAtomic (ELit LitCons { litArgs = []})  = True
isManifestAtomic _ = False

-- | whether an expression is small enough that it can be duplicated without code size growing too much. (work may be repeated)
isSmall e | isAtomic e = True
isSmall ELit {} = True
isSmall EPrim {} = True
isSmall EError {} = True
isSmall e | (EVar _,xs) <- fromAp e = length xs <= 4
isSmall _ = False

-- | whether an expression may be duplicated or pushed inside a lambda without duplicating too much work

isCheap :: E -> Bool
isCheap EError {} = True
isCheap ELit {} = True
isCheap EPi {} = True
isCheap ELam {} = True -- should exclude values dropped at compile time
isCheap x | isAtomic x = True
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
whnfOrBot (ELit LitCons { litArgs = xs }) = all isAtomic xs
whnfOrBot (EPi (TVr { tvrIdent =  j, tvrType =  x }) y) | not (j `member` (freeVars y :: IdSet)) = isAtomic x && isAtomic y
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

eToPat e = f e where
    f (ELit LitCons { litAliasFor = af,  litName = x, litArgs = ts, litType = t }) = do
        ts <- mapM cv ts
        return litCons { litAliasFor = af, litName = x, litArgs = ts, litType = t }
    f (ELit (LitInt e t)) = return (LitInt e t)
    f (EPi (TVr { tvrType =  a}) b)  = do
        a <- cv a
        b <- cv b
        return litCons { litName = tc_Arrow, litArgs = [a,b], litType = eStar }
    f x = fail $ "E.Values.eToPat: " ++ show x
    cv (EVar v) = return v
    cv e = fail $ "E.Value.eToPat.cv: " ++ show e

patToE p = f p where
    f LitCons { litName = arr, litArgs = [a,b], litType = t} | t == eStar = return $ EPi tvr { tvrType = EVar a } (EVar b)
    f (LitCons { litAliasFor = af,  litName = x, litArgs = ts, litType = t }) = do
       return $  ELit litCons { litAliasFor = af, litName = x, litArgs = map EVar ts, litType = t }
    f (LitInt e t) = return $ ELit (LitInt e t)
