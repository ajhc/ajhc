{-------------------------------------------------------------------------------

        Copyright:              Mark Jones and The Hatchet Team 
                                (see file Contributors)

        Module:                 Type

        Description:            Manipulation of types

                                The main tasks implemented by this module are:
                                        - type substitution
                                        - type unification
                                        - type matching
                                        - type quantification

        Primary Authors:        Mark Jones and Bernie Pope

        Notes:                  See the file License for license information

                                Large parts of this module were derived from
                                the work of Mark Jones' "Typing Haskell in
                                Haskell", (http://www.cse.ogi.edu/~mpj/thih/)

-------------------------------------------------------------------------------}

module Type (kind,
             nullSubst,
             (@@),
             Types (..),
             (+->),
             merge,
             mgu,
             match,
             quantify,
             unQuantify,
             toScheme,
             makeAssump,
             assumpScheme,
             assumpToPair,
             pairToAssump,
             assumpId,
             tTTuple,
             Instantiate (..)
             ) where 

import HsSyn   (HsName (..))
import List    (union, nub)
import Data.FiniteMap
import FrontEnd.Env
import Representation
import Monad   (foldM)
import VConsts


--------------------------------------------------------------------------------

class Types t where
  apply :: Subst -> t -> t
  tv    :: t -> [Tyvar]

class Instantiate t where
  inst  :: [Type] -> t -> t

instance Instantiate Type where
  inst ts (TAp l r)     = TAp (inst ts l) (inst ts r)
  inst ts (TArrow l r)  = TArrow (inst ts l) (inst ts r)
--  inst ts (TTuple args) = TTuple $ map (inst ts) args
  inst ts t@(TGen n _)  | n < length ts = ts !! n
                        | otherwise = error $ "inst TGen " ++ show (ts,t)
  inst ts t             = t

instance Instantiate a => Instantiate [a] where
  inst ts = map (inst ts)

instance Instantiate t => Instantiate (Qual t) where
  inst ts (ps :=> t) = inst ts ps :=> inst ts t

instance Instantiate Pred where
  inst ts (IsIn c t) = IsIn c (inst ts t)

class HasKind t where
  kind :: t -> Kind
instance HasKind Tyvar where
  kind (Tyvar _ _ k) = k
instance HasKind Tycon where
  kind (Tycon v k) = k
instance HasKind Type where
  kind (TCon tc) = kind tc
  kind (TVar u)  = kind u
  kind (TAp t _) = case (kind t) of
                     (Kfun _ k) -> k
  kind (TArrow _l _r) = Star
--  kind (TTuple _args) = Star
  kind (TGen _ tv) = kind tv
  --kind x = error $ "Type:kind: " ++ show x

-----------------------------------------------------------------------------

instance Types t => Types (Qual t) where
  apply s (ps :=> t) = apply s ps :=> apply s t
  tv (ps :=> t)      = tv ps `union` tv t

instance Types Pred where
  apply s (IsIn c t) = IsIn c (apply s t)
  tv (IsIn c t)      = tv t

--------------------------------------------------------------------------------

-- substitutions

nullSubst  :: Subst
nullSubst   = zeroFM 

(+->)      :: Tyvar -> Type -> Subst
Tyvar u _ _ +-> t     = unitFM u t

instance Types Type where
  
  -- attempting to cache successful substitutions doesn't
  -- seem to make much difference, as the variables are 
  -- mostly independent

--  apply s (TVar var@(Tyvar name _kind)) 
--     = case lookupSubstitutionMap s name of
--          Just t  -> t
--          Nothing -> TVar var 
  apply s x@(TVar (Tyvar var _ _)) 
     = case lookupFM s var of
          Just t  -> t
          Nothing -> x
  apply s (TAp l r)     = TAp (apply s l) (apply s r)
  apply s (TArrow l r)  = TArrow (apply s l) (apply s r)
--  apply s (TTuple args) = TTuple $ map (apply s) args 
  apply _ t         = t

  tv (TVar u)      = [u]
  tv (TAp l r)     = tv l `union` tv r
  tv (TArrow l r)  = tv l `union` tv r 
  -- tv (TTuple args) = concatMap tv args 
--  tv (TTuple args) = foldl union [] $ map tv args 
  tv _             = []

instance Types a => Types [a] where
  apply s = map (apply s)              -- it may be worth using a cached version of apply in this circumstance? 
  tv      = nub . concat . map tv

infixr 4 @@
(@@)       :: Subst -> Subst -> Subst
s1 @@ s2 
   =(joinFM s1OverS2 s1)
   where
   s1OverS2 = mapSubstitution s1 s2 

merge      :: Monad m => Subst -> Subst -> m Subst
merge s1 s2 = if agree then return s else fail $ "merge: substitutions don't agree" 
 where
 s = joinFM s1 s2
 agree = all (\v -> lookupFM s1 (v) == lookupFM s2 (v)) $ map fst $ toListFM $ s1 `intersectFM` s2
-- agree = all (\v -> apply s1 (TVar v) == apply s2 (TVar v)) $ map fst $ toListFM $ s1 `intersectFM` s2

-- highly specialised version of lookupFM for
-- a Substitution. It is worth specialising this as it is called
-- frequently during a call to apply
-- according to profiling almost half of the computation time
-- is spent here

{-
lookupSubstitutionMap :: FiniteMap Tyvar Type -> HsName -> Maybe Type
lookupSubstitutionMap (Node (Tyvar k _kind) e _ sm gr) k'
   | k' <  k    = lookupSubstitutionMap sm k' 
   | k' >  k    = lookupSubstitutionMap gr k' 
   | otherwise  = Just e
lookupSubstitutionMap Leaf _
   = Nothing
-- specialised version of mapFM for substitutions

mapSubstitution :: Subst -> FiniteMap Tyvar Type -> FiniteMap Tyvar Type 
mapSubstitution s (Node k e n sm gr)  = Node k (apply s e) n (mapSubstitution s sm) (mapSubstitution s gr)
mapSubstitution s Leaf                = Leaf
-}


mapSubstitution s fm =(mapFM (\_ v -> apply s v) fm)

rnfFM fm = foldFM (\k e a -> k `seq` e `seq` a) () fm `seq` fm

--------------------------------------------------------------------------------

-- unification

mgu     :: Monad m => Type -> Type -> m Subst
varBind :: Monad m => Tyvar -> Type -> m Subst

mgu (TAp l r) (TAp l' r') 
   = do s1 <- mgu l l'
        s2 <- mgu (apply s1 r) (apply s1 r')
        return (s2 @@ s1)

mgu (TArrow l r) (TArrow l' r')
   = do s1 <- mgu l l' 
        s2 <- mgu (apply s1 r) (apply s1 r')
        return (s2 @@ s1)


-- DEPR TTuple
--mgu (TTuple args1) (TTuple args2)
--   = do let lenArgs1 = length args1
--        let lenArgs2 = length args2
--        -- check the dimensions of the tuples are the same
--        case lenArgs1 == lenArgs2 of
--           True  -> foldM (\oldSub (t1,t2) -> case mgu (apply oldSub t1) (apply oldSub t2) of
--                                                 Nothing      -> Nothing
--                                                 Just newSub  -> return (newSub @@ oldSub)) 
--                          nullSubst
--                          (zip args1 args2)
--           False -> Nothing 

mgu (TVar u) t        = varBind u t
mgu t (TVar u)        = varBind u t
mgu (TCon tc1) (TCon tc2)
           | tc1==tc2 = return nullSubst
           | otherwise = fail "mgu: Constructors don't match"
--mgu (TGen n tv) (TGen n' tv') | n == n' = varBind tv' (TVar tv)
mgu t1 t2  = fail "mgu: types do not unify"

varBind u t | t == TVar u      = return nullSubst
            | u `elem` tv t    = fail "varBind: occurs check fails"
            | kind u == kind t = return (u +-> t)
            | otherwise        = fail "varBind: kinds do not match"

match :: Monad m => Type -> Type -> m Subst

match (TAp l r) (TAp l' r') 
   = do sl <- match l l'
        sr <- match r r'
        merge sl sr

match (TArrow l r) (TArrow l' r') 
   = do sl <- match l l'
        sr <- match r r'
        merge sl sr


-- DEPR TTuple
--match (TTuple args1) (TTuple args2) 
--   = do let lenArgs1 = length args1
--        let lenArgs2 = length args2
--        -- check the dimensions of the tuples are the same
--        case lenArgs1 == lenArgs2 of
--           True  -> foldM (\oldSub (t1,t2) -> case match t1 t2 of
--                                                 Nothing      -> Nothing
--                                                 Just newSub  -> merge oldSub newSub)
--                          nullSubst
--                          (zip args1 args2)
--           False -> Nothing 

match (TVar u) t
   | kind u == kind t = return (u +-> t)

match (TCon tc1) (TCon tc2)
   | tc1==tc2         = return nullSubst

match t1 t2           = fail $ "match: " ++ show (t1,t2)

tTTuple ts | length ts < 2 = error "tTTuple"
tTTuple ts = foldl TAp (toTuple (length ts)) ts
-----------------------------------------------------------------------------

instance Types Scheme where
  apply s (Forall ks qt) = Forall ks (apply s qt)
  tv (Forall ks qt)      = tv qt

quantify      :: [Tyvar] -> Qual Type -> Scheme
quantify vs qt = Forall ks (apply s qt)
 where vs' = [ v | v <- tv qt, v `elem` vs ]
       ks  = map kind vs'
       s   = listToFM $ map (\(a@(Tyvar x _ _),b) -> (x,b a)) $ zip vs' (map TGen [0..])

toScheme      :: Type -> Scheme
toScheme t     = Forall [] ([] :=> t)

unQuantify :: Scheme -> (Qual Type)
unQuantify (Forall _ (ps :=> t)) =  map uq' ps :=> uq t where
    uq (TAp a b) = TAp (uq a) (uq b)
    uq (TArrow a b) = TArrow (uq a) (uq b)
--    uq (TTuple ts) = TTuple (map uq ts)
    uq (TGen _ tv) = TVar tv
    uq x = x
    uq' (IsIn s t) = IsIn s (uq t)

-----------------------------------------------------------------------------

assumpToPair :: Assump -> (HsName, Scheme)
assumpToPair (n :>: s) = (n,s)

pairToAssump :: (HsName, Scheme) -> Assump
pairToAssump (n,s) = (n :>: s)

instance Types Assump where
  apply s (i :>: sc) = i :>: (apply s sc)
  tv (i :>: sc)      = tv sc


assumpId :: Assump -> HsName 
assumpId (id :>: _scheme) = id

assumpScheme :: Assump -> Scheme
assumpScheme (_id :>: scheme) = scheme 

makeAssump :: HsName -> Scheme -> Assump
makeAssump name scheme = name :>: scheme
