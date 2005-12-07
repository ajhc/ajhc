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
             schemeToType,
             Instantiate (..)
             ) where

import Control.Monad.Error
import Control.Monad.Trans
import Control.Monad.Writer
import Data.IORef
import List    (union, nub)
import qualified Data.Map as Map

import GenUtil
import Name.Name
import Name.VConsts
import Representation


--------------------------------------------------------------------------------

class Types t where
  apply :: Subst -> t -> t
  tv    :: t -> [Tyvar]

class Instantiate t where
  inst  :: [Type] -> t -> t

instance Instantiate Type where
  inst ts (TAp l r)     = TAp (inst ts l) (inst ts r)
  inst ts (TArrow l r)  = TArrow (inst ts l) (inst ts r)
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
  kind Tyvar { tyvarKind = k} = k
instance HasKind Tycon where
  kind (Tycon v k) = k
instance HasKind Type where
  kind (TCon tc) = kind tc
  kind (TVar u)  = kind u
  kind (TAp t _) = case (kind t) of
                     (Kfun _ k) -> k
                     x -> error $ "Type.kind: Invalid kind in type application for "++show t++": "++show x
  kind (TArrow _l _r) = Star
  kind (TGen _ tv) = kind tv
  kind (TForAll _ (_ :=> t)) = kind t
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
nullSubst   = Map.empty

(+->)      :: Tyvar -> Type -> Subst
u +-> t     = Map.singleton u t

instance Types Type where
  apply s x@(TVar var)
     = case Map.lookup var s of
          Just t  -> t
          Nothing -> x
  apply s (TAp l r)     = TAp (apply s l) (apply s r)
  apply s (TArrow l r)  = TArrow (apply s l) (apply s r)
  apply _ t         = t

  tv (TVar u)      = [u]
  tv (TAp l r)     = tv l `union` tv r
  tv (TArrow l r)  = tv l `union` tv r
  tv _             = []

instance Types a => Types [a] where
  apply s = map (apply s)              -- it may be worth using a cached version of apply in this circumstance?
  tv      = nub . concat . map tv

infixr 4 @@
(@@)       :: Subst -> Subst -> Subst
s1 @@ s2
   =(Map.union s1OverS2 s1)
   where
   s1OverS2 = mapSubstitution s1 s2

merge      :: Monad m => Subst -> Subst -> m Subst
merge s1 s2 = if agree then return s else fail $ "merge: substitutions don't agree"
 where
 s = Map.union s1 s2
 agree = all (\v -> (Map.lookup v s1 :: Maybe Type) == Map.lookup v s2 ) $ map fst $ Map.toList $ s1 `Map.intersection` s2
-- agree = all (\v -> apply s1 (TVar v) == apply s2 (TVar v)) $ map fst $ toListFM $ s1 `intersectFM` s2



mapSubstitution s fm =(Map.map (\v -> apply s v) fm)

--------------------------------------------------------------------------------

-- unification

mgu     :: MonadIO m => Type -> Type -> m (Maybe Subst)
varBind :: Monad m => Tyvar -> Type -> m Subst

mgu x y = do
    r <- runErrorT (mgu'' x y)
    case r of
        Right x -> return (Just x)
        Left (_::String) -> return Nothing


mgu'' x y = do
    x' <- findType x
    y' <- findType y
    mgu' x' y'

mgu' (TAp l r) (TAp l' r')
   = do s1 <- mgu'' l l'
        --s2 <- mgu'' (apply s1 r) (apply s1 r')
        s2 <- mgu'' r r'
        return (s2 @@ s1)

mgu' (TArrow l r) (TArrow l' r')
   = do s1 <- mgu'' l l'
        --s2 <- mgu'' (apply s1 r) (apply s1 r')
        s2 <- mgu'' r r'
        return (s2 @@ s1)

mgu' t@(TVar Tyvar { tyvarRef = Nothing }) (TVar u@Tyvar { tyvarRef = Just _ } )  = varBind' u t
mgu' (TVar u) t        = varBind' u t
mgu' t (TVar u)        = varBind' u t
mgu' (TCon tc1) (TCon tc2)
           | tc1==tc2 = return nullSubst
           | otherwise = fail "mgu: Constructors don't match"
--mgu (TGen n tv) (TGen n' tv') | n == n' = varBind tv' (TVar tv)
mgu' t1 t2  = fail "mgu: types do not unify"

varBind' u t | t == TVar u      = return nullSubst
            | u `elem` tv t    = fail "varBind: occurs check fails"
            | kind u == kind t, Just r <- tyvarRef u = do
                Nothing <- liftIO $ readIORef r
                liftIO $ writeIORef r (Just t)
                return (u +-> t)
            | otherwise        = fail "varBind: kinds do not match"


varBind u t | t == TVar u      = return nullSubst
            | u `elem` tv t    = fail "varBind: occurs check fails"
            | kind u == kind t = return (u +-> t)
            | otherwise        = fail "varBind: kinds do not match"

match :: Monad m => Type -> Type -> m Subst

match x y = do
    --x' <- findType x
    --y' <- findType y
    match' x y

match' (TAp l r) (TAp l' r')
   = do sl <- match l l'
        sr <- match r r'
        merge sl sr

match' (TArrow l r) (TArrow l' r')
   = do sl <- match l l'
        sr <- match r r'
        merge sl sr

match' (TVar u) t
   | kind u == kind t = return (u +-> t)

match' (TCon tc1) (TCon tc2)
   | tc1==tc2         = return nullSubst

match' t1 t2           = fail $ "match: " ++ show (t1,t2)

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
       s   = Map.fromList $ map (\(a,b) -> (a,b a)) $ zip vs' (map TGen [0..])

toScheme      :: Type -> Scheme
toScheme t     = Forall [] ([] :=> t)

unQuantify :: Scheme -> (Qual Type)
unQuantify (Forall _ (ps :=> t)) =  map uq' ps :=> uq t where
    uq (TAp a b) = TAp (uq a) (uq b)
    uq (TArrow a b) = TArrow (uq a) (uq b)
    uq (TGen _ tv) = TVar tv
    uq x = x
    uq' (IsIn s t) = IsIn s (uq t)

schemeToType :: Scheme -> Type
schemeToType (Forall _ (ps :=> t)) = tForAll ( snds $ snubFst xs) (ps' :=> t') where
    ((ps',t'),xs) = runWriter $ do
        ps' <- mapM uq' ps
        t' <- uq t
        return (ps',t')
    uq (TAp a b) = liftM2 TAp (uq a) (uq b)
    uq (TArrow a b) = liftM2 TArrow (uq a) (uq b)
    uq (TGen n tv) = do
        tell [(n,tv)]
        return $ TVar tv
    uq (TForAll xs (ps :=> t)) = do
        ps' <- mapM uq' ps
        t' <- uq t
        return $ tForAll xs (ps' :=> t')
    uq x = return x
    uq' (IsIn s t) = liftM (IsIn s) (uq t)

-----------------------------------------------------------------------------

assumpToPair :: Assump -> (Name, Scheme)
assumpToPair (n :>: s) = (n,s)

pairToAssump :: (Name, Scheme) -> Assump
pairToAssump (n,s) = (n :>: s)

instance Types Assump where
  apply s (i :>: sc) = i :>: (apply s sc)
  tv (i :>: sc)      = tv sc


assumpId :: Assump -> Name
assumpId (id :>: _scheme) = id

assumpScheme :: Assump -> Scheme
assumpScheme (_id :>: scheme) = scheme

makeAssump :: Name -> Scheme -> Assump
makeAssump name scheme = name :>: scheme
