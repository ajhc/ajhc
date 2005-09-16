{-# OPTIONS -fglasgow-exts #-}
module E.E where

import GenUtil
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Set as Set
import Control.Monad.Identity
import Monad
import Data.Generics
import Data.FunctorM
import Maybe
import List
import DDataUtil()
import Doc.DocLike
import VConsts
import Name
import Data.Graph as G
import FreeVars
import Binary
import Atom
import CanType
import {-# SOURCE #-} E.Subst
import C.Prims
import Data.Monoid
import Number
import qualified Info


--------------------------------------
-- Lambda Cube (it's just fun to say.)
--------------------------------------


data Lit e t = LitInt Number t |  LitCons Name [e] t --  | LitFrac Rational t   LitInt !Integer t  |
	deriving(Data,Eq,Ord, Typeable)
        {-!derive: is, GhcBinary !-}


instance (Show e,Show t) => Show (Lit e t) where
    show (LitInt x _) = show x
    show (LitCons n es t) = parens $  hsep (show n:map show es) <> "::" <> show t

instance Functor (Lit e) where
    fmap f x = runIdentity $ fmapM (return . f) x

instance FunctorM (Lit e) where
    fmapM f x = case x of
        (LitCons a es e) -> do  e <- f e; return (LitCons a es e)
        LitInt i t -> do t <- f t; return $ LitInt i t


data ESort =
    EStar     -- ^ the sort of types
    | EHash   -- ^ the sort of unboxed types
    | EBox    -- ^ the sort of types of types

data E = EAp E E
    | ELam TVr E
    | EPi TVr E
    | EVar TVr
    | Unknown
    | ESort !Int
    | ELit !(Lit E E)
    | ELetRec [(TVr, E)] E
    | EPrim APrim [E] E
    | EError String E
    | ECase {
       eCaseScrutinee :: E,
       eCaseBind :: TVr,
       eCaseAlts :: [Alt E],
       eCaseDefault :: (Maybe E)
       }
	deriving(Data,Eq, Ord, Typeable, Show)
    {-! derive: is, from, GhcBinary !-}


data EBind = EBind [TVr] E
    deriving(Data,Eq,Typeable,Show)
    {-! derive: GhcBinary !-}


-- | extract out EAp nodes a value and the arguments it is applied to.
fromAp :: E -> (E,[E])
fromAp e = f [] e where
    f as (EAp e a) = f (a:as) e
    f as e  =  (e,as)

-- | deconstruct EPi terms, getting function argument types.

fromPi :: E -> (E,[TVr])
fromPi e = f [] e where
    f as (EPi v e) = f (v:as) e
    f as e  =  (e,reverse as)

-- | deconstruct ELam term.

fromLam :: E -> (E,[TVr])
fromLam e = f [] e where
    f as (ELam v e) = f (v:as) e
    f as e  =  (e,reverse as)

type TVr = TVr' E
data TVr' e = TVr { tvrIdent :: !Int, tvrType :: e, tvrInfo :: Info.Info }
    deriving(Data,Typeable)
    {-! derive: update !-}

tVr x y = tvr { tvrIdent = x, tvrType = y }
tvr = TVr { tvrIdent = 0, tvrType = Unknown, tvrInfo = mempty }

data TvrBinary = TvrBinaryNone | TvrBinaryAtom Atom | TvrBinaryInt Int
    {-! derive: GhcBinary !-}

instance Binary TVr where
    put_ bh (TVr 0 e _) = do
        put_ bh (TvrBinaryNone)
        put_ bh e
    put_ bh (TVr (i) e _) | Just x <- intToAtom i = do
        put_ bh (TvrBinaryAtom x)
        put_ bh e
    put_ bh (TVr (i) e _) = do
        put_ bh (TvrBinaryInt i)
        put_ bh e
    get bh = do
        (x ) <- get bh
        e <- get bh
        case x of
            TvrBinaryNone -> return $ TVr 0 e mempty
            TvrBinaryAtom a -> return $ TVr (atomIndex a) e mempty
            TvrBinaryInt i -> return $ TVr (i) e mempty

instance Show a => Show (TVr' a) where
    show TVr { tvrIdent = 0, tvrType = e} = "(_::" ++ show e ++ ")"
    show TVr { tvrIdent = (x), tvrType =  e} | Just n <- intToAtom x  = "(v" ++ show (fromAtom n::Name) ++ "::" ++ show e ++ ")"
    show TVr { tvrIdent = (x), tvrType = e}  = "(v" ++ show x ++ "::" ++ show e ++ ")"

tvrNum TVr { tvrIdent =  n } = n


instance FunctorM TVr' where
    fmapM f t = do e <- f (tvrType t); return t { tvrType = e }
instance Functor TVr' where
    fmap f t = runIdentity (fmapM (return . f) t)



data Alt e = Alt (Lit TVr e) e
    deriving(Data,Show,Eq,Ord,Typeable)
       {-!derive: GhcBinary !-}

altHead :: Alt E -> Lit () ()
altHead (Alt l _) = litHead  l
litHead :: Lit a b -> Lit () ()
litHead (LitInt x _) = LitInt x ()
litHead (LitCons s _ _) = LitCons s [] ()

litBinds ((LitCons _ xs _) ) = xs
litBinds _ = []

patToLitEE (LitCons n [a,b] t) | t == eStar, n == tArrow = EPi (tVr 0 (EVar a)) (EVar b)
patToLitEE (LitCons n xs t) = ELit $ LitCons n (map EVar xs) t
patToLitEE (LitInt x t) = ELit $ LitInt x t

tArrow = toName TypeConstructor ("Prelude","->")

caseBodies :: E -> [E]
caseBodies ec = [ b | Alt _ b <- eCaseAlts ec] ++ maybeToMonad (eCaseDefault ec)
casePats ec =  [ p | Alt p _ <- eCaseAlts ec]
caseBinds ec = eCaseBind ec : concat [ xs  | LitCons _ xs _ <- casePats ec]


instance Eq TVr where
    (==) (TVr { tvrIdent = i }) (TVr { tvrIdent = i' }) = i == i'
    (/=) (TVr { tvrIdent = i }) (TVr { tvrIdent = i' }) = i /= i'

instance Ord TVr where
    compare (TVr { tvrIdent = x }) (TVr { tvrIdent = y }) = compare x y
    x < y = tvrIdent x < tvrIdent y
    x > y = tvrIdent x > tvrIdent y
    x >= y = tvrIdent x >= tvrIdent y
    x <= y = tvrIdent x <= tvrIdent y


isWHNF ELit {} = True
isWHNF ELam {} = True
isWHNF EPi {} = True
isWHNF ESort {} = True
isWHNF (ELetRec _ e) = isWHNF e
isWHNF _ = False


-----------
-- E values
-----------

instance TypeNames E where
    tStar = ESort 0
    tInt = ELit (LitCons tInt [] eStar)
    tRational = ELit (LitCons (toName TypeConstructor ("Ratio","Ratio")) [tInteger] eStar)
    tChar = ELit (LitCons tChar [] eStar)
    tBool = ELit (LitCons tBool [] eStar)
    tUnit = ELit (LitCons tUnit [] eStar)
    tString =  (ELit (litCons TypeConstructor ("Prelude","[]") [tChar] eStar))
    tInteger = ELit (LitCons tInteger [] eStar)
    tWorld__ = ELit (LitCons tWorld__ [] eStar)
    tIntzh = ELit (LitCons tIntzh [] eStar)
    tIntegerzh = ELit (LitCons tIntegerzh [] eStar)
    tCharzh = ELit (LitCons tCharzh [] eStar)

instance ConNames E where
    vTrue = ELit vTrue
    vFalse = ELit vFalse
    vUnit  = ELit vUnit

instance ConNames (Lit x E) where
    vTrue  = (LitCons vTrue [] tBool)
    vFalse = (LitCons vFalse [] tBool)
    vUnit  = (LitCons vUnit [] tUnit)


vWorld__ = ELit (litCons DataConstructor ("Jhc.IO","World__") [] tWorld__)

-- types
etIO t = ELit (litCons TypeConstructor ("Prelude.IO","IO") [t] tStar)
--etWorld = ELit (litCons TypeConstructor ("Jhc.IO","World__") [] tStar)
--tRational = ELit (litCons TypeConstructor ("Prelude.Ratio","Rational") [] tStar)
tAbsurd k = ELit (litCons TypeConstructor "Absurd#" [] k)

tFunc a b = ePi (tVr 0 a) b

-- values



tvrSilly = tVr ((-1)) Unknown

-----------------
-- E constructors
-----------------

litCons t x y z = LitCons (toName t x) y z



eBox :: E
eBox = ESort 1

eStar :: E
eStar = ESort 0


sortLetDecls ds = sortBy f ds where
    f (TVr { tvrIdent = i },_) (TVr { tvrIdent = j } ,_) = compare i j

ePi a b = EPi a b

eLam v (EError s t) = EError s (ePi v t)
eLam v t = ELam v t


-- | throw away first n EPi terms
discardArgs :: Int -> E -> E
discardArgs 0 e = e
discardArgs n (EPi _ b) | n > 0 = discardArgs (n - 1) b
discardArgs _ _ = error "discardArgs"


-- | construct a letret, throwing away empty declarations
eLetRec :: [(TVr,E)] -> E -> E
eLetRec ds e = f (filter ((/= 0) . tvrNum . fst) ds) where
    f [] = e
    f ds = ELetRec ds e


tvrName :: Monad m => TVr  -> m Name
tvrName (TVr {tvrIdent =  n }) | Just a <- intToAtom n = return $ fromAtom a
tvrName tvr = fail $ "TVr is not Name: " ++ show tvr

tvrShowName :: TVr -> String
tvrShowName t = maybe ('x':(show $ tvrNum t)) show (tvrName t)


---------------------------
-- | compatable approximation
---------------------------

eCompat :: E -> E -> Bool
eCompat x y | x == y = True
eCompat (EAp e1 e2) (EAp ea eb) = eCompat e1 ea && eCompat e2 eb
eCompat (ELam (TVr { tvrType =  e1 }) e2) (ELam (TVr { tvrType =  ea }) eb) = eCompat e1 ea && eCompat e2 eb
eCompat (EPi (TVr { tvrType = e1 }) e2) (EPi (TVr { tvrType = ea }) eb) = eCompat e1 ea && eCompat e2 eb
eCompat (EVar _) _ = True
eCompat _ (EVar _) = True
eCompat (ELetRec _ e1) (ELetRec _ e2) = eCompat e1 e2
eCompat (ELit (LitCons n es t)) (ELit (LitCons n' es' t')) = n == n' && all (uncurry eCompat) (zip es es') && eCompat t t'
eCompat x y = x == y


-------------------------
-- finding free variables
-------------------------


instance FreeVars E IS.IntSet where
    freeVars e = IS.fromAscList (fsts . IM.toAscList $ freeVs e)
instance FreeVars E (Set.Set Int) where
    freeVars e = Set.fromAscList (fsts . IM.toAscList $ freeVs e)
instance FreeVars E [Int] where
    freeVars e =  IM.keys $ freeVs e
instance FreeVars E (IM.IntMap TVr) where
    freeVars = freeVs
instance FreeVars E (Set.Set TVr) where
    freeVars x = Set.fromList $ freeVars x
instance FreeVars E [TVr] where
    freeVars x = IM.elems $ freeVars x
instance FreeVars (Alt E) (IM.IntMap TVr) where
    freeVars as@(Alt l e) = IM.unions $ freeVars (getType l):(freeVars e IM.\\ IM.fromList [ (tvrNum t,t) | t <- litBinds l]):( map (freeVars . getType) $ litBinds l)
instance FreeVars E t => FreeVars TVr t where
    freeVars tvr = freeVars (getType tvr)
instance FreeVars (Alt E) (Set.Set Int) where
    freeVars as@(Alt l e) = Set.unions $ freeVars (getType l):(freeVars e Set.\\ Set.fromList [ tvrNum t | t <- litBinds l]):( map (freeVars . getType) $ litBinds l)


instance FreeVars E x => FreeVars (Lit TVr E) x where
    freeVars l =  mconcat $ freeVars (getType l):(map (freeVars . getType) $ litBinds l)



freeVs :: E -> IM.IntMap TVr
freeVs =   fv where
    (<>) = IM.union
    delete = IM.delete
    fv (EAp e1 e2) = fv e1 <> fv e2
    fv (EVar tvr@(TVr { tvrIdent =  ( i), tvrType =  t })) = IM.insert i tvr (fv t)
    fv (ELam (TVr { tvrIdent = i, tvrType = t}) e) =  (delete i $ fv e <> fv t)
    fv (EPi (TVr { tvrIdent =  i, tvrType = t}) e) =  (delete i $ fv e <> fv t)
    fv (ELetRec dl e) =  ((tl <> bl <> fv e) IM.\\ IM.fromList ll)  where
        (ll,tl,bl) = liftT3 (id,IM.unions,IM.unions) $ unzip3 $
            map (\(tvr@(TVr { tvrIdent = j, tvrType =  t}),y) -> ((j,tvr), fv t, fv y)) dl
    fv (EError _ e) = fv e
    fv (ELit l) = fvLit l
    fv (EPrim _ es e) = IM.unions $ fv e : map fv es
    fv (ECase e b as d) = IM.unions ( fv e:freeVars (getType $ b):(IM.delete (tvrNum b) $ IM.unions (freeVars d:map freeVars as)  ):[])
    fv Unknown = IM.empty
    fv ESort {} = IM.empty
    fvLit (LitCons _ es e) = IM.unions $ fv e:map fv es
    fvLit l = freeVs (getType l)



-- | separate out recursive strongly connected components from a declaration list

decomposeDefns :: [(TVr, E)] -> [Either (TVr, E) [(TVr,E)]]
decomposeDefns bs = map f mp where
    mp = G.stronglyConnComp [ (v,i,freeVars t `mappend` freeVars e) | v@(TVr i t _ ,e) <- bs]
    f (AcyclicSCC v) = Left v
    f (CyclicSCC vs) = Right vs


-- | pull apart an ELet and separate out recursive strongly connected components from an ELet.
decomposeLet :: E ->  ([Either (TVr, E) [(TVr,E)]],E)
decomposeLet (ELetRec ds e) = (decomposeDefns ds,e)
decomposeLet e = ([],e)


sortStarLike e = e /= eBox && typ e == eBox
sortTypeLike e = e /= eBox && not (sortStarLike e) && sortStarLike (typ e)
sortTermLike e = e /= eBox && not (sortStarLike e) && not (sortTypeLike e) && sortTypeLike (typ e)

-- Fast (and lazy, and perhaps unsafe) typeof
typ ::  E -> E
typ (ESort 0) =  eBox
typ (ESort 1) = error "Box inhabits nowhere."
typ (ESort _) = error "What sort of sort is this?"
typ (ELit l) = getType l
typ (EVar v) =  getType v
typ (EPi _ b) = typ b
typ (EAp a b) = eAp (typ a) b
typ (ELam (TVr { tvrIdent = x, tvrType =  a}) b) = EPi (tVr x a) (typ b)
typ (ELetRec _ e) = typ e
typ (ECase {eCaseScrutinee = e, eCaseDefault = Just d}) | sortTypeLike e = typ d
typ (ECase {eCaseAlts = (x:_)}) = getType x
typ (ECase {eCaseDefault = Just e}) = typ e
typ (ECase _ _ [] Nothing) = error "empty case"
typ (EError _ e) = e
typ (EPrim _ _ t) = t
typ Unknown = Unknown

instance CanType E E where
    getType = typ
instance CanType TVr E where
    getType = tvrType
instance CanType (Lit x t) t where
    getType (LitInt _ t) = t
    getType (LitCons _ _ t) = t
instance CanType e t => CanType (Alt e) t where
    getType (Alt _ e) = getType e


eAp (EPi (TVr { tvrIdent =  0 }) b) _ = b
eAp (EPi t b) e = subst t e b
--eAp (EPrim n es t@(EPi _ _)) b = EPrim n (es ++ [b]) (eAp t b)  -- only apply if type is pi-like
eAp (ELit (LitCons n es t)) b = (ELit (LitCons n (es ++ [b]) (eAp t b)))
eAp (EError s t) b = EError s (eAp t b)
eAp a b = EAp a b


