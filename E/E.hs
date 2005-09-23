{-# OPTIONS -fglasgow-exts #-}
module E.E where

import GenUtil
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
import Binary
import Atom
import C.Prims
import Char(chr)
import Data.Monoid
import Number
import qualified Info.Info as Info


--------------------------------------
-- Lambda Cube (it's just fun to say.)
--------------------------------------


data Lit e t = LitInt Number t |  LitCons Name [e] t
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



isAtomic :: E -> Bool
--isAtomic e | sortTypeLike e = True
isAtomic EVar {}  = True
isAtomic e = isFullyConst e

fullyConst :: Monad m => E -> m ()
fullyConst (ELit (LitCons _ [] _)) = return ()
fullyConst (ELit (LitCons _ xs _)) = mapM_ fullyConst xs
fullyConst ELit {} = return ()
fullyConst (EPi (TVr { tvrType = t }) x) = do
    fullyConst t
    fullyConst x
fullyConst _ = fail "not fully constant"

isFullyConst :: E -> Bool
isFullyConst = maybe False (const True) . fullyConst

isBottom EError {} = True
isBottom _ = False


caseBodiesMapM :: Monad m => (E -> m E) -> E -> m E
caseBodiesMapM f (ECase e b as d) = do
    let g (Alt l e) = f e >>= return . Alt l
    as' <- mapM g as
    d' <- fmapM f d
    return $ ECase e b as' d'
caseBodiesMapM _ _ = error "caseBodiesMapM"

toList :: Monad m => E -> m  [E]
toList (ELit (LitCons n [e,b] _)) | vCons == n = toList b >>= \x -> return (e:x)
toList (ELit (LitCons n [] _)) | vEmptyList == n = return []
toList _ = fail "toList: not list"

toString x = toList x >>= mapM fromChar where
    fromChar (ELit (LitCons dc [ELit (LitInt ch t)] _ot)) | dc == dc_Char && t == tCharzh = return (chr $ fromIntegral ch)
    fromChar _ = fail "fromChar: not char"

dc_Addr = toName DataConstructor ("Jhc.Addr","Addr")
dc_Char = toName DataConstructor ("Prelude","Char")
dc_JustIO = toName DataConstructor ("Jhc.IO", "JustIO")
dc_Rational = toName DataConstructor ("Ratio",":%")

