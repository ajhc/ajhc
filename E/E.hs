{-# OPTIONS -fglasgow-exts #-}
module E.E where

import GenUtil
import Control.Monad.Identity
import Monad
import Data.Generics
import Data.FunctorM
import Maybe
import List
import Doc.DocLike
import Name.VConsts
import Name.Name
import Name.Names
import Binary
import Atom
import C.Prims
import Char(chr)
import Data.Monoid
import Number
import {-# SOURCE #-} Info.Binary(putInfo,getInfo)
import qualified Info.Info as Info


--------------------------------------
-- Lambda Cube (it's just fun to say.)
--------------------------------------

type Id = Int

data Lit e t = LitInt Number t |  LitCons Name [e] t
    deriving(Data,Eq,Ord,Typeable)
        {-!derive: is, GhcBinary !-}


instance (Show e,Show t) => Show (Lit e t) where
    showsPrec _ (LitInt x t) = parens $  shows x <> showString "::" <> shows t
    showsPrec _ (LitCons n es t) = parens $  hsep (shows n:map shows es) <> showString "::" <> shows t

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
    deriving(Data,Eq, Ord, Typeable)
    {-! derive: is, GhcBinary !-}

instance Show ESort where
    showsPrec _ EStar = showString "*"
    showsPrec _ EHash = showString "#"
    showsPrec _ EBox = showString "BOX"

data E = EAp E E
    | ELam TVr E
    | EPi TVr E
    | EVar TVr
    | Unknown
    | ESort ESort
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
data TVr' e = TVr { tvrIdent :: !Id, tvrType :: e, tvrInfo :: Info.Info }
    deriving(Data,Typeable)
    {-! derive: update !-}

tVr x y = tvr { tvrIdent = x, tvrType = y }
tvr = TVr { tvrIdent = 0, tvrType = Unknown, tvrInfo = mempty }

data TvrBinary = TvrBinaryNone | TvrBinaryAtom Atom | TvrBinaryInt Int
    {-! derive: GhcBinary !-}

instance Binary TVr where
    put_ bh (TVr { tvrIdent = 0, tvrType =  e, tvrInfo = nf} ) = do
        put_ bh (TvrBinaryNone)
        put_ bh e
        putInfo bh nf
    put_ bh (TVr { tvrIdent = i, tvrType =  e, tvrInfo = nf}) | Just x <- intToAtom i = do
        put_ bh (TvrBinaryAtom x)
        put_ bh e
        putInfo bh nf
    put_ bh (TVr { tvrIdent = i, tvrType =  e, tvrInfo = nf}) = do
        unless (even i) $ fail "number not even"
        put_ bh (TvrBinaryInt i)
        put_ bh e
        putInfo bh nf
    get bh = do
        (x ) <- get bh
        e <- get bh
        nf <- getInfo bh
        case x of
            TvrBinaryNone -> return $ TVr 0 e nf
            TvrBinaryAtom a -> return $ TVr (atomIndex a) e nf
            TvrBinaryInt i -> return $ TVr (i) e nf


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

patToLitEE (LitCons n [a,b] t) | t == eStar, n == tc_Arrow = EPi (tVr 0 (EVar a)) (EVar b)
patToLitEE (LitCons n xs t) = ELit $ LitCons n (map EVar xs) t
patToLitEE (LitInt x t) = ELit $ LitInt x t


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
    tStar = eStar
    tInt = ELit (LitCons tInt [] eStar)
    tRational = ELit (LitCons tc_Ratio [tInteger] eStar)
    tChar = ELit (LitCons tChar [] eStar)
    tBool = ELit (LitCons tBool [] eStar)
    tUnit = ELit (LitCons tUnit [] eStar)
    tString =  (ELit (LitCons tc_List [tChar] eStar))
    tInteger = ELit (LitCons tInteger [] eStar)
    tWorld__ = ELit (LitCons tWorld__ [] eHash)
    tIntzh = ELit (LitCons tIntzh [] eHash)
    tIntegerzh = ELit (LitCons tIntegerzh [] eHash)
    tCharzh = ELit (LitCons tCharzh [] eHash)

instance ConNames E where
    vTrue = ELit vTrue
    vFalse = ELit vFalse
    vUnit  = ELit vUnit

instance ConNames (Lit x E) where
    vTrue  = (LitCons vTrue [] tBool)
    vFalse = (LitCons vFalse [] tBool)
    vUnit  = (LitCons vUnit [] tUnit)



tFunc a b = ePi (tVr 0 a) b

-- values



tvrSilly = tVr ((-1)) Unknown

-----------------
-- E constructors
-----------------


eBox :: E
eBox = ESort EBox

eStar :: E
eStar = ESort EStar

eHash :: E
eHash = ESort EHash


sortLetDecls ds = sortBy f ds where
    f (TVr { tvrIdent = i },_) (TVr { tvrIdent = j } ,_) = compare (maybe (show i) show $ fromId i) (maybe (show j) show $ fromId j)

ePi a b = EPi a b

eLam v (EError s t) = EError s (ePi v t)
eLam v t = ELam v t


-- | throw away first n EPi terms
discardArgs :: Int -> E -> E
discardArgs 0 e = e
discardArgs n (EPi _ b) | n > 0 = discardArgs (n - 1) b
discardArgs _ _ = error "discardArgs"


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


tAbsurd k = ELit (LitCons tc_Absurd [] k)
tPtr t = ELit (LitCons tc_Ptr [t] eStar)


p_unsafeCoerce = primPrim "unsafeCoerce"
p_integralCast = primPrim "integralCast"

