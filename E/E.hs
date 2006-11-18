{-# OPTIONS -fglasgow-exts #-}
module E.E where

import GenUtil
import Control.Monad.Identity
import Monad
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
import Name.Id
import Util.SetLike as S


--------------------------------------
-- Lambda Cube (it's just fun to say.)
-- We are now based on a PTS, which is
-- a generalization of the lambda cube
-- see E.TypeCheck for a description
-- of the type system.
--------------------------------------


data Lit e t = LitInt { litNumber :: Number, litType :: t }
    | LitCons  { litName :: Name, litArgs :: [e], litType :: t, litAliasFor :: Maybe E }
--    | LitAlias { litName :: Name, litArgs :: [e], litType :: t, litAliasFor :: Maybe E }
    deriving(Eq,Ord)
        {-!derive: is, GhcBinary !-}

litCons = LitCons { litName = error "litName: name not set", litArgs = [], litType = error "litCons: type not set", litAliasFor = Nothing }

instance (Show e,Show t) => Show (Lit e t) where
    showsPrec p (LitInt x t) = showParen (p > 10) $  shows x <> showString "::" <> shows t
    showsPrec p LitCons { litName = n, litArgs = es, litType = t } = showParen (p > 10) $ hsep (shows n:map (showsPrec 11) es) <> showString "::" <> shows t

instance Functor (Lit e) where
    fmap f x = runIdentity $ fmapM (return . f) x

instance FunctorM (Lit e) where
    fmapM f x = case x of
        LitCons { litName = a, litArgs = es, litType = e, litAliasFor = af } -> do  e <- f e; return LitCons { litName = a, litArgs = es, litType = e, litAliasFor = af }
        LitInt i t -> do t <- f t; return $ LitInt i t


data ESort =
    EStar     -- ^ the sort of types
    | EHash   -- ^ the sort of unboxed types
    | EBox    -- ^ the sort of types of types
    deriving(Eq, Ord)
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
    | ELetRec { eDefs :: [(TVr, E)], eBody :: E }
    | EPrim APrim [E] E
    | EError String E
    | ECase {
       eCaseScrutinee :: E,
       eCaseType :: E, -- due to GADTs and typecases, the final type of the expression might not be so obvious, so we include it here.
       eCaseBind :: TVr,
       eCaseAlts :: [Alt E],
       eCaseDefault :: (Maybe E)
       }
	deriving(Eq, Ord, Show)
    {-! derive: is, from, GhcBinary !-}



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
    showsPrec n TVr { tvrIdent = 0, tvrType = e} = showParen (n > 10) $ showString "_::" . shows e
    showsPrec n TVr { tvrIdent = x, tvrType = e} = showParen (n > 10) $ case fromId x of
        Just n -> shows n . showString "::" . shows e
        Nothing  -> shows x . showString "::" . shows e



instance FunctorM TVr' where
    fmapM f t = do e <- f (tvrType t); return t { tvrType = e }
instance Functor TVr' where
    fmap f t = runIdentity (fmapM (return . f) t)

instance Show e => Show (Alt e) where
    showsPrec n (Alt l e) = showParen (n > 10) $ shows l . showString " -> " . shows e


data Alt e = Alt (Lit TVr e) e
    deriving(Eq,Ord)
       {-!derive: GhcBinary !-}

altHead :: Alt E -> Lit () ()
altHead (Alt l _) = litHead  l
litHead :: Lit a b -> Lit () ()
litHead (LitInt x _) = LitInt x ()
litHead LitCons { litName = s, litAliasFor = af } = litCons { litName = s, litType = (), litAliasFor = af }

litBinds (LitCons { litArgs = xs } ) = xs
litBinds _ = []

patToLitEE LitCons { litName = n, litArgs = [a,b], litType = t } | t == eStar, n == tc_Arrow = EPi (tVr 0 (EVar a)) (EVar b)
patToLitEE LitCons { litName = n, litArgs = xs, litType = t, litAliasFor = af } = ELit $ LitCons { litName = n, litArgs = (map EVar xs), litType = t, litAliasFor = af }
patToLitEE (LitInt x t) = ELit $ LitInt x t


caseBodies :: E -> [E]
caseBodies ec = [ b | Alt _ b <- eCaseAlts ec] ++ maybeToMonad (eCaseDefault ec)
casePats ec =  [ p | Alt p _ <- eCaseAlts ec]
caseBinds ec = eCaseBind ec : concat [ xs  | LitCons { litArgs = xs } <- casePats ec]


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
isWHNF ELetRec { eBody = e } = isWHNF e
isWHNF _ = False


-----------
-- E values
-----------

instance TypeNames E where
    tStar = eStar
    tInt = ELit (litCons { litName = tInt, litArgs = [], litType = eStar })
    tRational = ELit (litCons { litName = tc_Ratio, litArgs = [tInteger], litType = eStar })
    tChar = ELit (litCons { litName = tChar, litArgs = [], litType = eStar })
    tBool = ELit (litCons { litName = tBool, litArgs = [], litType = eStar })
    tUnit = ELit (litCons { litName = tUnit, litArgs = [], litType = eStar })
    tString =  (ELit (litCons { litName = tc_List, litArgs = [tChar], litType = eStar }))
    tInteger = ELit (litCons { litName = tInteger, litArgs = [], litType = eStar })
    tWorld__ = ELit (litCons { litName = tWorld__, litArgs = [], litType = eHash })
    tIntzh = ELit (litCons { litName = tIntzh, litArgs = [], litType = eHash })
    tIntegerzh = ELit (litCons { litName = tIntegerzh, litArgs = [], litType = eHash })
    tCharzh = ELit (litCons { litName = tCharzh, litArgs = [], litType = eHash })

instance ConNames E where
    vTrue = ELit vTrue
    vFalse = ELit vFalse
    vUnit  = ELit vUnit

instance ConNames (Lit E E) where
    vTrue  = (litCons { litName = dc_Boolzh, litArgs = [ELit (LitInt 1 tIntzh)], litType = tBool })
    vFalse = (litCons { litName = dc_Boolzh, litArgs = [ELit (LitInt 0 tIntzh)], litType = tBool })
    vUnit  = (litCons { litName = vUnit, litArgs = [], litType = tUnit })


tBox = ELit (litCons { litName = tc_Box, litArgs = [], litType = eStar })

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
tvrName (TVr {tvrIdent =  n }) | Just a <- fromId n = return a
tvrName tvr = fail $ "TVr is not Name: " ++ show tvr

tvrShowName :: TVr -> String
tvrShowName t = maybe ('x':(show $ tvrIdent t)) show (tvrName t)






isBottom EError {} = True
isBottom _ = False


caseBodiesMapM :: Monad m => (E -> m E) -> E -> m E
caseBodiesMapM f ec@ECase { eCaseAlts = as, eCaseDefault = d } = do
    let g (Alt l e) = f e >>= return . Alt l
    as' <- mapM g as
    d' <- fmapM f d
    return $ ec { eCaseAlts = as', eCaseDefault = d' }
caseBodiesMapM _ _ = error "caseBodiesMapM"

toList :: Monad m => E -> m  [E]
toList (ELit LitCons { litName = n, litArgs = [e,b] }) | vCons == n = toList b >>= \x -> return (e:x)
toList (ELit LitCons { litName = n, litArgs = [] }) | vEmptyList == n = return []
toList _ = fail "toList: not list"

toString x = toList x >>= mapM fromChar where
    fromChar (ELit (LitCons { litName = dc, litArgs = [ELit (LitInt ch t)], litType = _ot })) | dc == dc_Char && t == tCharzh = return (chr $ fromIntegral ch)
    fromChar _ = fail "fromChar: not char"


tAbsurd k = ELit (litCons { litName = tc_Absurd, litArgs = [], litType = k })


ltTuple ts = ELit $ litCons { litName = nameTuple TypeConstructor (length ts), litArgs = ts, litType = eStar }
ltTuple' ts = ELit $ litCons { litName = unboxedNameTuple TypeConstructor (length ts), litArgs = ts, litType = eHash }

p_unsafeCoerce = primPrim "unsafeCoerce"
p_toTag = primPrim "toTag"
p_fromTag = primPrim "fromTag"


