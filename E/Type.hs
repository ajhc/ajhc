module E.Type where

import Data.FunctorM
import Control.Monad.Identity


import Atom
import Binary
import C.Prims
import Data.Typeable
import Doc.DocLike
import Name.Id
import Name.Name
import Number
import qualified Info.Info as Info
import {-# SOURCE #-} Info.Binary(putInfo,getInfo)

data RuleType = RuleSpecialization | RuleUser | RuleCatalyst
    deriving(Eq)
 {-! derive: GhcBinary !-}

-- a rule in its user visible form

data Rule = Rule {
    ruleHead :: TVr,
    ruleBinds :: [TVr],
    ruleArgs :: [E],
    ruleNArgs :: {-# UNPACK #-} !Int,
    ruleBody :: E,
    ruleType :: RuleType,
    ruleUniq :: (Module,Int),
    ruleName :: Atom
    }
 {-! derive: GhcBinary !-}

data ARules = ARules {
    aruleFreeVars :: IdSet,
    aruleRules :: [Rule]
    }
    deriving(Typeable)

data Lit e t = LitInt { litNumber :: Number, litType :: t }
    | LitCons  { litName :: Name, litArgs :: [e], litType :: t, litAliasFor :: Maybe E }
    deriving(Eq,Ord)
        {-!derive: is, GhcBinary !-}


--------------------------------------
-- Lambda Cube (it's just fun to say.)
-- We are now based on a PTS, which is
-- a generalization of the lambda cube
-- see E.TypeCheck for a description
-- of the type system.
--------------------------------------

data ESort =
    EStar         -- ^ the sort of boxed lazy types
    | EBang       -- ^ the sort of boxed strict types
    | EHash       -- ^ the sort of unboxed types
    | ETuple      -- ^ the sort of unboxed tuples
    | EHashHash   -- ^ the supersort of unboxed types
    | EStarStar   -- ^ the supersort of boxed types
    | ESortNamed Name -- ^ user defined sorts
    deriving(Eq, Ord)
    {-! derive: is, GhcBinary !-}


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



instance Functor (Lit e) where
    fmap f x = runIdentity $ fmapM (return . f) x

instance FunctorM (Lit e) where
    fmapM f x = case x of
        LitCons { litName = a, litArgs = es, litType = e, litAliasFor = af } -> do  e <- f e; return LitCons { litName = a, litArgs = es, litType = e, litAliasFor = af }
        LitInt i t -> do t <- f t; return $ LitInt i t

instance Show ESort where
    showsPrec _ EStar = showString "*"
    showsPrec _ EHash = showString "#"
    showsPrec _ EStarStar = showString "**"
    showsPrec _ EHashHash = showString "##"
    showsPrec _ ETuple = showString "(#)"
    showsPrec _ EBang = showString "!"

instance (Show e,Show t) => Show (Lit e t) where
    showsPrec p (LitInt x t) = showParen (p > 10) $  shows x <> showString "::" <> shows t
    showsPrec p LitCons { litName = n, litArgs = es, litType = t } = showParen (p > 10) $ hsep (shows n:map (showsPrec 11) es) <> showString "::" <> shows t

instance Show a => Show (TVr' a) where
    showsPrec n TVr { tvrIdent = 0, tvrType = e} = showParen (n > 10) $ showString "_::" . shows e
    showsPrec n TVr { tvrIdent = x, tvrType = e} = showParen (n > 10) $ case fromId x of
        Just n -> shows n . showString "::" . shows e
        Nothing  -> shows x . showString "::" . shows e


type TVr = TVr' E
data TVr' e = TVr { tvrIdent :: !Id, tvrType :: e, tvrInfo :: Info.Info }
    {-! derive: update !-}

data Alt e = Alt (Lit TVr e) e
    deriving(Eq,Ord)
       {-!derive: GhcBinary !-}

instance FunctorM TVr' where
    fmapM f t = do e <- f (tvrType t); return t { tvrType = e }
instance Functor TVr' where
    fmap f t = runIdentity (fmapM (return . f) t)

instance Show e => Show (Alt e) where
    showsPrec n (Alt l e) = showParen (n > 10) $ shows l . showString " -> " . shows e


instance Eq TVr where
    (==) (TVr { tvrIdent = i }) (TVr { tvrIdent = i' }) = i == i'
    (/=) (TVr { tvrIdent = i }) (TVr { tvrIdent = i' }) = i /= i'

instance Ord TVr where
    compare (TVr { tvrIdent = x }) (TVr { tvrIdent = y }) = compare x y
    x < y = tvrIdent x < tvrIdent y
    x > y = tvrIdent x > tvrIdent y
    x >= y = tvrIdent x >= tvrIdent y
    x <= y = tvrIdent x <= tvrIdent y

-- Binary instance
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

