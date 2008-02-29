-- | The definitions related to jhc core

module E.Type where

import Maybe
import Control.Monad.Identity
import Data.Traversable
import Data.Foldable hiding(concat)
import Control.Applicative


import StringTable.Atom
import C.Prims
import Data.Typeable
import Doc.DocLike hiding((<$>))
import Name.Id
import Util.Gen
import Name.Name
import Name.Names
import Number
import Info.Types
import qualified Info.Info as Info

{-@Internals

# Jhc core normalized forms

Jhc core has a number of 'normalized forms' in which certain invarients are
met. many routines expect code to be in a certain form, and guarentee theier
output is also in a given form. The type system also can change with each form
by adding/removing terms from the PTS axioms and rules.

normalized form alpha
: There are basically no restrictions other than the code is typesafe, but
certain constructs that are checked by the type checker are okay when they
wouldn't otherwise be. In particular, 'newtype' casts still exist at the data
level.  'enum' scrutinizations are creations may be in terms of the virtual
constructors rather than the internal representations. let may bind unboxed
values, which is normaly not allowed.


normalized form beta
: This is like alpha except all data type constructors and case scrutinizations
are in their final form. As in, newtype coercions are removed, Enums are
desugared etc. also, 'let' bindings of unboxed values are translated to the
appropriate 'case' statements. The output of E.FromHs is in this form.

normalized form blue
: This is the form that most routines work on.

normalized form larry
: post lambda-lifting

normalized form mangled
: All polymorphism has been replaced with subtyping


-}

-- the type of a supercombinator
data Comb = Comb {
    combHead :: TVr,
    combBody :: E,
    combRules :: [Rule]
    }

instance HasProperties Comb where
    modifyProperties f comb = combHead_u (modifyProperties f) comb
    getProperties comb = getProperties $ combHead comb
    putProperties p comb = combHead_u (putProperties p) comb

instance HasProperties TVr where
    modifyProperties f = tvrInfo_u (modifyProperties f)
    getProperties = getProperties . tvrInfo
    putProperties prop =  tvrInfo_u (putProperties prop)

combBody_u f r@Comb{combBody  = x} = r{combBody = f x}
combHead_u f r@Comb{combHead  = x} = r{combHead = f x}
combRules_u f r@Comb{combRules  = x} = r{combRules = f x}
combBody_s v =  combBody_u  (const v)
combHead_s v =  combHead_u  (const v)
combRules_s v =  combRules_u  (const v)
emptyComb = Comb { combHead = tvr, combBody = Unknown, combRules = [] }
combIdent = tvrIdent . combHead
combArgs  = snd . fromLam . combBody
combABody = fst . fromLam . combBody
combTriple comb = (combHead comb,combArgs comb,combABody comb)
combTriple_s (t,as,e) comb = comb { combHead = t, combBody = Prelude.foldr ELam e as }

data RuleType = RuleSpecialization | RuleUser | RuleCatalyst
    deriving(Eq)

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

data ARules = ARules {
    aruleFreeVars :: IdSet,
    aruleRules :: [Rule]
    }
    deriving(Typeable)

data Lit e t = LitInt { litNumber :: Number, litType :: t }
    | LitCons  { litName :: Name, litArgs :: [e], litType :: t, litAliasFor :: Maybe E }
    deriving(Eq,Ord)
        {-!derive: is, Functor, Foldable, Traversable !-}


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
    {-! derive: is !-}


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
       eCaseDefault :: (Maybe E),
       eCaseAllFV  :: IdSet
       }
	deriving(Eq, Ord, Show)
    {-! derive: is, from !-}



--instance Functor (Lit e) where
--    fmap f x = runIdentity $ fmapM (return . f) x

--instance FunctorM (Lit e) where
--    fmapM f x = case x of
--        LitCons { litName = a, litArgs = es, litType = e, litAliasFor = af } -> do  e <- f e; return LitCons { litName = a, litArgs = es, litType = e, litAliasFor = af }
--        LitInt i t -> do t <- f t; return $ LitInt i t

instance Show ESort where
    showsPrec _ EStar = showString "*"
    showsPrec _ EHash = showString "#"
    showsPrec _ EStarStar = showString "**"
    showsPrec _ EHashHash = showString "##"
    showsPrec _ ETuple = showString "(#)"
    showsPrec _ EBang = showString "!"
    showsPrec _ (ESortNamed n) = shows n

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
        {-!derive: update, Functor, Foldable, Traversable !-}

data Alt e = Alt (Lit TVr e) e
    deriving(Eq,Ord)

--instance FunctorM TVr' where
--    fmapM f t = do e <- f (tvrType t); return t { tvrType = e }
--instance Functor TVr' where
--    fmap f t = runIdentity (fmapM (return . f) t)

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


-- simple querying routines
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


litCons = LitCons { litName = error "litName: name not set", litArgs = [], litType = error "litCons: type not set", litAliasFor = Nothing }

-----------------
-- E constructors
-----------------

eStar :: E
eStar = ESort EStar

eHash :: E
eHash = ESort EHash

tVr x y = tvr { tvrIdent = x, tvrType = y }
tvr = TVr { tvrIdent = 0, tvrType = Unknown, tvrInfo = Info.empty }



--  Imported from other files :-
