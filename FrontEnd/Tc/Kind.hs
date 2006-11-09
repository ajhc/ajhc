module FrontEnd.Tc.Kind(
    Kind(..),
    KBase(..),
    Kindvar(..),
    KindConstraint(..),
    kindCombine,
    kindStar,
    kindUTuple,
    kindFunRet,
    unfoldKind
    ) where

import Data.Monoid
import Control.Monad
import Data.IORef

import Binary
import Doc.DocLike
import Doc.PPrint(pprint,PPrint)

{-

 KFunRet = ?
 KUTuple = (#)
 Star    = *
 Kfun    = (->)

 we have the following subkinding going on

   ?
  / \
 *  (#)


-}

data KBase = Star | KUTuple | KFunRet
    deriving(Eq, Ord)   -- but we need them for kind inference
    {-! derive: GhcBinary !-}

kindStar = KBase Star
kindUTuple = KBase KUTuple
kindFunRet = KBase KFunRet

data Kind  = KBase KBase
           | Kfun Kind Kind
           | KVar Kindvar               -- variables aren't really allowed in haskell in kinds
             deriving(Eq, Ord)   -- but we need them for kind inference
    {-! derive: GhcBinary !-}


kindCombine :: Monad m => Kind -> Kind -> m Kind
kindCombine x y = g x y where
    f Star Star = return Star
    f KUTuple KUTuple = return KUTuple
    f KFunRet KFunRet = return KFunRet

    f KFunRet Star = return Star
    f Star KFunRet = return Star
    f KFunRet KUTuple = return KUTuple
    f KUTuple KFunRet = return KUTuple
    f x y = fail $ "kindCombine: " ++ show (x,y)
    g (KBase x) (KBase y) = f x y >>= return . KBase
    g (Kfun a b) (Kfun a' b') = return Kfun `ap` g a a' `ap` g b b'
    g x y = fail $ "kindCombine: " ++ show (x,y)

data KindConstraint
    = KindSimple  -- ^ * | kindSimple -> kindSimple
    | KindFunRet  -- ^ ??, so * or (#) or ??
    | KindStar    -- ^ must be *
    | KindAny     -- ^ no constraints
    deriving(Eq,Ord,Show)

instance Monoid KindConstraint where
    mempty = KindAny
    mappend a b | a == b = a
    mappend KindAny k = k
    mappend KindStar _ = KindStar
    mappend KindSimple KindFunRet = KindStar
    mappend k1 k2 = mappend k2 k1

data Kindvar = Kindvar {
    kvarUniq :: !Int,
    kvarRef :: IORef (Maybe Kind),
    kvarConstraint :: KindConstraint
    }

instance Binary Kindvar where
    put_ _ _ = return ()
    get _ = return (error "Binary.Kindvar.get")

instance Eq Kindvar where
    a == b = kvarUniq a == kvarUniq b
instance Ord Kindvar where
    a `compare` b = kvarUniq a `compare` kvarUniq b

instance Show Kind where
    showsPrec _ k = pprint k

instance Show Kindvar where
    showsPrec _ k = pprint k


instance Show KBase where
    showsPrec _ Star = showString "*"
    showsPrec _ KUTuple = showString "(#)"
    showsPrec _ KFunRet = showString "?"

instance DocLike d => PPrint d KBase where
    pprint kb = text (show kb)

instance DocLike d => PPrint d Kind where
   pprint (KBase b) = pprint b
   pprint (KVar kindVar)   = pprint kindVar
   pprint (Kfun (KBase b) k2)   = pprint b <+> text "->" <+> pprint k2
   pprint (Kfun (KVar b)  k2)   = pprint b <+> text "->" <+> pprint k2
   pprint (Kfun k1   b) = text "(" <> pprint k1 <> text ")" <+> text "->" <+> pprint b
--   pprint (Kfun k1   (KVar b)) = text "(" <> pprint k1 <> text ")" <+> text "->" <+> pprint b
--   pprint (Kfun k1   k2)   = text "(" <> pprint k1 <> text ") -> (" <> pprint k2 <> text ")"

instance DocLike d =>  PPrint d Kindvar where
   pprint Kindvar { kvarUniq = s } = text $ 'k':show s

--  * -> * == [*,*]
--  (*->*->*) -> * -> * == [(*->*->*), *, *]
unfoldKind :: Kind -> [Kind]
unfoldKind (Kfun k1 k2) = k1 : unfoldKind k2
unfoldKind v = [v]

