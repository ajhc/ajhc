module FrontEnd.Tc.Kind(
    Kind(..),
    Kindvar(..),
    unfoldKind
    ) where

import Data.Generics

import Binary
import Doc.DocLike
import Doc.PPrint(pprint,PPrint)

data Kind  = Star
           | Kfun Kind Kind
           | KVar Kindvar               -- variables aren't really allowed in haskell in kinds
             deriving(Data,Typeable, Eq, Ord)   -- but we need them for kind inference
    {-! derive: GhcBinary !-}

newtype Kindvar = Kindvar Int deriving
    (Data,Binary,Typeable,Ord,Eq,Show)

instance Show Kind where
    showsPrec _ k = pprint k

instance DocLike d => PPrint d Kind where
   pprint Star = text "*"
   pprint (Kfun Star k2)   = text "* -> " <> pprint k2
   pprint (Kfun k1   Star) = text "(" <> pprint k1 <> text ")" <> text " -> *"
   pprint (Kfun k1   k2)   = text "(" <> pprint k1 <> text ") -> (" <> pprint k2 <> text ")"
   pprint (KVar kindVar)   = pprint kindVar

instance DocLike d =>  PPrint d Kindvar where
   pprint (Kindvar s) = text $ 'k':show s

--  * -> * == [*,*]
--  (*->*->*) -> * -> * == [(*->*->*), *, *]
unfoldKind :: Kind -> [Kind]
unfoldKind Star = [Star]
unfoldKind (KVar v) = [KVar v]
unfoldKind (Kfun k1 k2) = k1 : unfoldKind k2

