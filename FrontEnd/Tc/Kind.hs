module FrontEnd.Tc.Kind(
    Kind(..),
    Kindvar(..),
    kindCombine,
    unfoldKind
    ) where

import Data.Generics
import Control.Monad

import Binary
import Doc.DocLike
import Doc.PPrint(pprint,PPrint)

{-

 KFunRet = ??
 KUTuple = (#)
 Star    = *
 Kfun    = (->)

 we have the following subkinding going on

   ??
  /  \
 *   (#)


-}

data Kind  = Star
           | Kfun Kind Kind
           | KUTuple                    -- ^ kind of unboxed tuples
           | KFunRet                    -- ^ either a * or a (#)
           | KVar Kindvar               -- variables aren't really allowed in haskell in kinds
             deriving(Data,Typeable, Eq, Ord)   -- but we need them for kind inference
    {-! derive: GhcBinary !-}

kindCombine :: Monad m => Kind -> Kind -> m Kind
kindCombine x y = f x y where
    f Star Star = return Star
    f KUTuple KUTuple = return KUTuple
    f KFunRet KFunRet = return KFunRet

    f KFunRet Star = return Star
    f Star KFunRet = return Star
    f KFunRet KUTuple = return KUTuple
    f KUTuple KFunRet = return KUTuple
    f (Kfun a b) (Kfun a' b') = return Kfun `ap` f a a' `ap` f b b'
    f x y = fail $ "kindCombine: " ++ show (x,y)


newtype Kindvar = Kindvar Int deriving
    (Data,Binary,Typeable,Ord,Eq,Show)

instance Show Kind where
    showsPrec _ k = pprint k

instance DocLike d => PPrint d Kind where
   pprint Star = text "*"
   pprint KUTuple = text "(#)"
   pprint KFunRet = text "??"
   pprint (Kfun Star k2)   = text "* -> " <> pprint k2
   pprint (Kfun KUTuple k2)   = text "(#) -> " <> pprint k2  -- ^ this is invalid
   pprint (Kfun KFunRet k2)   = text "?? -> " <> pprint k2  -- ^ this is invalid
   pprint (Kfun k1   Star) = text "(" <> pprint k1 <> text ")" <> text " -> *"
   pprint (Kfun k1   KUTuple) = text "(" <> pprint k1 <> text ")" <> text " -> (#)"
   pprint (Kfun k1   KFunRet) = text "(" <> pprint k1 <> text ")" <> text " -> ??"
   pprint (Kfun k1   k2)   = text "(" <> pprint k1 <> text ") -> (" <> pprint k2 <> text ")"
   pprint (KVar kindVar)   = pprint kindVar

instance DocLike d =>  PPrint d Kindvar where
   pprint (Kindvar s) = text $ 'k':show s

--  * -> * == [*,*]
--  (*->*->*) -> * -> * == [(*->*->*), *, *]
unfoldKind :: Kind -> [Kind]
unfoldKind (Kfun k1 k2) = k1 : unfoldKind k2
unfoldKind v = [v]

