
-- | A Pretty printing class using multiparameter type classes for
-- maximal generality with some useful instances.
--
-- the pprinted type comes as the last argument so newtype deriving can be used
-- in more places.

module Doc.PPrint where

import Doc.DocLike
import qualified Data.Map as Map

{-
 - some useful fixities for comparison
 -
 - application left 10
 - infixr 9  .
 - infixr 8  ^, ^^, **
 - infixl 7  *  , /, `quot`, `rem`, `div`, `mod`
 - infixl 6  +, -
 - infixr 5  :
 - infix  4  ==, /=, <, <=, >=, >
 - infixr 3  &&
 - infixr 2  ||
 - infixl 1  >>, >>=
 - infixr 1  =<<
 - infixr 0  $, $!, `seq`
 -
 -}

data Assoc = AssocLeft | AssocRight | AssocNone
    deriving(Eq,Ord,Show)

class DocLike d => PPrint d a  where
    pprint ::  a -> d
    pprintAssoc :: Assoc -> Int -> a -> d

    pprintAssoc _ _ a = pprint a
    pprint a = pprintAssoc AssocNone (-1) a


    pplist    ::  [a] -> d
    pplist    xs = brackets (hcat (punctuate comma (map pprint xs)))

pprintParen :: PPrint d a => a -> d
pprintParen = pprintPrec 11

pprintPrec n a = pprintAssoc AssocNone n  a

instance PPrint d a => PPrint d [a] where
    pprint  = pplist

instance DocLike d => PPrint d Char where
  pprint  = char
  pplist  = text

instance DocLike d => PPrint d Integer where
  pprint  = tshow

instance DocLike d => PPrint d Int where
  pprint  = tshow

instance DocLike d => PPrint d Float where
  pprint  = tshow

instance DocLike d => PPrint d Double where
  pprint  = tshow

instance DocLike d => PPrint d () where
    pprint () = text "()"

instance (PPrint d a, PPrint d b) => PPrint d (a,b) where
  pprint (x,y) = parens (hsep [pprint x <> comma, pprint y])

checkAssoc a1 n1 a2 n2 | n2 < n1 = id
                       | n1 == n2 && a1 == a2 && a1 /= AssocNone = id
                       | otherwise = parens

checkAssocApp a n p = checkAssoc AssocLeft 10 a n p

pprintBinary a1 n1 a2 n2 x1 b x2 = checkAssoc a1 n1 a2 n2 $ pprintAssoc l n1 x1 <+> b <+> pprintAssoc r n1 x2 where
    l = if a1 == AssocLeft then AssocLeft else AssocNone
    r = if a1 == AssocRight then AssocRight else AssocNone

instance (PPrint d a, PPrint d b) => PPrint d (Either a b) where
  pprintAssoc a n (Left x)  = checkAssocApp a n $ text "Left" <+> pprintPrec 10 x
  pprintAssoc a n (Right x) = checkAssocApp a n $ text "Right" <+> pprintPrec 10 x

instance (PPrint d a, PPrint d b, PPrint d c) => PPrint d (a,b,c) where
  pprint (x,y,z) = parens (hsep [pprint x <> comma,
                                pprint y <> comma,
                                pprint z])

instance (PPrint d a, PPrint d b) => PPrint d (Map.Map a b) where
    pprint m = vcat [ pprint x <+> text "=>" <+> pprint y | (x,y) <- Map.toList m]


