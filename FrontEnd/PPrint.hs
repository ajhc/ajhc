-----------------------------------------------------------------------------
-- PPrint:	Print functions
-----------------------------------------------------------------------------

--module PPrint(module PPrint, module Text.PrettyPrint.HughesPJ) where
module PPrint(PPrint(..), module Text.PrettyPrint.HughesPJ, pretty) where
import Text.PrettyPrint.HughesPJ
import Doc.DocLike
import Doc.PPrint

pretty  :: PPrint Doc a => a -> String
pretty   = render . pprint

{-

-----------------------------------------------------------------------------
-- This module contains definitions that do not appear in the
-- typeset version of the paper.

-----------------------------------------------------------------------------
-- Pretty printing; a replacement for Show:


ppParen    :: Bool -> Doc -> Doc
ppParen t x = if t then parens x else x

class PPrint a where
  pprint    :: a -> Doc

  parPprint :: a -> Doc
  parPprint  = parens . pprint

  pplist    :: [a] -> Doc
  pplist    xs = brackets (hcat (punctuate comma (map pprint xs)))

  pptuple   :: [a] -> Doc
  pptuple   xs = parens (hcat (punctuate comma (map pprint xs)))

instance PPrint a => PPrint [a] where
  pprint  = pplist

instance PPrint Char where
  pprint  = char
  pplist  = text

instance PPrint Integer where
  pprint  = integer

instance PPrint Int where
  pprint  = int

instance PPrint Float where
  pprint  = float

instance PPrint Double where
  pprint  = double

instance (PPrint a, PPrint b) => PPrint (a,b) where
  pprint (x,y) = parens (sep [pprint x <> comma, pprint y])

instance (PPrint a, PPrint b, PPrint c) => PPrint (a,b,c) where
  pprint (x,y,z) = parens (sep [pprint x <> comma,
                                pprint y <> comma,
                                pprint z])

-----------------------------------------------------------------------------
-}
