-- -*- Haskell -*-

module E.Show(ePretty,render,prettyE,ePrettyEx) where

import E.E
import Doc.DocLike
import Doc.Pretty
import Doc.PPrint

render :: Doc -> String
prettyE :: E -> String
ePrettyEx :: E -> Doc
ePretty :: E -> Doc

instance DocLike d => PPrint d TVr
instance PPrint Doc E
instance PPrint String E
instance PPrint String (Lit E E)
