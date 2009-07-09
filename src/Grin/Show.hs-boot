module Grin.Show where

import Doc.Pretty
import Atom
import {-# SOURCE #-} Grin.Grin

prettyFun :: (Atom.Atom,Grin.Grin.Lam) -> Doc.Pretty.Doc
prettyExp :: Doc.Pretty.Doc -> Grin.Grin.Exp -> Doc.Pretty.Doc
prettyVal :: Grin.Grin.Val -> Doc.Pretty.Doc
