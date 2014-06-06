module Deriving.Typeable(deriveTypeable) where

import Deriving.Type
import Deriving.Util
import FrontEnd.HsSyn
import FrontEnd.Syn.Q
import Name.Names

deriveTypeable :: Int -> Name -> SrcLoc -> Module -> Data -> Q HsDecl
deriveTypeable tnum tname sloc mod d@D { .. } = do
    mkInstN tnum sloc mod d tname []
