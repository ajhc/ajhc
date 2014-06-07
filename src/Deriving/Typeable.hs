module Deriving.Typeable(deriveTypeable) where

import Deriving.Type
import Deriving.Util
import FrontEnd.HsSyn
import FrontEnd.Syn.Q
import Name.Names

deriveTypeable :: Int -> Name -> Derive -> Module -> Data -> Q HsDecl
deriveTypeable tnum tname der mod d@D { .. } = do
    mkInstN tnum der mod d tname []
