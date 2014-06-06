module Deriving.Type where

import FrontEnd.HsSyn
import Name.Name

data Data = D {
    name        :: Name,
    constraints :: [(ClassName,Var)],
    vars      :: [Var],
    body      :: [Body],
    derives   :: [ClassName],
    statement :: DeclType
    } deriving (Eq,Show)

data Body = Body {
    constructor :: Name,
    labels :: [Name],
    types :: [HsBangType]
    } deriving (Eq,Show)

type Var = Name

data Derive = Derive {
    deriveSrcLoc :: SrcLoc,
    deriveHead :: HsClassHead,
    standAlone :: !Bool,
    deriveData :: Maybe Data
} deriving(Show)
