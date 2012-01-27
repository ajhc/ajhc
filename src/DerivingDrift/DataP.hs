module DerivingDrift.DataP where

import FrontEnd.HsSyn

data Statement = DataStmt | NewTypeStmt
    deriving (Eq,Show)

data Data = D {
    name :: Name,		-- type name
    constraints :: [(Class,Var)],
    vars :: [Var],		-- Parameters
    body :: [Body],
    derives :: [Class],		-- derived classes
    statement :: Statement
    } deriving (Eq,Show)

data Body = Body {
    constructor :: Constructor,
    labels :: [Name],
    types :: [HsBangType]
    } deriving (Eq,Show)

type Name = String
type Var = String
type Class = String
type Constructor = String
