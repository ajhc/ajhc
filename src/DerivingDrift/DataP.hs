module DerivingDrift.DataP where

import Name.Name hiding(Class)
import FrontEnd.HsSyn

data Statement = DataStmt | NewTypeStmt
    deriving (Eq,Show)

data Data = D {
    name :: Name,		-- type name
    constraints :: [(String,Var)],
    vars :: [Var],		-- Parameters
    body :: [Body],
    derives :: [String],		-- derived classes
    statement :: Statement
    } deriving (Eq,Show)

data Body = Body {
    constructor :: Constructor,
    labels :: [Name],
    types :: [HsBangType]
    } deriving (Eq,Show)

type Var = String
type Class = String
type Constructor = String
