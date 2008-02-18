-- Adaptation and extension of a parser for data definitions given in
-- appendix of G. Huttons's paper - Monadic Parser Combinators.
--
-- Parser does not accept infix data constructors. This is a shortcoming that
-- needs to be fixed.

module DerivingDrift.DataP where

import Char
import HsSyn


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

