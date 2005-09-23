-- | some useful types to use in Info's that don't really fit anywhere else
module Info.Types where

import Data.Dynamic
import qualified Data.Set as Set

import Atom
import Binary
import MapBinaryInstance()

-- | list of properties of a function, such as specified by use pragmas or options
newtype Properties = Properties (Set.Set Atom)
    deriving(Typeable,Show,Binary)

-- | how many manifest lambdas are in a functions definition
newtype Arity = Arity Int
    deriving(Typeable,Show,Ord,Eq,Num,Binary)




