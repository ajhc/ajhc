-- | some useful types to use in Info's that don't really fit anywhere else
module Info.Types where

import Data.Dynamic
import Data.Monoid
import qualified Data.Set as Set

import Atom
import Binary
import MapBinaryInstance()

-- | list of properties of a function, such as specified by use pragmas or options
newtype Properties = Properties (Set.Set Atom)
    deriving(Typeable,Show,Binary,Monoid)

-- | how many manifest lambdas are in a functions definition
newtype Arity = Arity Int
    deriving(Typeable,Show,Ord,Eq,Num,Binary)

-- | how the variable is bound
data BindType = CaseDefault | CasePattern | LetBound | LambdaBound | PiBound
    deriving(Typeable,Show,Ord,Eq)

-- | whether the variable is exported from the current module
data ExportStatus = Exported
    deriving(Typeable,Show,Ord,Eq)

instance Binary ExportStatus where
    put_ _ _ = return ()
    get _ = return Exported


prop_INLINE = toAtom "INLINE"
prop_NOINLINE = toAtom "NOINLINE"

