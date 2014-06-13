-- | some useful types to use in Info's that don't really fit anywhere else
module Info.Types(module Info.Types, module Info.Property, module Info.Properties) where

import Info.Property
import Info.Properties
import Data.Dynamic

-- | how many arguments a function my be applied to before it performs work and whether it bottoms out after that many arguments
data Arity = Arity Int Bool
    deriving(Typeable,Show,Ord,Eq)

-- | how the variable is bound
--data BindType = CaseDefault | CasePattern | LetBound | LambdaBound | PiBound
--    deriving(Show,Ord,Eq)
