-- | some useful types to use in Info's that don't really fit anywhere else
module Info.Types(module Info.Types, module Info.Properties) where

import Info.Properties
import Data.Dynamic
import Data.Monoid
import List hiding(insert,delete)
import qualified Data.Set as Set

import Atom
import Util.HasSize
import Util.SetLike
import qualified Info.Info as Info


-- | how many arguments a function my be applied to before it performs work and whether it bottoms out after that many arguments
data Arity = Arity Int Bool
    deriving(Typeable,Show,Ord,Eq)

-- | how the variable is bound
data BindType = CaseDefault | CasePattern | LetBound | LambdaBound | PiBound
    deriving(Typeable,Show,Ord,Eq)

instance Show Properties where
    showsPrec _ props = shows (toList props)


-- | list of properties of a function, such as specified by use pragmas or options
newtype Properties = Properties (EnumSet Property)
    deriving(Typeable,Eq,HasSize,Monoid,SetLike,BuildSet Property,ModifySet Property,IsEmpty)



class HasProperties a where
    modifyProperties :: (Properties -> Properties) -> a -> a
    getProperties :: a -> Properties
    putProperties :: Properties -> a -> a

    setProperty :: Property -> a -> a
    unsetProperty :: Property -> a -> a
    getProperty :: Property -> a -> Bool
    setProperties :: [Property] -> a -> a

    unsetProperty prop = modifyProperties (delete prop)
    setProperty prop = modifyProperties (insert prop)
    setProperties xs = modifyProperties (`mappend` fromList xs)
    getProperty atom = member atom . getProperties

instance HasProperties Properties where
    getProperties prop = prop
    putProperties prop _ = prop
    modifyProperties f = f


instance HasProperties Info.Info where
    modifyProperties f info = case Info.lookup info of
        Just x -> Info.insert (f x) info
        Nothing -> Info.insert (f mempty) info
    getProperties info = case Info.lookup info of
        Just p -> p
        Nothing -> mempty
    putProperties prop info = Info.insert prop info


