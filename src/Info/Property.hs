module Info.Property where

import Data.Dynamic
import Data.Monoid
import Info.Properties
import Util.BitSet
import Util.HasSize
import Util.SetLike

instance Show Properties where
    showsPrec _ props = shows (toList props)

-- | list of properties of a function, such as specified by use pragmas or options
newtype Properties = Properties (EnumBitSet Property)
    deriving(Eq,Collection,SetLike,HasSize,Monoid,Unionize,IsEmpty)

type instance Elem Properties = Property
type instance Key Properties = Property

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
