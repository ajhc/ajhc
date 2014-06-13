module Info.Info(
    T,
    Info(..),
    Entry(..),
    HasInfo(..),
    Info.Info.lookup,
    Info.Info.lookupTyp,
    insertWith,
    insert,
    maybeInsert,
    entryType,
    singleton,
    member,
    delete,
    fetch,
    extend,
    empty
    ) where

import Data.Dynamic
import Data.Monoid
import Info.Property
import qualified Data.Map as Map

-- extensible type indexed product

type T = Info

data Entry = Entry {
    entryThing   :: Dynamic,
    entryString  :: String
    }

entryType = dynTypeRep . entryThing

instance Eq Entry where
    a == b = entryType a == entryType b

instance Show Entry where
    showsPrec _ x = showString (entryString x)

instance Ord Entry where
    compare a b = compare (entryType a) (entryType b)

data Info = Info { infoProperties :: !Properties, infoMap :: (Map.Map TypeRep Entry) }
    deriving(Typeable)

-- the Eq and Ord instances for info make them all seem equivalent.
instance Eq Info where
    _ == _ = True
instance Ord Info where
    compare _ _ = EQ

instance Show Info where
    show Info {.. } = show (Map.toList infoMap)

--instance Data Info where
--    toConstr = undefined
--    dataTypeOf = undefined

instance Monoid Info where
    mempty = empty
    mappend (Info ap as) (Info bp bs) = Info (mappend ap bp) (Map.union as bs)

class HasInfo a where
    getInfo :: a -> Info
    modifyInfo :: (Info -> Info) -> a -> a

instance HasInfo Info where
    getInfo = id
    modifyInfo f x = f x

lookupTyp :: forall a . Typeable a => a -> Info -> Maybe a
lookupTyp _ = f where
    f (Info _ mp) = Map.lookup typ mp >>= fromDynamic . entryThing
    typ = typeOf (undefined :: a)

lookup :: forall a m . (Monad m,Typeable a) => Info -> m a
lookup = maybe (fail $ "Info: could not find: " ++ show typ) return . f where
    typ = typeOf (undefined :: a)
    f = lookupTyp (undefined :: a)

insertWith :: (Show a,Typeable a) => (a -> a -> a) -> a -> Info -> Info
insertWith f newx Info { .. }  = Info { infoMap = Map.insert typ (newEntry newx) infoMap, .. } where
    typ = typeOf newx

newEntry :: (Typeable a,Show a) => a -> Entry
newEntry x = Entry { entryThing = toDyn x, entryString = show x }

insert :: (Show a,Typeable a) => a -> Info -> Info
insert newx Info { .. } = Info { infoMap = Map.insert typ (newEntry newx) infoMap, .. } where
    typ = typeOf newx

maybeInsert :: (Show a, Typeable a) => Maybe a -> Info -> Info
maybeInsert Nothing = id
maybeInsert (Just x) = insert x

singleton :: (Show a,Typeable a) => a -> Info
singleton x = insert x empty

delete :: (Typeable a) => a -> Info -> Info
delete x info = deleteTyp (typeOf x) info

deleteTyp :: TypeRep -> Info -> Info
deleteTyp typ (Info a mp) = Info a (Map.delete typ mp) where

fetch :: (Monoid a, Typeable a) => Info -> a
fetch info = maybe mempty id  (Info.Info.lookup info)

member :: (Typeable a) => a -> Info -> Bool
member x Info { .. } = Map.member typ infoMap where
    typ = typeOf x

extend :: (Show a,Monoid a, Typeable a) => a -> Info -> Info
extend x info = insertWith mappend x info

empty :: Info
empty = Info mempty mempty

instance HasProperties Info where
    modifyProperties f Info { .. } = Info { infoProperties = f infoProperties, .. }
    getProperties x = infoProperties x
    putProperties infoProperties i  = i { infoProperties }
