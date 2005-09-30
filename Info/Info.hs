module Info.Info(T,Info(..),Entry(..),Info.Info.lookup,insertWith,insert,singleton,delete, fetch, extend, empty) where

import Data.Dynamic
import Data.Generics
import Data.Monoid
import Monad
import qualified Data.Map as Map

import Atom
import GenUtil
import Util.HasSize

-- extensible type indexed product

type T = Info

data Entry = Entry {
    entryThing   :: Dynamic,
    entryString  :: String,
    entryType    :: Atom
    }

instance Eq Entry where
    a == b = entryType a == entryType b

instance Show Entry where
    showsPrec _ x = showString (entryString x)

instance Ord Entry where
    compare a b = compare (entryType a) (entryType b)

newtype Info = Info (Map.Map Atom Entry)
    deriving(HasSize,Typeable)

instance Show Info where
    show (Info ds) = show (sortUnder (show . entryType) (Map.elems ds))

instance Data Info where
    toConstr = undefined
    dataTypeOf = undefined

instance Monoid Info where
    mempty = empty
    mappend (Info as) (Info bs) = Info (Map.union as bs)


lookup :: forall a m .  (Monad m,Typeable a) => Info -> m a
lookup (Info mp) = do
    let typ = createTyp (undefined :: a)
    case Map.lookup typ mp of
        Just Entry { entryThing = x } -> case fromDynamic x of
            Just x -> return x
            Nothing -> error "Info.lookup: this can't happen"
        Nothing -> fail $ "Info: could not find " ++ show typ


createTyp :: Typeable a => a -> Atom
createTyp x = toAtom (show (typeOf x))

insertWith :: (Show a,Typeable a) => (a -> a -> a) -> a -> Info -> Info
insertWith f x (Info mp) = Info (Map.insert typ (newEntry typ nx) mp) where
    typ = createTyp x
    nx = case Map.lookup typ mp of
        Nothing -> x
        Just Entry { entryThing = d } -> f x (fromDyn d (error "can't happen"))


newEntry typ x = Entry { entryThing = toDyn x, entryString = show x, entryType = typ }

{-
insertWith f x (Info ds) = Info (g ds []) where
    g [] rs = (toDyn x:rs)
    g (d:ds) rs
        | Just y <- fromDynamic d = toDyn (f x y):(ds ++ rs)
        | otherwise = g ds (d:rs)
-}

insert :: (Show a,Typeable a) => a -> Info -> Info
insert x info = insertWith const x info

singleton :: (Show a,Typeable a) => a -> Info
singleton x = insert x empty

delete :: (Typeable a) => a -> Info -> Info
delete x info = error "Info.delete"

fetch :: (Monoid a, Typeable a) => Info -> a
fetch info = maybe mempty id  (Info.Info.lookup info)

extend :: (Show a,Monoid a, Typeable a) => a -> Info -> Info
extend x info = insertWith mappend x info

empty :: Info
empty = Info Map.empty

{-

newtype Info = Info (Map.Map TypeRep Dynamic)
    deriving(Monoid,HasSize)


lookup :: (Monad m,Typeable a) => Info -> m a
lookup (Info fm) :: m a = case Map.lookup tr fm of
        Just x -> return (fromDyn x undefined :: a)
        Nothing -> fail $ "Info: could not find " ++ show tr
    where tr = typeOf (undefined :: a)


fetch :: (Monoid a, Typeable a) => Info -> a
fetch info = maybe mempty id  (Info.lookup info)

insert :: (Typeable a) => a -> Info -> Info
insert x (Info fm) = Info (Map.insert (typeOf x) (toDyn x) fm)

insertWith :: (Typeable a) => (a -> a -> a) -> a -> Info -> Info
insertWith f x (Info fm) = Info (Map.adjust (\y -> toDyn $ f x (fromDyn y undefined)) (typeOf x)  fm)

extend :: (Monoid a, Typeable a) => a -> Info -> Info
extend x info = insertWith mappend x info

-}
