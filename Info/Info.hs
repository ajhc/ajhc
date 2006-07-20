module Info.Info(
    T,
    Info(..),
    Entry(..),
    HasInfo(..),
    Info.Info.lookup,
    insertWith,
    insert,
    limit,
    maybeInsert,
    singleton,
    member,
    delete,
    fetch,
    extend,
    empty,
    infoMap,
    infoMapM
    ) where

import Data.Dynamic
import Data.Generics
import Data.Monoid
import Monad
import qualified Data.List as List

import Atom
import GenUtil
import Util.HasSize

-- extensible type indexed product

type T = Info

data Entry = Entry {
    entryThing   :: Dynamic,
    entryString  :: String,
    entryType    :: TypeRep
    }

instance Eq Entry where
    a == b = entryType a == entryType b

instance Show Entry where
    showsPrec _ x = showString (entryString x)

instance Ord Entry where
    compare a b = compare (show $ entryType a) (show $ entryType b)

newtype Info = Info [Entry]
    deriving(HasSize,Typeable)

instance Show Info where
    show (Info ds) = show (sortUnder (show . entryType) ds)

instance Data Info where
    toConstr = undefined
    dataTypeOf = undefined

instance Monoid Info where
    mempty = empty
    mappend (Info as) (Info bs) = Info (List.union as bs)

class HasInfo a where
    getInfo :: a -> Info
    modifyInfo :: (Info -> Info) -> a -> a

instance HasInfo Info where
    getInfo = id
    modifyInfo f x = f x


lookup :: forall a m .  (Monad m,Typeable a) => Info -> m a
lookup (Info mp) = do
    let typ = typeOf (undefined :: a)
        f [] = fail $ "Info: could not find " ++ show typ
        f (x:xs) | entryType x == typ = case fromDynamic (entryThing x) of
            Just x -> return x
            Nothing -> error "Info.lookup: this can't happen"
        f (_:xs) = f xs
    f mp


createTyp :: Typeable a => a -> Atom
createTyp (_::a) = toAtom (show (typeOf (undefined :: a)))

insertWith :: (Show a,Typeable a) => (a -> a -> a) -> a -> Info -> Info
insertWith f newx (Info mp) = Info (g mp) where
    g [] = [newEntry newx]
    g (x:xs) | entryType x == typ = newEntry (f newx (fromDyn (entryThing x) (error "can't happen"))):xs
             | otherwise = x:g xs
    typ = typeOf newx


newEntry :: (Typeable a,Show a) => a -> Entry
newEntry x = Entry { entryThing = toDyn x, entryString = show x, entryType = typeOf x }


insert :: (Show a,Typeable a) => a -> Info -> Info
insert newx (Info nfo) = Info $ newEntry newx:f nfo where
    f [] = []
    f (x:xs) | entryType x == typ = xs
             | otherwise = x:f xs
    typ = typeOf newx

maybeInsert :: (Show a, Typeable a) => Maybe a -> Info -> Info
maybeInsert Nothing = id
maybeInsert (Just x) = insert x

singleton :: (Show a,Typeable a) => a -> Info
singleton x = insert x empty

infoMapM :: (Typeable a, Typeable b, Show b, Monad m) => (a -> m b) -> Info -> m Info
infoMapM f i = case Info.Info.lookup i of
    Just x -> do
        n <- f x
        return (insert n (delete x i))
    Nothing -> return i

infoMap :: (Typeable a, Typeable b, Show b) => (a -> b) -> Info ->  Info
infoMap f i = case Info.Info.lookup i of
    Just x -> insert (f x) (delete x i)
    Nothing -> i

delete :: (Typeable a) => a -> Info -> Info
delete x info = deleteTyp (typeOf x) info

deleteTyp :: TypeRep -> Info -> Info
deleteTyp typ (Info mp) = Info (f mp) where
    f [] = []
    f (x:xs) | entryType x == typ = xs
             | otherwise = x:f xs

limit :: [TypeRep] -> Info -> Info
limit trs (Info mp) = Info (f mp) where
    f (x:xs) | entryType x `elem` trs = x:f xs
             | otherwise = f xs
    f [] = []

fetch :: (Monoid a, Typeable a) => Info -> a
fetch info = maybe mempty id  (Info.Info.lookup info)

member :: (Typeable a) => a -> Info -> Bool
member x (Info s) = f s where
    typ = typeOf x
    f [] = False
    f (x:xs) | entryType x == typ = True
             | otherwise = f xs

extend :: (Show a,Monoid a, Typeable a) => a -> Info -> Info
extend x info = insertWith mappend x info

empty :: Info
empty = Info []

