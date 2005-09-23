module Info.Info where

import Data.Dynamic
import Data.Monoid
import Data.Generics
import HasSize
import Monad
import qualified Data.Set as Set
import Atom

-- extensible type indexed product

type T = Info

newtype Info = Info [Dynamic]
    deriving(HasSize,Typeable)

instance Data Info where
    toConstr = undefined

instance Monoid Info where
    mempty = Info []
    mappend (Info as) (Info bs) = Info ([ b | b <- bs, not (show b `Set.member` bss) ] ++ as) where
        bss = Set.fromList $ map show bs


lookup :: forall a m .  (Monad m,Typeable a) => Info -> m a
lookup (Info ds)  = case msum (map fromDynamic ds) of
    Just x -> return x
    Nothing -> fail $ "Info: could not find " ++ show (typeOf (undefined :: a))

insertWith :: (Typeable a) => (a -> a -> a) -> a -> Info -> Info
insertWith f x (Info ds) = Info (g ds []) where
    g [] rs = (toDyn x:rs)
    g (d:ds) rs
        | Just y <- fromDynamic d = toDyn (f x y):(ds ++ rs)
        | otherwise = g ds (d:rs)

insert :: (Typeable a) => a -> Info -> Info
insert x info = insertWith const x info


delete :: (Typeable a) => a -> Info -> Info
delete x info = error "Info.delete"

fetch :: (Monoid a, Typeable a) => Info -> a
fetch info = maybe mempty id  (Info.Info.lookup info)

extend :: (Monoid a, Typeable a) => a -> Info -> Info
extend x info = insertWith mappend x info

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
