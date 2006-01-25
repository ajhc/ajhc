module Fixer.VMap where

import Doc.DocLike
import Fixer.Fixer
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Typeable
import Data.Monoid
import GenUtil
import List(intersperse)


-- VMap general data type for finding the fixpoint of a general tree-like structure.

data VMap n = VMap (Map.Map (n,Int) (VMap n)) (Set.Set n)
    deriving(Typeable)

vmapSingleton n = VMap Map.empty (Set.singleton n)

vmapArgSingleton n i v
    | isBottom v = bottom
    | otherwise = VMap (Map.singleton (n,i) v) Set.empty

vmapArg n i (VMap map _) = case Map.lookup (n,i) map of
    Just x -> x
    Nothing -> bottom

vmapValue :: Ord n => n -> [VMap n] -> VMap n
vmapValue n xs = pruneVMap $ VMap (Map.fromAscList (zip (zip (repeat n) [0..]) xs)) (Set.singleton n)

vmapHeads (VMap _ set) = Set.toList set
vmapJustHeads (VMap _ set) = VMap Map.empty set

pruneVMap (VMap map set) = VMap map' set where
    map' = Map.filter f map
    f vs = not $ isBottom vs

instance (Ord n,Show n) => Show (VMap n) where
    showsPrec _ (VMap n s) = braces (hcat (intersperse (char ',') $ (map f $ snub $ fsts  (Map.keys n) ++ Set.toList s) )) where
        f a = (if a `Set.member` s then tshow a else char '#' <> tshow a) <> tshow (g a)
        g a = sortUnder fst [ (i,v) | ((a',i),v) <- Map.toList n, a' == a ]

instance Ord n => Fixable (VMap n) where
    bottom = VMap Map.empty Set.empty
    isBottom (VMap m s) = Map.null m && Set.null s
    lub (VMap as ns) (VMap as' ns') = pruneVMap $ VMap (Map.unionWith lub as as') (Set.union ns ns')
    minus (VMap n1 w1) (VMap n2 w2) = pruneVMap $ VMap (Map.fromAscList $ [
            case Map.lookup (a,i) n2 of
                Just v' ->  ((a,i),v `minus` v')
                Nothing ->  ((a,i),v)
        | ((a,i),v) <- Map.toAscList n1 ] ) (w1 Set.\\ w2)

instance Ord n => Monoid (VMap n) where
    mempty = bottom
    mappend = lub
