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

data VMap n = VMap {
    vmapArgs :: (Map.Map (n,Int) (VMap n)),
    vmapNodes :: (Set.Set n)
    }
    deriving(Typeable)

emptyVMap :: Ord a => VMap a
emptyVMap = VMap { vmapArgs = mempty, vmapNodes = mempty }

vmapSingleton n = emptyVMap { vmapNodes = (Set.singleton n) }

vmapArgSingleton n i v
    | isBottom v = emptyVMap
    | otherwise = emptyVMap { vmapArgs = (Map.singleton (n,i) v) }

vmapArg n i VMap { vmapArgs =  map } = case Map.lookup (n,i) map of
    Just x -> x
    Nothing -> bottom

vmapValue :: Ord n => n -> [VMap n] -> VMap n
vmapValue n xs = pruneVMap VMap { vmapArgs = Map.fromAscList (zip (zip (repeat n) [0..]) xs), vmapNodes = Set.singleton n }

vmapHeads VMap { vmapNodes = set } = Set.toList set
vmapJustHeads VMap { vmapNodes = set } = emptyVMap { vmapNodes = set }

pruneVMap VMap { vmapArgs = map, vmapNodes =  set} = VMap {vmapArgs = map', vmapNodes = set} where
    map' = Map.filter f map
    f vs = not $ isBottom vs

instance (Ord n,Show n) => Show (VMap n) where
    showsPrec _ (VMap n s) = braces (hcat (intersperse (char ',') $ (map f $ snub $ fsts  (Map.keys n) ++ Set.toList s) )) where
        f a = (if a `Set.member` s then tshow a else char '#' <> tshow a) <> (if null (g a) then empty else tshow (g a))
        g a = sortUnder fst [ (i,v) | ((a',i),v) <- Map.toList n, a' == a ]

instance Ord n => Fixable (VMap n) where
    bottom = emptyVMap
    isBottom VMap { vmapArgs = m, vmapNodes = s } = Map.null m && Set.null s
    lub VMap { vmapArgs = as, vmapNodes = ns } VMap { vmapArgs = as', vmapNodes = ns'} = pruneVMap $ VMap {vmapArgs = Map.unionWith lub as as', vmapNodes = Set.union ns ns' }
    minus VMap { vmapArgs = n1, vmapNodes = w1} VMap { vmapArgs = n2, vmapNodes = w2 } = pruneVMap $ VMap { vmapArgs = Map.fromAscList $ [
            case Map.lookup (a,i) n2 of
                Just v' ->  ((a,i),v `minus` v')
                Nothing ->  ((a,i),v)
        | ((a,i),v) <- Map.toAscList n1 ], vmapNodes = (w1 Set.\\ w2) }

instance Ord n => Monoid (VMap n) where
    mempty = bottom
    mappend = lub

