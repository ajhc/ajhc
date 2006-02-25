module Fixer.VMap(
    VMap(),
    Proxy(..),
    vmapSingleton,
    vmapArgSingleton,
    vmapArg,
    vmapValue,
    vmapMember,
    vmapProxyIndirect,
    vmapPlaceholder,
    vmapDropArgs,
    vmapHeads
    )where

import Data.Monoid
import Data.Typeable
import List(intersperse)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Doc.DocLike
import Fixer.Fixer
import GenUtil


-- VMap general data type for finding the fixpoint of a general tree-like structure.

data VMap p n = VMap {
    vmapArgs    :: Map.Map (n,Int) (VMap p n),
    vmapNodes   :: Set.Set (Either (Proxy p) n)
    }
    deriving(Typeable)

-- A placeholder is either a placeholder, or an indirection of a placeholder.
data Proxy p = Proxy p | ProxyArg (Proxy p) Int
    deriving(Eq,Ord,Typeable)

instance Show p => Show (Proxy p) where
    showsPrec n (Proxy p) = showsPrec n p
    showsPrec n (ProxyArg p i) = showsPrec n p .  (('-':show i) ++)

emptyVMap :: (Ord a,Ord b) => VMap a b
emptyVMap = VMap { vmapArgs = mempty, vmapNodes = mempty }

vmapSingleton n = emptyVMap { vmapNodes = Set.singleton (Right n) }

vmapArgSingleton n i v
    | isBottom v = emptyVMap
    | otherwise = emptyVMap { vmapArgs = Map.singleton (n,i) v }

vmapArg n i vm@VMap { vmapArgs =  map } = case Map.lookup (n,i) map of
    Just x -> x `lub` vmapProxyIndirect i vm
    Nothing -> vmapProxyIndirect i vm

vmapProxyIndirect :: (Show p,Show n,Ord p,Ord n,Fixable (VMap p n)) => Int -> VMap p n -> VMap p n
vmapProxyIndirect i vm = emptyVMap {  vmapNodes = Set.fromList [  Left p {- (ProxyArg p i) -} | Left p <- Set.toList $ vmapNodes vm] }

vmapValue :: (Show p,Show n,Ord p,Ord n) => n -> [VMap p n] -> VMap p n
vmapValue n xs = pruneVMap VMap { vmapArgs = Map.fromAscList (zip (zip (repeat n) [0..]) xs), vmapNodes = Set.singleton (Right n) }

vmapPlaceholder :: (Show p,Show n,Ord p,Ord n) => p  -> VMap p n
vmapPlaceholder p = emptyVMap { vmapNodes = Set.singleton $ Left (Proxy p) }

vmapDropArgs vm = vm { vmapArgs = mempty }

vmapHeads VMap { vmapNodes = set }
    | any isLeft (Set.toList set) = fail "vmapHeads: VMap has a placeholder"
    | otherwise = return $ rights $ Set.toList set
vmapMember n VMap { vmapNodes = set } = Right n `Set.member` set || any isLeft (Set.toList set)


pruneVMap VMap { vmapArgs = map, vmapNodes =  set} = VMap {vmapArgs = map', vmapNodes = set} where
    map' = Map.filter f map
    f vs = not $ isBottom vs

instance (Ord p,Ord n,Show p,Show n) => Show (VMap p n) where
    showsPrec _ VMap { vmapArgs = n, vmapNodes = s } = braces (hcat (intersperse (char ',') $ (map f $ snub $ (map Right $ fsts $ Map.keys n) ++ Set.toList s) )) where
        f a = (if a `Set.member` s then tshow a else char '#' <> tshow a) <> (if null (g a) then empty else tshow (g a))
        g a = sortUnder fst [ (i,v) | ((a',i),v) <- Map.toList n, Right a' == a ]

instance (Show p,Show n,Ord p,Ord n) => Fixable (VMap p n) where
    bottom = emptyVMap
    isBottom VMap { vmapArgs = m, vmapNodes = s } = Map.null m && Set.null s
    lub VMap { vmapArgs = as, vmapNodes = ns } VMap { vmapArgs = as', vmapNodes = ns'} = pruneVMap $ VMap {vmapArgs = Map.unionWith lub as as', vmapNodes = Set.union ns ns' }
    minus VMap { vmapArgs = n1, vmapNodes = w1} VMap { vmapArgs = n2, vmapNodes = w2 } = pruneVMap $ VMap { vmapArgs = Map.fromAscList $ [
            case Map.lookup (a,i) n2 of
                Just v' ->  ((a,i),v `minus` v')
                Nothing ->  ((a,i),v)
        | ((a,i),v) <- Map.toAscList n1 ], vmapNodes = (w1 Set.\\ w2) }
    lte x@VMap { vmapArgs = as, vmapNodes = ns } y@VMap { vmapArgs = as', vmapNodes = ns'} = any isLeft (Set.toList ns') || isBottom (x `minus` y)
    showFixable x = show x

instance (Show p,Show n,Ord p,Ord n) => Monoid (VMap p n) where
    mempty = bottom
    mappend = lub

