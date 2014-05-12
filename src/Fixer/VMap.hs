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

import Data.Monoid(Monoid(..))
import qualified Data.Typeable as T -- qualified to avoid clashing with T.Proxy
import List(intersperse)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Doc.DocLike
import Fixer.Fixer
import GenUtil

-- | General data type for finding the fixpoint of a general tree-like structure.

data VMap p n = VMap {
    vmapArgs    :: Map.Map (n,Int) (VMap p n),
    vmapNodes   :: Either (Proxy p) (Set.Set n)
    }
    deriving(T.Typeable)

data Proxy p = Proxy p | DepthExceeded
    deriving(Eq,Ord,T.Typeable)

instance Show p => Show (Proxy p) where
    showsPrec n (Proxy p) = showsPrec n p
    showsPrec n DepthExceeded = ('*':)

emptyVMap :: (Ord p, Ord n) => VMap p n
emptyVMap = VMap { vmapArgs = mempty, vmapNodes = Right mempty }

vmapSingleton :: (Ord p, Ord n) => n -> VMap p n
vmapSingleton n = emptyVMap { vmapNodes = Right $ Set.singleton n }

vmapArgSingleton :: (Ord p,Ord n,Show p,Show n) => n -> Int -> VMap p n -> VMap p n
vmapArgSingleton n i v
    | isBottom v = emptyVMap
    | otherwise = pruneVMap $ emptyVMap { vmapArgs = Map.singleton (n,i) v }

vmapArg :: (Ord p,Ord n,Show p,Show n) => n -> Int -> VMap p n -> VMap p n
vmapArg n i vm@VMap { vmapArgs =  map } = case Map.lookup (n,i) map of
    Just x -> x `lub` vmapProxyIndirect i vm
    Nothing -> vmapProxyIndirect i vm

vmapProxyIndirect :: (Show p,Show n,Ord p,Ord n,Fixable (VMap p n)) => Int -> VMap p n -> VMap p n
vmapProxyIndirect _ VMap { vmapNodes = Left l } = emptyVMap { vmapNodes = Left l }
vmapProxyIndirect _ _ = emptyVMap

vmapValue :: (Show p,Show n,Ord p,Ord n) => n -> [VMap p n] -> VMap p n
vmapValue n xs = pruneVMap VMap { vmapArgs = Map.fromAscList (zip (zip (repeat n) [0..]) xs), vmapNodes = Right $ Set.singleton n }

vmapPlaceholder :: (Show p,Show n,Ord p,Ord n) => p  -> VMap p n
vmapPlaceholder p = emptyVMap { vmapNodes = Left (Proxy p) }

vmapDropArgs :: Ord n => VMap p n -> VMap p n
vmapDropArgs vm = vm { vmapArgs = mempty }

vmapHeads :: Monad m => VMap p n -> m [n]
vmapHeads VMap { vmapNodes = Left _ } = fail "vmapHeads: VMap is unknown"
vmapHeads VMap { vmapNodes = Right set } = return $ Set.toList set

vmapMember :: Ord n => n -> VMap p n -> Bool
vmapMember n VMap { vmapNodes = Left _ } = True
vmapMember n VMap { vmapNodes = Right set } = n `Set.member` set

pruneVMap :: (Ord n,Ord p,Show n,Show p) => VMap p n -> VMap p n
pruneVMap vmap = f (7::Int) vmap where
    f 0 _ = emptyVMap { vmapNodes = Left DepthExceeded }
    f _ VMap { vmapNodes = Left p} = emptyVMap {vmapNodes = Left p}
    f n VMap { vmapArgs = map, vmapNodes =  set} = VMap {vmapArgs = map', vmapNodes = set} where
        map' = Map.filter g (Map.map (f (n - 1)) map)
        g vs = not $ isBottom vs

instance (Ord p,Ord n,Show p,Show n) => Show (VMap p n) where
    showsPrec n VMap { vmapNodes = Left p } = showsPrec n p
    showsPrec _ VMap { vmapArgs = n, vmapNodes = Right s } = braces (hcat (intersperse (char ',') $ (map f $ snub $ (fsts $ Map.keys n) ++ Set.toList s) )) where
        f a = (if a `Set.member` s then tshow a else char '#' <> tshow a) <> (if null (g a) then empty else tshow (g a))
        g a = sortUnder fst [ (i,v) | ((a',i),v) <- Map.toList n, a' == a ]

instance (Show p,Show n,Ord p,Ord n) => Fixable (VMap p n) where
    bottom = emptyVMap
    isBottom VMap { vmapArgs = m, vmapNodes = Right s } = Map.null m && Set.null s
    isBottom _ = False
    lub x y | x `lte` y = y
    lub x y | y `lte` x = x
    lub VMap { vmapNodes = Left p } _ = emptyVMap { vmapNodes = Left p }
    lub _ VMap { vmapNodes = Left p } = emptyVMap { vmapNodes = Left p }
    lub VMap { vmapArgs = as, vmapNodes = Right ns } VMap { vmapArgs = as', vmapNodes = Right ns'} = pruneVMap $ VMap {vmapArgs = Map.unionWith lub as as', vmapNodes = Right $ Set.union ns ns' }
    minus _ VMap { vmapNodes = Left _ } = bottom
    minus x@VMap { vmapNodes = Left _ } _ = x
    minus VMap { vmapArgs = n1, vmapNodes = Right w1} VMap { vmapArgs = n2, vmapNodes = Right w2 } = pruneVMap $ VMap { vmapArgs = Map.fromAscList $ [
            case Map.lookup (a,i) n2 of
                Just v' ->  ((a,i),v `minus` v')
                Nothing ->  ((a,i),v)
        | ((a,i),v) <- Map.toAscList n1 ], vmapNodes = Right (w1 Set.\\ w2) }
    lte _ VMap { vmapNodes = Left _ } = True
    lte VMap { vmapNodes = Left _ } _ = False
    lte x@VMap { vmapArgs = as, vmapNodes = Right ns } y@VMap { vmapArgs = as', vmapNodes = Right ns'} =  (Set.null (ns Set.\\ ns') && (Map.null $ Map.differenceWith (\a b -> if a `lte` b then Nothing else Just undefined) as as'))
    showFixable x = show x

instance (Show p,Show n,Ord p,Ord n) => Monoid (VMap p n) where
    mempty = bottom
    mappend = lub
