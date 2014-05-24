module Ho.Collected(
    CollectedHo(..),
    choDataTable,
    choClassHierarchy,
    choTypeSynonyms,
    choFixities,
    choAssumps,
    choRules,
    choEs,
    updateChoHo
    )where

import Control.Monad.Identity
import Data.List
import Data.Monoid

import DataConstructors
import E.Annotate
import E.E
import Ho.Type
import Info.Types
import Name.Names
import Util.SetLike
import qualified Data.Map as Map
import qualified Info.Info as Info

choDataTable = hoDataTable . hoBuild . choHo
choClassHierarchy = hoClassHierarchy . hoTcInfo . choHo
choTypeSynonyms = hoTypeSynonyms . hoTcInfo . choHo
choFixities = hoFixities . hoTcInfo . choHo
choAssumps = hoAssumps . hoTcInfo . choHo
choRules = hoRules . hoBuild . choHo
choEs cho = [ (combHead c,combBody c) | c <- values $  choCombinators cho]

instance Monoid CollectedHo where
    mempty = updateChoHo CollectedHo {
        choExternalNames = mempty,
        choOrphanRules = mempty,
        choHoMap = Map.singleton mod_Prim_ pho,
        choCombinators = mempty,
        choHo = error "choHo-a",
        choVarMap = error "choVarMap-a",
        choLibDeps = mempty
        } where pho = mempty { hoBuild = mempty { hoDataTable = dataTablePrims } }
    a `mappend` b = updateChoHo CollectedHo {
        choExternalNames = choExternalNames a `mappend` choExternalNames b,
        choVarMap = error "choVarMap-b",
        choOrphanRules = choOrphanRules a `mappend` choOrphanRules b,
        choCombinators = choCombinators a `mergeChoCombinators` choCombinators b,
        choLibDeps = choLibDeps a `mappend` choLibDeps b,
        choHo = error "choHo-b",
        choHoMap = Map.union (choHoMap a) (choHoMap b)
        }

updateChoHo cho = cho { choHo = ho, choVarMap = varMap } where
    ho = hoBuild_u (hoEs_u f) . mconcat . Map.elems $ choHoMap cho
    f ds = runIdentity $ annotateDs mmap  (\_ -> return) (\_ -> return) (\_ -> return) (map g ds) where
        mmap = sfilter (\(k,_) -> (k `notElem` (map (tvrIdent . fst) ds))) varMap
    g (t,e) = case mlookup (tvrIdent t) varMap of
        Just (Just (EVar t')) -> (t',e)
        _ -> (t,e)
    varMap = fmap (\c -> Just (EVar $ combHead c)) $ choCombinators cho

-- this will have to merge rules and properties.
mergeChoCombinators :: IdMap Comb -> IdMap Comb -> IdMap Comb
mergeChoCombinators x y = unionWith f x y where
    f c1 c2 = combRules_s (combRules c1 `Data.List.union` combRules c2) . combHead_s (merge (combHead c1) (combHead c2)) $ c1
    merge ta tb = ta { tvrInfo = minfo' }   where
        minfo = tvrInfo ta `mappend` tvrInfo tb
        minfo' = dex (undefined :: Properties) $ minfo
        dex dummy y = g (Info.lookup (tvrInfo tb) `asTypeOf` Just dummy) where
            g Nothing = y
            g (Just x) = Info.insertWith mappend x y
