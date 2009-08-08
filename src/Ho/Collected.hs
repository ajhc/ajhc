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

import Data.Monoid
import Control.Monad.Identity
import Data.List

import Util.SetLike
import Ho.Type
import E.E
import Name.Name
import DataConstructors
import Info.Types
import E.Annotate
import qualified Info.Info as Info
import qualified Data.Map as Map


choDataTable = hoDataTable . hoBuild . choHo
choClassHierarchy = hoClassHierarchy . hoTcInfo . choHo
choTypeSynonyms = hoTypeSynonyms . hoTcInfo . choHo
choFixities = hoFixities . hoTcInfo . choHo
choAssumps = hoAssumps . hoTcInfo . choHo
choRules = hoRules . hoBuild . choHo
choEs cho = [ (combHead c,combBody c) | c <- melems $  choCombinators cho]

instance Monoid CollectedHo where
    mempty = updateChoHo CollectedHo {
        choExternalNames = mempty,
        choOrphanRules = mempty,
        choHoMap = Map.singleton primModule pho,
        choCombinators = mempty,
        choHo = error "choHo-a",
        choVarMap = mempty
        } where pho = mempty { hoBuild = mempty { hoDataTable = dataTablePrims } }
    a `mappend` b = updateChoHo CollectedHo {
        choExternalNames = choExternalNames a `mappend` choExternalNames b,
        choVarMap = choVarMap a `mergeChoVarMaps` choVarMap b,
        choOrphanRules = choOrphanRules a `mappend` choOrphanRules b,
        choCombinators = choCombinators a `mergeChoCombinators` choCombinators b,
        choHo = error "choHo-b",
        choHoMap = Map.union (choHoMap a) (choHoMap b)
        }

updateChoHo cho = cho { choHo = ho } where
    ho = hoBuild_u (hoEs_u f) . mconcat . Map.elems $ choHoMap cho
    f ds = runIdentity $ annotateDs mmap  (\_ -> return) (\_ -> return) (\_ -> return) (map g ds) where
        mmap = mfilterWithKey (\k _ -> (k `notElem` (map (tvrIdent . fst) ds))) (choVarMap cho)
    g (t,e) = case mlookup (tvrIdent t) (choVarMap cho) of
        Just (Just (EVar t')) -> (t',e)
        _ -> (t,e)
 --   ae = runIdentity . annotate (choVarMap cho) (\_ -> return) (\_ -> return) (\_ -> return)

-- this will have to merge rules and properties.
mergeChoVarMaps :: IdMap (Maybe E) -> IdMap (Maybe E) -> IdMap (Maybe E)
mergeChoVarMaps x y = munionWith f x y where
    f (Just (EVar x)) (Just (EVar y)) = Just . EVar $ merge x y
    f x y = error "mergeChoVarMaps: bad merge."
    merge ta tb = ta { tvrInfo = minfo' }   where
        minfo = tvrInfo ta `mappend` tvrInfo tb
        minfo' = dex (undefined :: Properties) $ minfo
        dex dummy y = g (Info.lookup (tvrInfo tb) `asTypeOf` Just dummy) where
            g Nothing = y
            g (Just x) = Info.insertWith mappend x y

-- this will have to merge rules and properties.
mergeChoCombinators :: IdMap Comb -> IdMap Comb -> IdMap Comb
mergeChoCombinators x y = munionWith f x y where
    f c1 c2 = combRules_s  (combRules c1 `Data.List.union`  combRules c2) . combHead_s (merge (combHead c1) (combHead c2)) $ c1
    merge ta tb = ta { tvrInfo = minfo' }   where
        minfo = tvrInfo ta `mappend` tvrInfo tb
        minfo' = dex (undefined :: Properties) $ minfo
        dex dummy y = g (Info.lookup (tvrInfo tb) `asTypeOf` Just dummy) where
            g Nothing = y
            g (Just x) = Info.insertWith mappend x y

