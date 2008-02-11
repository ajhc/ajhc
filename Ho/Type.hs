module Ho.Type where

import Data.Monoid
import qualified Data.Map as Map

import StringTable.Atom
import DataConstructors(DataTable)
import E.E(TVr,E)
import E.Rules(Rules)
import E.TypeCheck()
import FrontEnd.Class(ClassHierarchy)
import FrontEnd.Infix(FixityMap)
import FrontEnd.KindInfer(KindEnv)
import FrontEnd.SrcLoc(SrcLoc)
import FrontEnd.Tc.Type(Type())
import HsSyn(Module)
import Support.MapBinaryInstance()
import Name.Id
import Name.Name(Name)
import TypeSynonyms(TypeSynonyms)
import PackedString
import qualified Support.MD5 as MD5


-- the collected information that is passed around
data CollectedHo = CollectedHo {
    choFiles :: Map.Map Module MD5.Hash,
    choModules :: Map.Map Module MD5.Hash,
    choExternalNames :: IdSet,
    choVarMap :: IdMap (Maybe E),
    choHo :: Ho
    }

instance Monoid CollectedHo where
    mempty = collectedHo
    a `mappend` b = CollectedHo {
        choFiles = choFiles a `mappend` choFiles b,
        choModules = choModules a `mappend` choModules b,
        choExternalNames = choExternalNames a `mappend` choExternalNames b,
        choVarMap = choVarMap a `mappend` choVarMap b,
        choHo = choHo a `mappend` choHo b
        }

choDataTable cho = hoDataTable $ choHo cho

collectedHo :: CollectedHo
collectedHo = CollectedHo { choFiles = mempty, choModules = mempty, choExternalNames = mempty, choHo = mempty, choVarMap = mempty }

data HoHeader = HoHeader {
    -- * my sha1 id
    hohHash       :: MD5.Hash,
    -- * Haskell Source files depended on
    hohDepends    :: [(Module,MD5.Hash)],
    -- * Other objects depended on
    hohModDepends :: [(Module,MD5.Hash)],
    -- * metainformation, filled for hl-files, empty for normal objects.
    hohMetaInfo   :: [(Atom,PackedString)]
    }

-- The raw data as it appears on disk
data Ho = Ho {
    -- * libraries depended on
    hoExports :: Map.Map Module [Name],
    hoDefs :: Map.Map Name (SrcLoc,[Name]),
    hoAssumps :: Map.Map Name Type,        -- used for typechecking
    hoFixities :: FixityMap,
    hoKinds :: KindEnv,                      -- used for typechecking
    hoClassHierarchy :: ClassHierarchy,
    hoTypeSynonyms :: TypeSynonyms,
    -- Filled in by E generation
    hoDataTable :: DataTable,
    hoEs :: [(TVr,E)],
    hoRules :: Rules
    }

instance Monoid Ho where
    mempty = Ho mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty
    mappend a b = Ho {
        hoExports = hoExports a `mappend` hoExports b,
        hoDefs = hoDefs a `mappend` hoDefs b,
        hoAssumps = hoAssumps a `mappend` hoAssumps b,
        hoFixities = hoFixities a `mappend` hoFixities b,
        hoKinds = hoKinds a `mappend` hoKinds b,
        hoClassHierarchy = hoClassHierarchy a `mappend` hoClassHierarchy b,
        hoTypeSynonyms = hoTypeSynonyms a `mappend` hoTypeSynonyms b,
        hoDataTable = hoDataTable a `mappend` hoDataTable b,
        hoEs = hoEs a `mappend` hoEs b,
        hoRules = hoRules a `mappend` hoRules b
    }



