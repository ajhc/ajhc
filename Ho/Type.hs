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
    {-! derive: update !-}

instance Monoid CollectedHo where
    mempty = collectedHo
    a `mappend` b = CollectedHo {
        choFiles = choFiles a `mappend` choFiles b,
        choModules = choModules a `mappend` choModules b,
        choExternalNames = choExternalNames a `mappend` choExternalNames b,
        choVarMap = choVarMap a `mappend` choVarMap b,
        choHo = choHo a `mappend` choHo b
        }

choDataTable cho = hoDataTable $ hoBuild (choHo cho)

collectedHo :: CollectedHo
collectedHo = CollectedHo { choFiles = mempty, choModules = mempty, choExternalNames = mempty, choHo = mempty, choVarMap = mempty }


-- this is the immutable information about modules that depnends only on their contents
-- it can be trusted even if the ho file itself is out of date.
data HoIDeps = HoIDeps {
    hoIDeps :: Map.Map MD5.Hash (Module,[Module])
    }

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

-- data only needed for name resolution
data HoExp = HoExp {
    hoExports :: Map.Map Module [Name],
    hoDefs :: Map.Map Name (SrcLoc,[Name])
    }


data HoBuild = HoBuild {
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
    {-! derive: update !-}

data Ho = Ho {
    hoExp :: HoExp,
    hoBuild :: HoBuild
    }
    {-! derive: update !-}

instance Monoid Ho where
    mempty = Ho mempty mempty
    mappend a b = Ho {
        hoExp = hoExp a `mappend` hoExp b,
        hoBuild = hoBuild a `mappend` hoBuild b
    }

instance Monoid HoExp where
    mempty = HoExp mempty mempty
    mappend a b = HoExp {
        hoExports = hoExports a `mappend` hoExports b,
        hoDefs = hoDefs a `mappend` hoDefs b
    }

instance Monoid HoBuild where
    mempty = HoBuild mempty mempty mempty mempty mempty mempty mempty mempty
    mappend a b = HoBuild {
        hoAssumps = hoAssumps a `mappend` hoAssumps b,
        hoFixities = hoFixities a `mappend` hoFixities b,
        hoKinds = hoKinds a `mappend` hoKinds b,
        hoClassHierarchy = hoClassHierarchy a `mappend` hoClassHierarchy b,
        hoTypeSynonyms = hoTypeSynonyms a `mappend` hoTypeSynonyms b,
        hoDataTable = hoDataTable a `mappend` hoDataTable b,
        hoEs = hoEs a `mappend` hoEs b,
        hoRules = hoRules a `mappend` hoRules b
    }



