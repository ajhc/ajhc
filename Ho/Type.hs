module Ho.Type where

import Data.Monoid
import qualified Data.Map as Map

import DataConstructors(DataTable,dataTablePrims)
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
import Data.Binary
import qualified Support.MD5 as MD5

type SourceHash = MD5.Hash
type HoHash     = MD5.Hash

-- the collected information that is passed around
data CollectedHo = CollectedHo {
    -- this is a list of external names that are valid but that we may not know anything else about
    -- it is used to recognize invalid ids.
    choExternalNames :: IdSet,
    -- this is a map of ids to their full TVrs with all rules and whatnot attached.
    choVarMap :: IdMap (Maybe E),
    -- these are rules that may need to be retroactively applied to other modules
    choOrphanRules :: Rules,
    -- the hos
    choHoMap :: Map.Map String Ho
    }
    {-! derive: update !-}


-- this is the immutable information about modules that depnends only on their contents
-- it can be trusted even if the ho file itself is out of date.
newtype HoIDeps = HoIDeps {
    hoIDeps :: Map.Map SourceHash (Module,[Module])
    }
    deriving(Binary)

data HoHeader = HoHeader {
    -- * my sha1 id
    hohHash       :: HoHash,
    -- * Haskell Source files depended on
    hohDepends    :: [(Module,SourceHash)],
    -- * Other objects depended on to be considered up to date.
    hohModDepends :: [HoHash],
    -- * metainformation, filled for hl-files, empty for normal objects.
    hohMetaInfo   :: [(String,PackedString)]
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



