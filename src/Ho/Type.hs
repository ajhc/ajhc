module Ho.Type where

import Data.Monoid
import qualified Data.Map as Map

import DataConstructors(DataTable)
import E.Rules(Rules)
import E.Type
import E.TypeCheck()
import FrontEnd.Class(ClassHierarchy)
import FrontEnd.Infix(FixityMap)
import FrontEnd.KindInfer(KindEnv)
import FrontEnd.SrcLoc(SrcLoc)
import FrontEnd.Tc.Type(Type())
import FrontEnd.HsSyn(Module)
import Support.MapBinaryInstance()
import Name.Id
import Name.Name(Name)
import FrontEnd.TypeSynonyms(TypeSynonyms)
import PackedString
import Data.Binary
import qualified Support.MD5 as MD5
import Data.Version
import FrontEnd.Rename(FieldMap())


-- A SourceHash is the hash of a specific file, it is associated with a
-- specific 'Module' that said file implements.
type SourceHash = MD5.Hash
-- HoHash is a unique identifier for a ho file or library.
type HoHash     = MD5.Hash


-- while a 'Module' is a single Module associated with a single haskell source
-- file, a 'ModuleGroup' identifies a group of mutually recursive modules.
-- Generally it is chosen from among the Modules making up the group, but the
-- specific choice has no other meaning. We could use the HoHash, but for readability
-- reasons when debugging it makes more sense to choose an arbitrary Module.
type ModuleGroup = Module

-- the collected information that is passed around
-- this is not stored in any file, but is what is collected from the ho files.
data CollectedHo = CollectedHo {
    -- this is a list of external names that are valid but that we may not know
    -- anything else about it is used to recognize invalid ids.
    choExternalNames :: IdSet, choCombinators  :: IdMap Comb,
    -- this is a map of ids to their full TVrs with all rules and whatnot
    -- attached.
    choVarMap :: IdMap (Maybe E),
    -- these are rules that may need to be retroactively applied to other
    -- modules
    choOrphanRules :: Rules,
    -- the hos
    choHoMap :: Map.Map String Ho }
    {-! derive: update !-}


-- The header contains basic information about the file, it should be enough to determine whether
-- we can discard the file right away or consider it further.

data HoHeader = HoHeader {
    -- * the version of the file format. it comes first so we don't try to read data that may be in a different format.
    hohVersion  :: Int,
    -- * my sha1 id
    hohHash     :: HoHash,
    -- * the human readable name, either the ModuleGroup or the library name and version.
    hohName     :: Either ModuleGroup (PackedString,Version),
    -- * library dependencies
    hohLibDeps  :: [(PackedString,HoHash)],
    -- * arch dependencies, these say whether the file is specialized for a
    -- given arch.
    hohArchDeps :: [(PackedString,PackedString)]
    }

-- These are the dependencies needed to check if a ho file is up to date.  it
-- only appears in ho files as hl files do not have source code to check
-- against or depend on anything but other libraries.
data HoIDeps = HoIDeps {
    -- * modules depended on indexed by a hash of the source.
    hoIDeps :: Map.Map SourceHash (Module,[Module]),
    -- * Haskell Source files depended on
    hoDepends    :: [(Module,SourceHash)],
    -- * Other objects depended on to be considered up to date.
    hoModDepends :: [HoHash] }

data HoLib = HoLib {
    -- * arbitrary metainformation such as library author, web site, etc.
    hoMetaInfo   :: [(String,PackedString)],
    hoModuleMap  :: Map.Map Module ModuleGroup,
--    hoModuleDeps :: Map.Map ModuleGroup [ModuleGroup],
    hoHiddenModules :: [Module]
    }

data CollectedTc = CollectedTc {
    ctcClassHierarchy :: ClassHierarchy,
    ctcInfo :: HoTcInfo
    }
    {-! derive: update, Monoid !-}

-- data only needed for type checking.
data HoTcInfo = HoTcInfo {
    hoExports :: Map.Map Module [Name],
    hoDefs :: Map.Map Name (SrcLoc,[Name]),
    hoAssumps :: Map.Map Name Type,        -- used for typechecking
    hoFixities :: FixityMap,
    hoKinds :: KindEnv,                    -- used for typechecking
    hoTypeSynonyms :: TypeSynonyms,
    hoClassHierarchy :: ClassHierarchy,
    hoFieldMap :: FieldMap
    }
    {-! derive: update, Monoid !-}

-- classes and the datatable are used by both the core transformations and
-- front end typechecking so they are in their own section

-- this needs datatable
newtype HoClass = HoClass {
    hoClasses :: Map.Map Module ClassHierarchy
    } deriving(Binary,Monoid)

data HoBuild = HoBuild {
    -- Filled in by E generation
    hoDataTable :: DataTable,
    hoEs :: [(TVr,E)],
    hoRules :: Rules
    }
    {-! derive: update, Monoid !-}

data Ho = Ho {
    hoModuleGroup :: ModuleGroup,
    hoTcInfo :: HoTcInfo,
    hoBuild :: HoBuild
    }
    {-! derive: update !-}

instance Monoid Ho where
    mempty = Ho (error "unknown module group") mempty mempty
    mappend ha hb = Ho (hoModuleGroup ha) (hoTcInfo ha `mappend` hoTcInfo hb) (hoBuild ha `mappend` hoBuild hb)

{-
instance Monoid Ho where
    mempty = Ho mempty mempty
    mappend a b = Ho {
        hoTcInfo = hoTcInfo a `mappend` hoTcInfo b,
        hoBuild = hoBuild a `mappend` hoBuild b
    }

instance Monoid HoTcInfo where
    mempty = HoTcInfo mempty mempty
    mappend a b = HoTcInfo {
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


 -}
