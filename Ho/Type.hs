module Ho.Type where

import Data.Monoid
import qualified Data.Map as Map

import Atom(Atom)
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
import MapBinaryInstance()
import Info.Types
import Name.Id
import Name.Name(Name)
import Util.SetLike
import TypeSynonyms(TypeSynonyms)



type CheckSum = String
type LibraryName = String

-- the collected information that is passed around
data CollectedHo = CollectedHo {
    choExternalNames :: IdSet,
    choVarMap :: IdMap (Maybe E),
    choHo :: Ho
    }

instance Monoid CollectedHo where
    mempty = collectedHo
    a `mappend` b = CollectedHo {
        choExternalNames = choExternalNames a `mappend` choExternalNames b,
        choVarMap = choVarMap a `mappend` choVarMap b,
        choHo = choHo a `mappend` choHo b
        }

choDataTable cho = hoDataTable $ choHo cho

collectedHo :: CollectedHo
collectedHo = CollectedHo { choExternalNames = mempty, choHo = mempty, choVarMap = mempty }

-- The raw data as ut appears on disk
data Ho = Ho {
    -- filled in by front end
    hoModules :: Map.Map Module (Either FileDep (LibraryName,CheckSum)),     -- ^ Map of module to ho file, This never actually ends up in the binary file on disk, but is filled in when the file is read, libraries have no non-library dependencies.
    -- * libraries depended on
    hoLibraries :: Map.Map LibraryName CheckSum,
    hoExports :: Map.Map Module [Name],
    hoDefs :: Map.Map Name (SrcLoc,[Name]),
    hoAssumps :: Map.Map Name Type,        -- used for typechecking
    hoFixities :: FixityMap,
    hoKinds :: KindEnv,                      -- used for typechecking
    hoClassHierarchy :: ClassHierarchy,
    hoTypeSynonyms :: TypeSynonyms,
    hoProps :: IdMap Properties,
    -- Filled in by E generation
    hoDataTable :: DataTable,
    hoEs :: Map.Map Name (TVr,E),
    hoRules :: Rules,
    hoUsedIds :: IdSet
    }

instance Monoid Ho where
    mempty = Ho mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty
    mappend a b = Ho {
        hoModules = hoModules a `mappend` hoModules b,
        hoLibraries = hoLibraries a `mappend` hoLibraries b,
        hoExports = hoExports a `mappend` hoExports b,
        hoDefs = hoDefs a `mappend` hoDefs b,
        hoAssumps = hoAssumps a `mappend` hoAssumps b,
        hoFixities = hoFixities a `mappend` hoFixities b,
        hoKinds = hoKinds a `mappend` hoKinds b,
        hoClassHierarchy = hoClassHierarchy a `mappend` hoClassHierarchy b,
        hoTypeSynonyms = hoTypeSynonyms a `mappend` hoTypeSynonyms b,
        hoProps = munionWith mappend (hoProps a) (hoProps b),
        hoDataTable = hoDataTable a `mappend` hoDataTable b,
        hoEs = hoEs a `mappend` hoEs b,
        hoRules = hoRules a `mappend` hoRules b,
        hoUsedIds = hoUsedIds a `mappend` hoUsedIds b
    }



-- | Contains hopefully enough meta-info to uniquely identify a file
-- independent of its name.

data FileDep = FileDep {
    fileName :: Atom,
    fileModifyTime :: Int,
    fileDeviceID :: Atom,
    fileFileID :: Int,
    fileFileSize :: Int
    } deriving(Show)





--  Imported from other files :-
