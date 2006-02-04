module Ho.Type where

import Binary

import Atom(Atom)
import Class(ClassHierarchy)
import DataConstructors(DataTable)
import E.E(TVr,Id,E)
import E.Rules(Rules)
import E.TypeCheck()
import FrontEnd.SrcLoc(SrcLoc)
import FrontEnd.Infix(FixityMap)
import Ho.LibraryMap(LibraryName,CheckSum)
import HsSyn(Module)
import FrontEnd.KindInfer(KindEnv)
import MapBinaryInstance()
import Name.Name(Name)
import PackedString(PackedString)
import Representation(Scheme)
import TypeSynonyms(TypeSynonyms)


import Data.Monoid
import qualified Data.Map as Map
import qualified Data.Set as Set


data HoHeader = HoHeader {
    -- * FIXME - is this used for something?
    hohGeneration :: Int,
    -- * Haskell Source files depended on
    hohDepends    :: [FileDep],             
    -- * Other objects depended on
    hohModDepends :: [(Module,FileDep)],
    -- * metainformation, filled for hl-files, empty for normal objects.
    hohMetaInfo   :: [(PackedString,PackedString)]
    }
    {-! derive: GhcBinary !-}

data Ho = Ho {
    -- filled in by front end
    hoModules :: Map.Map Module FileDep,     -- ^ Map of module to ho file, This never actually ends up in the binary file on disk, but is filled in when the file is read.
    -- * libraries depended on
    hoLibraries :: Map.Map LibraryName CheckSum,
    hoExports :: Map.Map Module [Name],
    hoDefs :: Map.Map Name (SrcLoc,[Name]),
    hoAssumps :: Map.Map Name Scheme,        -- used for typechecking
    hoFixities :: FixityMap,
    hoKinds :: KindEnv,                      -- used for typechecking
    hoClassHierarchy :: ClassHierarchy,
    hoTypeSynonyms :: TypeSynonyms,
    hoProps :: Map.Map Name [Atom],
    -- Filled in by E generation
    hoDataTable :: DataTable,
    hoEs :: Map.Map Name (TVr,E),
    hoRules :: Rules,
    hoUsedIds :: Set.Set Id
    }
    {-! derive: GhcBinary, Monoid !-}


-- | Contains hopefully enough meta-info to uniquely identify a file
-- independent of its name.

data FileDep = FileDep {
    fileName :: Atom,
    fileModifyTime :: Int,
    fileDeviceID :: Atom,
    fileFileID :: Int,
    fileFileSize :: Int
    } deriving(Show)
    {-! derive: GhcBinary !-}
