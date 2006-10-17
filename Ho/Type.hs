module Ho.Type where

import Binary

import Atom(Atom)
import FrontEnd.Class(ClassHierarchy)
import DataConstructors(DataTable)
import E.E(TVr,E)
import E.Rules(Rules)
import E.TypeCheck()
import FrontEnd.SrcLoc(SrcLoc)
import FrontEnd.Infix(FixityMap)
import HsSyn(Module)
import FrontEnd.KindInfer(KindEnv)
import MapBinaryInstance()
import Name.Name(Name)
import PackedString(PackedString)
import Representation(Type())
import TypeSynonyms(TypeSynonyms)
import Name.Id


import Data.Monoid
import qualified Data.Map as Map
import qualified Data.Set as Set

type CheckSum = String
type LibraryName= String


data HoHeader = HoHeader {
    -- * Haskell Source files depended on
    hohDepends    :: [FileDep],
    -- * Other objects depended on
    hohModDepends :: [(Module,FileDep)],
    -- * metainformation, filled for hl-files, empty for normal objects.
    hohMetaInfo   :: [(PackedString,PackedString)]
    }

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
    hoProps :: Map.Map Name [Atom],
    -- Filled in by E generation
    hoDataTable :: DataTable,
    hoEs :: Map.Map Name (TVr,E),
    hoRules :: Rules,
    hoUsedIds :: Set.Set Id
    }
    {-! derive: Monoid !-}


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



instance Binary HoHeader where
    put_ bh (HoHeader ab ac ad) = do
	    put_ bh ab
	    lazyPut bh ac
	    lazyPut bh ad
    get bh = do
    ab <- get bh
    ac <- lazyGet bh
    ad <- lazyGet bh
    return (HoHeader ab ac ad)

instance Binary Ho where
    put_ bh (Ho aa ab ac ad ae af ag ah ai aj ak al am an) = do
	    lazyPut bh aa
	    lazyPut bh ab
	    lazyPut bh ac
	    lazyPut bh ad
	    lazyPut bh ae
	    lazyPut bh af
	    lazyPut bh ag
	    lazyPut bh ah
	    lazyPut bh ai
	    lazyPut bh aj
	    lazyPut bh ak
	    lazyPut bh al
	    lazyPut bh am
	    lazyPut bh an
    get bh = do
    aa <- lazyGet bh
    ab <- lazyGet bh
    ac <- lazyGet bh
    ad <- lazyGet bh
    ae <- lazyGet bh
    af <- lazyGet bh
    ag <- lazyGet bh
    ah <- lazyGet bh
    ai <- lazyGet bh
    aj <- lazyGet bh
    ak <- lazyGet bh
    al <- lazyGet bh
    am <- lazyGet bh
    an <- lazyGet bh
    return (Ho aa ab ac ad ae af ag ah ai aj ak al am an)


--  Imported from other files :-
