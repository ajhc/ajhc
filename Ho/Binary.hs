module Ho.Binary where


import Ho.Type
import Data.Binary
import PackedString(PackedString)
import HsSyn(Module)
import Name.Binary
import MapBinaryInstance

data HoHeader = HoHeader {
    -- * Haskell Source files depended on
    hohDepends    :: [FileDep],
    -- * Other objects depended on
    hohModDepends :: [(Module,FileDep)],
    -- * metainformation, filled for hl-files, empty for normal objects.
    hohMetaInfo   :: [(PackedString,PackedString)]
    }

instance Binary HoHeader where
    put (HoHeader ab ac ad) = do
	    put ab
	    put ac
	    put ad
    get = do
    ab <- get
    ac <- get
    ad <- get
    return (HoHeader ab ac ad)

instance Binary Ho where
    put (Ho aa ab ac ad ae af ag ah ai aj ak al am an) = do
	    put aa
	    put ab
	    put ac
	    putMap ad
	    putMap ae
	    put af
	    put ag
	    put ah
	    put ai
	    put aj
	    put ak
	    putMap al
	    put am
	    put an
    get = do
    aa <- get
    ab <- get
    ac <- get
    ad <- getMap
    ae <- getMap
    af <- get
    ag <- get
    ah <- get
    ai <- get
    aj <- get
    ak <- get
    al <- getMap
    am <- get
    an <- get
    return (Ho aa ab ac ad ae af ag ah ai aj ak al am an)

instance Binary FileDep where
    put (FileDep aa ab ac ad ae) = do
        put aa
        put ab
        put ac
        put ad
        put ae
    get = do
        aa <- get
        ab <- get
        ac <- get
        ad <- get
        ae <- get
        return (FileDep aa ab ac ad ae)

