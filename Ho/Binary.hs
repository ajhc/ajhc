module Ho.Binary where


import Ho.Type
import Binary
import PackedString(PackedString)
import HsSyn(Module)
import Name.Binary

data HoHeader = HoHeader {
    -- * Haskell Source files depended on
    hohDepends    :: [FileDep],
    -- * Other objects depended on
    hohModDepends :: [(Module,FileDep)],
    -- * metainformation, filled for hl-files, empty for normal objects.
    hohMetaInfo   :: [(PackedString,PackedString)]
    }

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

instance Binary FileDep where
    put_ bh (FileDep aa ab ac ad ae) = do
        put_ bh aa
        put_ bh ab
        put_ bh ac
        put_ bh ad
        put_ bh ae
    get bh = do
        aa <- get bh
        ab <- get bh
        ac <- get bh
        ad <- get bh
        ae <- get bh
        return (FileDep aa ab ac ad ae)

