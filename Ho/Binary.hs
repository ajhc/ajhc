module Ho.Binary where


import Control.Monad
import Data.Binary

import Ho.Type
import HsSyn(Module)
import MapBinaryInstance
import Name.Binary()
import PackedString(PackedString)
import Util.SHA1 as SHA1

data HoHeader = HoHeader {
    -- * Haskell Source files depended on
    hohDepends    :: [(Module,SHA1.Hash)],
    -- * Other objects depended on
    hohModDepends :: [(Module,SHA1.Hash)],
    -- * my sha1 id
    hohHash       :: SHA1.Hash,
    -- * metainformation, filled for hl-files, empty for normal objects.
    hohMetaInfo   :: [(PackedString,PackedString)]
    }

instance Binary ABCDE where
    put (ABCDE a b c d e) = put a >> put b >> put c >> put d >> put e
    get = return ABCDE `ap` get `ap` get `ap` get `ap` get `ap` get

instance Binary HoHeader where
    put (HoHeader aa ab ac ad) = do
	    put aa
	    put ab
	    put ac
	    put ad
    get = do
        aa <- get
        ab <- get
        ac <- get
        ad <- get
        return (HoHeader aa ab ac ad)

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


