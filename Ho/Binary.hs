module Ho.Binary() where


import Control.Monad
import Data.Binary

import Ho.Type
import Support.MapBinaryInstance
import Name.Binary()


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
    put (Ho ac ad ae af ag ah ai ak al am) = do
	    put ac
	    putMap ad
	    putMap ae
	    put af
	    put ag
	    put ah
	    put ai
	    put ak
	    put al
	    put am
    get = do
    ac <- get
    ad <- getMap
    ae <- getMap
    af <- get
    ag <- get
    ah <- get
    ai <- get
    ak <- get
    al <- get
    am <- get
    return (Ho ac ad ae af ag ah ai ak al am)


