module Ho.Binary() where


import Control.Monad
import Data.Binary
import Data.Version

import Ho.Type
import Support.MapBinaryInstance
import Name.Binary()



instance Binary HoBuild where
    put (HoBuild ae af ag ah ai ak al am) = do
	    putMap ae
	    put af
	    put ag
	    put ah
	    put ai
	    put ak
	    put al
	    put am
    get = do
    ae <- getMap
    af <- get
    ag <- get
    ah <- get
    ai <- get
    ak <- get
    al <- get
    am <- get
    return (HoBuild ae af ag ah ai ak al am)


instance Binary HoExp where
    put (HoExp ac ad) = do
	    put ac
	    putMap ad
    get = do
    ac <- get
    ad <- getMap
    return (HoExp ac ad)



instance Data.Binary.Binary HoHeader where
    put (HoHeader aa ab ac ad ae) = do
	    Data.Binary.put aa
	    Data.Binary.put ab
	    Data.Binary.put ac
	    Data.Binary.put ad
	    Data.Binary.put ae
    get = do
    aa <- get
    ab <- get
    ac <- get
    ad <- get
    ae <- get
    return (HoHeader aa ab ac ad ae)

instance Data.Binary.Binary HoIDeps where
    put (HoIDeps aa ab ac) = do
	    Data.Binary.put aa
	    Data.Binary.put ab
	    Data.Binary.put ac
    get = do
    aa <- get
    ab <- get
    ac <- get
    return (HoIDeps aa ab ac)

instance Data.Binary.Binary HoLib where
    put (HoLib aa ab ac) = do
	    Data.Binary.put aa
	    Data.Binary.put ab
	    Data.Binary.put ac
    get = do
    aa <- get
    ab <- get
    ac <- get
    return (HoLib aa ab ac)



instance Binary Data.Version.Version where
    put (Version a b) = put a >> put b 
    get = liftM2 Version get get

