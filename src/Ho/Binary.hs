module Ho.Binary() where


import Control.Monad
import Data.Binary
import Data.Version

import Ho.Type
import Support.MapBinaryInstance
import Name.Binary()
import FrontEnd.Rename(FieldMap(..))






instance Binary FieldMap where
    put (FieldMap ac ad) = do
	    putMap ac
	    putMap ad
    get = do
    ac <- getMap
    ad <- getMap
    return (FieldMap ac ad)



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

{-
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
    -}



instance Binary Data.Version.Version where
    put (Version a b) = put a >> put b 
    get = liftM2 Version get get



instance Data.Binary.Binary HoTcInfo where
    put (HoTcInfo aa ab ac ad ae af ag ah) = do
	    Data.Binary.put aa
	    putMap ab
	    putMap ac
	    Data.Binary.put ad
	    Data.Binary.put ae
	    Data.Binary.put af
	    Data.Binary.put ag
	    Data.Binary.put ah
    get = do
    aa <- get
    ab <- getMap
    ac <- getMap
    ad <- get
    ae <- get
    af <- get
    ag <- get
    ah <- get
    return (HoTcInfo aa ab ac ad ae af ag ah)


instance Data.Binary.Binary HoBuild where
    put (HoBuild aa ab ac) = do
	    Data.Binary.put aa
	    Data.Binary.put ab
	    Data.Binary.put ac
    get = do
    aa <- get
    ab <- get
    ac <- get
    return (HoBuild aa ab ac)

