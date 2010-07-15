module E.Binary() where

import Data.Binary
import E.Type
import Monad
import Name.Binary()
import FrontEnd.HsSyn()
import {-# SOURCE #-} Info.Binary(putInfo,getInfo)




instance Binary TVr where
    put TVr { tvrIdent = eid, tvrType =  e, tvrInfo = nf} = do
        put eid
        put e
        putInfo nf
    get  = do
        x <- get
        e <- get
        nf <- getInfo
        return $ TVr x e nf


instance Data.Binary.Binary RuleType where
    put RuleSpecialization = do
	    Data.Binary.putWord8 0
    put RuleUser = do
	    Data.Binary.putWord8 1
    put RuleCatalyst = do
	    Data.Binary.putWord8 2
    get = do
	    h <- Data.Binary.getWord8
	    case h of
	      0 -> do
		    return RuleSpecialization
	      1 -> do
		    return RuleUser
	      2 -> do
		    return RuleCatalyst
	      _ -> fail "invalid binary data found"

instance Data.Binary.Binary Rule where
    put (Rule aa ab ac ad ae af ag ah) = do
	    Data.Binary.put aa
	    Data.Binary.put ab
	    Data.Binary.put ac
	    Data.Binary.put ad
	    Data.Binary.put ae
	    Data.Binary.put af
	    Data.Binary.put ag
	    Data.Binary.put ah
    get = do
    aa <- get
    ab <- get
    ac <- get
    ad <- get
    ae <- get
    af <- get
    ag <- get
    ah <- get
    return (Rule aa ab ac ad ae af ag ah)

instance Data.Binary.Binary ARules where
    put (ARules aa ab) = do
	    Data.Binary.put aa
	    Data.Binary.put ab
    get = do
    aa <- get
    ab <- get
    return (ARules aa ab)


instance (Data.Binary.Binary e,
	  Data.Binary.Binary t) => Data.Binary.Binary (Lit e t) where
    put (LitInt aa ab) = do
	    Data.Binary.putWord8 0
	    Data.Binary.put aa
	    Data.Binary.put ab
    put (LitCons ac ad ae af) = do
	    Data.Binary.putWord8 1
	    Data.Binary.put ac
	    Data.Binary.put ad
	    Data.Binary.put ae
	    Data.Binary.put af
    get = do
	    h <- Data.Binary.getWord8
	    case h of
	      0 -> do
		    aa <- Data.Binary.get
		    ab <- Data.Binary.get
		    return (LitInt aa ab)
	      1 -> do
		    ac <- Data.Binary.get
		    ad <- Data.Binary.get
		    ae <- Data.Binary.get
		    af <- Data.Binary.get
		    return (LitCons ac ad ae af)
	      _ -> fail "invalid binary data found"


instance Data.Binary.Binary ESort where
    put EStar = do
	    Data.Binary.putWord8 0
    put EBang = do
	    Data.Binary.putWord8 1
    put EHash = do
	    Data.Binary.putWord8 2
    put ETuple = do
	    Data.Binary.putWord8 3
    put EHashHash = do
	    Data.Binary.putWord8 4
    put EStarStar = do
	    Data.Binary.putWord8 5
    put (ESortNamed aa) = do
	    Data.Binary.putWord8 6
	    Data.Binary.put aa
    get = do
	    h <- Data.Binary.getWord8
	    case h of
	      0 -> do
		    return EStar
	      1 -> do
		    return EBang
	      2 -> do
		    return EHash
	      3 -> do
		    return ETuple
	      4 -> do
		    return EHashHash
	      5 -> do
		    return EStarStar
	      6 -> do
		    aa <- Data.Binary.get
		    return (ESortNamed aa)
	      _ -> fail "invalid binary data found"


instance Data.Binary.Binary E where
    put (EAp aa ab) = do
	    Data.Binary.putWord8 0
	    Data.Binary.put aa
	    Data.Binary.put ab
    put (ELam ac ad) = do
	    Data.Binary.putWord8 1
	    Data.Binary.put ac
	    Data.Binary.put ad
    put (EPi ae af) = do
	    Data.Binary.putWord8 2
	    Data.Binary.put ae
	    Data.Binary.put af
    put (EVar ag) = do
	    Data.Binary.putWord8 3
	    Data.Binary.put ag
    put Unknown = do
	    Data.Binary.putWord8 4
    put (ESort ah) = do
	    Data.Binary.putWord8 5
	    Data.Binary.put ah
    put (ELit ai) = do
	    Data.Binary.putWord8 6
	    Data.Binary.put ai
    put (ELetRec aj ak) = do
	    Data.Binary.putWord8 7
	    Data.Binary.put aj
	    Data.Binary.put ak
    put (EPrim al am an) = do
	    Data.Binary.putWord8 8
	    Data.Binary.put al
	    Data.Binary.put am
	    Data.Binary.put an
    put (EError ao ap) = do
	    Data.Binary.putWord8 9
	    Data.Binary.put ao
	    Data.Binary.put ap
    put (ECase aq ar as at au av) = do
	    Data.Binary.putWord8 10
	    Data.Binary.put aq
	    Data.Binary.put ar
	    Data.Binary.put as
	    Data.Binary.put at
	    Data.Binary.put au
	    Data.Binary.put av
    get = do
	    h <- Data.Binary.getWord8
	    case h of
	      0 -> do
		    aa <- Data.Binary.get
		    ab <- Data.Binary.get
		    return (EAp aa ab)
	      1 -> do
		    ac <- Data.Binary.get
		    ad <- Data.Binary.get
		    return (ELam ac ad)
	      2 -> do
		    ae <- Data.Binary.get
		    af <- Data.Binary.get
		    return (EPi ae af)
	      3 -> do
		    ag <- Data.Binary.get
		    return (EVar ag)
	      4 -> do
		    return Unknown
	      5 -> do
		    ah <- Data.Binary.get
		    return (ESort ah)
	      6 -> do
		    ai <- Data.Binary.get
		    return (ELit ai)
	      7 -> do
		    aj <- Data.Binary.get
		    ak <- Data.Binary.get
		    return (ELetRec aj ak)
	      8 -> do
		    al <- Data.Binary.get
		    am <- Data.Binary.get
		    an <- Data.Binary.get
		    return (EPrim al am an)
	      9 -> do
		    ao <- Data.Binary.get
		    ap <- Data.Binary.get
		    return (EError ao ap)
	      10 -> do
		    aq <- Data.Binary.get
		    ar <- Data.Binary.get
		    as <- Data.Binary.get
		    at <- Data.Binary.get
		    au <- Data.Binary.get
		    av <- Data.Binary.get
		    return (ECase aq ar as at au av)
	      _ -> fail "invalid binary data found"



instance (Data.Binary.Binary e) => Data.Binary.Binary (Alt e) where
    put (Alt aa ab) = do
	    Data.Binary.put aa
	    Data.Binary.put ab
    get = do
    aa <- get
    ab <- get
    return (Alt aa ab)


