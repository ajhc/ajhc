module E.Binary() where

import E.Type
import {-# SOURCE #-} Info.Binary(putInfo,getInfo)
import Binary
import Atom
import Monad

{-!derive: is !-}

-- Binary instance
data TvrBinary = TvrBinaryNone | TvrBinaryAtom Atom | TvrBinaryInt Int

instance Binary TVr where
    put_ bh (TVr { tvrIdent = 0, tvrType =  e, tvrInfo = nf} ) = do
        put_ bh (TvrBinaryNone)
        put_ bh e
        putInfo bh nf
    put_ bh (TVr { tvrIdent = i, tvrType =  e, tvrInfo = nf}) | Just x <- intToAtom i = do
        put_ bh (TvrBinaryAtom x)
        put_ bh e
        putInfo bh nf
    put_ bh (TVr { tvrIdent = i, tvrType =  e, tvrInfo = nf}) = do
        unless (even i) $ fail "number not even"
        put_ bh (TvrBinaryInt i)
        put_ bh e
        putInfo bh nf
    get bh = do
        (x ) <- get bh
        e <- get bh
        nf <- getInfo bh
        case x of
            TvrBinaryNone -> return $ TVr 0 e nf
            TvrBinaryAtom a -> return $ TVr (atomIndex a) e nf
            TvrBinaryInt i -> return $ TVr (i) e nf


instance Binary RuleType where
    put_ bh RuleSpecialization = do
	    putByte bh 0
    put_ bh RuleUser = do
	    putByte bh 1
    put_ bh RuleCatalyst = do
	    putByte bh 2
    get bh = do
	    h <- getByte bh
	    case h of
	      0 -> do
		    return RuleSpecialization
	      1 -> do
		    return RuleUser
	      2 -> do
		    return RuleCatalyst
	      _ -> fail "invalid binary data found"

instance Binary Rule where
    put_ bh (Rule aa ab ac ad ae af ag ah) = do
	    put_ bh aa
	    put_ bh ab
	    put_ bh ac
	    put_ bh ad
	    put_ bh ae
	    put_ bh af
	    put_ bh ag
	    put_ bh ah
    get bh = do
    aa <- get bh
    ab <- get bh
    ac <- get bh
    ad <- get bh
    ae <- get bh
    af <- get bh
    ag <- get bh
    ah <- get bh
    return (Rule aa ab ac ad ae af ag ah)

instance (Binary e,Binary t) => Binary (Lit e t) where
    put_ bh (LitInt aa ab) = do
	    putByte bh 0
	    put_ bh aa
	    put_ bh ab
    put_ bh (LitCons ac ad ae af) = do
	    putByte bh 1
	    put_ bh ac
	    put_ bh ad
	    put_ bh ae
	    put_ bh af
    get bh = do
	    h <- getByte bh
	    case h of
	      0 -> do
		    aa <- get bh
		    ab <- get bh
		    return (LitInt aa ab)
	      1 -> do
		    ac <- get bh
		    ad <- get bh
		    ae <- get bh
		    af <- get bh
		    return (LitCons ac ad ae af)
	      _ -> fail "invalid binary data found"


instance Binary ESort where
    put_ bh EStar = do
	    putByte bh 0
    put_ bh EBang = do
	    putByte bh 1
    put_ bh EHash = do
	    putByte bh 2
    put_ bh ETuple = do
	    putByte bh 3
    put_ bh EHashHash = do
	    putByte bh 4
    put_ bh EStarStar = do
	    putByte bh 5
    put_ bh (ESortNamed aa) = do
	    putByte bh 6
	    put_ bh aa
    get bh = do
	    h <- getByte bh
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
		    aa <- get bh
		    return (ESortNamed aa)
	      _ -> fail "invalid binary data found"


instance Binary E where
    put_ bh (EAp aa ab) = do
	    putByte bh 0
	    put_ bh aa
	    put_ bh ab
    put_ bh (ELam ac ad) = do
	    putByte bh 1
	    put_ bh ac
	    put_ bh ad
    put_ bh (EPi ae af) = do
	    putByte bh 2
	    put_ bh ae
	    put_ bh af
    put_ bh (EVar ag) = do
	    putByte bh 3
	    put_ bh ag
    put_ bh Unknown = do
	    putByte bh 4
    put_ bh (ESort ah) = do
	    putByte bh 5
	    put_ bh ah
    put_ bh (ELit ai) = do
	    putByte bh 6
	    put_ bh ai
    put_ bh (ELetRec aj ak) = do
	    putByte bh 7
	    put_ bh aj
	    put_ bh ak
    put_ bh (EPrim al am an) = do
	    putByte bh 8
	    put_ bh al
	    put_ bh am
	    put_ bh an
    put_ bh (EError ao ap) = do
	    putByte bh 9
	    put_ bh ao
	    put_ bh ap
    put_ bh (ECase aq ar as at au) = do
	    putByte bh 10
	    put_ bh aq
	    put_ bh ar
	    put_ bh as
	    put_ bh at
	    put_ bh au
    get bh = do
	    h <- getByte bh
	    case h of
	      0 -> do
		    aa <- get bh
		    ab <- get bh
		    return (EAp aa ab)
	      1 -> do
		    ac <- get bh
		    ad <- get bh
		    return (ELam ac ad)
	      2 -> do
		    ae <- get bh
		    af <- get bh
		    return (EPi ae af)
	      3 -> do
		    ag <- get bh
		    return (EVar ag)
	      4 -> do
		    return Unknown
	      5 -> do
		    ah <- get bh
		    return (ESort ah)
	      6 -> do
		    ai <- get bh
		    return (ELit ai)
	      7 -> do
		    aj <- get bh
		    ak <- get bh
		    return (ELetRec aj ak)
	      8 -> do
		    al <- get bh
		    am <- get bh
		    an <- get bh
		    return (EPrim al am an)
	      9 -> do
		    ao <- get bh
		    ap <- get bh
		    return (EError ao ap)
	      10 -> do
		    aq <- get bh
		    ar <- get bh
		    as <- get bh
		    at <- get bh
		    au <- get bh
		    return (ECase aq ar as at au)
	      _ -> fail "invalid binary data found"


instance (Binary e) => Binary (Alt e) where
    put_ bh (Alt aa ab) = do
	    put_ bh aa
	    put_ bh ab
    get bh = do
    aa <- get bh
    ab <- get bh
    return (Alt aa ab)

instance Binary TvrBinary where
    put_ bh TvrBinaryNone = do
	    putByte bh 0
    put_ bh (TvrBinaryAtom aa) = do
	    putByte bh 1
	    put_ bh aa
    put_ bh (TvrBinaryInt ab) = do
	    putByte bh 2
	    put_ bh ab
    get bh = do
	    h <- getByte bh
	    case h of
	      0 -> do
		    return TvrBinaryNone
	      1 -> do
		    aa <- get bh
		    return (TvrBinaryAtom aa)
	      2 -> do
		    ab <- get bh
		    return (TvrBinaryInt ab)
	      _ -> fail "invalid binary data found"

--  Imported from other files :-
