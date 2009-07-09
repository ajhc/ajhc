#ifndef _JHC_TYPEABLE_H
#define _JHC_TYPEABLE_H


-- // This file is for backwards compatability with libraries that depend on the ghc internal typeable mechanism


#define INSTANCE_TYPEABLE0(tycon,y,z) deriving instance Typeable tycon

#define INSTANCE_TYPEABLE1(tycon,y,z) deriving instance Typeable1 tycon

#define INSTANCE_TYPEABLE2(tycon,y,z) deriving instance Typeable2 tycon

#define INSTANCE_TYPEABLE3(tycon,y,z) deriving instance Typeable3 tycon

#define INSTANCE_TYPEABLE4(tycon,y,z) deriving instance Typeable4 tycon

#endif
