{-# OPTIONS_JHC -fffi  #-}
module Data.Unique (
   -- * Unique objects
   Unique(),            -- instance (Eq, Ord)
   newUnique,           -- :: IO Unique
   hashUnique           -- :: Unique -> Int
 ) where

import Foreign.Storable
import Foreign.Ptr

-- | An abstract unique object.  Objects of type 'Unique' may be
-- compared for equality and ordering and hashed into 'Int'.
newtype Unique = Unique Int deriving (Eq,Ord)


-- | Creates a new object of type 'Unique'.  The value returned will
-- not compare equal to any other value of type 'Unique' returned by
-- previous calls to 'newUnique'.  There is no limit on the number of
-- times 'newUnique' may be called.
newUnique :: IO Unique
newUnique = do
    n <- peek c_data_unique
    poke c_data_unique (n + 1)
    return $ Unique n

-- | Hashes a 'Unique' into an 'Int'.  Two 'Unique's may hash to the
-- same value, although in practice this is unlikely.  The 'Int'
-- returned makes a good hash key.
hashUnique :: Unique -> Int
hashUnique (Unique u) = u


foreign import ccall "&jhc_data_unique" c_data_unique :: Ptr Int

