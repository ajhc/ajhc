{-# LANGUAGE ForeignFunctionInterface #-}
module PackedString (
    PackedString,
    packString,
    packAddr_,
    unpackPS,
    ) where

import Data.Binary
import Data.Generics
import Data.Monoid
import GHC.Exts
import System.IO.Unsafe
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BSU
import qualified Data.ByteString.Unsafe as BS

newtype PackedString = PS BS.ByteString
    deriving(Typeable,Binary,Eq,Ord,Monoid,Data)

instance Show PackedString where
    showsPrec p ps r = showsPrec p (unpackPS ps) r

-- | Convert a 'String' into a 'PackedString'
packString :: String -> PackedString
packString str = PS (BSU.fromString str)

packAddr_ :: Addr# -> PackedString
packAddr_ addr = PS $ unsafePerformIO (BS.unsafePackAddress addr)

unpackPS :: PackedString -> String
unpackPS (PS bs) = BSU.toString bs

instance IsString PackedString where
    fromString = packString
