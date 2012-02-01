{-# LANGUAGE ForeignFunctionInterface #-}
module PackedString (
    PackedString,
    packString,
    unpackPS,
    ) where

import Data.Binary
import Data.Generics
import Data.Monoid
import GHC.Exts
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BSU

newtype PackedString = PS BS.ByteString
    deriving(Typeable,Binary,Eq,Ord,Monoid,Data)

instance Show PackedString where
    showsPrec p ps r = showsPrec p (unpackPS ps) r

-- | Convert a 'String' into a 'PackedString'
packString :: String -> PackedString
packString str = PS (BSU.fromString str)

unpackPS :: PackedString -> String
unpackPS (PS bs) = BSU.toString bs

instance IsString PackedString where
    fromString = packString
