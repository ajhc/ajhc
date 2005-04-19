module Foreign.C.String where

import Char
import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign.Ptr
import Data.Word

type CString = Ptr CChar
type CStringLen = (Ptr CChar, Int)

nUL :: CChar
nUL = 0

peekCString :: CString -> IO String
peekCString cp = do
    cs <- peekArray0 nUL cp
    return (cCharsToChars cs)

-- TODO UTF8
-- cast [CChar] to [Char]
--
cCharsToChars :: [CChar] -> [Char]
cCharsToChars xs  = map castCCharToChar xs

-- cast [Char] to [CChar]
--
charsToCChars :: [Char] -> [CChar]
charsToCChars xs  = map castCharToCChar xs

castCCharToChar :: CChar -> Char
castCCharToChar ch = chr (fromIntegral (fromIntegral ch :: Word8))

castCharToCChar :: Char -> CChar
castCharToCChar ch = fromIntegral (ord ch)

