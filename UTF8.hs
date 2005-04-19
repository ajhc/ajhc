--  $Id: UTF8.hs,v 1.4 2004/02/28 04:20:46 john Exp $
-- arch-tag: 596040c5-d420-4cc6-add6-c4612cfe2d27

module UTF8(toUTF, fromUTF) where

import Bits
import Char
import Word(Word8)



-- | Convert Unicode characters to UTF-8.
toUTF :: String -> [Word8]
toUTF [] = []
toUTF (x:xs) | ord x<=0x007F = (fromIntegral $ ord x):toUTF xs
	     | ord x<=0x07FF = fromIntegral (0xC0 .|. ((ord x `shift` (-6)) .&. 0x1F)):
			       fromIntegral (0x80 .|. (ord x .&. 0x3F)):
			       toUTF xs
	     | otherwise     = fromIntegral (0xE0 .|. ((ord x `shift` (-12)) .&. 0x0F)):
			       fromIntegral (0x80 .|. ((ord x `shift` (-6)) .&. 0x3F)):
			       fromIntegral (0x80 .|. (ord x .&. 0x3F)):
			       toUTF xs

-- | Convert UTF-8 to Unicode.

fromUTF :: [Word8] -> String
fromUTF xs = fromUTF' (map fromIntegral xs) where 
    fromUTF' [] = []
    fromUTF' (all@(x:xs)) 
	| x<=0x7F = (chr (x)):fromUTF' xs
	| x<=0xBF = err
	| x<=0xDF = twoBytes all
	| x<=0xEF = threeBytes all
	| otherwise   = err
    twoBytes (x1:x2:xs) = chr  ((((x1 .&. 0x1F) `shift` 6) .|.
			       (x2 .&. 0x3F))):fromUTF' xs
    twoBytes _ = error "fromUTF: illegal two byte sequence"

    threeBytes (x1:x2:x3:xs) = chr ((((x1 .&. 0x0F) `shift` 12) .|.
				    ((x2 .&. 0x3F) `shift` 6) .|.
				    (x3 .&. 0x3F))):fromUTF' xs
    threeBytes _ = error "fromUTF: illegal three byte sequence" 
    
    err = error "fromUTF: illegal UTF-8 character"
