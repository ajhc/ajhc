{-
Copyright (C) 2001, 2004 Ian Lynagh <igloo@earth.li>

Modified by Einar Karttunen to remove dependency on packed strings
and autoconf.

Modified by John Meacham for code cleanups.


This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software Foundation,
Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

-}

{-# OPTIONS -funbox-strict-fields -fglasgow-exts -fno-warn-name-shadowing -O2 #-}

module Util.SHA1 (sha1String,sha1file,sha1Bytes,hashToBytes,sha1Handle,ABCDE(..),Hash,emptyHash) where


import Control.Monad (unless)
import Data.Char (intToDigit,ord)
import Foreign
import Foreign.C
import System.IO
import System.IO.Unsafe (unsafePerformIO)

type Hash = ABCDE
data ABCDE = ABCDE !Word32 !Word32 !Word32 !Word32 !Word32
    deriving(Eq,Ord)

emptyHash = ABCDE 0 0 0 0 0

data XYZ = XYZ !Word32 !Word32 !Word32

sha1String :: String -> Hash
sha1String ss = sha1Bytes (toUTF ss) where
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


sha1Bytes :: [Word8] -> Hash
sha1Bytes ss = unsafePerformIO $ do
    let len = length ss
        plen = sha1_step_1_2_plength len
    allocaBytes plen $ \ptr -> do
    pokeArray ptr ss
    let num_nuls = (55 - len) `mod` 64
    pokeArray (advancePtr ptr len) ((128:replicate num_nuls 0)++(reverse $ size_split 8 (fromIntegral len*8)))
    let abcde = sha1_step_3_init
    let ptr' = castPtr ptr
    unless big_endian $ fiddle_endianness ptr' plen
    res <- sha1_step_4_main abcde ptr' plen
    return res


{-# NOINLINE sha1Handle #-}
sha1Handle :: Handle -> IO Hash
sha1Handle h = do
    hSeek h AbsoluteSeek 0
    len <- hFileSize h
    len <- return $ fromIntegral len
    let plen = sha1_step_1_2_plength len
    allocaBytes plen $ \ptr -> do
    cnt <- hGetBuf h ptr len
    unless (cnt == len) $ fail "sha1File - read returned too few bytes"
    hSeek h AbsoluteSeek 0
    let num_nuls = (55 - len) `mod` 64
    pokeArray (advancePtr ptr len) ((128:replicate num_nuls 0)++(reverse $ size_split 8 (fromIntegral len*8)))
    let abcde = sha1_step_3_init
    let ptr' = castPtr ptr
    unless big_endian $ fiddle_endianness ptr' plen
    res <- sha1_step_4_main abcde ptr' plen
    return res

{-# NOINLINE sha1file #-}
sha1file :: FilePath -> IO Hash
sha1file fp = do
    h   <- openBinaryFile fp ReadMode
    hash <- sha1Handle h
    hClose h
    return hash

big_endian = unsafePerformIO $ do
    let x :: Word32
        x = 0x12345678
    s <- with x $ \ptr -> peekCStringLen (castPtr ptr,4)
    case s of
      "\x12\x34\x56\x78" -> return True
      "\x78\x56\x34\x12" -> return False
      _                  -> error "Testing endianess failed"

fiddle_endianness :: Ptr Word32 -> Int -> IO ()
fiddle_endianness p 0 = p `seq` return ()
fiddle_endianness p n
 = do x <- peek p
      poke p $ shiftL x 24
           .|. shiftL (x .&. 0xff00) 8
           .|. (shiftR x 8 .&. 0xff00)
           .|. shiftR x 24
      fiddle_endianness (p `advancePtr` 1) (n - 4)

-- sha1_step_1_2_pad_length assumes the length is at most 2^61.
-- This seems reasonable as the Int used to represent it is normally 32bit,
-- but obviously could go wrong with large inputs on 64bit machines.
-- The PackedString library should probably move to Word64s if this is an
-- issue, though.
--
-- sha1_step_1_2_pad_length :: PackedString -> PackedString
-- sha1_step_1_2_pad_length s
--  = let len = lengthPS s
--        num_nuls = (55 - len) `mod` 64
--        padding = 128:replicate num_nuls 0
--        len_w8s = reverse $ size_split 8 (fromIntegral len*8)
--    in concatLenPS (len + 1 + num_nuls + 8)
--                   [s, packWords padding, packWords len_w8s]

sha1_step_1_2_plength :: Int -> Int
sha1_step_1_2_plength len = (len + 1 + num_nuls + 8) where num_nuls = (55 - len) `mod` 64


size_split :: Int -> Integer -> [Word8]
size_split 0 _ = []
size_split p n = fromIntegral d:size_split (p-1) n'
 where (n', d) = divMod n 256

sha1_step_3_init :: ABCDE
sha1_step_3_init = ABCDE 0x67452301 0xefcdab89 0x98badcfe 0x10325476 0xc3d2e1f0

sha1_step_4_main :: ABCDE -> Ptr Word32 -> Int -> IO ABCDE
sha1_step_4_main abcde _ 0 = return $! abcde
sha1_step_4_main (ABCDE a0@a b0@b c0@c d0@d e0@e) s len
    = do
         (e, b) <- doit f1 0x5a827999 (x 0) a b c d e
         (d, a) <- doit f1 0x5a827999 (x 1) e a b c d
         (c, e) <- doit f1 0x5a827999 (x 2) d e a b c
         (b, d) <- doit f1 0x5a827999 (x 3) c d e a b
         (a, c) <- doit f1 0x5a827999 (x 4) b c d e a
         (e, b) <- doit f1 0x5a827999 (x 5) a b c d e
         (d, a) <- doit f1 0x5a827999 (x 6) e a b c d
         (c, e) <- doit f1 0x5a827999 (x 7) d e a b c
         (b, d) <- doit f1 0x5a827999 (x 8) c d e a b
         (a, c) <- doit f1 0x5a827999 (x 9) b c d e a
         (e, b) <- doit f1 0x5a827999 (x 10) a b c d e
         (d, a) <- doit f1 0x5a827999 (x 11) e a b c d
         (c, e) <- doit f1 0x5a827999 (x 12) d e a b c
         (b, d) <- doit f1 0x5a827999 (x 13) c d e a b
         (a, c) <- doit f1 0x5a827999 (x 14) b c d e a
         (e, b) <- doit f1 0x5a827999 (x 15) a b c d e
         (d, a) <- doit f1 0x5a827999 (m 16) e a b c d
         (c, e) <- doit f1 0x5a827999 (m 17) d e a b c
         (b, d) <- doit f1 0x5a827999 (m 18) c d e a b
         (a, c) <- doit f1 0x5a827999 (m 19) b c d e a
         (e, b) <- doit f2 0x6ed9eba1 (m 20) a b c d e
         (d, a) <- doit f2 0x6ed9eba1 (m 21) e a b c d
         (c, e) <- doit f2 0x6ed9eba1 (m 22) d e a b c
         (b, d) <- doit f2 0x6ed9eba1 (m 23) c d e a b
         (a, c) <- doit f2 0x6ed9eba1 (m 24) b c d e a
         (e, b) <- doit f2 0x6ed9eba1 (m 25) a b c d e
         (d, a) <- doit f2 0x6ed9eba1 (m 26) e a b c d
         (c, e) <- doit f2 0x6ed9eba1 (m 27) d e a b c
         (b, d) <- doit f2 0x6ed9eba1 (m 28) c d e a b
         (a, c) <- doit f2 0x6ed9eba1 (m 29) b c d e a
         (e, b) <- doit f2 0x6ed9eba1 (m 30) a b c d e
         (d, a) <- doit f2 0x6ed9eba1 (m 31) e a b c d
         (c, e) <- doit f2 0x6ed9eba1 (m 32) d e a b c
         (b, d) <- doit f2 0x6ed9eba1 (m 33) c d e a b
         (a, c) <- doit f2 0x6ed9eba1 (m 34) b c d e a
         (e, b) <- doit f2 0x6ed9eba1 (m 35) a b c d e
         (d, a) <- doit f2 0x6ed9eba1 (m 36) e a b c d
         (c, e) <- doit f2 0x6ed9eba1 (m 37) d e a b c
         (b, d) <- doit f2 0x6ed9eba1 (m 38) c d e a b
         (a, c) <- doit f2 0x6ed9eba1 (m 39) b c d e a
         (e, b) <- doit f3 0x8f1bbcdc (m 40) a b c d e
         (d, a) <- doit f3 0x8f1bbcdc (m 41) e a b c d
         (c, e) <- doit f3 0x8f1bbcdc (m 42) d e a b c
         (b, d) <- doit f3 0x8f1bbcdc (m 43) c d e a b
         (a, c) <- doit f3 0x8f1bbcdc (m 44) b c d e a
         (e, b) <- doit f3 0x8f1bbcdc (m 45) a b c d e
         (d, a) <- doit f3 0x8f1bbcdc (m 46) e a b c d
         (c, e) <- doit f3 0x8f1bbcdc (m 47) d e a b c
         (b, d) <- doit f3 0x8f1bbcdc (m 48) c d e a b
         (a, c) <- doit f3 0x8f1bbcdc (m 49) b c d e a
         (e, b) <- doit f3 0x8f1bbcdc (m 50) a b c d e
         (d, a) <- doit f3 0x8f1bbcdc (m 51) e a b c d
         (c, e) <- doit f3 0x8f1bbcdc (m 52) d e a b c
         (b, d) <- doit f3 0x8f1bbcdc (m 53) c d e a b
         (a, c) <- doit f3 0x8f1bbcdc (m 54) b c d e a
         (e, b) <- doit f3 0x8f1bbcdc (m 55) a b c d e
         (d, a) <- doit f3 0x8f1bbcdc (m 56) e a b c d
         (c, e) <- doit f3 0x8f1bbcdc (m 57) d e a b c
         (b, d) <- doit f3 0x8f1bbcdc (m 58) c d e a b
         (a, c) <- doit f3 0x8f1bbcdc (m 59) b c d e a
         (e, b) <- doit f2 0xca62c1d6 (m 60) a b c d e
         (d, a) <- doit f2 0xca62c1d6 (m 61) e a b c d
         (c, e) <- doit f2 0xca62c1d6 (m 62) d e a b c
         (b, d) <- doit f2 0xca62c1d6 (m 63) c d e a b
         (a, c) <- doit f2 0xca62c1d6 (m 64) b c d e a
         (e, b) <- doit f2 0xca62c1d6 (m 65) a b c d e
         (d, a) <- doit f2 0xca62c1d6 (m 66) e a b c d
         (c, e) <- doit f2 0xca62c1d6 (m 67) d e a b c
         (b, d) <- doit f2 0xca62c1d6 (m 68) c d e a b
         (a, c) <- doit f2 0xca62c1d6 (m 69) b c d e a
         (e, b) <- doit f2 0xca62c1d6 (m 70) a b c d e
         (d, a) <- doit f2 0xca62c1d6 (m 71) e a b c d
         (c, e) <- doit f2 0xca62c1d6 (m 72) d e a b c
         (b, d) <- doit f2 0xca62c1d6 (m 73) c d e a b
         (a, c) <- doit f2 0xca62c1d6 (m 74) b c d e a
         (e, b) <- doit f2 0xca62c1d6 (m 75) a b c d e
         (d, a) <- doit f2 0xca62c1d6 (m 76) e a b c d
         (c, e) <- doit f2 0xca62c1d6 (m 77) d e a b c
         (b, d) <- doit f2 0xca62c1d6 (m 78) c d e a b
         (a, c) <- doit f2 0xca62c1d6 (m 79) b c d e a
         let abcde' = ABCDE (a0 + a) (b0 + b) (c0 + c) (d0 + d) (e0 + e)
         sha1_step_4_main abcde' (s `advancePtr` 16) (len - 64)
 where {-# INLINE f1 #-}
       f1 (XYZ x y z) = (x .&. y) .|. ((complement x) .&. z)
       {-# INLINE f2 #-}
       f2 (XYZ x y z) = x `xor` y `xor` z
       {-# INLINE f3 #-}
       f3 (XYZ x y z) = (x .&. y) .|. (x .&. z) .|. (y .&. z)
       {-# INLINE x #-}
       x n = peek (s `advancePtr` n)
       {-# INLINE m #-}
       m n = do let base = s `advancePtr` (n .&. 15)
                x0 <- peek base
                x1 <- peek (s `advancePtr` ((n - 14) .&. 15))
                x2 <- peek (s `advancePtr` ((n - 8) .&. 15))
                x3 <- peek (s `advancePtr` ((n - 3) .&. 15))
                let res = rotateL (x0 `xor` x1 `xor` x2 `xor` x3) 1
                poke base res
                return res
       {-# INLINE doit #-}
       doit f k i a b c d e = a `seq` c `seq`
           do i' <- i
              return (rotateL a 5 + f (XYZ b c d) + e + i' + k,
                      rotateL b 30)

hashToBytes :: Hash -> [Word8]
hashToBytes (ABCDE a b c d e) = tb a . tb b . tb c . tb d . tb e $ [] where
    tb :: Word32 -> [Word8] -> [Word8]
    tb n = showIt 4 n
    showIt :: Int -> Word32 -> [Word8] -> [Word8]
    showIt 0 _ r = r
    showIt i x r = case quotRem x 256 of
                       (y, z) -> let c = fromIntegral z
                                 in c `seq` showIt (i-1) y (c:r)


instance Show ABCDE where
    showsPrec _ (ABCDE a b c d e) = showAsHex a . showAsHex b . showAsHex c . showAsHex d . showAsHex e

showAsHex :: Word32 -> ShowS
showAsHex n = showIt 8 n
   where
    showIt :: Int -> Word32 -> String -> String
    showIt 0 _ r = r
    showIt i x r = case quotRem x 16 of
                       (y, z) -> let c = intToDigit (fromIntegral z)
                                 in c `seq` showIt (i-1) y (c:r)

