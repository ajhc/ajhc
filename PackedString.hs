{-# OPTIONS_GHC -ffi #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.PackedString
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- An efficient implementation of strings.
--
-----------------------------------------------------------------------------


-- Original GHC implementation by Bryan O\'Sullivan,
-- rewritten to use UArray by Simon Marlow.
-- modified by John Meacham for use in ginsu
-- arch-tag: 8ad19c9c-9511-48a1-b25a-f5f98a386b8c

module PackedString (
	-- * The @PackedString@ type
        PackedString(..),      -- abstract, instances: Eq, Ord, Show, Typeable

         -- * Converting to and from @PackedString@s
	packString,  -- :: String -> PackedString
	unpackPS,    -- :: PackedString -> String
        showsPS,
        -- toString,
        toUTF8,
--        lengthPS,
--        utfLengthPS,

	joinPS,      -- :: PackedString -> [PackedString] -> PackedString
	-- * List-like manipulation functions
	nilPS,       -- :: PackedString
	consPS,      -- :: Char -> PackedString -> PackedString
	nullPS,      -- :: PackedString -> Bool
	appendPS,    -- :: PackedString -> PackedString -> PackedString
--        foldrPS,
        hashPS,
        filterPS,
--        foldlPS,
--        headPS,
	concatPS    -- :: [PackedString] -> PackedString



    ) where

import Data.Array.IO
import Data.Typeable
import Data.Char
import Data.Int
import Data.Binary
import qualified Data.ByteString as BS

import Bits
import GHC.Exts
import Data.Array.Base
import Word
import Data.Monoid
import Foreign.C.Types

instance Monoid PackedString where
    mempty = nilPS
    mappend x y = appendPS x y
    mconcat xs = concatPS xs

-- -----------------------------------------------------------------------------
-- PackedString type declaration

-- | A space-efficient representation of a 'String', which supports various
-- efficient operations.  A 'PackedString' contains full Unicode 'Char's.
newtype PackedString = PS BS.ByteString
    deriving(Typeable,Binary,Eq,Ord)


instance Show PackedString where
    showsPrec p ps r = showsPrec p (unpackPS ps) r


-- -----------------------------------------------------------------------------
-- Constructor functions

-- | The 'nilPS' value is the empty string.
nilPS :: PackedString
nilPS = PS BS.empty

-- | The 'consPS' function prepends the given character to the
-- given string.
consPS :: Char -> PackedString -> PackedString
consPS c cs = packString (c : (unpackPS cs)) -- ToDo:better

-- | Convert a 'String' into a 'PackedString'
packString :: String -> PackedString
packString str = PS $ (BS.pack $ toUTF str)


-- -----------------------------------------------------------------------------
-- Destructor functions (taking PackedStrings apart)


unpackPS :: PackedString -> String
unpackPS (PS bs) = fromUTF (BS.unpack bs)
--unpackPS (PS (UArray _ (I# e) ba)) = unpackFoldrUtf8# (ba) (e +# 1#) f [] where
--    f ch r = C# ch : r

showsPS :: PackedString -> String -> String
showsPS ps = (unpackPS ps ++)
--showsPS  (PS (UArray _ (I# e) ba)) xs = unpackFoldrUtf8# (ba) (e +# 1#) f xs where
--    f ch r = C# ch : r


toUTF8 :: PackedString -> [Word8]
toUTF8 (PS ba) = BS.unpack ba

--lengthPS :: PackedString -> Int
--lengthPS (PS (UArray _ (I# e) ba)) =  unpackFoldlUtf8#  (\x _ -> x + 1) 0 ba (e +# 1#)

--utfLengthPS :: PackedString -> Int
--utfLengthPS (PS (UArray _ e _)) = e + 1

--headPS :: PackedString -> Char
--headPS ps = case unpackPS ps of
--    (x:_) -> x
--    [] -> error "headPS: empty PackedString"

-- | The 'indexPS' function returns the character in the string at the given position.
--indexPS :: PackedString -> Int -> Char
--indexPS (PS ps) i = ps ! i

-- | The 'headPS' function returns the first element of a 'PackedString' or throws an
-- error if the string is empty.
--headPS :: PackedString -> Char
--headPS ps
--  | nullPS ps = error "Data.PackedString.headPS: head []"
--  | otherwise  = indexPS ps 0

-- | The 'tailPS' function returns the tail of a 'PackedString' or throws an error
-- if the string is empty.
--tailPS :: PackedString -> PackedString
--tailPS ps
--  | len <= 0 = error "Data.PackedString.tailPS: tail []"
--  | len == 1 = nilPS
--  | otherwise  = substrPS ps 1 (len - 1)
--  where
--    len = lengthPS ps

-- | The 'nullPS' function returns True iff the argument is null.
nullPS :: PackedString -> Bool
nullPS (PS ps) = BS.null ps

-- | The 'appendPS' function appends the second string onto the first.
appendPS :: PackedString -> PackedString -> PackedString
appendPS (PS xs) (PS ys) = PS (BS.append xs ys)


-- | The 'filterPS' function filters out the appropriate substring.
filterPS :: (Char -> Bool) -> PackedString -> PackedString {-or String?-}
filterPS pred ps = packString (filter pred (unpackPS ps))

-- | The 'foldlPS' function behaves like 'foldl' on 'PackedString's.
-- note, this version is strict. (behaves like foldl' )
--foldlPS :: (a -> Char -> a) -> a -> PackedString -> a
--foldlPS f b (PS (UArray _ (I# e) ba)) = unpackFoldlUtf8# (\x y -> f x (C# y)) b ba (e +# 1#)

-- | The 'foldrPS' function behaves like 'foldr' on 'PackedString's.
--foldrPS :: (Char -> a -> a) -> a -> PackedString -> a
--foldrPS f b (PS (UArray _ (I# e) ba)) = unpackFoldrUtf8# ba (e +# 1#) (\x y -> f (C# x)  y) b
--foldrPS f v ps = foldr f v (unpackPS ps)

hashPS :: PackedString -> Int32
hashPS (PS arr) = f 5381 (BS.unpack arr) where
    f x [] = x
    f m (c:cs) = n `seq` f n cs where
        n = ((m `shiftL` 5) + m ) `xor` fromIntegral c
{-

hashPS' :: PackedString -> Int32
hashPS' (PS (UArray 0 (I# e) ba)) = fromIntegral $ unpackFoldlUtf8# f 5381 ba (e +# 1#) where
    f m c = ((m `shiftL` 5) + m ) `xor` I# (ord# c)

hashPS :: PackedString -> Word
hashPS (PS (UArray 0 (I# e) ba)) =  W# (f (unsafeCoerce# 5381#) 0#) where
    f m c
        | c >=# (e +# 1#) = m
        | otherwise = f (((m `uncheckedShiftL#` 5#) `plusWord#` m ) `xor#`  (((indexWord8Array# ba c)))) (c +# 1#)
-}



-- | The 'takePS' function takes the first @n@ characters of a 'PackedString'.
--takePS :: Int -> PackedString -> PackedString
--takePS n ps = substrPS ps 0 (n-1)

-- | The 'dropPS' function drops the first @n@ characters of a 'PackedString'.
--dropPS	:: Int -> PackedString -> PackedString
--dropPS n ps = substrPS ps n (lengthPS ps - 1)

-- | The 'splitWithPS' function splits a 'PackedString' at a given index.
--splitAtPS :: Int -> PackedString -> (PackedString, PackedString)
--splitAtPS  n ps  = (takePS n ps, dropPS n ps)

-- | The 'takeWhilePS' function is analogous to the 'takeWhile' function.
--takeWhilePS :: (Char -> Bool) -> PackedString -> PackedString
--takeWhilePS pred ps = packString (takeWhile pred (unpackPS ps))

-- | The 'dropWhilePS' function is analogous to the 'dropWhile' function.
--dropWhilePS :: (Char -> Bool) -> PackedString -> PackedString
--dropWhilePS pred ps = packString (dropWhile pred (unpackPS ps))

-- | The 'elemPS' function returns True iff the given element is in the string.
--elemPS :: Char -> PackedString -> Bool
--elemPS c ps = c `elem` unpackPS ps

-- | The 'spanPS' function returns a pair containing the result of
-- running both 'takeWhilePS' and 'dropWhilePS'.
--spanPS :: (Char -> Bool) -> PackedString -> (PackedString, PackedString)
--spanPS  p ps = (takeWhilePS p ps, dropWhilePS p ps)

-- | The 'breakPS' function breaks a string at the first position which
-- satisfies the predicate.
--breakPS :: (Char -> Bool) -> PackedString -> (PackedString, PackedString)
--breakPS p ps = spanPS (not . p) ps

-- | The 'linesPS' function splits the input on line-breaks.
--linesPS :: PackedString -> [PackedString]
--linesPS ps = splitPS '\n' ps

-- | The 'unlinesPS' function concatenates the input list after
-- interspersing newlines.
--unlinesPS :: [PackedString] -> PackedString
--unlinesPS = joinPS (packString "\n")

-- | The 'wordsPS' function is analogous to the 'words' function.
--wordsPS :: PackedString -> [PackedString]
--wordsPS ps = filter (not.nullPS) (splitWithPS isSpace ps)

-- | The 'unwordsPS' function is analogous to the 'unwords' function.
--unwordsPS :: [PackedString] -> PackedString
--unwordsPS = joinPS (packString " ")

-- | The 'reversePS' function reverses the string.
--reversePS :: PackedString -> PackedString
--reversePS ps = packString (reverse (unpackPS ps))

-- | The 'concatPS' function concatenates a list of 'PackedString's.
concatPS :: [PackedString] -> PackedString
concatPS pss = packString (concat (map unpackPS pss))

------------------------------------------------------------

-- | The 'joinPS' function takes a 'PackedString' and a list of 'PackedString's
-- and concatenates the list after interspersing the first argument between
-- each element of the list.
joinPS :: PackedString -> [PackedString] -> PackedString
joinPS filler pss = concatPS (splice pss)
 where
  splice []  = []
  splice [x] = [x]
  splice (x:y:xs) = x:filler:splice (y:xs)

-- ToDo: the obvious generalisation
{-
  Some properties that hold:

  * splitPS x ls = ls'
      where False = any (map (x `elemPS`) ls')

  * joinPS (packString [x]) (splitPS x ls) = ls
-}

-- | The 'splitPS' function splits the input string on each occurance of the given 'Char'.
--splitPS :: Char -> PackedString -> [PackedString]
--splitPS c = splitWithPS (== c)

-- | The 'splitWithPS' function takes a character predicate and splits the input string
-- at each character which satisfies the predicate.
--splitWithPS :: (Char -> Bool) -> PackedString -> [PackedString]
--splitWithPS pred (PS ps) =
-- splitify 0
-- where
--  len = lengthPS (PS ps)

--  splitify n
--   | n >= len = []
--   | otherwise =
--      let
--       break_pt = first_pos_that_satisfies pred ps len n
--      in
--      if break_pt == n then -- immediate match, empty substring
--         nilPS
--	 : splitify (break_pt + 1)
--      else
--         substrPS (PS ps) n (break_pt - 1) -- leave out the matching character
--         : splitify (break_pt + 1)
--
--first_pos_that_satisfies pred ps len n =
--   case [ m | m <- [n..len-1], pred (ps ! m) ] of
--	[]    -> len
--	(m:_) -> m

-- -----------------------------------------------------------------------------
-- Local utility functions

-- The definition of @_substrPS@ is essentially:
-- @take (end - begin + 1) (drop begin str)@.

-- | The 'substrPS' function takes a 'PackedString' and two indices
-- and returns the substring of the input string between (and including)
-- these indices.
--substrPS :: PackedString -> Int -> Int -> PackedString
--substrPS (PS ps) begin end = packString [ ps ! i | i <- [begin..end] ]

-- -----------------------------------------------------------------------------
-- hPutPS

-- | Outputs a 'PackedString' to the specified 'Handle'.
--
-- NOTE: the representation of the 'PackedString' in the file is assumed to
-- be in the ISO-8859-1 encoding.  In other words, only the least signficant
-- byte is taken from each character in the 'PackedString'.
--hPutPS :: Handle -> PackedString -> IO ()
--hPutPS h (PS ps) = do
--  let l = lengthPS (PS ps)
--  arr <- newArray_ (0, l-1)
--  sequence_ [ writeArray arr i (fromIntegral (ord (ps ! i))) | i <- [0..l-1] ]
--  hPutArray h arr l

-- -----------------------------------------------------------------------------
-- hGetPS

-- | Read a 'PackedString' directly from the specified 'Handle'.
-- This is far more efficient than reading the characters into a 'String'
-- and then using 'packString'.
--
-- NOTE: as with 'hPutPS', the string representation in the file is
-- assumed to be ISO-8859-1.
--hGetPS :: Handle -> Int -> IO PackedString
--hGetPS h i = do
--  arr <- newArray_ (0, i-1)
--  l <- hGetArray h arr i
--  chars <- mapM (\i -> readArray arr i >>= return.chr.fromIntegral) [0..l-1]
--  return (packString chars)


utfCount :: String -> Int#
utfCount cs = uc 0# cs where
    uc n []  = n
    uc n (x:xs)
        | ord x <= 0x7f = uc (n +# 1#) xs
        | ord x <= 0x7ff = uc (n +# 2#) xs
        | ord x <= 0xffff = uc (n +# 3#) xs
        | ord x <= 0x1fffff = uc (n +# 4#) xs
        | ord x <= 0x3ffffff = uc (n +# 5#) xs
        | ord x <= 0x7fffffff = uc (n +# 6#) xs
        | otherwise = error "invalid string"


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




{-# INLINE unpackFoldrUtf8# #-}
unpackFoldrUtf8# :: ByteArray# -> Int# -> (Char# -> b -> b) -> b -> b
unpackFoldrUtf8# addr count f e = unpack 0# where
    unpack nh
      | nh ==# count  = e
      | ch `leChar#` '\x7F'# =  ch `f` unpack (nh +# 1#)
      | ch `leChar#` '\xDF'# =
           (chr# (((ord# ch                                  -# 0xC0#) `uncheckedIShiftL#`  6#) +#
                     (ord# (indexCharArray# addr (nh +# 1#)) -# 0x80#))) `f`
          unpack (nh +# 2#)
      | ch `leChar#` '\xEF'# =
           (chr# (((ord# ch                                  -# 0xE0#) `uncheckedIShiftL#` 12#) +#
                    ((ord# (indexCharArray# addr (nh +# 1#)) -# 0x80#) `uncheckedIShiftL#`  6#) +#
                     (ord# (indexCharArray# addr (nh +# 2#)) -# 0x80#))) `f`
          unpack (nh +# 3#)
      | otherwise            =
           (chr# (((ord# ch                                  -# 0xF0#) `uncheckedIShiftL#` 18#) +#
                    ((ord# (indexCharArray# addr (nh +# 1#)) -# 0x80#) `uncheckedIShiftL#` 12#) +#
                    ((ord# (indexCharArray# addr (nh +# 2#)) -# 0x80#) `uncheckedIShiftL#`  6#) +#
                     (ord# (indexCharArray# addr (nh +# 3#)) -# 0x80#))) `f`
          unpack (nh +# 4#)
      where
	ch = indexCharArray# addr nh

{-# INLINE unpackFoldlUtf8# #-}
unpackFoldlUtf8# ::  (a -> Char# -> a) -> a -> ByteArray# -> Int# -> a
unpackFoldlUtf8# f e addr count = unpack 0# e where
    unpack nh e
      | nh ==# count  = e
      | ch `leChar#` '\x7F'# = let n = (f e ch) in n `seq` unpack (nh +# 1#) n
      | ch `leChar#` '\xDF'# =
           let n = f e (chr# (((ord# ch                                  -# 0xC0#) `uncheckedIShiftL#`  6#) +#
                     (ord# (indexCharArray# addr (nh +# 1#)) -# 0x80#))) in n `seq` unpack (nh +# 2#) n
      | ch `leChar#` '\xEF'# =
         let n = f e (chr# (((ord# ch                        -# 0xE0#) `uncheckedIShiftL#` 12#) +#
                    ((ord# (indexCharArray# addr (nh +# 1#)) -# 0x80#) `uncheckedIShiftL#`  6#) +#
                     (ord# (indexCharArray# addr (nh +# 2#)) -# 0x80#))) in n `seq` unpack (nh +# 3#) n
      | otherwise            =
         let n = f e (chr# (((ord# ch                        -# 0xF0#) `uncheckedIShiftL#` 18#) +#
                    ((ord# (indexCharArray# addr (nh +# 1#)) -# 0x80#) `uncheckedIShiftL#` 12#) +#
                    ((ord# (indexCharArray# addr (nh +# 2#)) -# 0x80#) `uncheckedIShiftL#`  6#) +#
                     (ord# (indexCharArray# addr (nh +# 3#)) -# 0x80#))) in n `seq` unpack (nh +# 4#) n
      where
	ch = indexCharArray# addr nh


{-

less efficient non-ghc versions

-- | Convert a 'PackedString' into a 'String'
--unpackPS :: PackedString -> String
--unpackPS (PS ps) = fromUTF (elems ps)
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

-}
