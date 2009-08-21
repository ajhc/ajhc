{-# OPTIONS_JHC -N #-}
module Prelude.CType (
    isAscii, isLatin1, isControl, isPrint, isSpace, isUpper, isLower,
    isAlpha, isDigit, isOctDigit, isHexDigit, isAlphaNum,
    digitToInt, intToDigit,
    toUpper, toLower
    ) where

import Jhc.Basics
import Jhc.Order
import Jhc.Num
import Data.Word
import Jhc.List
import Jhc.IO

-- Character-testing operations
isAscii, isLatin1, isControl, isPrint, isSpace, isUpper, isLower,
 isAlpha, isDigit, isOctDigit, isHexDigit, isAlphaNum :: Char -> Bool

isAscii c                =  c < '\x80'

isLatin1 c               =  c <= '\xff'

isControl c              =  c < ' ' || c >= '\DEL' && c <= '\x9f'

isPrint c               =  isLatin1 c && not (isControl c)

isSpace c                =  c `elem` " \t\n\r\f\v\xA0"

isUpper c                =  c >= 'A' && c <= 'Z'

isLower c                =  c >= 'a' && c <= 'z'

isAlpha c                =  isUpper c || isLower c

isDigit c                =  c >= '0' && c <= '9'

isOctDigit c             =  c >= '0' && c <= '7'

isHexDigit c             =  isDigit c || c >= 'A' && c <= 'F' ||
                                         c >= 'a' && c <= 'f'

isAlphaNum c             =  isAlpha c || isDigit c

-- Digit conversion operations
digitToInt :: Char -> Int
digitToInt c
  | isDigit c            =  ord c - ord '0'
  | c >= 'a' && c <= 'f' =  ord c - (ord 'a' + 10)
  | c >= 'A' && c <= 'F' =  ord c - (ord 'A' + 10)
  | otherwise            =  error "Char.digitToInt: not a digit"

intToDigit :: Int -> Char
intToDigit i = f (fromIntegral i :: Word) where
    f w | w < 10 = chr (ord '0' + i)
        | w < 16 = chr ((ord 'a' - 10) + i)
        | otherwise = error "Char.intToDigit: not a digit"

-- Case-changing operations
toUpper :: Char -> Char
toUpper c | isLower c = chr $ ord c - 32
          | otherwise = c

toLower :: Char -> Char
toLower c | isUpper c = chr $ ord c + 32
          | otherwise = c

elem    :: Char -> [Char] -> Bool
elem _ []	= False
elem x (y:ys)
    | x == y = True
    | otherwise = f y ys where
        f y _ | x == y = True
        f _ (y:ys) = f y ys
        f _ [] = False
