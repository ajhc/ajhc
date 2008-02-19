{-# OPTIONS_JHC -N #-}

module Jhc.Inst.Show() where

import Data.Int
import Data.Word
import Jhc.Basics
import Jhc.Num
import Jhc.Order
import Jhc.Show

-- we convert them to Word or WordMax so the showIntAtBase specialization can occur.

instance Show Word where
    showsPrec _ x = showWord x

instance Show Word8 where
    showsPrec _ x = showWord (fromIntegral x :: Word)

instance Show Word16 where
    showsPrec _ x = showWord (fromIntegral x :: Word)

instance Show Word32 where
    showsPrec _ x = showWord (fromIntegral x :: Word)

instance Show Word64 where
    showsPrec _ x = showWordMax (fromIntegral x :: WordMax)

instance Show WordPtr where
    showsPrec _ x = showWordMax (fromIntegral x :: WordMax)

instance Show WordMax where
    showsPrec _ x = showWordMax x

instance Show Int where
    showsPrec p x
        | x < 0 = showParen (p > 6) (showChar '-' . showWord (fromIntegral $ negate x :: Word))
        | otherwise = showWord (fromIntegral x :: Word)

instance Show Integer where
    showsPrec p x
        | x < 0 = showParen (p > 6) (showChar '-' . showWordMax (fromIntegral $ negate x :: WordMax))
        | otherwise = showWordMax (fromIntegral x :: WordMax)

instance Show Int8 where
    showsPrec p x = showsPrec p (fromIntegral x :: Int)
instance Show Int16 where
    showsPrec p x = showsPrec p (fromIntegral x :: Int)
instance Show Int32 where
    showsPrec p x = showsPrec p (fromIntegral x :: Int)
instance Show Int64 where
    showsPrec p x = showsPrec p (fromIntegral x :: Integer)
instance Show IntPtr where
    showsPrec p x = showsPrec p (fromIntegral x :: Integer)


-- specialized base 10 only versions of show
showWord :: Word -> String -> String
showWord w rest = case quotRem w 10 of
    (n',d) -> if n' == 0 then rest' else showWord n' rest'
        where rest' = chr (fromIntegral d + ord '0') : rest

showWordMax :: WordMax -> String -> String
showWordMax w rest = case quotRem w 10 of
    (n',d) -> if n' == 0 then rest' else showWordMax n' rest'
        where rest' = chr (fromIntegral d + ord '0') : rest

