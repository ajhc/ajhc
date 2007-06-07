{-# OPTIONS_JHC -N #-}

module Jhc.Inst.Show() where

import Jhc.Show
import Prelude.Text
import Data.Int
import Jhc.Basics
import Data.Word
import Jhc.Order
import Jhc.Num
import Numeric(showInt)

-- we convert them to Word or WordMax so the showIntAtBase specialization can occur.

instance Show Word where
    showsPrec _ x = showInt x

instance Show Word8 where
    showsPrec _ x = showInt (fromIntegral x :: Word)

instance Show Word16 where
    showsPrec _ x = showInt (fromIntegral x :: Word)

instance Show Word32 where
    showsPrec _ x = showInt (fromIntegral x :: Word)

instance Show Word64 where
    showsPrec _ x = showInt (fromIntegral x :: WordMax)

instance Show WordPtr where
    showsPrec _ x = showInt (fromIntegral x :: WordMax)

instance Show WordMax where
    showsPrec _ x = showInt x

instance Show Int where
    showsPrec p x
        | x < 0 = showParen (p > 6) (showChar '-' . showInt (fromIntegral $ negate x :: Word))
        | otherwise = showInt (fromIntegral x :: Word)

instance Show Integer where
    showsPrec p x
        | x < 0 = showParen (p > 6) (showChar '-' . showInt (fromIntegral $ negate x :: WordMax))
        | otherwise = showInt (fromIntegral x :: WordMax)

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


