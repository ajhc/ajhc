{-# OPTIONS_JHC -fno-prelude #-}
module Jhc.Inst.Show() where

import Jhc.Basics
import Jhc.Class.Num
import Jhc.Class.Ord
import Jhc.Class.Real
import Jhc.Show
import Jhc.Type.C

-- we convert them to Word or WordMax so the showIntAtBase specialization can occur.

fromIntegral   :: (Integral a, Num b) => a -> b
fromIntegral x =  fromInteger (toInteger x)

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
        | p `seq` x `seq` False = undefined
        | x < 0 = showParen (p > 6) (showChar '-' . showWord (fromIntegral $ negate x :: Word))
        | True = showWord (fromIntegral x :: Word)

instance Show Integer where
    showsPrec p x
        | p `seq` x `seq` False = undefined
        | x < 0 = showParen (p > 6) (showChar '-' . showWordMax (fromIntegral $ negate x :: WordMax))
        | True = showWordMax (fromIntegral x :: WordMax)

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
instance Show IntMax where
    showsPrec p x = showsPrec p (fromIntegral x :: Integer)

instance Show CSize where
    showsPrec p x = showsPrec p (fromIntegral x :: Integer)
instance Show CInt where
    showsPrec p x = showsPrec p (fromIntegral x :: Integer)

instance Show CChar where
    showsPrec p x = showsPrec p (fromIntegral x :: Int)
instance Show CSChar where
    showsPrec p x = showsPrec p (fromIntegral x :: Int)
instance Show CUChar where
    showsPrec _ x = showWord (fromIntegral x :: Word)
instance Show CUInt where
    showsPrec _ x = showWord (fromIntegral x :: Word)
instance Show CWchar where
    showsPrec _ x = showWord (fromIntegral x :: Word)

-- specialized base 10 only versions of show
showWord :: Word -> String -> String
showWord w rest = w `seq` case quotRem w 10 of
    (n',d) -> n' `seq` d `seq` rest' `seq` if n' == 0 then rest' else showWord n' rest'
        where rest' = chr (fromIntegral d + ord '0') : rest

showWordMax :: WordMax -> String -> String
showWordMax w rest = w `seq` case quotRem w 10 of
    (n',d) -> n' `seq` d `seq` rest' `seq` if n' == 0 then rest' else showWordMax n' rest'
        where rest' = chr (fromIntegral d + ord '0') : rest
