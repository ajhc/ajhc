{-# OPTIONS_JHC -fno-prelude -fm4 #-}

module Jhc.Inst.Read() where

import Prelude.Text
import Data.Int
import Jhc.Basics
import Data.Word
import Jhc.Order
import Jhc.Float
import Prelude.Float
import Jhc.Num
import Numeric(showSigned, showInt, readSigned, readDec, showFloat,
               readFloat, lexDigits)

-- Reading at the Integer type avoids
-- possible difficulty with minInt

m4_define(READINST,{{
instance  Read $1  where
  readsPrec p r = [(fromInteger i, t) | (i,t) <- readsPrec p r]
}})


READINST(Int8)
READINST(Int16)
READINST(Int32)
READINST(Int64)
READINST(IntMax)
READINST(IntPtr)

m4_define(READWORD,{{
instance  Read $1  where
  readsPrec _ r = readDec r
}})

READWORD(Word)
READWORD(Word8)
READWORD(Word16)
READWORD(Word32)
READWORD(Word64)
READWORD(WordMax)
READWORD(WordPtr)


instance Read () where
    readsPrec p    = readParen False
                            (\r -> [((),t) | ("(",s) <- lex r,
                                             (")",t) <- lex s ] )

instance  Read Double  where
    readsPrec p         = readSigned readDouble

instance  Read Float  where
    readsPrec p s        = [ (doubleToFloat x,y) | (x,y) <- readSigned readDouble s]
