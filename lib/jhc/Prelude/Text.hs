{-# OPTIONS_JHC -N #-}
module Prelude.Text (
    ReadS, ShowS,
    Read(readsPrec, readList),
    Show(showsPrec, show, showList),
    reads, shows, read, lex,
    showChar, showString, readParen, showParen,readIO,readLn ) where

-- The instances of Read and Show for
--      Bool, Maybe, Either, Ordering
-- are done via "deriving" clauses in Prelude.hs
import Jhc.Basics
import Jhc.Float
import Jhc.Inst.Show()
import Jhc.IO
import Jhc.Maybe
import Jhc.Monad
import Jhc.Num
import Jhc.Order
import Jhc.Show
import Jhc.Text.Read
import Prelude.Float
import Prelude.IO
import Jhc.Inst.Num()


import Data.Char(isSpace, isAlpha, isDigit, isAlphaNum,
                 showLitChar, readLitChar, lexLitChar)

import Numeric(showSigned, showInt, readSigned, readDec, showFloat,
               readFloat, lexDigits)

readLn :: Read a => IO a
readLn =  do l <- getLine
             r <- readIO l
             return r

  -- raises an exception instead of an error
readIO   :: Read a => String -> IO a
readIO s =  case [x | (x,t) <- reads s, ("","") <- lex t] of
              [x] -> return x
              []  -> ioError (userError "Prelude.readIO: no parse")
              _   -> ioError (userError "Prelude.readIO: ambiguous parse")



read             :: (Read a) => String -> a
read s           =  case [x | (x,t) <- reads s, ("","") <- lex t] of
                         [x] -> x
                         []  -> error "Prelude.read: no parse"
                         _   -> error "Prelude.read: ambiguous parse"



instance  Read Int  where
  readsPrec p r = [(fromInteger i, t) | (i,t) <- readsPrec p r]
        -- Reading at the Integer type avoids
        -- possible difficulty with minInt


instance  Read Integer  where
    readsPrec p         = readSigned readDec

instance  Show Float  where
    showsPrec p         = showFloat


instance  Show Double  where
    showsPrec p         = showFloat


instance  Show Char  where
    showsPrec p '\'' = showString "'\\''"
    showsPrec p c    = showChar '\'' . showLitChar c . showChar '\''

    showList cs = showChar '"' . showl cs
                 where showl ""       = showChar '"'
                       showl ('"':cs) = showString "\\\"" . showl cs
                       showl (c:cs)   = showLitChar c . showl cs

instance  Read Char  where
    readsPrec p      = readParen False
                            (\r -> [(c,t) | ('\'':s,t)<- lex r,
                                            (c,"\'")  <- readLitChar s])

    readList = readParen False (\r -> [(l,t) | ('"':s, t) <- lex r,
                                               (l,_)      <- readl s ])
        where readl ('"':s)      = [("",s)]
              readl ('\\':('&':s)) = readl s
              readl s            = [(c:cs,u) | (c ,t) <- readLitChar s,
                                               (cs,u) <- readl t       ]


instance  (Read a) => Read [a]  where
    readsPrec p      = readList



instance Read Bool where
    readsPrec d input =
              (\ inp -> [((False) , rest) | ("False" , rest) <- lex inp]) input
              ++
              (\ inp -> [((True) , rest) | ("True" , rest) <- lex inp]) input


instance Read Ordering where
    readsPrec d input =
              (\ inp -> [((LT) , rest) | ("LT" , rest) <- lex inp]) input
              ++
              (\ inp -> [((EQ) , rest) | ("EQ" , rest) <- lex inp]) input
              ++
              (\ inp -> [((GT) , rest) | ("GT" , rest) <- lex inp]) input




instance (Read a) => Read (Maybe a) where
    readsPrec d input =
	      (\ inp -> [((Nothing) , rest) | ("Nothing" , rest) <- lex inp])
	      input
	      ++
	      readParen (d > 9)
	      (\ inp ->
	       [((Just aa) , rest) | ("Just" , inp) <- lex inp ,
		(aa , rest) <- readsPrec 10 inp])
	      input

