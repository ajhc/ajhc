{-# OPTIONS_JHC -fno-prelude #-}
module Jhc.Text.Read where

import Jhc.Basics
import Jhc.Int
import Jhc.List
import Jhc.Num
import Jhc.Order
-- CI import Jhc.Type.Basic
import Prelude.CType

type  ReadS a  = String -> [(a,String)]

class  Read a  where
    readsPrec        :: Int -> ReadS a
    readList         :: ReadS [a]

        -- Minimal complete definition:
        --      readsPrec
    readList         = readParen False (\r -> [pr | ("[",s)  <- lex r,
                                                    pr       <- readl s])
                       where readl  s = [([],t)   | ("]",t)  <- lex s] ++
                                        [(x:xs,u) | (x,t)    <- reads s,
                                                    (xs,u)   <- readl' t]
                             readl' s = [([],t)   | ("]",t)  <- lex s] ++
                                        [(x:xs,v) | (",",t)  <- lex s,
                                                    (x,u)    <- reads t,
                                                    (xs,v)   <- readl' u]

reads            :: (Read a) => ReadS a
reads            =  readsPrec zero

readParen        :: Bool -> ReadS a -> ReadS a
readParen b g    =  if b then mandatory else optional
                    where optional r  = g r ++ mandatory r
                          mandatory r = [(x,u) | ("(",s) <- lex r,
                                                 (x,t)   <- optional s,
                                                 (")",u) <- lex t    ]

-- This lexer is not completely faithful to the Haskell lexical syntax.
-- Current limitations:
--    Qualified names are not handled properly
--    Octal and hexidecimal numerics are not recognized as a single token
--    Comments are not treated properly

lex              :: ReadS String
lex ""           =  [("","")]
lex (c:s)
   | isSpace c   =  lex (dropWhile isSpace s)
lex ('\'':s)     =  [('\'':ch++"'", t) | (ch,'\'':t)  <- lexLitChar s,
                                         ch /= "'" ]
lex ('"':s)      =  [('"':str, t)      | (str,t) <- lexString s]
                    where
                    lexString ('"':s) = [("\"",s)]
                    lexString s = [(ch++str, u)
                                         | (ch,t)  <- lexStrItem s,
                                           (str,u) <- lexString t  ]

                    lexStrItem ('\\':('&':s)) =  [("\\&",s)]
                    lexStrItem ('\\':(c:s)) | isSpace c
                                           =  [("\\&",t) |
                                               '\\':t <-
                                                   [dropWhile isSpace s]]
                    lexStrItem s           =  lexLitChar s

lex (c:s) | isSingle c = [([c],s)]
          | isSym c    = [(c:sym,t)       | (sym,t) <- [span isSym s]]
          | isAlpha c  = [(c:nam,t)       | (nam,t) <- [span isIdChar s]]
          | isDigit c  = [(c:(ds++fe),t)  | (ds,s')  <- [span isDigit s],
                                            (fe,t)  <- lexFracExp s'     ]
          | otherwise  = []    -- bad character
             where
              isSingle c =  c `elem` ",;()[]{}_`"
              isSym c    =  c `elem` "!@#$%&*+./<=>?\\^|:-~"
              isIdChar c =  isAlphaNum c || c `elem` "_'"

              lexFracExp ('.':(c:cs)) | isDigit c
                            = [('.':ds++e,u) | (ds,t) <- lexDigits (c:cs),
                                               (e,u)  <- lexExp t]
              lexFracExp s  = lexExp s

              lexExp (e:s) | e `elem` "eE"
                       = [(e:c:ds,u) | (c:t)  <- [s], c `elem` "+-",
                                                 (ds,u) <- lexDigits t] ++
                         [(e:ds,t)   | (ds,t) <- lexDigits s]
              lexExp s = [("",s)]

asciiTab :: [String]
asciiTab = --listArray ('\NUL', ' ')
           ["NUL", "SOH", "STX", "ETX", "EOT", "ENQ", "ACK", "BEL",
            "BS",  "HT",  "LF",  "VT",  "FF",  "CR",  "SO",  "SI",
            "DLE", "DC1", "DC2", "DC3", "DC4", "NAK", "SYN", "ETB",
            "CAN", "EM",  "SUB", "ESC", "FS",  "GS",  "RS",  "US",
            "SP"]

lexLitChar          :: ReadS String
lexLitChar ('\\':s) =  map (prefix '\\') (lexEsc s)
        where
          lexEsc :: String -> [(String,String)]
          lexEsc (c:s)     | c `elem` "abfnrtv\\\"'"  = [([c],s)]
          lexEsc ('^':(c:s)) | (c >= '@') && (c <= '_') = [(['^',c],s)]

          -- Numeric escapes
          lexEsc ('o':s)               = [prefix 'o' (span isOctDigit s)]
          lexEsc ('x':s)               = [prefix 'x' (span isHexDigit s)]
          lexEsc s@(d:_)   | isDigit d = [span isDigit s]

          -- Very crude approximation to \XYZ.
          lexEsc s@(c:_)   | isUpper c = [span isCharName s]
          lexEsc _                     = []

          isCharName c   = isUpper c || isDigit c
          prefix c (t,s) = (c:t, s)

lexLitChar (c:s)    =  [([c],s)]
lexLitChar ""       =  []

lexDigits        :: ReadS String
lexDigits        =  nonnull isDigit

nonnull          :: (Char -> Bool) -> ReadS String
nonnull p s      =  [(cs,t) | (cs@(_:_),t) <- [span p s]]

instance (Read a,Read b) => Read (Either a b) where
    readsPrec d input =
	      readParen (d > 9)
	      (\ inp ->
	       [((Left aa) , rest) | ("Left" , inp) <- lex inp ,
		(aa , rest) <- readsPrec 10 inp])
	      input
	      ++
	      readParen (d > 9)
	      (\ inp ->
	       [((Right aa) , rest) | ("Right" , inp) <- lex inp ,
		(aa , rest) <- readsPrec 10 inp])
	      input

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

-- readInt reads a string of digits using an arbitrary base.
-- Leading minus signs must be handled elsewhere.

{-# SPECIALIZE readInt :: Int -> (Char -> Bool) -> (Char -> Int) -> ReadS Int #-}
{-# SPECIALIZE readInt :: Integer -> (Char -> Bool) -> (Char -> Int) -> ReadS Integer #-}

readInt :: (Integral a) => a -> (Char -> Bool) -> (Char -> Int) -> ReadS a
readInt radix isDig digToInt s =
   [(foldl1 (\n d -> n * radix + d) (map (fromIntegral . digToInt) ds), r)
          | (ds,r) <- nonnull isDig s ]

-- Unsigned readers for various bases
readDec, readOct, readHex :: (Integral a) => ReadS a
readDec = readInt 10 isDigit    digitToInt
readOct = readInt  8 isOctDigit digitToInt
readHex = readInt 16 isHexDigit digitToInt
