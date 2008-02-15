module Data.Char (
    isAscii, isLatin1, isControl, isPrint, isSpace, isUpper, isLower,
    isAlpha, isDigit, isOctDigit, isHexDigit, isAlphaNum,
    digitToInt, intToDigit,
    toUpper, toLower,
    ord, chr,
    readLitChar, showLitChar, lexLitChar,
    module Prelude.CType,

        -- ...and what the Prelude exports
    Char, String
    ) where

--import Array         -- Used for character name table.
import Numeric (readDec, readOct, lexDigits, readHex)
import Prelude.Text
import Jhc.Basics
import Data.Word(Word())
import Prelude.CType





-- Text functions
readLitChar          :: ReadS Char
readLitChar ('\\':s) =  readEsc s
readLitChar (c:s)    =  [(c,s)]

readEsc          :: ReadS Char
readEsc ('a':s)  = [('\a',s)]
readEsc ('b':s)  = [('\b',s)]
readEsc ('f':s)  = [('\f',s)]
readEsc ('n':s)  = [('\n',s)]
readEsc ('r':s)  = [('\r',s)]
readEsc ('t':s)  = [('\t',s)]
readEsc ('v':s)  = [('\v',s)]
readEsc ('\\':s) = [('\\',s)]
readEsc ('"':s)  = [('"',s)]
readEsc ('\'':s) = [('\'',s)]
readEsc ('^':(c:s)) | c >= '@' && c <= '_'
                 = [(chr (ord c - ord '@'), s)]
readEsc s@(d:_) | isDigit d
                 = [(chr n, t) | (n,t) <- readDec s]
readEsc ('o':s)  = [(chr n, t) | (n,t) <- readOct s]
readEsc ('x':s)  = [(chr n, t) | (n,t) <- readHex s]
readEsc s@(c:_) | isUpper c
                 = let table = ('\DEL', "DEL") : zip ['\NUL' .. ] asciiTab
                   in case [(c,s') | (c, mne) <- table,
                                     ([],s') <- [match mne s]]
                      of (pr:_) -> [pr]
                         []     -> []
readEsc _        = []

match                         :: (Eq a) => [a] -> [a] -> ([a],[a])
match (x:xs) (y:ys) | x == y  =  match xs ys
match xs     ys               =  (xs,ys)

showLitChar               :: Char -> ShowS
showLitChar c | c > '\DEL' =  showChar '\\' .
                              protectEsc isDigit (shows (ord c))
showLitChar '\DEL'         =  showString "\\DEL"
showLitChar '\\'           =  showString "\\\\"
showLitChar c | c >= ' '   =  showChar c
showLitChar '\a'           =  showString "\\a"
showLitChar '\b'           =  showString "\\b"
showLitChar '\f'           =  showString "\\f"
showLitChar '\n'           =  showString "\\n"
showLitChar '\r'           =  showString "\\r"
showLitChar '\t'           =  showString "\\t"
showLitChar '\v'           =  showString "\\v"
showLitChar '\SO'          =  protectEsc (== 'H') (showString "\\SO")
showLitChar c              =  showString ('\\' : (asciiTab!!ord c))

protectEsc p f             = f . cont
                             where cont s@(c:_) | p c = "\\&" ++ s
                                   cont s             = s

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

