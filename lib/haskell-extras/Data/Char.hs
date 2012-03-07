module Data.Char (
    isAscii, isLatin1, isControl, isPrint, isSpace, isUpper, isLower,
    isAlpha, isDigit, isOctDigit, isHexDigit, isAlphaNum,
    digitToInt, intToDigit,
    toUpper, toLower,
    ord, chr,
    readLitChar, showLitChar, lexLitChar,

        -- ...and what the Prelude exports
    Char, String
    ) where

import Jhc.Basics
import Jhc.Enum
import Jhc.List
import Jhc.Num
import Jhc.Order
import Jhc.Show
import Jhc.Text.Read
import Prelude.Text
import Prelude.CType
