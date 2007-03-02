--
-- The Computer Language Shootout
-- http://shootout.alioth.debian.org/
--
-- compile with : ghc fastest.hs -o fastest
--
-- contributed by Greg Buchholz
-- Modified by Mirko Rahn, Don Stewart, Chris Kuklewicz and Lemmih
--
import Data.Char

main = print . new 0 =<< getContents

new i []       = i
new i ('-':xs) = neg 0 xs
    where neg n ('\n':xs) = new (i - n) xs
          neg n (x   :xs) = neg (parse x + (10 * n)) xs
new i (x:xs) = pos (parse x) xs
    where pos n ('\n':xs) = new (i + n) xs
          pos n (x   :xs) = pos (parse x + (10 * n)) xs

parse c = ord c - ord '0'
