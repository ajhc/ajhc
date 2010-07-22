module Doc.Attr(Attr(..), attrEmpty, ansi, html)  where

import Doc.DocLike


data Attr d = Attr {
   attrBold :: d -> d,
   attrColor :: String -> d -> d
}

attrEmpty = Attr { attrBold = id, attrColor = \_ -> id }


ansi,html :: DocLike d => (String -> d) -> Attr d

ansi oob = attrEmpty {
    attrBold = \x -> oob "\27[1m" <> x <> oob attrClear,
    attrColor = \c x -> oob ("\27[" ++ ansiColor c ++ "m") <> x <> oob attrClear
        }

html oob = attrEmpty {
    attrBold = \x -> oob "<b style=\"color: white\">" <> x <> oob "</b>",
    attrColor = \c x -> oob ("<span style=\"color: " ++ c ++ ";\">") <> x <> oob "</span>"
        }


ansiColor "black" = "0;30"
ansiColor "red"  = "0;31"
ansiColor "green"  = "0;32"
ansiColor "yellow"  = "0;33"
ansiColor "blue"  = "0;94"
ansiColor "magenta" = "0;35"
ansiColor "cyan"  = "0;36"
ansiColor "white" = "0;37"
ansiColor "lightgreen"  = "0;92"
ansiColor "lightred"  = "0;91"
--ansiColor "brightblue"  = "0;94"
ansiColor _ = "0"

attrClear = "\27[0m"


