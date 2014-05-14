module FrontEnd.Unlit(unlit) where

-- Part of the following code is from
-- "Report on the Programming Language Haskell",
--   version 1.2, appendix C.

import Char

data Classified = Program String | Blank | Comment
                | Include Int String | Pre String

classify :: [String] -> [Classified]
classify []                = []
classify (('\\':x):xs) | x == "begin{code}" = Blank : allProg xs
   where allProg [] = []  -- Should give an error message,
                          -- but I have no good position information.
         allProg (('\\':x):xs) |  x == "end{code}" = Blank : classify xs
	 allProg (x:xs) = Program x:allProg xs
classify (('>':x):xs)      = Program (' ':x) : classify xs
classify (('#':'l':'i':'n':'e':' ':x):xs)      = (case words x of
                                (line:file:_) | all isDigit line
                                   -> Include (read line) file
                                _  -> Pre x
                             ) : classify xs
classify (('#':x):xs)      = (case words x of
                                (line:file:_) | all isDigit line
                                   -> Include (read line) file
                                _  -> Pre x
                             ) : classify xs
classify (x:xs) | all isSpace x = Blank:classify xs
classify (x:xs)                 = Comment:classify xs

unclassify :: Classified -> String
unclassify (Program s) = s
unclassify (Pre s)     = '#':s
unclassify (Include i f) = '#':' ':show i ++ ' ':f
unclassify Blank       = ""
unclassify Comment     = ""

-- | Remove literate comments leaving normal haskell source.

unlit ::
    String      -- ^ Filename for error messages
    -> String   -- ^ literate source
    -> String   -- ^ deliterated source
unlit file lhs = (unlines
                 . map unclassify
                 . adjecent file (0::Int) Blank
                 . classify) (inlines lhs)

adjecent :: String -> Int -> Classified -> [Classified] -> [Classified]
adjecent file 0 _             (x              :xs) = x : adjecent file 1 x xs -- force evaluation of line number
adjecent file n y@(Program _) (x@Comment      :xs) = error (message file n "program" "comment")
adjecent file n y@(Program _) (x@(Include i f):xs) = x: adjecent f    i     y xs
adjecent file n y@(Program _) (x@(Pre _)      :xs) = x: adjecent file (n+1) y xs
adjecent file n y@Comment     (x@(Program _)  :xs) = error (message file n "comment" "program")
adjecent file n y@Comment     (x@(Include i f):xs) = x: adjecent f    i     y xs
adjecent file n y@Comment     (x@(Pre _)      :xs) = x: adjecent file (n+1) y xs
adjecent file n y@Blank       (x@(Include i f):xs) = x: adjecent f    i     y xs
adjecent file n y@Blank       (x@(Pre _)      :xs) = x: adjecent file (n+1) y xs
adjecent file n _             (x@next         :xs) = x: adjecent file (n+1) x xs
adjecent file n _             []                    = []

message "\"\"" n p c = "Line "++show n++": "++p++ " line before "++c++" line.\n"
message []     n p c = "Line "++show n++": "++p++ " line before "++c++" line.\n"
message file   n p c = "In file " ++ file ++ " at line "++show n++": "++p++ " line before "++c++" line.\n"

-- Re-implementation of 'lines', for better efficiency (but decreased laziness).
-- Also, importantly, accepts non-standard DOS and Mac line ending characters.
inlines s = lines' s id
  where
  lines' []             acc = [acc []]
  lines' ('\^M':'\n':s) acc = acc [] : lines' s id	-- DOS
  lines' ('\^M':s)      acc = acc [] : lines' s id	-- MacOS
  lines' ('\n':s)       acc = acc [] : lines' s id	-- Unix
  lines' (c:s)          acc = lines' s (acc . (c:))
