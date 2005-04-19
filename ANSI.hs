--  $Id: ANSI.hs,v 1.1 2002/08/13 17:20:52 john Exp john $

-- Copyright (c) 2002 John Meacham (john@foo.net)
-- 
-- Permission is hereby granted, free of charge, to any person obtaining a
-- copy of this software and associated documentation files (the
-- "Software"), to deal in the Software without restriction, including
-- without limitation the rights to use, copy, modify, merge, publish,
-- distribute, sublicense, and/or sell copies of the Software, and to
-- permit persons to whom the Software is furnished to do so, subject to
-- the following conditions:
-- 
-- The above copyright notice and this permission notice shall be included
-- in all copies or substantial portions of the Software.
-- 
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
-- OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
-- MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
-- IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
-- CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
-- TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
-- SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

module ANSI(
    move,
    clrToEOL,
    clrFromBOL,
    clrLine,
    clrToEOS,
    clrFrombos,
    clrScreen,
    attrClear,
    attrBold,
    attrUnderline,
    attrBlink,
    attrReverse,
    cursorOn,
    cursorOff,
    attrFG,
    attrBG,
    attr
) where

--import Prelude((++), show, Int, String)
import List

move ::  Int -> Int -> String
move x y = "\27[" ++ (show y) ++ ";" ++ (show x) ++ ";H"
clrToEOL = "\27[K" 
clrFromBOL = "\27[1K" 
clrLine = "\27[2K" 
clrToEOS = "\27[J" 
clrFrombos = "\27[1J" 
clrScreen = "\27[2J" 
attrClear = "\27[0m" 
attrBold = "\27[1m" 
attrUnderline = "\27[4m" 
attrBlink = "\27[5m" 
attrReverse = "\27[7m" 
cursorOn = "\27[?25h" 
cursorOff = "\27[?25l" 
attrFG :: Int -> String
attrFG c =  "\27[3" ++ (show c) ++ "m"
attrBG :: Int -> String
attrBG c =  "\27[4" ++ (show c) ++ "m"
attr :: [Int] -> String
attr cs = "\27[" ++ concat (intersperse ";" $ map show cs) ++ "m"
