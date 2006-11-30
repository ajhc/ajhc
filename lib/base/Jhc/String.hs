-- module for things dealing with string constants needed by the compiler internally
{-# OPTIONS_JHC -N -fffi -funboxed-values #-}
module Jhc.String where


import Jhc.Prim



eqString :: [Char] -> [Char] -> Bool__
eqString [] [] = 1#
eqString (x:xs) (y:ys) = case equalsChar (unbox x) (unbox y) of
    0# -> 0#
    1# -> eqString xs ys
eqString _ _ = 0#

foreign import primitive unbox :: Char -> Char__
foreign import primitive equalsChar :: Char__ -> Char__ -> Bool__





