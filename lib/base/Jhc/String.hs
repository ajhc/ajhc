-- module for things dealing with string constants needed by the compiler internally
{-# OPTIONS_JHC -N -fffi -funboxed-values #-}
module Jhc.String(
    eqString,
    unpackASCII
    )where


import Jhc.Prim

{-# VCONSTRUCTOR unpackASCII #-}
{-# NOINLINE unpackASCII #-}
unpackASCII :: Addr__ -> [Char]
unpackASCII addr = f addr where
    f addr = case constPeekByte addr of
        0# -> []
        c -> (box c:f (increment addr))

{-# NOINLINE eqUnpacked #-}
eqUnpacked :: Addr__ -> [Char] -> Bool__
eqUnpacked addr cs = f addr cs where
    f :: Addr__ -> [Char] -> Bool__
    f offset [] = case constPeekByte offset of 0# -> 1#; _ -> 0#
    f offset (c:cs) = case constPeekByte offset of
        0# -> 0#
        uc -> case equalsChar uc (unbox c) of
            0# -> 0#
            1# -> f (increment offset) cs

-- returns it in an Char__ even though it is just a byte
foreign import primitive constPeekByte :: Addr__ -> Char__


eqString :: [Char] -> [Char] -> Bool__
eqString [] [] = 1#
eqString (x:xs) (y:ys) = case equalsChar (unbox x) (unbox y) of
    0# -> 0#
    1# -> eqString xs ys
eqString _ _ = 0#

foreign import primitive unbox :: Char -> Char__
foreign import primitive increment :: Addr__ -> Addr__
foreign import primitive box :: Char__ -> Char
foreign import primitive equalsChar :: Char__ -> Char__ -> Bool__





