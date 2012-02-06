{-# OPTIONS_JHC -fno-prelude #-}
module Jhc.Show where

import Jhc.Int
import Jhc.Basics

type  ShowS    = String -> String

class  Show a  where
    showsPrec        :: Int -> a -> ShowS
    show             :: a -> String
    showList         :: [a] -> ShowS

        -- Mimimal complete definition:
        --      show or showsPrec
    showsPrec _ x s   = show x ++ s

    show x            = showsPrec zero x ""

    showList []       = showString "[]"
    showList (x:xs)   = showChar '[' . shows x . showl xs
                        where showl []     = showChar ']'
                              showl (x:xs) = showChar ',' . shows x .
                                             showl xs

shows            :: (Show a) => a -> ShowS
shows            =  showsPrec zero

{-# INLINE showChar, showString #-}
showChar         :: Char -> ShowS
showChar         =  (:)

showString       :: String -> ShowS
showString       =  (++)

showParen        :: Bool -> ShowS -> ShowS
showParen b p    =  if b then showChar '(' . p . showChar ')' else p

instance Show () where
    showsPrec _ () = showString "()"

instance (Show a, Show b) => Show (a,b)  where
    showsPrec _ (x,y) = showChar '(' . shows x . showChar ',' .
                                          shows y . showChar ')'

instance (Show a, Show b, Show c) => Show (a, b, c) where
    showsPrec _ (x,y,z) = showChar '(' . shows x . showChar ',' .
					    shows y . showChar ',' .
					    shows z . showChar ')'

instance Show a => Show [a]  where
    showsPrec p      = showList

instance Show Bool where
    showsPrec d (False) = showString "False"
    showsPrec d (True) = showString "True"

instance Show Ordering where
    showsPrec d (LT) = showString "LT"
    showsPrec d (EQ) = showString "EQ"
    showsPrec d (GT) = showString "GT"
