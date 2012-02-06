{-# OPTIONS_JHC -funboxed-tuples #-}
module Prelude(
    -- Prelude
    Bool(False, True),
    Maybe(Nothing, Just),
    Either(Left, Right),
    Ordering(LT, EQ, GT),
    Char, String, Int, Integer, Float, Double, Rational, IO,
    module Jhc.Basics, -- for list
--  List type: []((:), [])
--  Tuple types: (,)((,)), (,,)((,,)), etc.
--  Trivial type: ()(())
--  Functions: (->)

    Eq((==), (/=)),
    Ord(compare, (<), (<=), (>=), (>), max, min),
    Enum(succ, pred, toEnum, fromEnum, enumFrom, enumFromThen,
         enumFromTo, enumFromThenTo),
    Bounded(minBound, maxBound),
    Num((+), (-), (*), negate, abs, signum, fromInteger),
    Real(toRational),
    Integral(quot, rem, div, mod, quotRem, divMod, toInteger),
    Fractional((/), recip, fromRational),
    Floating(pi, exp, log, sqrt, (**), logBase, sin, cos, tan,
             asin, acos, atan, sinh, cosh, tanh, asinh, acosh, atanh),
    RealFrac(properFraction, truncate, round, ceiling, floor),
    RealFloat(floatRadix, floatDigits, floatRange, decodeFloat,
              encodeFloat, exponent, significand, scaleFloat, isNaN,
              isInfinite, isDenormalized, isIEEE, isNegativeZero, atan2),
    Monad((>>=), (>>), return, fail),
    Functor(fmap),
    mapM, mapM_, sequence, sequence_, (=<<),
    maybe, either,
    (&&), (||), not, otherwise,
    subtract, even, odd, gcd, lcm, (^), (^^),
    fromIntegral, realToFrac,
    fst, snd, curry, uncurry, id, const, (.), flip, ($), until,
    asTypeOf, error, undefined,
    seq, ($!),

    -- PreludeList
    map, (++), filter, concat, concatMap,
    head, last, tail, init, null, length, (!!),
    foldl, foldl1, scanl, scanl1, foldr, foldr1, scanr, scanr1,
    iterate, repeat, replicate, cycle,
    take, drop, splitAt, takeWhile, dropWhile, span, break,
    lines, words, unlines, unwords, reverse, and, or,
    any, all, elem, notElem, lookup,
    sum, product, maximum, minimum,
    zip, zip3, zipWith, zipWith3, unzip, unzip3,

    -- PreludeText
    ReadS, ShowS,
    Read(readsPrec, readList),
    Show(showsPrec, show, showList),
    reads, shows, read, lex,
    showChar, showString, readParen, showParen,

    -- PreludeIO
    FilePath, IOError, ioError, userError, catch,
    putChar, putStr, putStrLn, print,
    getChar, getLine, getContents, interact,
    readFile, writeFile, appendFile, readIO, readLn
    ) where

import Jhc.Basics
import Jhc.Float

-- CI import Jhc.Inst.Enum
-- CI import Jhc.Inst.Num
-- CI import Jhc.Inst.Order
-- CI import Jhc.Inst.Read
-- CI import Jhc.Inst.Show
-- CI import Jhc.Inst.Storable

-- CI import Data.Ratio
import Jhc.Enum
import Jhc.IO
import Jhc.List
import Jhc.Maybe
import Jhc.Monad
import Jhc.Num
import Jhc.Numeric
import Jhc.Order
-- CI import Jhc.Show
-- CI import Jhc.Tuples
-- CI import Jhc.Type.Basic
-- CI import Jhc.Type.Word(Int)
-- CI import Prelude.Float
import Prelude.IO
import Prelude.Text
import qualified Data.Char as Char(isSpace,ord,chr)

--infixr 9  .
--infixr 8  ^, ^^, **
--infixr 8  ^, ^^
--infixl 7  *  , /, `quot`, `rem`, `div`, `mod`
--infixl 6  +, -
--infixr 5  :
--infix  4  ==, /=, <, <=, >=, >
--infixr 3  &&
--infixr 2  ||
--infixl 1  >>, >>=
--infixr 1  =<<
--infixr 0  $, $!, `seq`

either :: (a -> c) -> (b -> c) -> Either a b -> c
either f g (Left x)  =  f x
either f g (Right y) =  g y

until :: (a -> Bool) -> (a -> a) -> a -> a
until p f x
     | p x       =  x
     | otherwise =  until p f (f x)

-- foldl, applied to a binary operator, a starting value (typically the
-- left-identity of the operator), and a list, reduces the list using
-- the binary operator, from left to right:
--  foldl f z [x1, x2, ..., xn] == (...((z `f` x1) `f` x2) `f`...) `f` xn
-- foldl1 is a variant that has no starting value argument, and  thus must
-- be applied to non-empty lists.  scanl is similar to foldl, but returns
-- a list of successive reduced values from the left:
--      scanl f z [x1, x2, ...] == [z, z `f` x1, (z `f` x1) `f` x2, ...]
-- Note that  last (scanl f z xs) == foldl f z xs.
-- scanl1 is similar, again without the starting element:
--      scanl1 f [x1, x2, ...] == [x1, x1 `f` x2, ...]

-- cycle ties a finite list into a circular one, or equivalently,
-- the infinite repetition of the original list.  It is the identity
-- on infinite lists.

cycle            :: [a] -> [a]
cycle []         =  error "Prelude.cycle: empty list"
cycle xs         =  xs' where xs' = xs ++ xs'

-- lines breaks a string up into a list of strings at newline characters.
-- The resulting strings do not contain newlines.  Similary, words
-- breaks a string up into a list of words, which were delimited by
-- white space.  unlines and unwords are the inverse operations.
-- unlines joins lines with terminating newlines, and unwords joins
-- words with separating spaces.

lines            :: String -> [String]
lines ""         =  []
lines s          =  let (l, s') = break (== '\n') s
                      in  l : case s' of
                                []      -> []
                                (_:s'') -> lines s''

words            :: String -> [String]
words s          =  case dropWhile Char.isSpace s of
                      "" -> []
                      s' -> w : words s''
                            where (w, s'') = break Char.isSpace s'

unlines          :: [String] -> String
unlines [] = []
unlines (l:ls) = l ++ '\n' : unlines ls
--unlines          =  concatMap (++ "\n")

unwords          :: [String] -> String
unwords []		=  ""
unwords [w]		= w
unwords (w:ws)		= w ++ ' ' : unwords ws

-- lookup key assocs looks up a key in an association list.

{- SPECIALIZE lookup :: forall b . Char -> (Char,b) -> Maybe b #-}
{- SPECIALIZE lookup :: forall b . Int -> (Int,b) -> Maybe b #-}

lookup           :: (Eq a) => a -> [(a,b)] -> Maybe b
lookup key []    =  Nothing
lookup key ((x,y):xys)
    | key == x   =  Just y
    | otherwise  =  f x y xys where
        f x y _ | key == x = Just y
        f _ _ ((x,y):xys)  = f x y xys
        f _ _ []           = Nothing

-- sum and product compute the sum or product of a finite list of numbers.

sum, product     :: (Num a) => [a] -> a
--sum              =  foldl (+) 0
--product          =  foldl (*) 1
sum l	= sum' l 0 where
    sum' []     a = a
    sum' (x:xs) a = sum' xs (a+x)
product	l = prod l 1 where
    prod []     a = a
    prod (x:xs) a = prod xs (a*x)

sum' l	= rsum l 0 where
    rsum []     a = a
    rsum (x:xs) a = a `seq` rsum xs (a+x)

{-# SPECIALIZE sum' :: [Int] -> Int #-}
{-# RULES "sum/Int" forall . sum = sum' :: [Int] -> Int #-}
{-# SPECIALIZE sum' :: [Double] -> Double #-}
{-# RULES "sum/Double" forall . sum = sum' :: [Double] -> Double #-}

-- maximum and minimum return the maximum or minimum value from a list,
-- which must be non-empty, finite, and of an ordered type.

maximum, minimum :: (Ord a) => [a] -> a
maximum []       =  error "Prelude.maximum: empty list"
maximum xs       =  foldl1 max xs

minimum []       =  error "Prelude.minimum: empty list"
minimum xs       =  foldl1 min xs

zip3             :: [a] -> [b] -> [c] -> [(a,b,c)]
zip3             =  zipWith3 (\a b c -> (a,b,c))

zipWith3         :: (a->b->c->d) -> [a]->[b]->[c]->[d]
zipWith3 z (a:as) (b:bs) (c:cs)
                 =  z a b c : zipWith3 z as bs cs
zipWith3 _ _ _ _ =  []

-- unzip transforms a list of pairs into a pair of lists.

unzip            :: [(a,b)] -> ([a],[b])
unzip            =  foldr (\(a,b) ~(as,bs) -> (a:as,b:bs)) ([],[])

unzip3           :: [(a,b,c)] -> ([a],[b],[c])
unzip3           =  foldr (\(a,b,c) ~(as,bs,cs) -> (a:as,b:bs,c:cs))
                          ([],[],[])

{-# RULES "drop/0"        forall . drop 0 = \xs -> xs #-}
{-# RULES "drop/1"        forall x xs . drop 1 (x:xs) = xs #-}
{-# RULES "drop/2"        forall x y xs . drop 2 (x:y:xs) = xs #-}
{-# RULES "drop/3"        forall x y z xs . drop 3 (x:y:z:xs) = xs #-}
{-# RULES "take/0"        forall xs . take 0 xs = [] #-}
{-# RULES "take/1"        forall x xs . take 1 (x:xs) = [x] #-}
{-# RULES "take/2"        forall x y xs . take 2 (x:y:xs) = [x,y] #-}
{-# RULES "take/3"        forall x y z xs . take 3 (x:y:z:xs) = [x,y,z] #-}
{-# RULES "!!/0"          forall x xs . (x:xs) !! 0 = x #-}
{-# RULES "!!/1"          forall x y xs . (x:y:xs) !! 1 = y #-}
{-# RULES "!!/2"          forall x y z xs . (x:y:z:xs) !! 2 = z #-}
{-# RULES "concat/Map"    forall f xs . concat (map f xs) = concatMap f xs #-}
{-# RULES "sequence/map"  forall f xs . sequence (map f xs) = mapM f xs #-}
{-# RULES "sequence_/map" forall f xs . sequence_ (map f xs) = mapM_ f xs #-}
{-# RULES "++/emptyr"     forall xs . xs ++ [] = xs #-}
{-# RULES "++/refix"      forall xs ys zs . (xs ++ ys) ++ zs = xs ++ (ys ++ zs) #-}
--{-# RULES "++/tick4"      forall x y z x' xs ys . (x:y:z:x':xs) ++ ys = x:y:z:x':(xs ++ ys) #-}
--{-# RULES "++/tick2"      forall x y xs ys . (x:y:xs) ++ ys = x:y:(xs ++ ys) #-}
--{-# RULES "++/tick1"      forall x xs ys . (x:xs) ++ ys = x:(xs ++ ys) #-}
{-# RULES "++/tick0"      forall xs . [] ++ xs = xs #-}
{-# RULES "++/tick1"      forall x xs . [x] ++ xs = x:xs #-}
{-# RULES "++/tick2"      forall x y xs . [x,y] ++ xs = x:y:xs #-}
{-# RULES "++/tick3"      forall x y z xs . [x,y,z] ++ xs = x:y:z:xs #-}
{-# RULES "map/map"       forall f g xs . map f (map g xs) = map (\x -> f (g x)) xs #-}
{-# RULES "concatMap/map" forall f g xs . concatMap f (map g xs) = concatMap (\x -> f (g x)) xs #-}
{---# RULES "concat/tick"   forall x xs . concat (x:xs) = x ++ concat xs #-}
{-# RULES "concat/[]"     concat [] = [] #-}
{-# RULES "map/[]"        forall f . map f [] = [] #-}
{-# RULES "concatMap/[]"  forall f . concatMap f [] = [] #-}
{-# RULES "concatMap/++"  forall xs ys f . concatMap f (xs ++ ys) = concatMap f xs ++ concatMap f ys #-}
{-# RULES "map/++"        forall xs ys f . map f (xs ++ ys) = map f xs ++ map f ys #-}

{-# RULES "foldr/map" forall k z f xs . foldr k z (map f xs) = foldr (\x y -> k (f x) y) z xs #-}
{-# RULES "foldr/concatMap" forall k z f xs . foldr k z (concatMap f xs) = foldr (\x y -> foldr k (f x) y) z xs #-}
{-# RULES "foldr/filter" forall k z f xs . foldr k z (filter f xs) = foldr (\x y -> if f x then k x y else y) z xs #-}
{-# RULES "foldr/++" forall k z xs ys . foldr k z (xs ++ ys) = foldr k (foldr k z ys) xs #-}
{-# RULES "foldr/concat" forall k z xs . foldr k z (concat xs) = foldr (\x y -> foldr k y x) z xs #-}
{-# RULES "foldr/repeat" forall k _z x . foldr k _z (repeat x) = let r = k x r in r #-}
-- causes horrible code bloat
-- {-# RULES "foldr/x:xs" forall k z x xs . foldr k z (x:xs) = k x (foldr k z xs) #-}
{-# RULES "foldr/zip" forall k z xs ys . foldr k z (zip xs ys) = let zip' (a:as) (b:bs) = k (a,b) (zip' as bs); zip' _ _ = z in zip' xs ys #-}
-- {-# RULES "foldr/sequence" forall k z xs . foldr k z (sequence xs) = foldr (\x y -> do rx <- x; ry <- y; return (k rx ry)) (return z) xs #-}
-- {-# RULES "foldr/mapM" forall k z f xs . foldr k z (mapM f xs) = foldr (\x y -> do rx <- f x; ry <- y; return (k rx ry)) (return z) xs   #-}

default(Int,Double)
