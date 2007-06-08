{-# OPTIONS_JHC -funboxed-tuples #-}
module Prelude(
    -- export everything here
    module Prelude,
    -- export types from elsewhere
    IO(),
    IOError(),
    Rational(),
    -- functions from elsewhere
    putStr,
    putStrLn,
    error,
    concatMap,
    concat,
    any,
    all,
    subtract,
    even,
    odd,
    foldr,
    and,
    filter,
    or,
    (!!),
    sequence,
    sequence_,
    -- submodules
    module Jhc.Basics,
    module Jhc.Float,
    module Jhc.Enum,
    module Jhc.Order,
    module Jhc.Show,
    Num(..),
    fromIntegral,
    realToFrac,
    Real(..),
    Integral(..),
    Fractional(..),
    Floating(..),
    RealFrac(properFraction,truncate,round,ceiling,floor),
    RealFloat(..),
    module Jhc.Monad,
    Int(),

    module Prelude.IO,
    module Prelude.Text
    ) where


import Jhc.Basics
import Jhc.Float
import Data.Int(Int())

import Jhc.Inst.Enum
import Jhc.Inst.Read
import Jhc.Inst.Show

import Data.Ratio
import Jhc.Enum
import Jhc.IO
import Jhc.List
import Jhc.Monad
import Jhc.Num
import Jhc.Order
import Jhc.Show
import Jhc.Tuples
import Prelude.Float
import Prelude.IO
import Prelude.IOError
import Prelude.Text
import qualified Data.Char as Char(isSpace,ord,chr)



-- infixr 9  .
--infixr 8  ^, ^^, **
infixr 8  ^, ^^
--infixl 7  *  , /, `quot`, `rem`, `div`, `mod`
--infixl 6  +, -
--infixr 5  :
--infix  4  ==, /=, <, <=, >=, >
--infixr 3  &&
--infixr 2  ||
--infixl 1  >>, >>=
--infixr 1  =<<
-- infixr 0  $, $!, `seq`



-- Numeric functions




{-# SPECIALIZE gcd :: Int -> Int -> Int #-}
{-# SPECIALIZE gcd :: Integer -> Integer -> Integer #-}
gcd              :: (Integral a) => a -> a -> a
gcd 0 0          =  error "Prelude.gcd: gcd 0 0 is undefined"
gcd x y          =  gcd' (abs x) (abs y)
                    where gcd' x 0  =  x
                          gcd' x y  =  gcd' y (x `rem` y)


{-# SPECIALIZE lcm :: Int -> Int -> Int #-}
{-# SPECIALIZE lcm :: Integer -> Integer -> Integer #-}
lcm              :: (Integral a) => a -> a -> a
lcm _ 0          =  0
lcm 0 _          =  0
lcm x y          =  abs ((x `quot` (gcd x y)) * y)


{-# SPECIALIZE (^) :: Int -> Int -> Int #-}
{-# SPECIALIZE (^) :: Integer -> Int -> Integer #-}
{-# SPECIALIZE (^) :: Double -> Int -> Double #-}

(^)              :: (Num a, Integral b) => a -> b -> a
x ^ 0            =  1
x ^ n | n > 0    =  f x (n-1) x
                    where f _ 0 y = y
                          f x n y = g x n  where
                                    g x n | even n  = g (x*x) (n `quot` 2)
                                          | otherwise = f x (n-1) (x*y)
_ ^ _            = error "Prelude.^: negative exponent"


(^^)             :: (Fractional a, Integral b) => a -> b -> a
x ^^ n           =  if n >= 0 then x^n else recip (x^(-n))






instance Monad Maybe where
    return x = Just x
    Nothing >>= _ = Nothing
    Just x >>= y = y x
    fail _ = Nothing



instance Functor Maybe where
    fmap _ Nothing = Nothing
    fmap f (Just x) = Just (f x)



-- Maybe

data Maybe a  =  Nothing | Just a
    deriving (Eq, Ord, Read, Show)


maybe :: b -> (a -> b) -> Maybe a -> b
maybe n f m = case m of
    Just x -> f x
    Nothing -> n

data Either a b = Left a | Right b
    deriving (Eq, Ord, Read, Show)

either :: (a -> c) -> (b -> c) -> Either a b -> c
either f g (Left x)  =  f x
either f g (Right y) =  g y



until            :: (a -> Bool) -> (a -> a) -> a -> a
until p f x
     | p x       =  x
     | otherwise =  until p f (f x)





{-# SUPERINLINE head, tail, null #-}
head             :: [a] -> a
head (x:_)       =  x
head []          =  error "Prelude.head: empty list"


tail             :: [a] -> [a]
tail (_:xs)      =  xs
tail []          =  error "Prelude.tail: empty list"


last             :: [a] -> a
last []          =  error "Prelude.last: empty list"
last (x:xs)      = last' x xs where
    last' x []     = x
    last' _ (y:ys) = last' y xs


init             :: [a] -> [a]
init []          =  error "Prelude.init: empty list"
init (x:xs)      =  init' x xs where
    init' _ [] = []
    init' y (z:zs) = y:init' z zs


null             :: [a] -> Bool
null []          =  True
null (_:_)       =  False

-- length returns the length of a finite list as an Int.

length           :: [a] -> Int
length xs = f xs 0 where
    f [] n = n
    f (_:xs) n = f xs $! n + 1


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




foldl1           :: (a -> a -> a) -> [a] -> a
foldl1 f (x:xs)  =  foldl f x xs
foldl1 _ []      =  error "Prelude.foldl1: empty list"



scanl1           :: (a -> a -> a) -> [a] -> [a]
scanl1 f (x:xs)  =  scanl f x xs
scanl1 _ []      =  []

-- foldr, foldr1, scanr, and scanr1 are the right-to-left duals of the
-- above functions.


--foldr :: (a -> b -> b) -> b -> [a] -> b
--foldr k z [] = z
--foldr k z (x:xs) = k x (foldr k z xs)


foldr1           :: (a -> a -> a) -> [a] -> a
foldr1 f [x]     =  x
foldr1 f (x:xs)  =  f x (foldr1 f xs)
foldr1 _ []      =  error "Prelude.foldr1: empty list"


scanr             :: (a -> b -> b) -> b -> [a] -> [b]
scanr f q0 []     =  [q0]
scanr f q0 (x:xs) =  f x q : qs where qs@(q:_) = scanr f q0 xs


scanr1          :: (a -> a -> a) -> [a] -> [a]
scanr1 f []     =  []
scanr1 f [x]    =  [x]
scanr1 f (x:xs) =  f x q : qs where qs@(q:_) = scanr1 f xs


-- replicate n x is a list of length n with x the value of every element

replicate        :: Int -> a -> [a]
replicate n x    = f n where
    f n | n <= 0 = []
    f n = let n' = n - 1 in n' `seq` (x:f n')

-- cycle ties a finite list into a circular one, or equivalently,
-- the infinite repetition of the original list.  It is the identity
-- on infinite lists.


cycle            :: [a] -> [a]
cycle []         =  error "Prelude.cycle: empty list"
cycle xs         =  xs' where xs' = xs ++ xs'

-- take n, applied to a list xs, returns the prefix of xs of length n,
-- or xs itself if n > length xs.  drop n xs returns the suffix of xs
-- after the first n elements, or [] if n > length xs.  splitAt n xs
-- is equivalent to (take n xs, drop n xs).


take :: Int -> [a] -> [a]
take n xs = f n xs where
    f n _      | n <= 0 =  []
    f _ []              =  []
    f n (x:xs)          =  x : f (n-1) xs


drop :: Int -> [a] -> [a]
drop n xs = f n xs where
    f n xs | n <= 0 =  xs
    f _ [] = []
    f n (_:xs) = f (n-1) xs



splitAt                  :: Int -> [a] -> ([a],[a])
--splitAt n xs             =  (take n xs, drop n xs)
splitAt n ls | n < 0	= ([], ls)
splitAt n ls = splitAt' n ls where
    splitAt' :: Int -> [a] -> ([a], [a])
    splitAt' 0  xs  = ([], xs)
    splitAt' _  []  = ([], [])
    splitAt' m (x:xs) = case splitAt' (m - 1) xs of
        (xs', xs'') -> (x:xs', xs'')

-- takeWhile, applied to a predicate p and a list xs, returns the longest
-- prefix (possibly empty) of xs of elements that satisfy p.  dropWhile p xs
-- returns the remaining suffix.  span p xs is equivalent to
-- (takeWhile p xs, dropWhile p xs), while break p uses the negation of p.


takeWhile               :: (a -> Bool) -> [a] -> [a]
takeWhile p []          =  []
takeWhile p (x:xs)
            | p x       =  x : takeWhile p xs
            | otherwise =  []


dropWhile               :: (a -> Bool) -> [a] -> [a]
dropWhile p []          =  []
dropWhile p xs@(x:xs')
            | p x       =  dropWhile p xs'
            | otherwise =  xs

span, break             :: (a -> Bool) -> [a] -> ([a],[a])
span p []            = ([],[])
span p xs@(x:xs')
            | p x       =  (x:ys,zs)
            | otherwise =  ([],xs)
                           where (ys,zs) = span p xs'

{-# INLINE break #-}
break p                 =  span (not . p)

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

-- elem is the list membership predicate, usually written in infix form,
-- e.g., x `elem` xs.  notElem is the negation.

infix  4  `elem`, `notElem`


-- the implementation looks a little funny, but the reason for the
-- inner loop is so that both the == function and the unboxing of the
-- argument may occur right away outside the inner loop when the list isn't
-- empty.


elem, notElem    :: (Eq a) => a -> [a] -> Bool
elem _ []	= False
elem x (y:ys)
    | x == y = True
    | otherwise = f y ys where
        f y _ | x == y = True
        f _ (y:ys) = f y ys
        f _ [] = False

{-# SPECIALIZE elem :: Char -> String -> Bool #-}
{-# SPECIALIZE elem :: Int -> [Int] -> Bool #-}
{-# RULES "elem/[]" forall c . elem c [] = False #-}
{-# RULES "elem/[_]" forall c v . elem c [v] = c == v #-}

notElem	_ []	=  True
notElem x (y:ys)
    | x == y = False
    | otherwise = f y ys where
        f y ys | x == y = False
        f _ (y:ys) = f y ys
        f _ [] = True

{-# SPECIALIZE notElem :: Char -> String -> Bool #-}
{-# SPECIALIZE notElem :: Int -> [Int] -> Bool #-}
{-# RULES "notElem/[]" forall c . notElem c [] = True #-}
{-# RULES "notElem/[_]" forall c v . notElem c [v] = c /= v #-}

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


instance Real Integer where
    toRational = fromInteger
instance Real Int where
    toRational = fromInt



{-# RULES "iterate/id" forall . iterate id = repeat #-}
{-# RULES "head/iterate"  forall f x . head (iterate f x) = x #-}
{-# RULES "head/repeat"   forall x . head (repeat x) = x #-}
{-# RULES "tail/repeat"   forall x . tail (repeat x) = repeat x #-}
{-# RULES "tail/iterate"  forall f x . tail (iterate f x) = iterate f (f x) #-}
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
{-# RULES "tail/map"      forall f xs . tail (map f xs) = map f (tail xs) #-}
{-# RULES "head/map"      forall f xs . head (map f xs) = f (head xs) #-}
{-# RULES "head/:"        forall x xs . head (x:xs) = x #-}
{-# RULES "tail/:"        forall x xs . tail (x:xs) = xs #-}
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
{-# RULES "sequence/[]"   sequence [] = return [] #-}
{-# RULES "sequence_/[]"  sequence_ [] = return () #-}
{-# RULES "mapM/[]"       forall f . mapM f [] = return [] #-}
{-# RULES "mapM_/[]"      forall f . mapM_ f [] = return () #-}
{-# RULES "concatMap/++"  forall xs ys f . concatMap f (xs ++ ys) = concatMap f xs ++ concatMap f ys #-}
{-# RULES "map/++"        forall xs ys f . map f (xs ++ ys) = map f xs ++ map f ys #-}
{-# RULES "sequence_/++"  forall xs ys . sequence_ (xs ++ ys) = sequence_ xs >> sequence_ ys #-}
{-# RULES "mapM_/++"      forall xs ys f . mapM_ f (xs ++ ys) = mapM_ f xs >> mapM_ f ys #-}

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
