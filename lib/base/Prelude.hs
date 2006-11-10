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
    foldr,
    and,
    or,
    (!!),
    sequence,
    sequence_,
    -- submodules
    module Jhc.Basics,
    module Jhc.Float,
    module Jhc.Enum,
    module Jhc.Order,
    module Jhc.Monad,
    Int(),

    module Prelude.IO,
    module Prelude.Text
    ) where


import Jhc.Basics
import Jhc.Float
import Data.Int(Int())

import Prelude.IO
import Prelude.IOError
import Prelude.Text
import Prelude.Float
import Data.Ratio
import qualified Data.Char as Char(isSpace,ord,chr)
import Jhc.IO
import Jhc.Tuples
import Jhc.List
import Jhc.Enum
import Jhc.Monad
import Jhc.Order


-- infixr 9  .
infixr 8  ^, ^^, **
infixl 7  *  , /, `quot`, `rem`, `div`, `mod`
infixl 6  +, -
--infixr 5  :
--infix  4  ==, /=, <, <=, >=, >
--infixr 3  &&
--infixr 2  ||
--infixl 1  >>, >>=
--infixr 1  =<<
-- infixr 0  $, $!, `seq`







-- Numeric classes


class  (Eq a, Show a) => Num a  where
    (+), (-), (*)    :: a -> a -> a
    negate           :: a -> a
    abs, signum      :: a -> a
    fromInteger      :: Integer -> a
    fromInt          :: Int -> a
--    fromIntMax       :: IntMax -> a
--    fromWordMax      :: WordMax -> a

        -- Minimal complete definition:
        --      All, except negate or (-)
    x - y            =  x + negate y
    negate x         =  0 - x
    fromInt i = fromInteger (toInteger i)
    fromInteger x = fromInt (toInt x)


class  (Num a, Ord a) => Real a  where
    toRational       ::  a -> Rational


class  (Real a, Enum a) => Integral a  where
    quot, rem        :: a -> a -> a
    div, mod         :: a -> a -> a
    quotRem, divMod  :: a -> a -> (a,a)
    toInteger        :: a -> Integer
    toInt            :: a -> Int
--    toIntMax         :: a -> IntMax
--    toWordMax        :: a -> WordMax

        -- Minimal complete definition:
        --      quotRem, toInteger
    n `quot` d       =  q  where (q,r) = quotRem n d
    n `rem` d        =  r  where (q,r) = quotRem n d
    n `div` d        =  q  where (q,r) = divMod n d
    n `mod` d        =  r  where (q,r) = divMod n d
    divMod n d       =  if signum r == - signum d then (q-1, r+d) else qr
                        where qr@(q,r) = quotRem n d
    quotRem n d       =  (n `quot` d, n `rem` d)
    --toInteger x = Integer (toInt x)
    --toInt x = case toInteger x of
    --    Integer y -> y
    toInteger x = toInteger (toInt x)
    toInt x = toInt (toInteger x)
    --toIntMax x = toIntMax (toInteger x)
    --toWordMax x = toWordMax (toInteger x)


class  (Num a) => Fractional a  where
    (/)              :: a -> a -> a
    recip            :: a -> a
    fromRational     :: Rational -> a

        -- Minimal complete definition:
        --      fromRational and (recip or (/))
    recip x          =  1 / x
    x / y            =  x * recip y


class  (Fractional a) => Floating a  where
    pi                  :: a
    exp, log, sqrt      :: a -> a
    (**), logBase       :: a -> a -> a
    sin, cos, tan       :: a -> a
    asin, acos, atan    :: a -> a
    sinh, cosh, tanh    :: a -> a
    asinh, acosh, atanh :: a -> a

        -- Minimal complete definition:
        --      pi, exp, log, sin, cos, sinh, cosh
        --      asin, acos, atan
        --      asinh, acosh, atanh
    x ** y           =  exp (log x * y)
    logBase x y      =  log y / log x
    sqrt x           =  x ** (1 / 2) -- 0.5        -- TODO Doubles
    tan  x           =  sin  x / cos  x
    tanh x           =  sinh x / cosh x



-- TODO Doubles
class  (Real a, Fractional a) => RealFrac a  where
    properFraction   :: (Integral b) => a -> (b,a)
    truncate, round  :: (Integral b) => a -> b
    ceiling, floor   :: (Integral b) => a -> b

        -- Minimal complete definition:
        --      properFraction
    truncate x       =  m  where (m,_) = properFraction x

    round x          =  let (n,r) = properFraction x
                            m     = if r < 0 then n - 1 else n + 1
                          in case signum (abs r - 0.5) of
                                -1 -> n
                                0  -> if even n then n else m
                                1  -> m

    ceiling x        =  if r > 0 then n + 1 else n
                        where (n,r) = properFraction x

    floor x          =  if r < 0 then n - 1 else n
                        where (n,r) = properFraction x


-- TODO Doubles
class  (RealFrac a, Floating a) => RealFloat a  where
    floatRadix       :: a -> Integer
    floatDigits      :: a -> Int
    floatRange       :: a -> (Int,Int)
    decodeFloat      :: a -> (Integer,Int)
    encodeFloat      :: Integer -> Int -> a
    exponent         :: a -> Int
    significand      :: a -> a
    scaleFloat       :: Int -> a -> a
    isNaN, isInfinite, isDenormalized, isNegativeZero, isIEEE
                     :: a -> Bool
    atan2            :: a -> a -> a

        -- Minimal complete definition:
        --      All except exponent, significand,
        --                 scaleFloat, atan2
    exponent x       =  if m == 0 then 0 else n + floatDigits x
                        where (m,n) = decodeFloat x

    significand x    =  encodeFloat m (- floatDigits x)
                        where (m,_) = decodeFloat x

    scaleFloat k x   =  encodeFloat m (n+k)
                        where (m,n) = decodeFloat x

    atan2 y x
      | x>0           =  atan (y/x)
      | x==0 && y>0   =  pi/2
      | x<0  && y>0   =  pi + atan (y/x)
      |(x<=0 && y<0)  ||
       (x<0 && isNegativeZero y) ||
       (isNegativeZero x && isNegativeZero y)
                      = -atan2 (-y) x
      | y==0 && (x<0 || isNegativeZero x)
                      =  pi    -- must be after the previous test on zero y
      | x==0 && y==0  =  y     -- must be after the other double zero tests
      | otherwise     =  x + y -- x or y is a NaN, return a NaN (via +)

instance Enum () where
    succ _      = error "Prelude.Enum.().succ: bad argument"
    pred _      = error "Prelude.Enum.().pred: bad argument"

    toEnum x | x == 0 = ()
             | otherwise    = error "Prelude.Enum.().toEnum: bad argument"

    fromEnum () = 0
    enumFrom () 	= [()]
    enumFromThen () () 	= let many = ():many in many
    enumFromTo () () 	= [()]
    enumFromThenTo () () () = let many = ():many in many

-- Numeric functions


subtract         :: (Num a) => a -> a -> a
subtract         =  flip (-)


even, odd        :: (Integral a) => a -> Bool
even n           =  n `rem` 2 == 0
odd              =  not . even


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


fromIntegral     :: (Integral a, Num b) => a -> b
fromIntegral     =  fromInteger . toInteger

{-# RULES
  "fromIntegral/Int"          fromIntegral = (id :: Int -> Int)
  "fromIntegral/Integer"      fromIntegral = (id :: Integer -> Integer)
  "fromIntegral/toInt"        fromIntegral = toInt
  "fromIntegral/fromInt"      fromIntegral = fromInt
  "fromIntegral/toInteger"    fromIntegral = toInteger
  "fromIntegral/fromInteger"  fromIntegral = fromInteger
 #-}


realToFrac     :: (Real a, Fractional b) => a -> b
realToFrac      =  fromRational . toRational




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
last [x]         =  x
last (_:xs)      =  last xs
last []          =  error "Prelude.last: empty list"


init             :: [a] -> [a]
init [x]         =  []
init (x:xs)      =  x : init xs
init []          =  error "Prelude.init: empty list"


null             :: [a] -> Bool
null []          =  True
null (_:_)       =  False

-- length returns the length of a finite list as an Int.

length           :: [a] -> Int
length xs = f xs 0 where
    f [] n = n
    f (_:xs) n = f xs $! n + 1
--length []        =  0
--length (_:l)     =  1 + length l

-- List index (subscript) operator, 0-origin

--(!!)                :: [a] -> Int -> a
--xs     !! n | n < 0 =  error "Prelude.!!: negative index"
--[]     !! _         =  error "Prelude.!!: index too large"
--(x:_)  !! 0         =  x
--(_:xs) !! n         =  xs !! (n-1)

--xs !! n | n < 0   =  error "Prelude.(!!): negative index\n"
--	| otherwise =  sub xs n where
--			    sub :: [a] -> Int -> a
--                            sub []     _ = error "Prelude.(!!): index too large\n"
--                            sub (y:ys) n = if n == 0
--					   then y
--					   else sub ys $! (n - 1)

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
replicate n x    =  take n (repeat x)

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
splitAt n xs             =  (take n xs, drop n xs)

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

elem, notElem    :: (Eq a) => a -> [a] -> Bool
elem x           =  any (== x)
notElem x        =  all (/= x)

-- lookup key assocs looks up a key in an association list.

lookup           :: (Eq a) => a -> [(a,b)] -> Maybe b
lookup key []    =  Nothing
lookup key ((x,y):xys)
    | key == x   =  Just y
    | otherwise  =  lookup key xys

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



instance Enum Integer where
    toEnum = fromInt
    fromEnum = toInt
    succ = (+ 1)
    pred = (+ -1)
    enumFrom x  =  x:enumFrom (x + 1)
    enumFromTo x y = f x where
        f x | x > y = []
            | otherwise = x:f (x + 1)
    enumFromThen x y = f x where
        z = y - x
        f x = x:f (x + z)
    enumFromThenTo x y z | y >= x = f x where
        inc = y - x
        f x | x <= z = x:f (x + inc)
            | otherwise = []
    enumFromThenTo x y z  = f x where
        inc = y - x
        f x | x >= z = x:f (x + inc)
            | otherwise = []


{-
instance (Ord a, Ord b) => Ord (a,b) where
    compare (x,y) (a,b) = case compare x a of
        EQ -> compare y b
        z -> z
    -}




{-
instance (Eq a, Eq b) => Eq (a,b) where
    (x,y) == (a,b) = x == a && y == b
    -}

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
