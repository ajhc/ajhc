{-# OPTIONS_JHC -fno-prelude #-}
module Jhc.List where

import Jhc.Basics
import Jhc.IO(error)
import Jhc.Int
import Jhc.Order

import Jhc.String


-- | our fusion routines

build :: (forall b . (a -> b -> b) -> b -> b) -> [a]
build g = g (:) []


augment :: forall a. (forall b. (a->b->b) -> b -> b) -> [a] -> [a]
augment g xs = g (:) xs

{-# RULES "foldr/nil" forall k z.   foldr k z []  = z  #-}
{-# RULES "foldr/single"  forall k z x . foldr k z [x] = k x z #-}
{-# RULES "foldr/double"  forall k z x y . foldr k z [x,y] = k x (k y z) #-}
{-# RULES "foldr/triple"  forall k z a b c . foldr k z [a,b,c] = k a (k b (k c z)) #-}
{-# RULES "foldr/id"      foldr (:) [] = \x -> x  #-}
{- "foldr/app"    	[1] forall ys. foldr (:) ys = \xs -> xs ++ ys -}

{-# RULES "foldr/build" forall k z (g :: forall b . (a -> b -> b) -> b -> b) . foldr k z (build g) = g k z #-}
{-# RULES "foldr/augment" forall k z xs (g::forall b. (a->b->b) -> b -> b) .  foldr k z (augment g xs) = g k (foldr k z xs) #-}
{-# RULES "foldr/single" forall k z x. foldr k z [x] = k x z #-}
{-# RULES "augment/build" forall (g::forall b. (a->b->b) -> b -> b)
		       (h::forall b. (a->b->b) -> b -> b) .
		       augment g (build h) = build (\c n -> g c (h c n)) #-}
{-# RULES "augment/nil"   forall (g::forall b. (a->b->b) -> b -> b) .  augment g [] = build g #-}

{-# RULES "foldr/unpackString"  forall k z (addr::Addr__) . foldr k z (unpackString addr) = unpackStringFoldr addr k z  #-}

-- a few pre-fusioned routines

filterIterate :: (a -> Bool) -> (a -> a) -> a -> [a]
filterIterate p f x = fi x where
    fi x | p x = x : fi (f x)
    fi x = fi (f x)

mapIterate :: (a -> b) -> (a -> a) -> a -> [b]
mapIterate f g x = fi x where
    fi x = f x : fi (g x)

filterMap :: (b -> Bool) -> (a -> b) -> [a] -> [b]
filterMap p f xs = fm xs where
    fm (x:xs) = let nx = f x in if p nx then nx:fm xs else fm xs
    fm [] = []

mapFilter :: (a -> b) -> (a -> Bool) -> [a] -> [b]
mapFilter f p xs = fm xs where
    fm (x:xs) = if p x then f x:fm xs else fm xs
    fm [] = []

{-# RULES "tail/map"      forall f xs . tail (map f xs) = map f (tail xs) #-}
{-# RULES "head/map"      forall f xs . head (map f xs) = f (head xs) #-}
{-# RULES "head/:"        forall x xs . head (x:xs) = x #-}
{-# RULES "tail/:"        forall x xs . tail (x:xs) = xs #-}

{-# RULES "filter/iterate" forall p f x . filter p (iterate f x) = filterIterate p f x  #-}
{-# RULES "map/iterate" forall f g x . map f (iterate g x) = mapIterate f g x  #-}
{-# RULES "map/filter" forall f p xs . map f (filter p xs) = mapFilter f p xs  #-}
{-# RULES "filter/map" forall f p xs . filter p (map f xs) = filterMap p f xs  #-}

-- efficient implementations of prelude routines

{-# CATALYST "and/foldr" forall . and = foldr (&&) True #-}
{-# CATALYST "or/foldr"  forall . or = foldr (||) False #-}

and, or          :: [Bool] -> Bool
and [] = True
and (False:_) = False
and (True:xs) = and xs

or [] = False
or (True:_) = True
or (False:xs) = or xs

{-# RULES "any/build"     forall p (g::forall b.(a->b->b)->b->b) .  any p (build g) = g ((||) . p) False #-}


{-# RULES "all/build"     forall p (g::forall b.(a->b->b)->b->b) .  all p (build g) = g ((&&) . p) True #-}


any, all         :: (a -> Bool) -> [a] -> Bool
any p xs = f xs where
    f [] = False
    f (x:xs) | p x = True
             | otherwise = f xs

all p xs = f xs where
    f [] = True
    f (x:xs) | not (p x) = False
             | otherwise = f xs

filter :: (a -> Bool) -> [a] -> [a]
filter p []                 = []
filter p (x:xs) | p x       = x : filter p xs
                | otherwise = filter p xs

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

infixl 9  !!

(!!)                :: [a] -> Int -> a
xs !! n | n < zero  =  error "Prelude.(!!): negative index"
	| otherwise =  sub xs n where
                sub :: [a] -> Int -> a
                sub _ n | n `seq` False = undefined
                sub []     _ = error "Prelude.(!!): index too large"
                sub (y:ys) n = if n == zero
                               then y
                               else sub ys $! (n `minus` one)

null             :: [a] -> Bool
null []          =  True
null (_:_)       =  False

-- length returns the length of a finite list as an Int.

length           :: [a] -> Int
length xs = f xs zero where
    f [] n = n
    f (_:xs) n = f xs $! n `plus` one

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


{-# RULES "head/iterate"  forall f x . head (iterate f x) = x #-}
{-# RULES "head/repeat"   forall x . head (repeat x) = x #-}
{-# RULES "tail/repeat"   forall x . tail (repeat x) = repeat x #-}
{-# RULES "tail/iterate"  forall f x . tail (iterate f x) = iterate f (f x) #-}
{-# RULES "iterate/id" forall . iterate id = repeat #-}



foldl1           :: (a -> a -> a) -> [a] -> a
foldl1 f (x:xs)  =  foldl f x xs
foldl1 _ []      =  error "Prelude.foldl1: empty list"



scanl1           :: (a -> a -> a) -> [a] -> [a]
scanl1 f (x:xs)  =  scanl f x xs
scanl1 _ []      =  []

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

{-
concatMap f = foldr ((++) . f) []
--concat xss = foldr (++) [] xss
concat xss = foldr (++) [] xss
concatMap f = foldr ((++) . f) []

and xs  = foldr (&&) True xs
sum xs = foldr (+) (0::Int) xs
(++) xs ys = augment (\c n -> foldr c n xs) ys
concat xs = foldr (++) [] xs
foldl f z xs = foldr (\b g a -> g (f a b)) id xs z

filter p xs = build (\c n -> foldr (filterFB c p) n xs)
{- RULES "filterFB" forall c p q. filterFB (filterFB c p) q = filterFB c (\x -> q x && p x) #-}

{- NOINLINE filterFB #-}
filterFB c p x r | p x       = x `c` r
		 | otherwise = r


{- NOINLINE iterateFB #-}
iterate f x = build (\c _n -> iterateFB c f x)
iterateFB c f x = x `c` iterateFB c f (f x)

head (x:xs) = x
head [] = badHead


map f xs =  build (\c n -> foldr (mapFB c f) n xs)
{- NOINLINE mapFB #-}
mapFB ::  (elt -> lst -> lst) -> (a -> elt) -> a -> lst -> lst
mapFB c f x ys = c (f x) ys


badHead = error "Prelude.head: empty list"

{-# RULES "head/build"   forall (g::forall b.(a->b->b)->b->b) . head (build g) = g (\x _ -> x) badHead #-}

{-# RULES "head/augment"   forall xs (g::forall b. (a->b->b) -> b -> b) .  head (augment g xs) = g (\x _ -> x) (head xs) #-}

--repeat x = build (\c _n -> repeatFB c x)
--repeatFB c x = xs where xs = x `c` xs


{-



{-# RULES forall xs n (g :: forall b . (a -> b -> b) -> b -> b) . build g !! n  = bangBang g n  #-}

bangBang :: (forall b . (a -> b -> b) -> b -> b) -> Int -> a
g `bangBang` n
    | n < 0 = error "Prelude.(!!): negative index\n"
    | otherwise = g c k  where
            sub _ n | n `seq` False = undefined
            sub []     _ = error "Prelude.(!!): index too large\n"
            sub (y:ys) n = if n == 0
                           then y
                           else sub ys $! (n - 1)

-}

(!!) :: [a] -> Int -> a
xs !! n = foldr bangFB bangCon xs n

bangCon _ = error "!! out of range"

bangFB :: a -> (Int -> a) -> Int -> a
bangFB x _xs m | m == 0 = x
bangFB _x xs m = xs $! (m - 1)

{-# INLINE bangFB #-}
{-# INLINE iterateFB #-}
{-# INLINE (!!) #-}



{-# RULES
"take"	   [~1] forall n xs . take n xs = case n of I# n# -> build (\c nil -> foldr (takeFB c nil) (takeConst nil) xs n#)
"takeList"  [1] forall n xs . foldr (takeFB (:) []) (takeConst []) xs n = takeUInt n xs
 #-}

{-# NOINLINE [0] takeConst #-}
-- just a version of const that doesn't get inlined too early, so we
-- can spot it in rules.  Also we need a type sig due to the unboxed Int#.
takeConst :: a -> Int# -> a
takeConst x _ = x

{-# NOINLINE [0] takeFB #-}
takeFB :: (a -> b -> c) -> c -> a -> (Int# -> b) -> Int# -> c
takeFB c n x xs m | m <=# 0#  = n
		  | otherwise = x `c` xs (m -# 1#)
  -}

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
