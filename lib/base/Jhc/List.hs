{-# OPTIONS_JHC -N #-}
module Jhc.List where

import Jhc.Basics
import Jhc.IO(error)
import Jhc.Int
import Jhc.Order
import Jhc.Monad


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

{-# RULES "filter/iterate" forall p f x . filter p (iterate f x) = filterIterate p f x  #-}
{-# RULES "map/iterate" forall f g x . map f (iterate g x) = mapIterate f g x  #-}
{-# RULES "map/filter" forall f p xs . map f (filter p xs) = mapFilter f p xs  #-}
{-# RULES "filter/map" forall f p xs . filter p (map f xs) = filterMap p f xs  #-}

-- efficient implementations of prelude routines

and, or          :: [Bool] -> Bool
and [] = True
and (False:_) = False
and (True:xs) = and xs

or [] = False
or (True:_) = True
or (False:xs) = or xs


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

infixl 9  !!

(!!)                :: [a] -> Int -> a
xs !! n | n < zero  =  error "Prelude.(!!): negative index\n"
	| otherwise =  sub xs n where
                sub :: [a] -> Int -> a
                sub _ n | n `seq` False = undefined
                sub []     _ = error "Prelude.(!!): index too large\n"
                sub (y:ys) n = if n == zero
                               then y
                               else sub ys $! (n `minus` one)



{- SPECIALIZE sequence :: forall a . [IO a] -> IO [a] #-}
{- SPECIALIZE sequence_ :: forall a . [IO a] -> IO () #-}
{- SPECIALIZE mapM :: forall a b . (a -> IO b) -> [a]-> IO [b] #-}
{- SPECIALIZE mapM_ :: forall a b . (a -> IO b) -> [a]-> IO () #-}

-- | use local routine so monad type is shared.
sequence       :: Monad m => [m a] -> m [a]
sequence xs = f xs where
    f [] = return []
    f (x:xs) = x >>= \r -> f xs >>= \rs -> return (r:rs)

sequence_      :: Monad m => [m a] -> m ()
sequence_ xs  =  f xs where
    f [] = return ()
    f (x:xs) = x >> f xs

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

