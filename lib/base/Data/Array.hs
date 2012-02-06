{-# OPTIONS_JHC -funboxed-tuples #-}
module Data.Array (
    module Data.Ix,  -- export all of Ix
    Array(),
    array,
    listArray,
    accumArray,
    (!),
    bounds,
    indices,
    elems,
    assocs,
    (//),
    accum,
    ixmap
    ) where

import Data.Ix
import Jhc.Int
import Jhc.Prim.Array
import Jhc.Prim.IO

infixl 9  !, //

data Array a b = MkArray !a !a (Array_ b)

array       :: (Ix a) => (a,a) -> [(a,b)] -> Array a b
array b@(s,e) ivs = case newArray (error "array: missing element") (rangeSize b) [(index b x,y) | (x,y) <- ivs] of
        arr -> MkArray s e arr

listArray             :: (Ix a) => (a,a) -> [b] -> Array a b
listArray b vs        =  array b (zipWith (\ a b -> (a,b)) (range b) vs)

(!)                   :: (Ix a) => Array a b -> a -> b
(!) (MkArray s e arr) i =  case unboxInt (index (s,e) i) of i' -> case indexArray__ arr i' of (# r #) -> r

bounds                :: (Ix a) => Array a b -> (a,a)
bounds (MkArray s e _)  =  (s,e)

indices               :: (Ix a) => Array a b -> [a]
indices               =  range . bounds

elems                 :: (Ix a) => Array a b -> [b]
elems a               =  [a!i | i <- indices a]

assocs                :: (Ix a) => Array a b -> [(a,b)]
assocs a              =  [(i, a!i) | i <- indices a]

(//)                  :: (Ix a) => Array a b -> [(a,b)] -> Array a b
a // []               = a
a // new_ivs          = array (bounds a) (old_ivs ++ new_ivs)
                      where
                  	old_ivs = [(i,a!i) | i <- indices a,
                                             i `notElem` new_is]
                  	new_is  = [i | (i,_) <- new_ivs]

accum                 :: (Ix a) => (b -> c -> b) -> Array a b -> [(a,c)] -> Array a b
accum f               =  foldl (\a (i,v) -> a // [(i,f (a!i) v)])

accumArray            :: (Ix a) => (b -> c -> b) -> b -> (a,a) -> [(a,c)] -> Array a b
accumArray f z b      =  accum f (array b [(i,z) | i <- range b])

ixmap                 :: (Ix a, Ix b) => (a,a) -> (a -> b) -> Array b c -> Array a c
ixmap b f a           = array b [(i, a ! f i) | i <- range b]

instance  (Ix a)          => Functor (Array a) where
    fmap fn a = array (bounds a) [ (a,fn b) | (a,b) <- assocs a ]

instance  (Ix a, Eq b)  => Eq (Array a b)  where
    a == a' =  assocs a == assocs a'

instance  (Ix a, Ord b) => Ord (Array a b)  where
    a <= a' =  assocs a <= assocs a'

instance  (Ix a, Show a, Show b) => Show (Array a b)  where
    showsPrec p a = showParen (p > arrPrec) (
                    showString "array " .
                    showsPrec (arrPrec+1) (bounds a) . showChar ' ' .
                    showsPrec (arrPrec+1) (assocs a)                  )
instance  (Ix a, Read a, Read b) => Read (Array a b)  where
    readsPrec p = readParen (p > arrPrec)
           (\r -> [ (array b as, u)
                  | ("array",s) <- lex r,
                    (b,t)       <- readsPrec (arrPrec+1) s,
                    (as,u)      <- readsPrec (arrPrec+1) t ])

-- Precedence of the 'array' function is that of application itself
arrPrec :: Int
arrPrec = 10

foreign import primitive newWorld__ :: a -> World__

newArray :: a -> Int -> [(Int,a)] -> Array_ a
newArray init n xs = case unboxInt n of
    n' -> case newWorld__ (init,n,xs) of
     w -> case newArray__ n' init w of
      (# w, arr #) -> let
        f :: MutArray_ a -> World__ -> [(Int,a)] -> World__
        f arr w [] = w
        f arr w ((i,v):xs) = case unboxInt i of i' -> case writeArray__ arr i' v w of w -> f arr w xs
            in case f arr w xs of w -> Array_ arr `worldDep_` w

foreign import primitive "dependingOn" worldDep_ :: Array_ b -> World__ -> Array_ b
