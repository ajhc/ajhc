module Data.Array.Unboxed where

import Data.Ix
import System.IO.Unsafe
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.ForeignPtr

infixl 9  !, //

data UArray i e = MkArray !i !i (ForeignPtr e)

array       :: (Ix a,Storable b) => (a,a) -> [(a,b)] -> UArray a b
array b@(s,e) ivs = MkArray s e (unsafePerformIO arr) where
    arr = do
        let f :: [(a,b)] -> b; f _ = undefined
        fp <- mallocForeignPtrBytes (sizeOf (f ivs) *  rangeSize b)
        withForeignPtr fp $ \ptr ->
            mapM_ (\ (i,v) -> pokeElemOff ptr (index b i) v) ivs
        return fp


listArray             :: (Ix a,Storable b) => (a,a) -> [b] -> UArray a b
listArray b vs        =  array b (zipWith (\ a b -> (a,b)) (range b) vs)

(!)                   :: (Ix a,Storable b) => UArray a b -> a -> b
(!) (MkArray s e arr) i = case (index (s,e) i) of i' -> unsafePerformIO (withForeignPtr arr (\ptr -> peekElemOff ptr i'))

bounds                :: (Ix a) => UArray a b -> (a,a)
bounds (MkArray s e _)  =  (s,e)

indices               :: (Ix a) => UArray a b -> [a]
indices               =  range . bounds

elems                 :: (Ix a,Storable b) => UArray a b -> [b]
elems a               =  [a!i | i <- indices a]

assocs                :: (Ix a,Storable b) => UArray a b -> [(a,b)]
assocs a              =  [(i, a!i) | i <- indices a]

(//)                  :: (Ix a,Storable b) => UArray a b -> [(a,b)] -> UArray a b
a // []               = a
a // new_ivs          = array (bounds a) (old_ivs ++ new_ivs)
                      where
                  	old_ivs = [(i,a!i) | i <- indices a,
                                             i `notElem` new_is]
                  	new_is  = [i | (i,_) <- new_ivs]

accum                 :: (Ix a,Storable b ) => (b -> c -> b) -> UArray a b -> [(a,c)] -> UArray a b
accum f               =  foldl (\a (i,v) -> a // [(i,f (a!i) v)])

accumArray            :: (Ix a,Storable b ) => (b -> c -> b) -> b -> (a,a) -> [(a,c)] -> UArray a b
accumArray f z b      =  accum f (array b [(i,z) | i <- range b])

ixmap                 :: (Ix a, Ix b,Storable c) => (a,a) -> (a -> b) -> UArray b c -> UArray a c
ixmap b f a           = array b [(i, a ! f i) | i <- range b]

--instance  (Ix a)          => Functor (UArray a) where
--    fmap fn a = array (bounds a) [ (a,fn b) | (a,b) <- assocs a ]

instance  (Ix a, Eq b, Storable b)  => Eq (UArray a b)  where
    a == a' =  assocs a == assocs a'

instance  (Ix a, Ord b, Storable b) => Ord (UArray a b)  where
    a <= a' =  assocs a <= assocs a'

instance  (Ix a, Show a, Show b, Storable b) => Show (UArray a b)  where
    showsPrec p a = showParen (p > arrPrec) (
                    showString "array " .
                    showsPrec (arrPrec+1) (bounds a) . showChar ' ' .
                    showsPrec (arrPrec+1) (assocs a)                  )
instance  (Ix a, Read a, Read b, Storable b) => Read (UArray a b)  where
    readsPrec p = readParen (p > arrPrec)
           (\r -> [ (array b as, u)
                  | ("array",s) <- lex r,
                    (b,t)       <- readsPrec (arrPrec+1) s,
                    (as,u)      <- readsPrec (arrPrec+1) t ])

-- Precedence of the 'array' function is that of application itself
arrPrec :: Int
arrPrec = 10

