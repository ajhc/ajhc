{-# OPTIONS_JHC -fm4 -fno-prelude -fffi -funboxed-values #-}
module Jhc.Enum(Enum(..),Bounded(..)) where
-- Enumeration and Bounded classes

import Jhc.Basics
import Jhc.Inst.PrimEnum()
import Jhc.Int
import Jhc.Prim.Bits
import Jhc.Prim.Prim

m4_include(Jhc/Enum.m4)

otherwise = True

class  Enum a  where
    succ, pred       :: a -> a
    toEnum           :: Int -> a
    fromEnum         :: a -> Int
    enumFrom         :: a -> [a]             -- [n..]
    enumFromThen     :: a -> a -> [a]        -- [n,n'..]
    enumFromTo       :: a -> a -> [a]        -- [n..m]
    enumFromThenTo   :: a -> a -> a -> [a]   -- [n,n'..m]

        -- Minimal complete definition:
        --      toEnum, fromEnum
--
-- NOTE: these default methods only make sense for types
--   that map injectively into Int using fromEnum
--  and toEnum.
    succ             =  toEnum . increment . fromEnum
    pred             =  toEnum . decrement . fromEnum
    enumFrom x       =  map toEnum [fromEnum x ..]
    enumFromTo x y   =  map toEnum [fromEnum x .. fromEnum y]
    enumFromThen x y =  map toEnum [fromEnum x, fromEnum y ..]
    enumFromThenTo x y z =
                        map toEnum [fromEnum x, fromEnum y .. fromEnum z]

class Bounded a  where
    minBound         :: a
    maxBound         :: a

instance Enum Int where
    succ = increment
    pred = decrement
    toEnum x = x
    fromEnum x = x

    enumFrom x  | x `seq` True  =  enumFromTo x maxBound
    enumFromThen c c' = [c, c' .. lastInt]
                      where lastInt | c' `intLt` c    = minBound
                                    | otherwise = maxBound
    enumFromTo x y = f x where
        f x | x `intGt` y = []
            | otherwise = x:f (increment x)
    enumFromThenTo x y z | y `intGte` x = f x where
        inc = y `minus` x
        f x | x `intLte` z = x:f (x `plus` inc)
            | otherwise = []
    enumFromThenTo x y z  = f x where
        inc = y `minus` x
        f x | x `intGte` z = x:f (x `plus` inc)
            | otherwise = []

foreign import primitive "box" boxBool :: Bool_ -> Bool
foreign import primitive "Gte" intGte' :: Int -> Int -> Bool_
foreign import primitive "Gt" intGt' :: Int -> Int -> Bool_
foreign import primitive "Lte" intLte' :: Int -> Int -> Bool_
foreign import primitive "Lt" intLt' :: Int -> Int -> Bool_
foreign import primitive "Lt" charLt' :: Char -> Char -> Bool_

intGte x y = boxBool (intGte' x y)
intGt x y = boxBool (intGt' x y)
intLte x y = boxBool (intLte' x y)
intLt x y = boxBool (intLt' x y)
charLt x y = boxBool (charLt' x y)

instance Enum Char where
    toEnum = chr
    fromEnum = ord
    enumFrom c        = [c .. maxBound::Char]
    enumFromThen c c' = [c, c' .. lastChar]
                      where lastChar :: Char
                            lastChar | c' `charLt` c    = minBound
                                     | otherwise = maxBound
--    enumFromTo (Char x) (Char y) = f x where
--        f x = case x `bits32UGt` y of
--            0# -> []
--            1# -> Char x:f (bits32Increment x)
--    enumFromThenTo (Char x) (Char y) (Char z) =
--        case y `bits32Sub` x of
--            inc -> let f x = case x `bits32UGte` z of
--                            1# -> Char x:f (x `bits32Add` inc)
--                            0# -> []
--             in f x

deriving instance Enum Bool
deriving instance Enum Ordering

instance Bounded Bool where
    minBound = False
    maxBound = True
instance Bounded Ordering where
    minBound = LT
    maxBound = GT
instance Bounded () where
    minBound = ()
    maxBound = ()
instance Bounded Char where
    minBound = Char 0#
    maxBound = Char 0x10ffff#

BOUNDED(Int)
BOUNDED(Integer)

--foreign import primitive "UGt"       bits32UGt       :: Bits32_ -> Bits32_ -> Bool__
--foreign import primitive "UGte"      bits32UGte      :: Bits32_ -> Bits32_ -> Bool__
--foreign import primitive "increment" bits32Increment :: Bits32_ -> Bits32_

--foreign import primitive "Add"       bits32Add       :: Bits32_ -> Bits32_ -> Bits32_
--foreign import primitive "Sub"       bits32Sub       :: Bits32_ -> Bits32_ -> Bits32_
