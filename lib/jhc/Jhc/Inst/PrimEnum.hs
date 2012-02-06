{-# OPTIONS_JHC -fno-prelude -fffi -funboxed-values #-}

-- | helper routines for deriving(Enum) instances
-- these routines help out the compiler when
-- deriving enums.
module Jhc.Inst.PrimEnum(enum_succ,enum_pred,enum_fromTo,enum_fromThen,enum_fromThenTo,enum_toEnum,enum_from) where

import Jhc.Basics
import Jhc.Int
import Jhc.Prim.Prim
import Jhc.Prim.Bits

{-# INLINE enum_toEnum, enum_succ, enum_pred, enum_fromTo, enum_fromThen, enum_fromThenTo, enum_from #-}

enum_toEnum :: (Enum__ -> a) -> Int__ -> Int -> a
enum_toEnum box max int = case unboxInt int of
    int_ -> case int_ `bits32UGt` max of
        1# -> toEnumError
        0# -> box (intToEnum int_)

enum_succ :: (Enum__ -> a) -> (a -> Enum__) -> Enum__ -> a -> a
enum_succ box debox max e = case debox e of
    e_ -> case e_ `enumEq` max of
        0# -> box (enumInc e_)
        1# -> succError

enum_pred :: (Enum__ -> a) -> (a -> Enum__) -> a -> a
enum_pred box debox e = case debox e of
    e_ -> case e_ `enumEq` 0# of
        0# -> box (enumDec e_)
        1# -> predError

enum_from :: (Enum__ -> a) -> (a -> Enum__) -> Enum__ -> a -> [a]
enum_from box debox max x = case debox x of
    x_ -> f x_ where
        f x = case x `enumGt` max of
            0# -> box x:f (enumInc x)
            1# -> []

enum_fromTo :: (Enum__ -> a) -> (a -> Enum__) -> a -> a -> [a]
enum_fromTo box debox x y = case debox y of
    y_ -> enum_from box debox y_ x

enum_fromThen :: (Enum__ -> a) -> (a -> Enum__) -> Enum__ -> a -> a -> [a]
enum_fromThen box debox max x y = case debox x of
    x_ -> case debox y of
        y_ -> case x_ `enumGt` y_ of
            0# -> enum_fromThenToUp' box x_ y_ max
            1# -> enum_fromThenToDown' box x_ y_ 0#

enum_fromThenTo :: (Enum__ -> a) -> (a -> Enum__) -> a -> a -> a -> [a]
enum_fromThenTo box debox x y z = case debox x of
    x_ -> case debox y of
        y_ -> case debox z of
            z_ -> case x_ `enumGt` y_ of
                0# -> enum_fromThenToUp' box x_ y_ z_
                1# -> enum_fromThenToDown' box x_ y_ z_

enum_fromThenToUp' :: (Enum__ -> a) -> Enum__ -> Enum__ -> Enum__ -> [a]
enum_fromThenToUp' box x y z = case y `enumSub` x of
            inc -> let f x = case x `enumGt` z of
                            0# -> box x:f (x `enumAdd` inc)
                            1# -> []
             in f x

enum_fromThenToDown' :: (Enum__ -> a) -> Enum__ -> Enum__ -> Enum__ -> [a]
enum_fromThenToDown' box x y z = case y `enumSub` x of
            inc -> let f x = case x `enumLt` z of
                            0# -> box x:f (x `enumAdd` inc)
                            1# -> []
             in f x

ix_range :: (Enum__ -> a) -> (a -> Enum__) -> (a,a) -> [a]
ix_range box debox (x,y) = enum_fromTo box debox x y

ix_index :: (Enum__ -> a) -> (a -> Enum__) -> (a,a) -> a -> Int
ix_index box debox (x,y) v = case debox v of
    v_ -> case debox x of
        x_ -> case debox y of
            y_ -> case v_ `enumLte` x_ of
                1# -> case v_ `enumLte` y_ of
                    1# -> boxInt (enumToInt (v_ `enumSub` x_))
                    0# -> ixIndexError
                0# -> ixIndexError

ix_inRange :: (Enum__ -> a) -> (a -> Enum__) -> (a,a) -> a -> Bool
ix_inRange box debox (x,y) v = case debox v of
    v_ -> case debox x of
        x_ -> case debox y of
            y_ -> case v_ `enumGte` x_ of
                1# -> boxBool (v_ `enumLte` y_)
                0# -> boxBool 0#

foreign import primitive "box"        boxBool :: Bool__ -> Bool
foreign import primitive "Add"        enumAdd :: Enum__ -> Enum__ -> Enum__
foreign import primitive "Eq"         enumEq  :: Enum__ -> Enum__ -> Bool__
foreign import primitive "Gt"         enumGt  :: Enum__ -> Enum__ -> Bool__
foreign import primitive "Gte"        enumGte :: Enum__ -> Enum__ -> Bool__
foreign import primitive "Lt"         enumLt  :: Enum__ -> Enum__ -> Bool__
foreign import primitive "Lte"        enumLte :: Enum__ -> Enum__ -> Bool__
foreign import primitive "Sub"        enumSub :: Enum__ -> Enum__ -> Enum__
foreign import primitive "U2U"        enumToInt :: Enum__ -> Int__
foreign import primitive "U2U"        intToEnum :: Int__ -> Enum__
foreign import primitive "UGt"       bits32UGt       :: Bits32_ -> Bits32_ -> Bool__
foreign import primitive "decrement"  enumDec :: Enum__ -> Enum__
foreign import primitive "error.Ix.Index: out of range" ixIndexError :: a
foreign import primitive "error.pred: out of range" predError :: a
foreign import primitive "error.succ: out of range" succError :: a
foreign import primitive "error.toEnum: out of range" toEnumError :: a
foreign import primitive "increment"  enumInc :: Enum__ -> Enum__
