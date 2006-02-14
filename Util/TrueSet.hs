module Util.TrueSet(
    TrueSet,
    fromList,
    member,
    empty,
    full,
    singleton,
    insert,
    delete,
    unions,
    union,
    intersect,
    intersects,
    difference,
    (\\)
    ) where

import qualified Data.Set as Set

infixl 9 \\


data TrueSet a = TrueSet (Set.Set a) Bool

False `xor` y = y
True `xor` y = not y

fromList xs = TrueSet (Set.fromList xs) False
member x (TrueSet s inv) = inv `xor` (Set.member x s)

invert (TrueSet x y) = TrueSet x (not y)
empty = TrueSet Set.empty False
full = TrueSet Set.empty False
singleton x = TrueSet (Set.singleton x) False
insert x (TrueSet s False) = TrueSet (Set.insert x s) False
insert x (TrueSet s True) = TrueSet (Set.delete x s) True
delete x (TrueSet s False) = TrueSet (Set.delete x s) False
delete x (TrueSet s True) = TrueSet (Set.insert x s) True

unions xs = foldlStrict union empty xs
intersects xs = foldlStrict intersect full xs

foldlStrict f z xs
  = case xs of
      []     -> z
      (x:xx) -> let z' = f z x in seq z' (foldlStrict f z' xx)

difference x y = x `intersect` invert y
m1 \\ m2 = difference m1 m2

(TrueSet x True)  `intersect` (TrueSet y True) = TrueSet (x `Set.union` y) True
(TrueSet x False) `intersect` (TrueSet y False) = TrueSet (x `Set.intersect` y) False
(TrueSet x True)  `intersect` (TrueSet y False) = TrueSet (y Set.\\ x) False
(TrueSet x False) `intersect` (TrueSet y True) = TrueSet (x Set.\\ y) False
(TrueSet x True)  `union` (TrueSet y True) = TrueSet (x `Set.intersect` y) True
(TrueSet x False) `union` (TrueSet y False) = TrueSet (x `Set.union` y) False
(TrueSet x True)  `union` (TrueSet y False) = TrueSet (x Set.\\ y) True
(TrueSet x False) `union` (TrueSet y True) = TrueSet (y Set.\\ x) True


