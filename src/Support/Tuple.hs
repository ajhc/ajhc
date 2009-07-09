module Support.Tuple where

import List(intersperse)

class Tuple a where
    tupleNil :: a
    tupleOne :: a -> a
    tupleMany :: [a] -> a

    tupleNil = tupleMany []
    tupleOne x = x

class FromTuple a where
    fromTuple :: a -> [a]


tuple :: Tuple a => [a] -> a
tuple [] = tupleNil
tuple [x] = tupleOne x
tuple xs = tupleMany xs


instance Tuple String where
    tupleMany xs = "(" ++ concat (intersperse "," xs) ++ ")"

