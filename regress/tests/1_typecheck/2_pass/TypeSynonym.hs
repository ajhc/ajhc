module Foo() where

-- check various type synonym expansions.

type Foo = Int
type Bar = Foo
type Bug x = Char
type App f x = f (Bug x)
type Fun = App Bug (Bug Char)
f :: Foo -> Int
f = id
(x :: Foo) = 4
y = (1 + 4 :: Bar) `div` 3

z = 'z' :: Fun

class Baz a where
    g :: a -> Bar

instance Baz (Bug (Bug String)) where
    g = fromEnum

w = let f = 'z' :: App Bug Foo in 4
