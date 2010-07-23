{-# OPTIONS_JHC -N -funboxed-tuples #-}

data MyWorld__ :: #


newtype State s a = State (s -> (# s, a #))

data Char

data Aiether x y = Aeft x | Aight y

data Bob  = Bob (forall a . a -> a)

newtype Foo f = Foo (f Char)

main :: Char
main = main
