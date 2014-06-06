# How typeclasses are implemented internally in jhc

Rather than the traditional dictionary passing implementation of type classes,
jhc performs explicit case matching on types. This is because optimizing out
higher order functions is a key to jhc's speed.

The reason this helps is that via a single scrutinization of the type you can
now inline all class operations directly from that type, This is because the
dictionary has a degree of freedom where different entries can point to
incoherent entries. like the (+) from Int and the (-) from Float. This is ruled
out by the front end type system, but the dictionary transformation loses this
information so optimizations based on it cannot be performed.

Internally jhc has always had a 'typecase' as part of its intermediate language
which was used to implement this. The reason we need a special form of case is
that inside the branch where we scrutinized the type and found it to be an Int,
we need to refine the previously unknown type to Int.

This type refinement is quite useful, and in fact, ghc exposes the same
mechanism via GADTs. allowing me to demonstrate an exact translation of jhc's
class mechanism as an executable ghc program.

## The code

In jhc, a shadow GADT is declared for all types that exactly mirrors the type
hierarchy. Resulting in:

> data Int
> data Bool = False | True
> data Maybe a = Nothing | Just a
>
> data Ty a where
>       TyInt :: Ty Int
>       TyBool :: Ty Bool
>       TyMaybe :: Ty a -> Ty (Maybe a)

At the end of compilation we collect all class instances together. jhc works in
a closed world so this

> class Equal a where
>       equal :: a -> a -> Bool
>
> class Number a where
>       plus :: a -> a -> a
>       minus :: a -> a -> a
>
> instance Equal Bool where
>       equal = equalBool
> instance Equal Int where
>       equal = equalInt
> instance Number Int where
>       plus = plusInt
>       minus = minusInt
> instance Equal a => Equal (Maybe a) where
>       equal Nothing Nothing = True
>       equal (Just x) (Just y) = equal x y

where we can assume fast unboxed primitives typed as follows exist

> plusInt,minusInt :: Int -> Int -> Int
> equalInt :: Int -> Int -> Bool

data Ty a where
    TyFun :: Ty a -> Ty b -> Ty (a -> b)
    TyInt :: Ty Int
    TyBool :: Ty Bool
    TyList :: Ty a -> Ty [a]
