module Unparse(Unparse(), Unparsable(..), unparse, unparse', Side(..), atom, atomize, bop, pop, fixitize) where

import Doc.DocLike

data Unparse a = Atom a | Pre a (Unparse a) | Fix (Unparse a) a (Unparse a) !Side !Int | Atomized (Unparse a) | Fixitized  !Side !Int a

data Side = R | L | N
    deriving(Eq)

atom :: a -> Unparse a
atom s = Atom s

atomize :: Unparse a -> Unparse a
atomize (Atomized x) = Atomized x
atomize (Atom a) = Atom a
atomize x = Atomized x

fixitize :: (Side,Int) -> a -> Unparse a
fixitize (s,i) a = Fixitized s i a

pop :: a -> Unparse a -> Unparse a
pop = Pre

bop :: (Side,Int) -> a -> Unparse a -> Unparse a -> Unparse a
bop (s,i) op a b = Fix a op b s i


data Unparsable a = Unparsable {
    unparseGroup :: a -> a,
    unparseCat :: a -> a -> a
    }

data Fix = FAtom | FPre | FFix !Side !Int

unparse :: DocLike a => Unparse a -> a
unparse up = unparse' Unparsable { unparseGroup = parens, unparseCat = (<>) } up

unparse' :: Unparsable a -> Unparse a -> a
unparse' Unparsable { unparseGroup = upg, unparseCat = (<>) } up = fst $ f up where
    f (Atom a) = atom a
    f (Atomized a) = (fst $ f a, FAtom)
    f (Fixitized s i a) = (a, FFix s i)
    f (Pre a up) = pop a (f up)
    f (Fix a op b s i) = bop (s,i) op (f a) (f b)

    bop (f1,f2) s (a,FAtom) (b,FAtom)  = (sop s a b, FFix f1 f2)
    bop f@(f1,f2) s (a,af) (b,bf) | lts L f af  && lts R f bf  = (sop s a b, FFix f1 f2)
    bop f s (a,af) b | not (lts L f af) = bop f s (mkatom (a,af)) b
    bop f s a (b,bf) | not (lts R f bf)  = bop f s a (mkatom (b,bf))
    bop _ _ _ _ = error "bop"

    pop s (x, FAtom) = ( s <> x, FPre)
    pop s x = pop s $ mkatom x

    atom a = (a,FAtom)
    mkatom (a,FAtom) = (a,FAtom)
    mkatom (a,_) = ( upg a , FAtom)

    sop op a b = a <> (op <> b)

    lts :: Side -> (Side,Int) -> Fix -> Bool
    lts _ _ FAtom = True
    lts _ _ FPre = True
    lts _ (_,n') (FFix  _ n ) | n' /= n = n' < n
    lts R (R,_) (FFix  R _ ) = True
    lts L (L,_) (FFix  L _ ) = True
    lts _ _ _ = False






--lts _ (N,_) (Fix (N,_)) = False



--type Unparse a = (a, Fix)

{-

bop :: Unparsable a => (Side,Int) -> a -> Unparse a -> Unparse a -> Unparse a
--bop f "" a b@(_,Pre) = bop f "" a (mkatom b)
bop (f1,f2) s (a,Atom) (b,Atom)  = (sopns s a b, Fix f1 f2)
bop f@(f1,f2) s (a,af) (b,bf) | lts L f af  && lts R f bf  = (sop s a b, Fix f1 f2)
bop f s (a,af) b | not (lts L f af) = bop f s (mkatom (a,af)) b
bop f s a (b,bf) | not (lts R f bf)  = bop f s a (mkatom (b,bf))

pop :: Unparsable a => a -> Unparse a -> Unparse a
pop s (x, Atom) = (unparseCat s  x, Pre)
pop s x = pop s $ mkatom x



--sop "" a b = a ++ " " ++ b
sop op a b = unparseSpace a $ unparseSpace op b
--sopns "" a b = a ++ " " ++ b
sopns op a b = unparseCat a $ unparseCat op b

mkatom (a,Atom) = (a,Atom)
mkatom (a,_) = ( unparseGroup a , Atom)
--sop "" a b = a ++ " " ++ b
sop op a b = unparseSpace a $ unparseSpace op b
--sopns "" a b = a ++ " " ++ b
sopns op a b = unparseCat a $ unparseCat op b

mkatom (a,Atom) = (a,Atom)
mkatom (a,_) = ( unparseGroup a , Atom)

instance Unparsable Doc where
    unparseCat  =  (<>)
    unparseSpace  =  (<>)
    unparseGroup  = parens
class Unparsable a where
    unparseGroup :: a -> a
    unparseCat :: a -> a -> a
    unparseSpace :: a -> a -> a
    unparseConcat :: [a] -> a
    unparseConcat = foldl1 unparseCat

instance Unparsable String where
    unparseGroup x = "(" ++ x ++ ")"
    unparseCat x y =  x ++ y
    unparseSpace x y = x ++ " " ++ y
    unparseConcat xs = concat xs


instance Unparsable () where
    unparseGroup _ = ()
    unparseCat _ _ = ()
    unparseSpace _ _ = ()
infixr 9  .
infixr 8  ^, ^^, **
infixl 7  *, /, `quot`, `rem`, `div`, `mod`
infixl 6  +, -

-- The (:) operator is built-in syntax, and cannot legally be given
-- a fixity declaration; but its fixity is given by:
--   infixr 5  :

infix  4  ==, /=, <, <=, >=, >
infixr 3  &&
infixr 2  ||
infixl 1  >>, >>=
infixr 1  =<<
infixr 0  $, $!, `seq`

a + b * c
a + (b * c)

d + a * b + c * d

plus = bop ((L,6)) "+"
minus = bop ((L,6)) "-"
times = bop ((L,7)) "*"
pow = bop ((L,8)) "^"
eq = bop ((N,4)) "=="


a,b,c,d,x,y, abcdr, abcdl, eql :: (String, Fix)

a = text "a"
b = text "b"
c = text "c"
d = text "d"
x = text "x"
y = text "y"

abcdr = foldl1 plus [a,b,c,d]
abcdl = foldr1 plus [a,b,c,d]
eql = foldl1 eq [a,b,c]

z = eq (plus a b) (pow (times b c) abcdl) `eq` eql


g = minus (plus (times (plus a b) (plus b c)) abcdr) abcdl


main = putStrLn $ fst $ foldl1 plus [g,eql, z ]
-}
