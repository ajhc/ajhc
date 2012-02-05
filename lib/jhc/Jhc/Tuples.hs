{-# OPTIONS_JHC -fno-prelude #-}

-- | A place to collect the tuple instances.

module Jhc.Tuples where

import Jhc.Basics
import Jhc.Show
import Jhc.Order
import Jhc.List
import Jhc.Text.Read

{- TUPGEN!

instance (#Tup Eq #t) => Eq (#Tup #t) where
    (#Tup #x) == (#Tup #y) = and [#List #x == #y]
    (#Tup #x) /= (#Tup #y) = or [#List #x /= #y]

-}

instance (Ord a, Ord b) => Ord (a,b) where
    compare (x,y) (a,b) = case compare x a of
        EQ -> compare y b
        z -> z

instance (Ord a, Ord b, Ord c) => Ord (a,b,c) where
    compare (x,y,z) (a,b,c) = case compare x a of
        EQ -> case compare y b of
            EQ -> compare z c
            z -> z
        z -> z

instance (Ord a, Ord b, Ord c, Ord d) => Ord (a,b,c,d) where
    compare (x,y,z,j) (a,b,c,d) = case compare x a of
        EQ -> case compare y b of
            EQ -> case compare z c of
                EQ -> compare j d
                z -> z
            z -> z
        z -> z

--instance (Eq a, Eq b) => Eq (a,b) where
--    (x,y) == (a,b) = x == a && y == b

instance  (Read a, Read b) => Read (a,b)  where
    readsPrec p       = readParen False
                            (\r -> [((x,y), w) | ("(",s) <- lex r,
                                                 (x,t)   <- reads s,
                                                 (",",u) <- lex t,
                                                 (y,v)   <- reads u,
                                                 (")",w) <- lex v ] )

instance  (Read a, Read b, Read c) => Read (a,b,c)  where
    readsPrec p       = readParen False
                            (\r -> [((x,y,z), w) | ("(",s) <- lex r,
                                                 (x,t)   <- reads s,
                                                 (",",u) <- lex t,
                                                 (y,v)   <- reads u,
                                                 (",",w) <- lex v,
                                                 (z,p)   <- reads w,
                                                 (")",w) <- lex p ] )

instance (Show a, Show b, Show c, Show d) => Show (a, b, c, d) where
    showsPrec _ (w,x,y,z) s = (showChar '(' . shows w . showChar ',' .
                                              shows x . showChar ',' .
                                              shows y . showChar ',' .
                                              shows z . showChar ')')
                              s

instance (Show a, Show b, Show c, Show d, Show e) => Show (a, b, c, d, e) where
    showsPrec _ (v,w,x,y,z) s = (showChar '(' . shows v . showChar ',' .
                                                shows w . showChar ',' .
                                                shows x . showChar ',' .
                                                shows y . showChar ',' .
                                                shows z . showChar ')')
                                    s

instance (Show a, Show b, Show c, Show d, Show e, Show f) => Show (a, b, c, d, e, f) where
    showsPrec _ (v,w,x,y,z,a) s = (showChar '(' . shows v . showChar ',' .
                                                  shows w . showChar ',' .
                                                  shows x . showChar ',' .
                                                  shows y . showChar ',' .
                                                  shows z . showChar ',' .
                                                  shows a . showChar ')')
                                    s

-- tupgen 2

instance (Eq t1,Eq t2) => Eq (t1,t2) where
    (x1,x2) == (y1,y2) = and [x1 == y1,x2 == y2]
    (x1,x2) /= (y1,y2) = or [x1 /= y1,x2 /= y2]

-- tupgen 3

instance (Eq t1,Eq t2,Eq t3) => Eq (t1,t2,t3) where
    (x1,x2,x3) == (y1,y2,y3) = and [x1 == y1,x2 == y2,x3 == y3]
    (x1,x2,x3) /= (y1,y2,y3) = or [x1 /= y1,x2 /= y2,x3 /= y3]

-- tupgen 4

instance (Eq t1,Eq t2,Eq t3,Eq t4) => Eq (t1,t2,t3,t4) where
    (x1,x2,x3,x4) == (y1,y2,y3,y4) = and [x1 == y1,x2 == y2,x3 == y3,x4 == y4]
    (x1,x2,x3,x4) /= (y1,y2,y3,y4) = or [x1 /= y1,x2 /= y2,x3 /= y3,x4 /= y4]

-- tupgen 5

instance (Eq t1,Eq t2,Eq t3,Eq t4,Eq t5) => Eq (t1,t2,t3,t4,t5) where
    (x1,x2,x3,x4,x5) == (y1,y2,y3,y4,y5) = and [x1 == y1,x2 == y2,x3 == y3,x4 == y4,x5 == y5]
    (x1,x2,x3,x4,x5) /= (y1,y2,y3,y4,y5) = or [x1 /= y1,x2 /= y2,x3 /= y3,x4 /= y4,x5 /= y5]

-- tupgen 6

instance (Eq t1,Eq t2,Eq t3,Eq t4,Eq t5,Eq t6) => Eq (t1,t2,t3,t4,t5,t6) where
    (x1,x2,x3,x4,x5,x6) == (y1,y2,y3,y4,y5,y6) = and [x1 == y1,x2 == y2,x3 == y3,x4 == y4,x5 == y5,x6 == y6]
    (x1,x2,x3,x4,x5,x6) /= (y1,y2,y3,y4,y5,y6) = or [x1 /= y1,x2 /= y2,x3 /= y3,x4 /= y4,x5 /= y5,x6 /= y6]

-- tupgen 7

instance (Eq t1,Eq t2,Eq t3,Eq t4,Eq t5,Eq t6,Eq t7) => Eq (t1,t2,t3,t4,t5,t6,t7) where
    (x1,x2,x3,x4,x5,x6,x7) == (y1,y2,y3,y4,y5,y6,y7) = and [x1 == y1,x2 == y2,x3 == y3,x4 == y4,x5 == y5,x6 == y6,x7 == y7]
    (x1,x2,x3,x4,x5,x6,x7) /= (y1,y2,y3,y4,y5,y6,y7) = or [x1 /= y1,x2 /= y2,x3 /= y3,x4 /= y4,x5 /= y5,x6 /= y6,x7 /= y7]
