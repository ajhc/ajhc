module Main where

-- Test program for various uses of higher order polymorphism

-- useful bits
newtype Id x = Id x
    deriving(Show)

-- interesting haskell 98 types
newtype Rec f = In (f (Rec f))
newtype StateM m s a = STM (a -> m (a,s))
newtype Bot = Bot Bot
    deriving(Show)

-- other stuff

-- TODO this causes segfault when shown
data Empty


-- forall in type synonym
type IdentityFunc = forall a . a -> a

-- explicit forall
id1 :: forall a . a -> a
id1 x = x



-- forall hoisting
id2 :: a -> IdentityFunc
id2 _ x = x

-- this should be rejected.
--id3 :: forall a . b -> a -> a
--id3 _ x = x


main = do
    putStrLn "Done."
