module Main where

-- Test program for various uses of higher order polymorphism

-- useful bits
newtype Id x = Id x
--    deriving(Show)

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

type Arg2 f b = f b

type IO' a = IO a

-- explicit forall
id1 :: forall a . a -> a
id1 x = x


-- type synonym with forall
id2 :: IdentityFunc
id2 x = x




-- forall hoisting
id3 :: a -> IdentityFunc
id3 _ x = x

-- ghc does not accept the following 2 without the parens

id4 :: a -> (forall a . a -> a)
id4 _ x = x

id5 :: a -> (forall b . (forall c . (forall d . b -> c -> d -> a)))
id5 a _ _ _ = a

id6 :: a -> (forall b . Show b => (forall d . (Eq b, Show d) => (b,d)))
id6 = undefined

-- this should be rejected.
--id3 :: forall a . b -> a -> a
--id3 _ x = x

-- synonyms may be partially applied in arguments to other type synonyms
synPart :: Arg2 IO' Int
synPart = undefined

-- incomplete partially applied synonym. should be rejected
--synPart' :: Arg2 IO
--synPart' = undefined


-- polymorphic components
data Bob  = Bob (forall a . a -> a)

-- this can't be handled yet
data Fred = Fred (forall a . a -> a) ((forall a . (forall b . b -> a ) -> a) -> Int)

xs :: [forall a . a -> a]
xs = [id,const undefined,error "three"]

myhead ::  [forall a . a -> a] -> (forall a . a -> a)
myhead = head

f (Bob x) = (x 'y',x (3::Int))

main = do
    putChar $ fst $ f (Bob id)
    putChar $ myhead xs 'z'
    putStrLn "Done."


