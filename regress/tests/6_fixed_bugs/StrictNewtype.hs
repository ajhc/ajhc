

newtype TID = TID Int
    deriving(Show)

data Foo = Foo !TID Char !Int
    deriving(Show)


main :: IO ()
main = print (Foo (TID 3) 'x' 4)
