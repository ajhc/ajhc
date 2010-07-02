-- This triggers the same issue that prevents HashTable from building

data Foo a = Foo [a] deriving Show

data Bar a = Bar !(Foo a) deriving Show

main = print (Bar (Foo ["Hi!"]))
