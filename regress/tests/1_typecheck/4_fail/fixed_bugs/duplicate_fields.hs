module Module where

data Foo = Foo { foo1 :: Int, foo2 :: !Int }

main :: IO ()
main = do
    case foo1 Foo { foo1 = 4, foo1 = 2 } of
        4 -> return ()
        2 -> return ()
