


x <|> y = y

main :: IO ()
main = do{ putStrLn "foo";  putStrLn "bar" } <|> putStrLn "baz"
