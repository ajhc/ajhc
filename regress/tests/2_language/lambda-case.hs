
f = \case
    4 -> const 'x'
    5 -> \case
        'a' -> 'b'
        'c' -> 'd'

main = do
    print $ f 5 'a'
    print $ f 4 'y'
