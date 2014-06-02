module Module where


bar,baz,bob :: [()]

bar = bar
baz = baz
bob = bob

foo = do
    bar
    x $ do
    baz
    y $ do
    bob
    where
    x = y
    y = x
