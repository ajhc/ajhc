module Module where

foo = let obj = 2 in
    let b = obj; c = obj in
    let x = case b of
            3 -> 2
            5 -> 6 in
    let y = c in
    3
