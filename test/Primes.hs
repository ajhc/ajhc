
suCC :: Int -> Int
suCC x = x + 1

isdivs :: Int  -> Int -> Bool
isdivs n x = mod x n /= 0

--the_filter :: [Int] -> [Int]
--the_filter (n:ns) = filter (isdivs n) ns

the_filter :: [Int] -> [Int]
the_filter (n:ns) = f ns where
    f [] = []
    f (x:ns) | isdivs n x = (x:f ns)
    f (_:ns) = f ns

primes :: [Int]
primes = map head (iterate the_filter (iterate suCC 2))
--primes = map head (iterate the_filter [2 .. ])

main = do
	--[arg] <- getArgs
	print $ primes !! 2000 -- (read arg)
