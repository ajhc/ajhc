module Main where


main :: IO ()
main = print $ sum [ length $ show n | n <- [1..1000000::Int] ]
