module Main (main) where

data Tree = Leaf | Branch [Tree]

mkTree   :: Int -> Tree
mkTree 0 = Leaf
mkTree _ = Branch [mkTree 0]

leaves             :: Tree -> Int
leaves Leaf        = 1
leaves (Branch ts) = sum $ map leaves ts

main :: IO ()
main = print . leaves . mkTree $ 1
