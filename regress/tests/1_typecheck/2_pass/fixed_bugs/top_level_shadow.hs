module ShouldPass where

null = 4

foo = let null = null in 4
