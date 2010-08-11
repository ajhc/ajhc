{-# OPTIONS_JHC -funboxed-tuples #-}

module ShouldCompile where

f x = (# x, x #) :: (# Int, Int #)
