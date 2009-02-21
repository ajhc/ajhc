module Main where

import Data.Int

main :: IO ()
main = do print (minBound :: Int8)
          print (maxBound :: Int8)
          print (minBound :: Int16)
          print (maxBound :: Int16)
          print (minBound :: Int32)
          print (maxBound :: Int32)
          print (minBound :: Int64)
          print (maxBound :: Int64)
