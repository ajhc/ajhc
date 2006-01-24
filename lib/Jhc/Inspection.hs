module Jhc.Inspection where


foreign import primitive indexToConstructor :: Int -> a
foreign import primitive getConstructorIndex :: a -> Int
foreign import primitive getConstructorName  :: a -> (String,String)
