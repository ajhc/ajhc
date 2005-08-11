module Jhc.Array where


data Array__ a



-- | the number representing the size of the array must be less than or equal to the number of
-- elements in the list or bad stuff happens.
foreign import primitive "unsafeNewArray__" :: Int -> [a] -> (Array__ a)


foreign import primitive "unsafeAt__" :: Array__ a -> Int -> a
foreign import primitive "unsafeCopyArray__" :: Int -> [Either (Int,Int,Array__ a) a] -> Array__ a


