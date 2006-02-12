module Jhc.Array where


-- The internal array type
data Array__ a

-- the built-in array quasi-monad
data AT a



-- none of these routines have run-time checks
foreign import primitive "newAT__" :: Int -> AT a -> Array__ a
foreign import primitive "writeAT__" :: Int -> a -> AT a
foreign import primitive "seqAT__" :: AT a -> AT a -> AT a
foreign import primitive "doneAT__" :: Int -> a -> AT a



-- lookup a value in an array
foreign import primitive "unsafeAt__" :: Array__ a -> Int -> a


newArray :: [a] -> Array__ a
newArray xs = newAT__ (length as) $ foldr assign doneAT (zip [0..] xs) where
    assign (i,v) rs = writeAT__ i v `seqAT__` rs

