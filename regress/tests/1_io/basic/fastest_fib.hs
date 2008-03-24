import Data.List
import Data.Word
import Data.Int

fib1 n = snd . foldl fib' (1, 0) . map toEnum $  unfoldl divs n
    where
        unfoldl f x = case f x of
                Nothing     -> []
                Just (u, v) -> unfoldl f v ++ [u]

        divs 0 = Nothing
        divs k = Just (uncurry (flip (,)) (k `divMod` 2))

        fib' (f, g) p
            | p         = (f*(f+2*g), f^(2::Int) + g^(2::Int))
            | otherwise = (f^(2::Int)+g^(2::Int),   g*(2*f-g))

main :: IO ()
main = do
    print (fib1 22 :: Int)
    print (fib1 22 :: Int8)
    print (fib1 22 :: Int16)
    print (fib1 22 :: Int32)
    print (fib1 22 :: Int64)
    print (fib1 22 :: Word)
    print (fib1 22 :: Word8)
    print (fib1 22 :: Word16)
    print (fib1 22 :: Word32)
    print (fib1 22 :: Word64)
    print (fib1 22 :: Integer)
