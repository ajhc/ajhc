--
-- The Great Computer Language Shootout
-- http://shootout.alioth.debian.org/
--
-- Haskell version of Isaac Gouy's Clean version, translated by Don Stewart
--

import System; import Numeric

main = do n <- getArgs >>= readIO . head
          let sums     = loop (1::Int) n 1 0 0 0 0 0 0 0 0 0
              fn (s,t) = putStrLn $ (showFFloat (Just 9) s []) ++ "\t" ++ t
          mapM_ (fn :: (Double, String) -> IO ()) (zip sums names)

names = ["(2/3)^k", "k^-0.5", "1/k(k+1)", "Flint Hills", "Cookson Hills"
        , "Harmonic", "Riemann Zeta", "Alternating Harmonic", "Gregory"]

loop i n alt a1 a2 a3 a4 a5 a6 a7 a8 a9
    | i !n !alt !a1 !a2 !a3 !a4 !a5 !a6 !a7 !a8 !a9 !False = undefined -- strict
    | k > n     = [ a1, a2, a3, a4, a5, a6, a7, a8, a9 ]
    | otherwise = loop (i+1) n (-alt)
                       (a1 + (2/3) ** (k-1))
                       (a2 + 1 / sqrt k)
                       (a3 + 1 / (k * (k + 1)))
                       (a4 + 1 / (k3 * sk * sk))
                       (a5 + 1 / (k3 * ck * ck))
                       (a6 + dk)
                       (a7 + 1 / k2)
                       (a8 + alt * dk)
                       (a9 + alt / (2 * k - 1))
    where k3 = k2*k; k2 = k*k; dk = 1/k; k = fromIntegral i; sk = sin k; ck = cos k; x!y = x`seq`y
