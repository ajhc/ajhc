-- The Great Computer Language Shootout
-- http://shootout.alioth.debian.org/
-- Based on the SML version, written by Matthias Blume.
-- Implemented in Haskell by Don Stewart
--
import System; import Data.Bits; import Data.Word; import Text.Printf; import Data.Char

main = do (w::Word32) <- getArgs >>= readIO . head
          printf "P4\n%d %d\n" (fromIntegral w::Int) (fromIntegral w::Int) >> yl 0 w w

yl y h w = if y < h then xl 0 y 0 8 h w else return ()

xl x y b n h w
    | x == w    = putChar (chr $ b `shiftL` n) >> yl (y+1) h w
    | otherwise = do
        (b',n') <- if n == 0 then putChar (chr b) >> return (0,8) else return (b,n)
        xl (x+1) y (b'+b'+ fromEnum (p x y w h)) (n'-1) h w

p (x::Word32) y w h = lp 0.0 0.0 50 (f x * 2.0 / f w - 1.5) (f y * 2.0 / f h - 1.0)
    where f = fromIntegral

lp r i k cr ci | r2 + i2 > (4.0 :: Double) = 0 :: Word32
               | k == (0 :: Word32)        = 1
               | otherwise                 = lp (r2-i2+cr) ((r+r)*i+ci) (k-1) cr ci
    where r2 = r*r ; i2 = i*i
