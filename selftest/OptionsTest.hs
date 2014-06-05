module OptionsTest(optionsTest) where

import GenUtil hiding(replicateM)
import Options
import Options.Map
import System.Random
import Util.BitSet
import Util.SetLike
import Util.Std
import qualified Data.Set as Set
import qualified Util.Seq as Seq

grind n xs = f n xs where
    f n ys | n < 0 = ys
    f n ys = g (n - 1) (ys `mappend` xs)
    g n ys = f (n - 1) (xs `mappend` ys)

optionsTest :: IO ()
optionsTest = do
    let num = 100000
    --print $ length $ Seq.toList $ grind num (Seq.singleton 'x')
    print $ length $ grind num "x"
    exitSuccess
    return ()
