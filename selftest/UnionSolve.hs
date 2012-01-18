import Control.Monad.Writer
import qualified Data.Map as Map

import Util.UnionSolve

main = do
    runTest test1
    runTest test2
    runTest test3

runTest t = do
    putStrLn "Test:"
    let w = execWriter t
    (x,y) <- solve putStrLn w
    mapM_ print (Map.toList x)
    mapM_ print (Map.toList y)

test1 = do
    tell $ 1 @>= True
    tell $ 2 @>=@ 1

test2 :: Writer (C () Int) ()
test2 = do
    tell $ 1 @>=@ 2
    tell $ 2 @>=@ 1
    tell $ 3 @>=@ 4
    tell $ 5 @=@ 6

test3 :: Writer (C Bool Int) ()
test3 = do
    forM_ [0 .. 9] $ \n -> do
        tell $ n @<=@ ((n + 1) `mod` 10)
    tell $ 3 @<= True


instance Fixable () where
    join () () = ()
    meet () () = ()
    lte () () = True
    eq () () = True
