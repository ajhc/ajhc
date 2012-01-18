import Control.Monad.Writer
import qualified Data.Map as Map

import Util.UnionSolve


main = do
    runTest test1

runTest t = do
    let w = execWriter test1
    (_,y) <- solve putStrLn w
    mapM_ print (Map.toList y)

test1 = do  
    tell $ 1 @>= True
    tell $ 2 @>=@ 1
    tell $ 3 @>=@ 4
