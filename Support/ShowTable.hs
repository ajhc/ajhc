module Support.ShowTable where

import Monad
import List
import qualified Data.Map as Map
import qualified Data.Set as Set

class ShowTable a where
    showTablePairs :: a -> [(String,String)]

instance (Show a,Show b) => ShowTable [(a,b)] where
    showTablePairs xs = [ (show x,show y) | (x,y) <- xs ]

instance (Show a,Show b) => ShowTable (Map.Map a b) where
    showTablePairs xs = [ (show x,show y) | (x,y) <- Map.toList xs ]

instance Show a => ShowTable (Set.Set a) where
    showTablePairs xs = [ (show x,"") | x <- Set.toList xs ]


printTable :: ShowTable a => String -> a -> IO ()
printTable title x = do
    unless (null title) $ putStrLn (title ++ ":")
    mapM_ putStrLn $ sort [ "  " ++ x ++ (if null y then "" else " - " ++ y) | (x,y) <- showTablePairs x]
