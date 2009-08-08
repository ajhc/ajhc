
-- | similar to GenUtil but can rely on non-haskell 98 features
module Util.Gen(module Util.Gen, module GenUtil) where

import Control.Monad.Writer
import Control.Monad.Identity
import Data.Monoid
import Data.List
import Directory
import System.IO
import Data.Maybe

import GenUtil hiding(replicateM)

mconcatMap f xs = mconcat (map f xs)
mintercalate x xs = mconcat (intersperse x xs)

mconcatMapM f xs = mapM f xs >>= return . mconcat


runEither :: String -> Either String a -> a
runEither msg (Left fm) = error $ msg ++ " - " ++ fm
runEither _ (Right a) = a

travCollect :: Monoid w => ((a -> Writer w a) -> a -> Writer w a) -> (a -> w) -> a -> w
travCollect fn col x = execWriter (f x) where
    f x = tell (col x) >> fn f x

forMn_ xs = forM_ (zip xs [0 :: Int .. ])
forMn xs = forM (zip xs [0 :: Int .. ])

shortenPath :: String -> IO String
shortenPath x@('/':_) = do
    cd <- getCurrentDirectory
    pwd <- lookupEnv "PWD"
    h <- lookupEnv "HOME"
    let f d = do
            d <- d
            '/':rest <- getPrefix d x
            return rest
    return $ fromJust $ f (return cd) `mplus` f pwd `mplus` liftM ("~/" ++) (f h) `mplus` return x
shortenPath x = return x


