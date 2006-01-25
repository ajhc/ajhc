module Fixer.Supply(
    Supply(),
    newSupply,
    supplyReadValues,
    supplyValue
    ) where

import Fixer.Fixer
import qualified Data.Map as Map
import Data.IORef
import Data.Typeable


-- maps b's to values of a's, creating them as needed.

data Supply b a = Supply Fixer (IORef (Map.Map b (Value a)))
    deriving(Typeable)


newSupply :: Fixer -> IO (Supply b a)
newSupply fixer = do
    ref <- newIORef Map.empty
    return $ Supply fixer ref

supplyValue :: (Ord b, Fixable a) => Supply b a -> b -> IO (Value a)
supplyValue (Supply fixer ref) b = do
    mp <- readIORef ref
    case Map.lookup b mp of
        Just v -> return v
        Nothing -> do
            v <- newValue fixer bottom
            modifyIORef ref (Map.insert b v)
            return v

supplyReadValues :: Supply b a -> IO [(b,a)]
supplyReadValues (Supply _fixer ref) = do
    mp <- readIORef ref
    flip mapM (Map.toList mp) $ \ (b,va) -> do
        a <- readValue va
        return (b,a)


