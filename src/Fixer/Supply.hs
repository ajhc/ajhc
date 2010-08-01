module Fixer.Supply(
    Supply(),
    newSupply,
    supplyReadValues,
    sValue,
    readSValue,
    supplyValue
    ) where

import Control.Monad.Trans
import Data.IORef
import Data.Typeable
import Fixer.Fixer
import qualified Data.Map as Map


-- maps b's to values of a's, creating them as needed.

data Supply b a = Supply Fixer {-# UNPACK #-} !(IORef (Map.Map b (Value a)))
    deriving(Typeable)


newSupply :: MonadIO m => Fixer -> m (Supply b a)
newSupply fixer = liftIO $ do
    ref <- newIORef Map.empty
    return $ Supply fixer ref

supplyValue :: (MonadIO m, Ord b, Fixable a) => Supply b a -> b -> m (Value a)
supplyValue (Supply fixer ref) b = liftIO $ do
    mp <- readIORef ref
    case Map.lookup b mp of
        Just v -> return v
        Nothing -> do
            v <- newValue fixer bottom
            modifyIORef ref (Map.insert b v)
            return v

sValue :: (Ord b, Fixable a) => Supply b a -> b -> (Value a)
sValue s b = ioValue (supplyValue s b)

supplyReadValues :: (Fixable a,MonadIO m) => Supply b a -> m [(b,a)]
supplyReadValues (Supply _fixer ref) = liftIO $ do
    mp <- readIORef ref
    flip mapM (Map.toList mp) $ \ (b,va) -> do
        a <- readValue va
        return (b,a)

readSValue :: (MonadIO m, Ord b, Fixable a) => Supply b a -> b -> m a
readSValue s b = do
    v <- supplyValue s b
    readValue v

