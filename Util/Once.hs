
-- | a simple type that only lets an IO action happen once, caching its result.

module Util.Once(Once, newOnce, runOnce, altOnce) where

import Data.IORef
import Data.Dynamic

newtype Once a = Once (IORef (Maybe a))
    deriving(Typeable)


-- | create a new Once object
newOnce :: IO (Once a)
newOnce = do
    ref <- newIORef Nothing
    return (Once ref)

-- | execute the action at most once, always returning the same result
runOnce :: Once a -> IO a -> IO a
runOnce (Once ref) action = do
    b <- readIORef ref
    case b of
        Just x -> return x
        Nothing -> do
            r <- action
            writeIORef ref (Just r)
            return r

-- | run first argument once, after which perform the second

altOnce :: Once () -> IO b -> IO b -> IO b
altOnce (Once ref) first second = do
    b <- readIORef ref
    case b of
        Just _ -> second
        Nothing -> do
            writeIORef ref (Just ())
            first



