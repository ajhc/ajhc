
-- | a simple type that only lets an IO action happen once, caching its result.

module Util.Once(
    Once,
    newOnce,
    runOnce,
    altOnce,

    OnceMap,
    newOnceMap,
    runOnceMap,
    altOnceMap,
    onceMapToList,
    onceMapKeys,
    onceMapElems

    ) where

import qualified Data.Map as Map
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


-- | run an IO action at most once for each distinct argument

newtype OnceMap a b = OnceMap (IORef (Map.Map a b))
    deriving(Typeable)


newOnceMap :: Ord a => IO (OnceMap a b)
newOnceMap = do
    r <- newIORef Map.empty
    return $ OnceMap r

runOnceMap :: Ord a => OnceMap a b -> a -> IO b -> IO b
runOnceMap (OnceMap r) x act = do
    m <- readIORef r
    case Map.lookup x m of
        Just y -> return y
        Nothing -> do
            y <- act
            modifyIORef r (Map.insert x y)
            return y

altOnceMap :: Ord a => OnceMap a () -> a -> IO b -> IO b -> IO b
altOnceMap (OnceMap ref) x first after = do
    m <- readIORef ref
    case Map.member x m of
        True -> after
        False -> do
            modifyIORef ref (Map.insert x ())
            first

onceMapToList :: OnceMap a b -> IO [(a,b)]
onceMapToList (OnceMap ref) = do
    m <- readIORef ref
    return $ Map.toList m

onceMapKeys :: OnceMap a b -> IO [a]
onceMapKeys (OnceMap ref) = do
    m <- readIORef ref
    return $ Map.keys m

onceMapElems :: OnceMap a b -> IO [b]
onceMapElems (OnceMap ref) = do
    m <- readIORef ref
    return $ Map.elems m

