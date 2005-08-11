module Data.IORef(
    IORef,	      -- abstract, instance of: Eq
    newIORef,	      -- :: a -> IO (IORef a)
    readIORef,	      -- :: IORef a -> IO a
    writeIORef,	      -- :: IORef a -> a -> IO ()
    modifyIORef,      -- :: IORef a -> (a -> a) -> IO ()
    atomicModifyIORef,-- :: IORef a -> (a -> (a,b)) -> IO b
    ) where

import Prelude.IO

data IORef a

foreign import primitive newIORef :: a -> IO (IORef a)
foreign import primitive readIORef :: IORef a -> IO a
foreign import primitive writeIORef :: IORef a -> a -> IO ()

foreign import primitive eqIORef :: IORef a -> IORef a -> Bool

instance Eq (IORef a) where
    x == y = eqIORef x y
    x /= y = not (eqIORef x y)

modifyIORef :: IORef a -> (a -> a) -> IO ()
modifyIORef ref f = writeIORef ref . f =<< readIORef ref

atomicModifyIORef r f = do
    a <- readIORef r
    case f a of
        (a',b) -> writeIORef r a' >> return b
