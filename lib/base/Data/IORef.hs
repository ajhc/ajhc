module Data.IORef(
    IORef,	      -- abstract, instance of: Eq
    newIORef,	      -- :: a -> IO (IORef a)
    readIORef,	      -- :: IORef a -> IO a
    writeIORef,	      -- :: IORef a -> a -> IO ()
    modifyIORef,      -- :: IORef a -> (a -> a) -> IO ()
    atomicModifyIORef,-- :: IORef a -> (a -> (a,b)) -> IO b
    ) where

import Jhc.IO

data Ref s a = Ref a

type IORef = Ref World__

foreign import primitive newRef__ :: forall s . a -> s -> (s,Ref s a)
foreign import primitive readRef__ :: forall s . Ref s a -> s -> (s,a)
foreign import primitive writeRef__ :: forall s . Ref s a -> a -> s -> s

foreign import primitive eqRef__ :: forall s . Ref s a -> Ref s a -> Bool

newIORef v = IO $ \_ world -> case newRef__ v world of
    (world',r) -> JustIO world' r
readIORef r = IO $ \_ world -> case readRef__ r world of
    (world',v) -> JustIO world' v
writeIORef r v = IO $ \_ world -> case writeRef__ r v world of
    world' -> JustIO world' ()

--instance Eq (IORef a) where
--    x == y = eqRef__ x y
--    x /= y = not (eqRef__ x y)

modifyIORef :: IORef a -> (a -> a) -> IO ()
modifyIORef ref f = writeIORef ref . f =<< readIORef ref

atomicModifyIORef r f = do
    a <- readIORef r
    case f a of
        (a',b) -> writeIORef r a' >> return b
