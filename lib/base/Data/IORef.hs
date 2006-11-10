{-# OPTIONS_JHC -N #-}
module Data.IORef(
    IORef(),	      -- abstract, instance of: Eq
    newIORef,	      -- :: a -> IO (IORef a)
    readIORef,	      -- :: IORef a -> IO a
    writeIORef,	      -- :: IORef a -> a -> IO ()
    modifyIORef,      -- :: IORef a -> (a -> a) -> IO ()
    atomicModifyIORef,-- :: IORef a -> (a -> (a,b)) -> IO b
    ) where

import Jhc.Basics
import Jhc.Order
import Jhc.IO

data IORef a = IORef (Ref a)
data Ref a = Ref a


foreign import primitive newRef__   :: a -> World__ -> (# World__, IORef a #)
foreign import primitive readRef__  :: IORef a -> World__ -> (# World__, a #)
foreign import primitive writeRef__ :: IORef a -> a -> World__ -> World__

{-# NOINLINE newIORef #-}
newIORef :: a -> IO (IORef a)
newIORef v = IO $ \w -> newRef__ v w


{-# NOINLINE readIORef #-}
readIORef :: IORef a -> IO a
readIORef r = IO $ \w -> readRef__ r w

{-# NOINLINE writeIORef #-}
writeIORef :: IORef a -> a -> IO ()
writeIORef r v = IO $ \w -> case writeRef__ r v w of w' -> (# w', () #)

foreign import primitive eqRef__ :: IORef a -> IORef a -> Bool

instance Eq (IORef a) where
    x == y = eqRef__ x y
    x /= y = not (eqRef__ x y)


{-# NOINLINE modifyIORef #-}
modifyIORef :: IORef a -> (a -> a) -> IO ()
modifyIORef ref f = IO $ \w -> case readRef__ ref w of
    (# w', a #) -> case writeRef__ ref (f a) w' of
        w'' -> (# w'', () #)

{-# NOINLINE atomicModifyIORef #-}
atomicModifyIORef :: IORef a -> (a -> (a,b)) -> IO b
atomicModifyIORef r f = IO $ \w -> case readRef__ r w of
    (# w', a #) -> case f a of
        (a',b) -> case writeRef__ r a' w' of
            w'' -> (# w'', b #)
{-
--newIORef v = IO $ \_ world -> case newRef__ v world of
--    (world',r) -> JustIO world' r
--readIORef r = IO $ \_ world -> case readRef__ r world of
--    (world',v) -> JustIO world' v
--writeIORef r v = IO $ \_ world -> case writeRef__ r v world of
--    world' -> JustIO world' ()
{-# NOINLINE newIORef #-}
newIORef :: a -> IO (IORef a)
newIORef v = do
    v' <- strictReturn v
    return (IORef v')

{-# NOINLINE readIORef #-}
readIORef :: IORef a -> IO a
readIORef r = do
    --v <- strictReturn r
    case r of
        IORef r -> strictReturn r
-}

--foreign import primitive newRef__ :: forall s . a -> s -> (s,Ref s a)
--foreign import primitive readRef__ :: forall s . Ref s a -> s -> (s,a)
