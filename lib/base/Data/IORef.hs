{-# OPTIONS_JHC -funboxed-values -fno-prelude -funboxed-tuples -fffi #-}
module Data.IORef(
    IORef(),	      -- abstract, instance of: Eq
    newIORef,	      -- :: a -> IO (IORef a)
    readIORef,	      -- :: IORef a -> IO a
    writeIORef,	      -- :: IORef a -> a -> IO ()
    modifyIORef,      -- :: IORef a -> (a -> a) -> IO ()
    atomicModifyIORef,-- :: IORef a -> (a -> (a,b)) -> IO b
    ) where

import Jhc.Prim.Array
import Jhc.Basics
import Jhc.Order
import Jhc.IO
import Jhc.Int

data IORef a = IORef (MutArray_ a)

newRef__ a = newArray__ 1# a
writeRef__ m v = writeArray__ m 0# v
readRef__ m = readArray__ m 0#

-- {-# NOINLINE newIORef #-}
newIORef :: a -> IO (IORef a)
newIORef v = fromUIO $ \w -> case newRef__ v w of (# w', r #) -> (# w', IORef r #)

-- {-# NOINLINE readIORef #-}
readIORef :: IORef a -> IO a
readIORef (IORef r) = fromUIO $ \w -> readRef__ r w

-- {-# NOINLINE writeIORef #-}
writeIORef :: IORef a -> a -> IO ()
writeIORef (IORef r) v = fromUIO $ \w -> case writeRef__ r v w of w' -> (# w', () #)

--foreign import primitive eqRef__ :: Ref__ a -> Ref__ a -> Bool

--instance Eq (IORef a) where
--    (IORef x) == (IORef y) = eqRef__ x y

--{-# NOINLINE modifyIORef #-}
modifyIORef :: IORef a -> (a -> a) -> IO ()
modifyIORef (IORef ref) f = fromUIO $ \w -> case readRef__ ref w of
    (# w', a #) -> case writeRef__ ref (f a) w' of
        w'' -> (# w'', () #)

--{-# NOINLINE atomicModifyIORef #-}
atomicModifyIORef :: IORef a -> (a -> (a,b)) -> IO b
atomicModifyIORef (IORef r) f = fromUIO $ \w -> case readRef__ r w of
    (# w', a #) -> case f a of
        (a',b) -> case writeRef__ r a' w' of
            w'' -> (# w'', b #)
