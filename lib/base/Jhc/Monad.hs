{-# OPTIONS_JHC -N #-}

module Jhc.Monad where

import Jhc.Basics
import Jhc.IO

-- Monadic classes

infixl 1  >>, >>=
infixr 1  =<<

class Functor f  where
    fmap              :: (a -> b) -> f a -> f b

{- INLINE return, fail, (>>=), (>>) -}
class Monad m  where
    (>>=)  :: m a -> (a -> m b) -> m b
    (>>)   :: m a -> m b -> m b
    return :: a -> m a
    fail   :: String -> m a

        -- Minimal complete definition:
        --      (>>=), return
    m >> k  =  m >>= \_ -> k
    fail s  = error s

mapM             :: Monad m => (a -> m b) -> [a] -> m [b]
mapM f as = go as where
    go [] = return []
    go (a:as) = do
        a' <- f a
        as' <- go as
        return (a':as')

mapM_            :: Monad m => (a -> m b) -> [a] -> m ()
mapM_ f as = go as where
    go [] = return ()
    go (a:as) = f a >> go as

(=<<)            :: Monad m => (a -> m b) -> m a -> m b
f =<< x          =  x >>= f



instance Monad [] where
    return x = [x]
    xs >>= f = concatMap f xs
    fail _ = []

instance Functor [] where
    fmap f (x:xs) = f x : fmap f xs
    fmap f [] = []

instance Monad IO where
    return x = IO $ \w -> (# w, x #)
    IO x >>= f = IO $ \w -> case x w of
        (# w, v #) -> case f v of
            IO g -> g w
    IO x >> IO y = IO $ \w -> case x w of
        (# w,  _ #) -> y w
    fail s = ioError $ userError s

instance Functor IO where
    fmap f a = a >>= \x -> return (f x)


