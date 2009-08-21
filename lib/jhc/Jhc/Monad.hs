{-# OPTIONS_JHC -N -funboxed-tuples #-}

module Jhc.Monad where

import Jhc.Basics
import Jhc.IO

-- Monadic classes

infixl 1  >>, >>=
infixr 1  =<<

class Functor f  where
    fmap              :: (a -> b) -> f a -> f b

{- INLINE return, fail, (>>=), (>>) -}
class Monad m where
    (>>=)  :: m a -> (a -> m b) -> m b
    (>>)   :: m a -> m b -> m b
    return :: a -> m a
    fail   :: String -> m a

        -- Minimal complete definition:
        --      (>>=), return
    m >> k  =  m >>= \_ -> k
    fail s  = error s

{- SPECIALIZE sequence :: forall a . [IO a] -> IO [a] #-}
{- SPECIALIZE sequence_ :: forall a . [IO a] -> IO () #-}
{- SPECIALIZE mapM :: forall a b . (a -> IO b) -> [a]-> IO [b] #-}
{- SPECIALIZE mapM_ :: forall a b . (a -> IO b) -> [a]-> IO () #-}

{-# RULES "sequence/[]"   sequence [] = return [] #-}
{-# RULES "sequence_/[]"  sequence_ [] = return () #-}
{-# RULES "mapM/[]"       forall f . mapM f [] = return [] #-}
{-# RULES "mapM_/[]"      forall f . mapM_ f [] = return () #-}
{-# RULES "sequence_/++"  forall xs ys . sequence_ (xs ++ ys) = sequence_ xs >> sequence_ ys #-}
{-# RULES "mapM_/++"      forall xs ys f . mapM_ f (xs ++ ys) = mapM_ f xs >> mapM_ f ys #-}

mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM f as = go as where
    go [] = return []
    go (a:as) = do
        a' <- f a
        as' <- go as
        return (a':as')

mapM_ :: Monad m => (a -> m b) -> [a] -> m ()
mapM_ f as = go as where
    go [] = return ()
    go (a:as) = f a >> go as

sequence :: Monad m => [m a] -> m [a]
sequence xs = f xs where
    f [] = return []
    f (x:xs) = x >>= \r -> f xs >>= \rs -> return (r:rs)

sequence_ :: Monad m => [m a] -> m ()
sequence_ xs  =  f xs where
    f [] = return ()
    f (x:xs) = x >> f xs

(=<<) :: Monad m => (a -> m b) -> m a -> m b
f =<< x =  x >>= f



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


