{-# OPTIONS_JHC -fno-prelude -funboxed-tuples #-}
module Jhc.Monad where

import Jhc.Basics
import Jhc.IO
import Jhc.Prim.IO

-- Monadic classes

infixl 1  >>, >>=
infixr 1  =<<

infixl 4  <$

class Functor f  where
    fmap :: (a -> b) -> f a -> f b
    (<$) :: a -> f b -> f a

    a <$ fb = fmap (const a) fb

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

instance Monad (ST s) where
    return x = ST $ \w -> (# w, x #)
    ST x >>= f = ST $ \w -> case x w of
        (# w, v #) -> case f v of
            ST g -> g w
    ST x >> ST y = ST $ \w -> case x w of
        (# w,  _ #) -> y w
--    fail s = ioError $ userError s

instance Monad IO where
    return x = fromUIO $ \w -> (# w, x #)
    x >> y =  x `thenIO_` y
    fail s = ioError $ userError s
    x >>= f = fromUIO $ \w -> case unIO x w of
        (# w, v #) -> unIO (f v) w

instance Functor IO where
    fmap f a = a >>= \x -> return (f x)
