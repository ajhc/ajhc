{-# OPTIONS_GHC -fglasgow-exts #-}

module Util.ReaderWriter(ReaderWriter(),runReaderWriter) where

import Data.Monoid
import Control.Monad.Reader
import Control.Monad.Writer
-- strict unboxed ReaderWriter monad


newtype ReaderWriter r w a = ReaderWriter { _runReaderWriter :: r -> (# a, w #) }


runReaderWriter :: ReaderWriter r w a -> r -> (a,w)
runReaderWriter (ReaderWriter m) r = case m r of
    (# a, w #) -> (a,w)

instance Functor (ReaderWriter r w) where
	fmap f (ReaderWriter g) = ReaderWriter $ \r -> case g r of
            (# a, w #) -> (# f a, w #)

instance (Monoid w) => Monad (ReaderWriter r w) where
	return a = ReaderWriter $ \_ -> (# a, mempty #)
	(ReaderWriter m) >>= k  = ReaderWriter $ \r -> case m r of
            (# a,w #) -> case k a of
                ReaderWriter g -> case g r of
                    (# b, w' #) -> let w'' = w `mappend` w' in w'' `seq` (# b, w'' #)
        (ReaderWriter f) >> (ReaderWriter g) = ReaderWriter $ \r -> case f r of
            (# _, w #) -> case g r of
                (# a, w' #) -> let w'' = w `mappend` w' in w'' `seq` (# a, w'' #)


instance (Monoid w) => MonadWriter w (ReaderWriter r w) where
	tell   w = ReaderWriter $ \ _ -> w `seq` (# (), w #)
	listen (ReaderWriter m) = ReaderWriter $ \r -> case m r of
            (# a , w #) -> (# (a,w), w #)
	pass  (ReaderWriter m) = ReaderWriter $ \r -> case m r of
           (# (a, f), w #) -> let w' = f w in w' `seq` (# a, w' #)

instance Monoid w => MonadReader r (ReaderWriter r w) where
	ask       = ReaderWriter $ \r -> (# r, mempty #)
	local f (ReaderWriter m) = ReaderWriter $ \r -> m (f r)


