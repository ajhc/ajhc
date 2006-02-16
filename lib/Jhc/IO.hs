module Jhc.IO where

import Prelude.IOError


-- this is treated specially by the compiler. it won't treat it as a product type.
data World__ = World__

data IOResult a = FailIO World__ IOError | JustIO World__ a
newtype IO a = IO (World__ -> IOResult a)


unsafePerformIO :: IO a -> a
unsafePerformIO (IO x) = case x (newWorld__ x) of
    FailIO _ z -> error $ case z of IOError z ->  z
    JustIO _ a -> a

unsafeInterleaveIO :: IO a -> IO a
unsafeInterleaveIO (IO action) = IO $ \w -> JustIO w $ case action w of
    FailIO _ z -> error $ case z of IOError z ->  z
    JustIO _ a -> a

instance Monad IO where
    return x = IO $ \w -> JustIO w x
    IO x >>= f = IO $ \w -> case x w of
        JustIO w v -> case f v of
            IO g -> g w
        FailIO w x -> FailIO w x
    IO x >> IO y = IO $ \w -> case x w of
        JustIO w _ -> y w
        FailIO w x -> FailIO w x
    fail s = ioError $ userError s

instance Functor IO where
    fmap f a = a >>= \x -> return (f x)

{-
fixIO :: (a -> IO a) -> IO a
fixIO k = IO $ \w -> let
            r@(JustIO _ ans) = case k ans of
                    IO z -> case z w of
                        FailIO _ z -> error $ case z of IOError z ->  z
                        z -> z
              in r
-}

fixIO :: (a -> IO a) -> IO a
fixIO k = IO $ \w -> let
            r = case k ans of
                    IO z -> z w
            ans = case r of
                FailIO _ _ -> error $ "IOError"
                JustIO _ z  -> z
               in r

-- | this creates a new world object that artificially depends on its argument to avoid CSE.
foreign import primitive newWorld__ :: a -> World__

-- throws away first argument. but causes second argument to artificially depend on it.
foreign import primitive drop__ :: forall a b. a -> b -> b


