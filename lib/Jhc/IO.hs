module Jhc.IO where

import Jhc.JumpPoint
import Prelude.IOError


-- this is treated specially by the compiler. it won't treat it as a product type.
data World__ = World__
type IOErrorCont = IOCont World__ IOError

--data IOResult a = FailIO World__ IOError | JustIO World__ a
data IOResult a = JustIO World__ a
newtype IO a = IO (IOErrorCont -> World__ -> IOResult a)


undefinedIOErrorCont :: IOErrorCont
undefinedIOErrorCont = error "Jhc.IO.undefinedIOErrorCont"

showError :: IOError -> IO b
showError (IOError z) = do
    putStrLn z
    c_exit 255
    return undefined

errorContinuation :: (IOErrorCont -> World__ -> IOResult a) -> IO a
errorContinuation x = newContinuation__ (\ncont -> IO $ \_ w -> x ncont w) showError

unsafePerformIO :: IO a -> a
unsafePerformIO (IO x) = case errorContinuation x of
    IO y -> case y undefinedIOErrorCont (newWorld__ x) of
        JustIO _ a -> a

-- we have to replace the error handler because the context might have quit by the time the value is evaluated.
unsafeInterleaveIO :: IO a -> IO a
unsafeInterleaveIO (IO action) = IO $ \c w -> JustIO w $ case action' c w of
    JustIO _ a -> a
    where
    IO action' = errorContinuation action

instance Monad IO where
    return x = IO $ \_ w -> JustIO w x
    IO x >>= f = IO $ \c w -> case x c w of
        JustIO w v -> case f v of
            IO g -> g c w
    IO x >> IO y = IO $ \c w -> case x c w of
        JustIO w _ -> y c w
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
fixIO k = IO $ \c w -> let
            r = case k ans of
                    IO z -> z c w
            ans = case r of
                JustIO _ z  -> z
               in r

ioError    ::  IOError -> IO a
ioError e   =  (IO $ \c w -> case callContinuation c e of IO cont -> cont c w)


catch ::  IO a -> (IOError -> IO a) -> IO a
catch (IO x) fn = newContinuation__ (\ncont -> IO $ \_ w -> x ncont w) fn

-- | this creates a new world object that artificially depends on its argument to avoid CSE.
foreign import primitive newWorld__ :: a -> World__

-- throws away first argument. but causes second argument to artificially depend on it.
foreign import primitive drop__ :: forall a b. a -> b -> b


