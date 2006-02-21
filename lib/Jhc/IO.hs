module Jhc.IO where

import Jhc.Hole
import Jhc.JumpPoint
import Prelude.IOError


-- this is treated specially by the compiler. it won't treat it as a product type.
data World__ = World__
type IOErrorCont = (JumpPoint,Hole IOError)

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

errorContinuation :: IO a -> IO a
errorContinuation x = catch x showError

unsafePerformIO :: IO a -> a
unsafePerformIO x = case errorContinuation x of
    IO y -> case y undefinedIOErrorCont (newWorld__ x) of
        JustIO _ a -> a

-- we have to replace the error handler because the context might have quit by the time the value is evaluated.
unsafeInterleaveIO :: IO a -> IO a
unsafeInterleaveIO action = IO $ \c w -> JustIO w $ case action' c w of
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


fixIO :: (a -> IO a) -> IO a
fixIO k = IO $ \c w -> let
            r = case k ans of
                    IO z -> z c w
            ans = case r of
                JustIO _ z  -> z
               in r

getJumpPoint :: IO (JumpPoint,Hole IOError)
getJumpPoint = IO $ \ jh w -> JustIO w jh

ioError    ::  IOError -> IO a
ioError e   =  do
    (jp,he) <- getJumpPoint
    fillHole he e
    jumpJumpPoint__ jp


catch ::  IO a -> (IOError -> IO a) -> IO a
catch (IO x) fn = do
    hole <- newHole
    withJumpPoint__ $ \jp b -> case b of
        False -> IO $ \_ w -> x (jp,hole) w
        True -> fn (readHole hole)

-- | this creates a new world object that artificially depends on its argument to avoid CSE.
foreign import primitive newWorld__ :: a -> World__

-- throws away first argument. but causes second argument to artificially depend on it.
foreign import primitive drop__ :: forall a b. a -> b -> b


