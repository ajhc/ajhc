{-# OPTIONS_JHC -funboxed-tuples #-}

module Jhc.IO(
    IO(..),
--    IOResult(..),
    World__(),
    IOErrorCont(),
    catch,
    dependingOn,
    fixIO,
    ioError,
    runExpr,
    runMain,
    runNoWrapper,
    exitFailure,
    strictReturn,
    undefinedIOErrorCont,
    unsafeInterleaveIO,
    unsafePerformIO
    ) where

import Jhc.Hole
import Jhc.JumpPoint
import Jhc.Prim
import Prelude.IOError


data IOErrorCont = IOErrorCont JumpPoint (Hole IOError)

--data IOResult a = FailIO World__ IOError | JustIO World__ a
-- data IOResult a = JustIO World__ a

newtype IO a = IO (IOErrorCont -> World__ -> (# World__, a #))


undefinedIOErrorCont :: IOErrorCont
undefinedIOErrorCont = IOErrorCont errorJumpPoint errorHole

showError :: IOError -> IO b
showError (IOError z) = do
    putStrLn z
    exitFailure

errorContinuation :: IO a -> IO a
errorContinuation x = catch x showError

unsafePerformIO :: IO a -> a
unsafePerformIO x = case newWorld__ x of
    world -> case errorContinuation x of
        IO y -> case y undefinedIOErrorCont world of
            (# _, a #) -> a

-- we have to replace the error handler because the context might have quit by the time the value is evaluated.
unsafeInterleaveIO :: IO a -> IO a
unsafeInterleaveIO action = IO $ \c w -> (# w , case action' c w of (# _,  a #) -> a #)
    where IO action' = errorContinuation action

instance Monad IO where
    return x = IO $ \_ w -> (# w,  x #)
    IO x >>= f = IO $ \c w -> case x c w of
        (# w, v #) -> case f v of
            IO g -> g c w
    IO x >> IO y = IO $ \c w -> case x c w of
        (# w,  _ #) -> y c w
    fail s = ioError $ userError s

instance Functor IO where
    fmap f a = a >>= \x -> return (f x)

data FixIO a = FixIO World__ a

fixIO :: (a -> IO a) -> IO a
fixIO k = IO $ \c w -> let
            r = case k ans of
                    IO z -> case z c w of
                        (# w, r #) -> FixIO w r
            ans = case r of
                FixIO _ z -> z
               in case r of
                FixIO w z -> (# w, z #)

getJumpPoint :: IO IOErrorCont
getJumpPoint = IO $ \ ioe w -> (# w, ioe #)

ioError    ::  IOError -> IO a
ioError e   =  do
    IOErrorCont jp he <- getJumpPoint
    fillHole he e
    jumpJumpPoint__ jp



catch ::  IO a -> (IOError -> IO a) -> IO a
catch (IO x) fn = do
    hole <- newHole
    withJumpPoint__ $ \jp b -> case b of
        False -> IO $ \_ w -> x (IOErrorCont jp hole) w
        True -> readHole hole >>= fn

-- | this creates a new world object that artificially depends on its argument to avoid CSE.
foreign import primitive newWorld__ :: a -> World__


-- throws away first argument. but causes second argument to artificially depend on it.
foreign import primitive "drop__" worldDep__ :: forall b. World__ -> b -> b

-- | this will return a value making it artificially depend on the state of the world. any uses of this value are guarenteed not to float before this point in the IO monad.
strictReturn :: a -> IO a
strictReturn a = IO $ \_ w -> (# w, worldDep__ w a #)

{-# INLINE runMain, runExpr #-}
-- | this is wrapped around 'main' when compiling programs. it catches any exceptions and prints them to the screen and dies appropriatly.
runMain :: IO a -> World__ -> World__
runMain main w = case run undefinedIOErrorCont w of
        (# w,  _ #) -> w
    where
    IO run = catch main $ \e -> do
            putStrLn "\nUncaught Exception:"
            putStrLn $ showIOError e
            exitFailure


-- | this is wrapped around arbitrary showable expressions when used as the main entry point
runExpr :: Show a => a -> World__ -> World__
runExpr x w = runNoWrapper (print x) w


-- | when no exception wrapper is wanted
runNoWrapper :: IO a -> World__ -> World__
runNoWrapper (IO run) w =
    case run undefinedIOErrorCont w of
        (# w, _ #) -> w

exitFailure :: IO a
exitFailure = IO $ \_ w -> exitFailure__ w

foreign import primitive exitFailure__ :: World__ -> (# World__, a #)



