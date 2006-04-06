module Jhc.IO(
    IO(..),
    IOResult(..),
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
import Prelude.IOError


-- this is treated very specially by the compiler. it is unboxed.
data World__
data IOErrorCont = IOErrorCont JumpPoint (Hole IOError)

--data IOResult a = FailIO World__ IOError | JustIO World__ a
data IOResult a = JustIO World__ a
newtype IO a = IO (IOErrorCont -> World__ -> IOResult a)


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

getJumpPoint :: IO IOErrorCont
getJumpPoint = IO $ \ ioe w -> JustIO w ioe

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
foreign import primitive drop__ :: forall a b. a -> b -> b

-- like 'const' but creates an artificial dependency on its second argument to guide optimization.
dependingOn :: b -> a -> b
dependingOn = flip drop__

-- throws away first argument. but causes second argument to artificially depend on it.
foreign import primitive "drop__" worldDep__ :: forall b. World__ -> b -> b

-- | this will return a value making it artificially depend on the state of the world. any uses of this value are guarenteed not to float before this point in the IO monad.
strictReturn :: a -> IO a
strictReturn a = IO $ \_ w -> JustIO w (worldDep__ w a)

{-# INLINE runMain, runExpr #-}
-- | this is wrapped around 'main' when compiling programs. it catches any exceptions and prints them to the screen and dies appropriatly.
runMain :: IO a -> World__ -> World__
runMain main w = case run undefinedIOErrorCont w of
        JustIO w _ -> w
    where
    IO run = catch main $ \e -> do
            putStrLn "\nUncaught Exception:"
            putStrLn $ showIOError e
            exitFailure


-- | this is wrapped around arbitrary showable expressions when used as the main entry point
runExpr :: Show a => a -> World__ -> World__
runExpr x w = runMain (print x) w

-- | when no exception wrapper is wanted
runNoWrapper :: IO a -> World__ -> World__
runNoWrapper (IO run) w =
    case run undefinedIOErrorCont w of
        JustIO w _ -> w

exitFailure :: IO a
exitFailure = IO $ \_ w -> exitFailure__ w

foreign import primitive exitFailure__ :: World__ -> IOResult a



