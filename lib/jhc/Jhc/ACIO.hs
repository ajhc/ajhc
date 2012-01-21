{-# OPTIONS_JHC -fno-prelude  #-}
module Jhc.ACIO where

import Jhc.IORef
import Jhc.IO
import Jhc.Monad


newtype ACIO a = ACIO (IO a)
    deriving(Monad,Functor)

unsafeIOToACIO :: IO a -> ACIO a
unsafeIOToACIO x = ACIO x

acioToIO :: ACIO a -> IO a
acioToIO (ACIO x) = x

newIORefAC :: a -> ACIO (IORef a)
newIORefAC a = unsafeIOToACIO (newIORef a)


runOnce :: IO a -> ACIO (IO a)
runOnce action = do
    ref <- newIORefAC Nothing
    return $ do
        v <- readIORef ref
        case v of
            Just v -> return v
            Nothing -> do
                v <- action
                writeIORef ref v
                return v

{- @Extensions

# Top Level Actions

Jhc supports monadic actions declared at the top level of your module. These
can be used to do things such as initialize IORefs or allocate static data. An example
of a top level action is the following.

    import Jhc.ACIO
    import Data.IORef

    ref <- newIORefAC 0

    count = do
        modifyIORef ref (1 +)
        readIORef ref >>= print

    main = do
        count
        count
        count

Which will print 1, 2, and 3. A special monad ACIO (which stands for Affine
Central IO) is provided to restrict what may take place in top level actions.
Basically, top level actions can only consist of IO that can be omitted or
reordered without changing the meaning of a program. In practice, this means
that it does not matter whether such actions are all performed at the beginning
or are only computed once on demand.

If you need to use arbitrary IO, a utility function 'runOnce' is provided.
using it you can ensure arbitrary IO actions are run only once and the return
values shared, however you must access the value inside the IO monad, thus
ensuring program integrity. An example using a hypothetical GUI library is below.


    import Jhc.ACIO

    getWindow <- runOnce $ do
        connection <- newGUIConnection
        window <- createWindow (640,480)
        setTitle window "My Global Window"
        return window

    main = do
        w <- getWindow
        draw w "Hello!"

Note, top level global variables can be indicative of design issues. In
general, they should only be used when necessary to interface with an external
library, opaque uses inside a library where the shared state can not be
externally observed, or inside your Main program as design dictates.

-}
