-- | this module provides a once-updatable value that may be used in pure code.
-- it is an _unchecked_ error to read a hole before it has been filled in.
-- filling in a hole has the effect of 'seq'ing its value immediatly so lift it in
-- a datatype if this is an issue.
--
-- this module should not be used unless you really know what you are doing.
-- incorrect usage may result in memory corruption.

module Jhc.Hole(Hole(),newHole,fillHole,readHole,errorHole) where

import Jhc.IO

newtype Hole a = Hole a

-- | unchecked error if readHole is evaled before fillHole has filled it in.
readHole :: Hole a -> IO a
readHole (Hole x) = strictReturn x

-- | create a new hole containing a garbage value. must not be read until it has been filled.
newHole :: IO (Hole a)
newHole = IO $ \_ world -> newHole__ world

-- | hole that can be written to and results discarded. never read this.
errorHole :: Hole a
errorHole = Hole undefined


-- | it is an unchecked error to fill in the same hole more than once.
fillHole :: Hole a -> a -> IO ()
fillHole r v = IO $ \_ world -> case fillHole__ r v world of
    world' -> JustIO world' ()

foreign import primitive newHole__  :: World__ -> IOResult (Hole a)
foreign import primitive fillHole__ :: Hole a -> a -> World__ ->World__

