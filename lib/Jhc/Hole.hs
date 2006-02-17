
module Jhc.Hole(Hole,newHole,fillHole,readHole) where

import Jhc.IO

newtype Hole a = Hole a



-- | unchecked error if readHole is evaled before fillHole has filled it in.
readHole :: Hole a -> a
readHole (Hole x) = x



foreign import primitive newHole__  :: World__ -> (World__,Hole a)
foreign import primitive fillHole__ :: Hole a -> a -> World__ ->World__

newHole :: IO (Hole a)
newHole = IO $ \_ world -> case newHole__ world of
    (world',r) -> JustIO world' r

fillHole :: Hole a -> a -> IO ()
fillHole r v = IO $ \_ world -> case fillHole__ r v world of
    world' -> JustIO world' ()

