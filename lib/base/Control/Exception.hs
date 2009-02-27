-- | This is mostly dummy, JHC does not support inexact exceptions.

module Control.Exception where

import Prelude hiding(catch)
import qualified Prelude as P

type IOException = IOError

data Exception = IOException IOException

-- throw :: Exception -> a

assert :: Bool -> a -> a
assert True x = x
assert False _ = error "assertion failure"

throwIO :: Exception -> IO a
throwIO (IOException ioe) = ioError ioe

catch :: IO a -> (Exception -> IO a) -> IO a
catch c h = P.catch c (h . IOException)

catchJust :: (Exception -> Maybe b) -> IO a -> (b -> IO a) -> IO a
catchJust et c h = catch c $ \e -> maybe (throwIO e) h (et e)

handle :: (Exception -> IO a) -> IO a -> IO a
handle = flip catch

handleJust :: (Exception -> Maybe b) -> (b -> IO a) -> IO a -> IO a
handleJust et h c = catchJust et c h

try :: IO a -> IO (Either Exception a)
try c = catch (fmap Right c) (return . Left)

tryJust :: (Exception -> Maybe b) -> IO a -> IO (Either b a)
tryJust et c = catchJust et (fmap Right c) (return . Left)

-- FIXME this is wrong!
evaluate :: a -> IO a
evaluate = return

-- mapException

ioErrors (IOException _) = True

block, unblock :: IO a -> IO a
block   = id
unblock = id

bracket        :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c
bracket before after m = do
        x  <- before
        rs <- try (m x)
        after x
        case rs of
           Right r -> return r
           Left  e -> throwIO e

bracket_        :: IO a -> (a -> IO b) -> IO c -> IO c
bracket_ before after m = bracket before after (const m)

finally :: IO a -> IO b -> IO a
finally cmd end = bracket_ (return ()) (const end) cmd
